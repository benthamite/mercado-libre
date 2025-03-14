;;; mercado-libre.el --- Query Mercado Libre from the comfort of Emacs -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/mercado-libre
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Query Mercado Libre from the comfort of Emacs.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'auth-source-pass)
(require 'json)
(require 'url-vars)

;;;; Variables

(defgroup mercado-libre ()
  "Query Mercado Libre from the comfort of Emacs."
  :group 'emacs)

(defcustom mercado-libre-listings-db-file
  (expand-file-name "mercado-libre-listings.el" user-emacs-directory)
  "File to store the database of previously seen Mercado Libre listings."
  :type 'file
  :group 'mercado-libre)

(defvar mercado-libre-listings-db (make-hash-table :test 'equal)
  "Hash table of previously seen Mercado Libre listings.
Key is `query:condition', value is a hash table of `(item-id .
timestamp)'.")

(defvar mercado-libre-item-cache (make-hash-table :test 'equal)
  "Cache for Mercado Libre item details to avoid redundant API calls.")

(defvar mercado-libre-buffer-name "*Mercado Libre Monitor*"
  "Buffer name for displaying Mercado Libre results.")

;;;; API Access

(defun mercado-libre-get-credentials ()
  "Get Mercado Libre API credentials from auth-source."
  (let ((client-id (auth-source-pass-get "app-id" "chrome/mercadolibre.com/benthamite"))
        (client-secret (auth-source-pass-get "app-key" "chrome/mercadolibre.com/benthamite")))
    (cons client-id client-secret)))

(defun mercado-libre-get-token (client-id client-secret)
  "Get auth token for Mercado Libre API using CLIENT-ID and CLIENT-SECRET."
  (if (or (null client-id) (null client-secret))
      (progn
        (message "Error: Missing Mercado Libre API credentials")
        nil)
    (let* ((url "https://api.mercadolibre.com/oauth/token")
           (url-request-method "POST")
           (url-request-extra-headers
            '(("Accept" . "application/json")
              ("Content-Type" . "application/x-www-form-urlencoded")))
           (url-request-data
            (format "grant_type=client_credentials&client_id=%s&client_secret=%s"
                    client-id client-secret))
           (response-buffer (url-retrieve-synchronously url t)))
      (mercado-libre-extract-token-from-response response-buffer))))

(defun mercado-libre-extract-token-from-response (buffer)
  "Extract access token from response BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n" nil t)
    (condition-case err
        (let ((json-data (json-read)))
          (cdr (assoc 'access_token json-data)))
      (error
       (message "Error parsing token response: %S" err)
       nil))))

(defun mercado-libre-make-auth-header (token)
  "Create authorization header using TOKEN."
  `(("Authorization" . ,(format "Bearer %s" token))))

;;;; Item Fetching

(defun mercado-libre-get-condition-param (condition)
  "Get API parameter for CONDITION (used/new/all)."
  (unless (string= condition "all")
    (format "&ITEM_CONDITION=%s"
            (if (string= condition "used") "2230581" "2230284"))))

(defun mercado-libre-build-search-url (query condition offset limit-per-page)
  "Build search URL for QUERY with CONDITION, OFFSET and LIMIT-PER-PAGE."
  (format "https://api.mercadolibre.com/sites/MLA/search?q=%s&limit=%d&offset=%d%s"
          (url-hexify-string query)
          limit-per-page offset
          (or (mercado-libre-get-condition-param condition) "")))

(defun mercado-libre-fetch-page (url token)
  "Fetch Mercado Libre search results from URL using TOKEN."
  (let* ((url-request-method "GET")
         (url-request-extra-headers (mercado-libre-make-auth-header token))
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (json-buffer (url-retrieve-synchronously url t)))
    (with-current-buffer json-buffer
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n" nil t)
      (json-read))))

(defun mercado-libre-get-all-listings (query condition token &optional limit)
  "Get all Mercado Libre listings for QUERY and CONDITION.
Uses pagination to get all results up to LIMIT (if provided). TOKEN is
the auth token."
  (let* ((offset 0)
         (limit-per-page 50)
         (all-results '())
         (more-results t)
         (progress-reporter (make-progress-reporter
                             (format "Fetching all listings for '%s'..." query) 0 100))
         (total-count nil))
    
    (while (and more-results (or (null limit) (< (length all-results) limit)))
      (let* ((url (mercado-libre-build-search-url query condition offset limit-per-page))
             (json-data (mercado-libre-fetch-page url token))
             (results (cdr (assoc 'results json-data)))
             (paging (cdr (assoc 'paging json-data))))
        
        ;; Get total count if we don't have it yet
        (unless total-count
          (setq total-count (or (cdr (assoc 'total paging)) 0))
          (mercado-libre-update-progress-message progress-reporter 0 total-count query))
        
        ;; Add results to our collection
        (setq all-results (append all-results results))
        
        ;; Update progress
        (progress-reporter-update
         progress-reporter
         (min 99 (* 100 (/ (float (length all-results)) (float (or total-count 1))))))
        
        ;; Check if we have more pages
        (setq offset (+ offset limit-per-page))
        (setq more-results (and results
                                (> (length results) 0)
                                (< offset (or total-count 0)))))
      
      ;; Sleep briefly to avoid overloading the API
      (sleep-for 0.2))
    
    (progress-reporter-done progress-reporter)
    all-results))

(defun mercado-libre-update-progress-message (reporter value total query)
  "Update REPORTER with appropriate message for VALUE, TOTAL and QUERY."
  (progress-reporter-force-update
   reporter value
   (format "Fetching %d listings for '%s'..." total query)))

(defun mercado-libre-get-item-details (item-id token)
  "Get detailed information about a specific item by ITEM-ID.
TOKEN is the auth token."
  (let* ((url (format "https://api.mercadolibre.com/items/%s" item-id))
         (url-request-method "GET")
         (url-request-extra-headers (mercado-libre-make-auth-header token))
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (json-buffer (url-retrieve-synchronously url t)))
    (with-current-buffer json-buffer
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n" nil t)
      (condition-case err
          (json-read)
        (error
         (message "Error getting item details for %s: %S" item-id err)
         nil)))))

(defun mercado-libre-get-item-details-cached (item-id token)
  "Get detailed information about a specific item by ITEM-ID with caching.
TOKEN is the auth token."
  (or (gethash item-id mercado-libre-item-cache)
      (let ((details (mercado-libre-get-item-details item-id token)))
        (when details
          (puthash item-id details mercado-libre-item-cache))
        details)))

;;;; Database Management

(defun mercado-libre-load-listings-db ()
  "Load the listings database from disk."
  (when (file-exists-p mercado-libre-listings-db-file)
    (with-temp-buffer
      (insert-file-contents mercado-libre-listings-db-file)
      (condition-case nil
          (setq mercado-libre-listings-db (read (current-buffer)))
        (error
         (message "Error reading Mercado Libre listings database. Starting fresh.")
         (setq mercado-libre-listings-db (make-hash-table :test 'equal)))))))

(defun mercado-libre-save-listings-db ()
  "Save the listings database to disk."
  (with-temp-file mercado-libre-listings-db-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 mercado-libre-listings-db (current-buffer)))))

(defun mercado-libre-get-query-db (query condition)
  "Get or create database for QUERY and CONDITION."
  (let ((cache-key (format "%s:%s" query condition)))
    (or (gethash cache-key mercado-libre-listings-db)
        (puthash cache-key (make-hash-table :test 'equal) mercado-libre-listings-db))))

(defun mercado-libre-update-query-db (query-db item-id value)
  "Store VALUE for ITEM-ID in QUERY-DB."
  (puthash item-id value query-db))

(defun mercado-libre-update-last-check-time (query-db)
  "Update last check time in QUERY-DB to current time."
  (let ((current-time (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" (current-time))))
    (puthash "last_check_time" current-time query-db)
    current-time))

(defun mercado-libre-get-last-check-time (query-db)
  "Get last check time from QUERY-DB."
  (gethash "last_check_time" query-db))

;;;; Display Functions

(defun mercado-libre-setup-result-buffer (title)
  "Set up the result buffer with TITLE."
  (with-current-buffer (get-buffer-create mercado-libre-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-mode)
      (insert title))))

(defun mercado-libre-create-progress-reporter (message-format query)
  "Create a progress reporter with MESSAGE-FORMAT for QUERY."
  (make-progress-reporter
   (format message-format query) 0 100))

(defun mercado-libre-prepare-image-dir ()
  "Create directory for Mercado Libre images if it doesn't exist."
  (let ((dir (expand-file-name "mercado-libre-images" temporary-file-directory)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun mercado-libre-download-image (url file)
  "Download image from URL to FILE."
  (when (and url (not (file-exists-p file)))
    (condition-case err
        (url-copy-file url file t)
      (error
       (message "Error downloading image %s: %S" url err)))))

(defun mercado-libre-get-best-image-url (item item-id)
  "Get the best available image URL for ITEM with ITEM-ID."
  (let ((image-url nil))
    ;; Try to get a better quality image from the item details
    (let ((item-details (gethash item-id mercado-libre-item-cache)))
      (when item-details
        (let ((pictures (cdr (assoc 'pictures item-details))))
          (when (and pictures (> (length pictures) 0))
            ;; Get the first picture's highest quality URL
            (let* ((first-pic (car pictures))
                   (secure-url (cdr (assoc 'secure_url first-pic))))
              (when secure-url
                (setq image-url secure-url)))))))
    
    ;; Fall back to thumbnail if we couldn't get a better image
    (or image-url (cdr (assoc 'thumbnail item)))))

(defun mercado-libre-format-item (item date-created image-file)
  "Format ITEM with DATE-CREATED and IMAGE-FILE for display."
  (let ((title (cdr (assoc 'title item)))
        (price (cdr (assoc 'price item)))
        (currency (cdr (assoc 'currency_id item)))
        (url (cdr (assoc 'permalink item)))
        (item-condition (cdr (assoc 'condition item)))
        (result ""))
    
    ;; Build result string
    (setq result (concat result (format "** %s\n" (or title "No Title"))))
    (setq result (concat result "   :PROPERTIES:\n"))
    (when price
      (setq result (concat result (format "   :PRICE: %s %s\n" (or currency "") price))))
    (when item-condition
      (setq result (concat result (format "   :CONDITION: %s\n" item-condition))))
    (setq result (concat result (format "   :PUBLISHED: %s\n"
                                        (format-time-string
                                         "%Y-%m-%d"
                                         (date-to-time date-created)))))
    (setq result (concat result "   :END:\n"))
    
    ;; Add image if available
    (when (and image-file (file-exists-p image-file))
      (setq result (concat result (format "[[file:%s]]\n\n" image-file))))
    
    ;; Add URL if available
    (when url
      (setq result (concat result (format "[[%s][View on Mercado Libre]]\n\n" url))))
    
    result))

(defun mercado-libre-insert-formatted-items (items-with-dates)
  "Insert formatted ITEMS-WITH-DATES into current buffer."
  (let ((image-dir (mercado-libre-prepare-image-dir)))
    (dolist (item-with-date items-with-dates)
      (let* ((date-created (car item-with-date))
             (item (cdr item-with-date))
             (item-id (cdr (assoc 'id item)))
             (image-url (mercado-libre-get-best-image-url item item-id))
             (image-file (when image-url
                           (expand-file-name (format "%s.jpg" (md5 image-url)) image-dir))))
        
        ;; Download image
        (when image-url
          (mercado-libre-download-image image-url image-file))
        
        ;; Insert formatted item
        (insert (mercado-libre-format-item item date-created image-file))))))

(defun mercado-libre-display-results (query condition items-with-dates &optional footer)
  "Display sorted ITEMS-WITH-DATES in a nicely formatted buffer.
FOOTER is optional text to display at the bottom of the results. QUERY and
and CONDITION are used for the buffer title."
  (message "Displaying %d sorted results" (length items-with-dates))
  (with-current-buffer (get-buffer-create mercado-libre-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-mode)
      (insert (format "* Mercado Libre Results for: %s (%s)\n\n"
                      query condition))
      
      ;; Insert items
      (mercado-libre-insert-formatted-items items-with-dates)
      
      ;; Add footer if provided
      (when footer
        (insert (format "* %s\n" footer)))))
  
  ;; Display buffer and show images
  (mercado-libre-display-buffer-with-images))

(defun mercado-libre-display-buffer-with-images ()
  "Display buffer and show inline images."
  (let ((window (display-buffer-pop-up-window (get-buffer mercado-libre-buffer-name) nil)))
    (when window
      (with-selected-window window
        (goto-char (point-min))
        (org-display-inline-images))))
  (org-display-inline-images))

(defun mercado-libre-display-no-results (query condition last-check-time query-db)
  "Display message when no new listings found.
QUERY, CONDITION, LAST-CHECK-TIME and QUERY-DB provide context."
  (with-current-buffer (get-buffer-create mercado-libre-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-mode)
      (insert (format "* Mercado Libre Monitor for: %s (%s)\n\n" query condition))
      (insert (format "No new listings found since last check (%s).\n"
                      (format-time-string "%Y-%m-%d %H:%M"
                                          (date-to-time last-check-time))))
      (insert (format "\nTotal listings tracked: %d\n"
                      (hash-table-count query-db))))))

;;;; Core Functionality

(defun mercado-libre-first-run (query condition token)
  "Do the initial run to build the database for QUERY.
CONDITION is \"used\", \"new\" or \"all\". TOKEN is the auth token."
  (let* ((query-db (mercado-libre-get-query-db query condition))
         (all-listings (mercado-libre-get-all-listings query condition token)))
    
    ;; Store IDs in the query-specific database
    (message "Building database with %d listings..." (length all-listings))
    (dolist (item all-listings)
      (let ((item-id (cdr (assoc 'id item))))
        (mercado-libre-update-query-db query-db item-id nil)))
    
    ;; Set the last check time
    (mercado-libre-update-last-check-time query-db)
    
    ;; Save the database
    (mercado-libre-save-listings-db)
    
    (message "Database built with %d listings for '%s'."
             (- (hash-table-count query-db) 1) query)))  ; -1 for last_check_time key

(defun mercado-libre-setup-first-run (query condition)
  "Set up the initial monitoring for QUERY with CONDITION."
  (message "First time monitoring '%s'. Performing initial setup..." query)
  (with-current-buffer (get-buffer-create mercado-libre-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-mode)
      (insert (format "* Mercado Libre: First-time setup for '%s' (%s)\n\n"
                      query condition))
      (insert "Building initial database. This may take a while...\n")))
  (switch-to-buffer mercado-libre-buffer-name))

(defun mercado-libre-complete-first-run ()
  "Complete the first run setup with success message."
  (message "Initial database built. Future checks will show only new listings.")
  (with-current-buffer mercado-libre-buffer-name
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\nSetup complete. Next time you run this command, ")
      (insert "you'll see only new listings since this check.\n"))))

(defun mercado-libre-find-new-listings (all-listings query-db)
  "Find listings in ALL-LISTINGS that don't exist in QUERY-DB."
  (let ((new-listings '())
        (new-count 0))
    (dolist (item all-listings)
      (let ((item-id (cdr (assoc 'id item))))
        (unless (gethash item-id query-db)
          (push item new-listings)
          (setq new-count (1+ new-count)))))
    (cons new-listings new-count)))

(defun mercado-libre-sort-listings-by-date (listings)
  "Sort LISTINGS by date (most recent first)."
  (sort listings
        (lambda (a b)
          (string> (or (cdr (assoc 'stop_time a)) "")
                   (or (cdr (assoc 'stop_time b)) "")))))

(defun mercado-libre-fetch-details-for-new-listings (new-listings token query-db)
  "Fetch details for NEW-LISTINGS using TOKEN and store in QUERY-DB."
  (let ((listings-with-dates '())
        (progress-reporter (make-progress-reporter
                            "Fetching details for new items..." 0 (length new-listings)))
        (count 0))
    
    (dolist (item new-listings)
      (let* ((item-id (cdr (assoc 'id item)))
             (item-details (mercado-libre-get-item-details-cached item-id token))
             (date-created (cdr (assoc 'date_created item-details))))
        
        ;; Only include items where we got a valid date
        (when date-created
          (push (cons date-created item) listings-with-dates)
          ;; Add to our database with the creation date
          (mercado-libre-update-query-db query-db item-id date-created))
        
        ;; Update progress
        (setq count (1+ count))
        (progress-reporter-update progress-reporter count)))
    
    (progress-reporter-done progress-reporter)
    listings-with-dates))

(defun mercado-libre-format-condition-header (condition new-count last-check-time count)
  "Format condition header with CONDITION, NEW-COUNT, LAST-CHECK-TIME and COUNT."
  (format "%s (%d new since %s, showing %d)"
          condition
          new-count
          (format-time-string "%Y-%m-%d" (date-to-time last-check-time))
          count))

(defun mercado-libre-format-footer (query-db last-check-time)
  "Format footer with QUERY-DB and LAST-CHECK-TIME information."
  (format "Total listings tracked: %d | Last check: %s"
          (- (hash-table-count query-db) 1) ; Subtract 1 for last_check_time key
          (format-time-string "%Y-%m-%d %H:%M" (date-to-time last-check-time))))

;;;###autoload
(defun mercado-libre-monitor (query &optional condition max-items)
  "Monitor for new Mercado Libre listings matching QUERY.
Shows only listings that have appeared since the last check. For
first-time queries, performs initial database setup automatically.
With prefix arg, prompt for CONDITION (used/new/all). With
\\[universal-argument] \\[universal-argument] prefix, also prompt for
MAX-ITEMS to display."
  (interactive
   (list (read-string "Monitor Mercado Libre for: ")
         (if current-prefix-arg
             (completing-read "Condition: " '("used" "new" "all") nil t "used")
           "used")
         (if (and current-prefix-arg (>= (prefix-numeric-value current-prefix-arg) 16))
             (read-number "Maximum new items to show: " 20)
           20)))
  
  (let* ((credentials (mercado-libre-get-credentials))
         (client-id (car credentials))
         (client-secret (cdr credentials))
         (token (mercado-libre-get-token client-id client-secret))
         (query-db (mercado-libre-get-query-db query condition))
         (last-check-time (mercado-libre-get-last-check-time query-db))
         (first-run (or (null query-db) (null last-check-time))))
    
    ;; Handle first run
    (when first-run
      (mercado-libre-setup-first-run query condition)
      (mercado-libre-first-run query condition token)
      (mercado-libre-complete-first-run)
      (keyboard-quit))
    
    ;; Regular monitoring
    (let* ((query-db (mercado-libre-get-query-db query condition))
           (last-check-time (mercado-libre-get-last-check-time query-db)))
      
      ;; Set up result buffer
      (mercado-libre-setup-result-buffer
       (format "* Mercado Libre Monitor for: %s (%s)\n\n" query condition))
      (with-current-buffer mercado-libre-buffer-name
        (let ((inhibit-read-only t))
          (insert (format "Checking for new listings since %s...\n"
                          (format-time-string "%Y-%m-%d %H:%M"
                                              (date-to-time last-check-time))))))
      (switch-to-buffer mercado-libre-buffer-name)
      
      ;; Fetch listings and process results
      (message "Fetching all listings for '%s'..." query)
      (let* ((all-listings (mercado-libre-get-all-listings query condition token))
             (new-results (mercado-libre-find-new-listings all-listings query-db))
             (new-listings (car new-results))
             (new-count (cdr new-results)))
        
        ;; Sort and limit new listings
        (setq new-listings (mercado-libre-sort-listings-by-date new-listings))
        (when (and max-items (> new-count max-items))
          (setq new-listings (seq-take new-listings max-items)))
        
        ;; Fetch details for new listings
        (message "Fetching details for %d new listings..." (length new-listings))
        (let ((listings-with-dates (mercado-libre-fetch-details-for-new-listings new-listings token query-db)))
          
          ;; Update last check time
          (mercado-libre-update-last-check-time query-db)
          (mercado-libre-save-listings-db)
          
          ;; Display results
          (if (null listings-with-dates)
              (mercado-libre-display-no-results query condition last-check-time query-db)
            
            ;; Sort listings by date
            (setq listings-with-dates
                  (sort listings-with-dates
                        (lambda (a b)
                          (string> (car a) (car b)))))
            
            ;; Display results
            (mercado-libre-display-results
             query
             (mercado-libre-format-condition-header
              condition new-count last-check-time (length listings-with-dates))
             listings-with-dates
             (mercado-libre-format-footer query-db last-check-time))))))))

;; Load the database when this file is loaded
(mercado-libre-load-listings-db)

(provide 'mercado-libre)
;;; mercado-libre.el ends here
