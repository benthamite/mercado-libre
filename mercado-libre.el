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

;;;; Functions

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
  
  (let* ((client-id (auth-source-pass-get "app-id" "chrome/mercadolibre.com/benthamite"))
         (client-secret (auth-source-pass-get "app-key" "chrome/mercadolibre.com/benthamite"))
         (token (mercado-libre-get-token client-id client-secret))
         (cache-key (format "%s:%s" query condition))
         (query-db (gethash cache-key mercado-libre-listings-db))
         (last-check-time (when query-db (gethash "last_check_time" query-db)))
         (first-run (or (null query-db) (null last-check-time))))
    
    ;; If this is the first run, do a first-run setup
    (when first-run
      (message "First time monitoring '%s'. Performing initial setup..." query)
      (with-current-buffer (get-buffer-create "*Mercado Libre Monitor*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-mode)
          (insert (format "* Mercado Libre: First-time setup for '%s' (%s)\n\n"
                          query condition))
          (insert "Building initial database. This may take a while...\n")))
      (switch-to-buffer "*Mercado Libre Monitor*")
      
      ;; Do the first run setup
      (mercado-libre-first-run query condition token)
      
      (message "Initial database built. Future checks will show only new listings.")
      (with-current-buffer "*Mercado Libre Monitor*"
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\nSetup complete. Next time you run this command, ")
          (insert "you'll see only new listings since this check.\n")))
      (return-from mercado-libre-monitor))
    
    ;; If we get here, this is a regular monitoring run
    ;; Get the query-specific database (which should now exist)
    (setq query-db (gethash cache-key mercado-libre-listings-db))
    (setq last-check-time (gethash "last_check_time" query-db))
    
    ;; Create result buffer with search info
    (with-current-buffer (get-buffer-create "*Mercado Libre Monitor*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "* Mercado Libre Monitor for: %s (%s)\n\n" query condition))
        (insert (format "Checking for new listings since %s...\n"
                        (format-time-string "%Y-%m-%d %H:%M"
                                            (date-to-time last-check-time))))))
    (switch-to-buffer "*Mercado Libre Monitor*")
    
    ;; Get current time for database update
    (let ((current-time (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" (current-time))))
      
      ;; Fetch all listings
      (message "Fetching all listings for '%s'..." query)
      (let* ((all-listings (mercado-libre-get-all-listings query condition token))
             (new-listings '())
             (new-count 0))
        
        ;; Find new listings (those not in query-db)
        (message "Identifying new listings...")
        (dolist (item all-listings)
          (let ((item-id (cdr (assoc 'id item))))
            (unless (gethash item-id query-db)
              (push item new-listings)
              (setq new-count (1+ new-count)))))
        
        ;; Sort new listings by stop_time (proxy for recency)
        (setq new-listings
              (sort new-listings
                    (lambda (a b)
                      (string> (or (cdr (assoc 'stop_time a)) "")
                               (or (cdr (assoc 'stop_time b)) "")))))
        
        ;; Limit to max-items
        (when (and max-items (> new-count max-items))
          (setq new-listings (seq-take new-listings max-items)))
        
        ;; Only fetch details for the listings we'll show
        (message "Fetching details for %d new listings..." (length new-listings))
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
                (puthash item-id date-created query-db))
              
              ;; Update progress
              (setq count (1+ count))
              (progress-reporter-update progress-reporter count)))
          
          (progress-reporter-done progress-reporter)
          
          ;; Update the last check time in the database
          (puthash "last_check_time" current-time query-db)
          (mercado-libre-save-listings-db)
          
          ;; Display results
          (if (null listings-with-dates)
              (with-current-buffer "*Mercado Libre Monitor*"
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (org-mode)
                  (insert (format "* Mercado Libre Monitor for: %s (%s)\n\n" query condition))
                  (insert (format "No new listings found since last check (%s).\n"
                                  (format-time-string "%Y-%m-%d %H:%M"
                                                      (date-to-time last-check-time))))
                  (insert (format "\nTotal listings tracked: %d\n"
                                  (hash-table-count query-db)))))
            
            ;; Sort by date
            (setq listings-with-dates
                  (sort listings-with-dates
                        (lambda (a b)
                          (string> (car a) (car b)))))
            
            ;; Display the results with date information
            (mercado-libre-display-results
             query
             (format "%s (%d new since %s, showing %d)"
                     condition
                     new-count
                     (format-time-string "%Y-%m-%d" (date-to-time last-check-time))
                     (length listings-with-dates))
             listings-with-dates
             token
             (format "Total listings tracked: %d | Last check: %s"
                     (- (hash-table-count query-db) 1) ; Subtract 1 for last_check_time key
                     (format-time-string "%Y-%m-%d %H:%M" (date-to-time last-check-time))))))))))

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
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n" nil t)
        (condition-case err
            (let ((json-data (json-read)))
              (cdr (assoc 'access_token json-data)))
          (error
           (message "Error parsing token response: %S" err)
           nil))))))

;; Make first-run internal to avoid duplication
(defun mercado-libre-first-run (query condition token)
  "Do the initial run to build the database for QUERY."
  (let* ((cache-key (format "%s:%s" query condition))
         (query-db (make-hash-table :test 'equal))
         (current-time (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" (current-time)))
         (all-listings (mercado-libre-get-all-listings query condition token)))
    
    ;; Store IDs in the query-specific database
    (message "Building database with %d listings..." (length all-listings))
    (dolist (item all-listings)
      (let ((item-id (cdr (assoc 'id item))))
        (puthash item-id nil query-db)))
    
    ;; Set the last check time
    (puthash "last_check_time" current-time query-db)
    
    ;; Store in the main database
    (puthash cache-key query-db mercado-libre-listings-db)
    (mercado-libre-save-listings-db)
    
    (message "Database built with %d listings for '%s'."
             (- (hash-table-count query-db) 1) query)))  ; -1 for last_check_time key

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

;; Load the database when this file is loaded
(mercado-libre-load-listings-db)

(provide 'mercado-libre)
;;; mercado-libre.el ends here

