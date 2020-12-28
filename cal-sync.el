
(require 'dash)
(require 'cl-macs)
(require 'subr-x)
(require 'icalendar)
(require 'org)
(require 'org-element)
(require 'ox-org)
(require 'url)

(defun cal-sync-convert-event (buffer)
  "Convert icalendar event buffer.
Returns a list '(start-d start-t end-d end-t summary description location)'
which can be fed into `cal-sync-insert-org-entry'."
  (with-current-buffer (icalendar--get-unfolded-buffer buffer)
    (goto-char (point-min))
    (let* ((ical-list (icalendar--read-element nil nil))
           (zone-map (icalendar--convert-all-timezones ical-list)))
      (->> (mapcar 'caddr (icalendar--all-events ical-list))
           (seq-remove
            (lambda (l) (or (assq 'RRULE l)
                            (assq 'RECURRENCE-ID l))))
           (mapcar
            (lambda (event)
              (cal-sync-enrich-properties event zone-map)))))))


(defun cal-sync-get-property (event property) ;; like icalendar--get-event-property
  (if-let ((value (alist-get property event)))
      (cadr value)))

(defun cal-sync-get-properties (event property) ;; like icalendar--get-event-properties
  (mapconcat 'caddr
             (seq-filter (lambda (prop) (eq (car prop) property)) event)
             ","))

(defun cal-sync-get-attr (event property) ;; like icalendar--get-event-property-attributes
  (if-let ((value (alist-get property event)))
      (car value)))

(defun cal-sync-ical-times (event-properties property &optional zone-map)
  (icalendar--decode-isodatetime
   (cal-sync-get-property event-properties property)
   nil
   (icalendar--find-time-zone
    (cal-sync-get-attr event-properties property)
    zone-map)))

(defun cal-sync-ical-times-span (event &optional zone-map)
  (let* ((dtstart-dec (cal-sync-ical-times event 'DTSTART zone-map))
         (duration (cal-sync-get-property event 'DURATION))
         (dtend-dec (cal-sync-ical-times event 'DTEND zone-map)))
    (when duration
      (let ((dtend-dec-d (icalendar--add-decoded-times
                          dtstart-dec
                          (icalendar--decode-isoduration duration))))
        (when (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
          (message "Inconsistent endtime and duration for %s" summary))
        (setq dtend-dec dtend-dec-d)))
    `((START nil ,(encode-time dtstart-dec)) (END nil ,(encode-time dtend-dec)))))

(defun cal-sync-enrich-properties (event-properties zone-map)
  (let ((summary (icalendar--convert-string-for-import
                  (or (cal-sync-get-property event-properties 'SUMMARY) "No Title"))))
    (append
     (if (string-match "^\\(?:\\(DL\\|S\\):\\s+\\)?\\(.*\\)$" summary)
         `((HEADING nil ,(match-string 2 summary))
           (E-TYPE nil ,(match-string 1 summary)))
       `((HEADING nil ,summary)
         (E-TYPE nil nil)))
     (cal-sync-ical-times-span event-properties zone-map)
     event-properties)))

(defun cal-sync--org-time-range (event-properties)
  (cl-flet ((org-time (time) (-> (cal-sync-get-property event-properties time)
                                 (org-timestamp-from-time t)
                                 (org-timestamp-translate))))
    (concat
     (let ((e-type (cal-sync-get-property event-properties 'E-TYPE)))
       (cond
        ((string= "S" e-type) "SCHEDULED: ")
        ((string= "DL" e-type) "DEADLINE: ")
        (t "")))
     (org-time 'START)
     "--"
     (org-time 'END))))


(defun cal-sync--org-entry (event)
  "Org block from given event data."
  (with-temp-buffer
    (insert  "* " (cal-sync-get-property event 'HEADING) "\n")
    (insert (cal-sync--org-time-range event) "\n")

    (if-let ((uid (cal-sync-get-property event 'UID)))
        (org-set-property "ID" (url-unhex-string uid)))

    (if-let ((location (org-string-nw-p (cal-sync-get-property event 'LOCATION))))
        (org-set-property
         "LOCATION"
         (replace-regexp-in-string "\n" ", "
                                   (icalendar--convert-string-for-import location))))

    (if-let ((description (org-string-nw-p (cal-sync-get-property event 'DESCRIPTION))))
        (insert
         (replace-regexp-in-string "\n " "\n"
                                   (icalendar--convert-string-for-import description)) "\n"))

    (if-let ((categories (org-string-nw-p (cal-sync-get-properties event 'CATEGORIES))))
        (progn
          (org-back-to-heading)
          (org-set-tags (split-string categories "[ ,]+"))))

    (decode-coding-string (buffer-string) 'utf-8)))

(defun cal-sync-events-url (server-url calendar-id)
  "Return URL for events."
  (let ((url (file-name-as-directory server-url)))
    (file-name-as-directory
     (if (string-match ".*%s.*" url)
	 (format url calendar-id)
       (concat url calendar-id)))))

;;; export

(defun cal-sync-entry (entry contents info)
  (cl-flet ((clean (pattern string) (replace-regexp-in-string pattern "" string nil nil 1)))
    (->> (org-icalendar-entry entry contents info)
         (clean "^UID:\\s-*\\(\\(DL\\|SC\\|TS\\)[0-9]*-\\)")
         (clean "^DESCRIPTION:.*?\\(\\s-*<[^>]+>\\(--<[^>]+>\\)?\\(\\\\n\\\\n\\)?\\)"))))

(org-export-define-derived-backend 'caldav 'org
  :translate-alist '((clock . ignore)
		     (footnote-definition . ignore)
		     (footnote-reference . ignore)
		     (headline . cal-sync-entry)
		     (inlinetask . ignore)
		     (planning . ignore)
		     (section . ignore)
		     ;;(inner-template . (lambda (c i) c))
		     (template . org-icalendar-template))
  :options-alist
  '((:exclude-tags
     "ICALENDAR_EXCLUDE_TAGS" nil org-icalendar-exclude-tags split)
    (:with-timestamps nil "<" org-icalendar-with-timestamps)
    ;; Other variables.
    (:icalendar-alarm-time nil nil org-icalendar-alarm-time)
    (:icalendar-categories nil nil org-icalendar-categories)
    (:icalendar-date-time-format nil nil org-icalendar-date-time-format)
    (:icalendar-include-bbdb-anniversaries nil nil org-icalendar-include-bbdb-anniversaries)
    (:icalendar-include-body nil nil org-icalendar-include-body)
    (:icalendar-include-sexps nil nil org-icalendar-include-sexps)
    (:icalendar-include-todo nil nil org-icalendar-include-todo)
    (:icalendar-store-UID nil nil org-icalendar-store-UID)
    (:icalendar-timezone nil nil org-icalendar-timezone)
    (:icalendar-use-deadline nil nil org-icalendar-use-deadline)
    (:icalendar-use-scheduled nil nil org-icalendar-use-scheduled))
  :filters-alist
  '((:filter-headline . org-icalendar-clear-blank-lines)))

(defun cal-sync-error-handling (status buffer)
  (when (or (plist-get status :error)
            (with-current-buffer buffer
              (goto-char (point-min))
              (not (looking-at "HTTP.*2[0-9][0-9]"))))
    (display-buffer buffer)
    t))

(defun cal-sync-org-entry-action (action &optional obj)
  (let ((url-request-method action)
	(url-request-data obj)
	(url-request-extra-headers '(("Content-type" . "text/calendar; charset=UTF-8")))
        (url (concat (cal-sync-events-url cal-sync-url cal-sync-calendar-id)
                     (org-id-get-create) ".ics")))
    (url-retrieve url (lambda (status action title)
                        (unless (cal-sync-error-handling status (current-buffer))
                          (message "%s: \"%s\" successful" action title)))
                  (list action (org-entry-get nil "ITEM")))))

(defun cal-sync-delete ()
  (interactive)
  (cal-sync-org-entry-action "DELETE"))

(defun cal-sync-push ()
  (interactive)
  (if-let ((not-rrule (not (string-match-p "rrule" (or (org-entry-get nil "TAGS") ""))))
           (content (buffer-substring-no-properties
	             (org-entry-beginning-position)
                     (org-entry-end-position)))
           (org-icalendar-categories '(local-tags)))
      (cal-sync-org-entry-action
       "PUT"
       (with-temp-buffer
         (insert content)
         (encode-coding-string
          (org-export-as 'caldav) 'utf-8)))))

(provide 'cal-sync)

;;; cal-sync.el ends here
