
(require 'subr-x)
(require 'icalendar)
(require 'org)
(require 'org-element)

(defun cal-sync-convert-event (buffer)
  "Convert icalendar event buffer.
Returns a list '(start-d start-t end-d end-t summary description location)'
which can be fed into `cal-sync-insert-org-entry'."
  (with-current-buffer (icalendar--get-unfolded-buffer buffer)
    (goto-char (point-min))
    (let* ((ical-list (icalendar--read-element nil nil))
           (zone-map (icalendar--convert-all-timezones ical-list)))
      (mapcar (lambda (event)
                (cal-sync-enrich-properties event zone-map))
              (icalendar--all-events ical-list)))))

(defun get-property (event property) ;; like icalendar--get-event-property
  (if-let ((value (alist-get property event)))
      (cadr value)))

(defun get-properties (event property) ;; like icalendar--get-event-properties
  (mapconcat 'caddr
             (seq-filter (lambda (prop) (eq (car prop) property)) event)
             ","))

(defun get-attr (event property) ;; like icalendar--get-event-property-attributes
  (if-let ((value (alist-get property event)))
      (car value)))

(defun -ical-times (event-properties property &optional zone-map)
  (icalendar--decode-isodatetime
   (get-property event-properties property)
   nil
   (icalendar--find-time-zone
    (get-attr event-properties property)
    zone-map)))

(defun -ical-times-span (event &optional zone-map)
  (let* ((dtstart-dec (-ical-times event 'DTSTART zone-map))
         (duration (get-property event 'DURATION))
         (dtend-dec (-ical-times event 'DTEND zone-map)))
    (when duration
      (let ((dtend-dec-d (icalendar--add-decoded-times
                          dtstart-dec
                          (icalendar--decode-isoduration duration))))
        (when (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
          (message "Inconsistent endtime and duration for %s" summary))
        (setq dtend-dec dtend-dec-d)))
    `((START nil ,(encode-time dtstart-dec)) (END nil ,(encode-time dtend-dec)))))

(defun cal-sync-enrich-properties (event zone-map)
  (let* ((event-properties (caddr event))
         (summary (icalendar--convert-string-for-import
                   (or (get-property event-properties 'SUMMARY) "No Title"))))
    (append
     (if (string-match "^\\(?:\\(DL\\|S\\):\\s+\\)?\\(.*\\)$" summary)
         `((HEADING nil ,(match-string 2 summary))
           (E-TYPE nil ,(match-string 1 summary)))
       `((HEADING nil ,summary)
         (E-TYPE nil nil)))
     (-ical-times-span event-properties zone-map)
     event-properties)))

(defun cal-sync--org-time-range (event-properties)
  (let ((e-type (get-property event-properties 'E-TYPE))
        (start (get-property event-properties 'START))
        (end (get-property event-properties 'END)))
    (concat
     (cond
      ((string= "S" e-type) "SCHEDULED: ")
      ((string= "DL" e-type) "DEADLINE: ")
      (t ""))
     (org-timestamp-translate
      (org-timestamp-from-time start t))
     "--"
     (org-timestamp-translate
      (org-timestamp-from-time end t)))))


(defun cal-sync--org-entry (event)
  "Org block from given event data."
  (with-temp-buffer
    (insert  "* " (get-property event 'HEADING) "\n")
    (insert (cal-sync--org-time-range event) "\n")

    (if-let ((uid (get-property event 'UID)))
        (org-set-property "ID" (url-unhex-string uid)))

    (if-let ((location (org-string-nw-p (get-property event 'LOCATION))))
        (org-set-property
         "LOCATION"
         (replace-regexp-in-string "\n" ", "
                                   (icalendar--convert-string-for-import location))))

    (if-let ((description (org-string-nw-p (get-property event 'DESCRIPTION))))
        (insert
         (replace-regexp-in-string "\n " "\n"
                                   (icalendar--convert-string-for-import description)) "\n"))

    (if-let ((categories (org-string-nw-p (get-properties event 'CATEGORIES))))
        (progn
          (org-back-to-heading)
          (org-set-tags (split-string categories "[ ,]+"))))

    (buffer-string)))

(defun cal-sync-push ()
  (interactive)
  (let ((content (buffer-substring-no-properties
	          (org-entry-beginning-position)
                  (org-entry-end-position)))
        (org-icalendar-exclude-tags '("rrule"))
        (org-icalendar-categories '(local-tags))
        (uid (org-id-get-create)))

    (org-caldav-save-resource
     (concat (org-caldav-events-url) uid org-caldav-uuid-extension)
     (encode-coding-string
      (with-current-buffer
          (with-temp-buffer
            (insert content)
            (org-export-to-buffer 'caldav "testa" nil))
        (buffer-string))
      'utf-8))))

(provide 'cal-sync)

;;; cal-sync.el ends here
