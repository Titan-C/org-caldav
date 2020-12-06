
(require 'icalendar)

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

(defun get-prop (event property) ;; equivalent icalendar--get-event-property
  (if-let ((value (alist-get property event)))
      (cadr value)))

(defun get-attr (event property) ;; equivalent icalendar--get-event-property-attributes
  (if-let ((value (alist-get property event)))
      (car value)))

(defun -ical-times (event-properties property &optional zone-map)
  (icalendar--decode-isodatetime
   (get-prop event-properties property)
   nil
   (icalendar--find-time-zone
    (get-attr event-properties property)
    zone-map)))

(defun -ical-times-span (event &optional zone-map)
  (let* ((dtstart-dec (-ical-times event 'DTSTART zone-map))
         (duration (get-prop event 'DURATION))
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
                   (or (get-prop event-properties 'SUMMARY) "No Title"))))
    (append
     (if (string-match "^\\(?:\\(DL\\|S\\):\\s+\\)?\\(.*\\)$" summary)
         `((HEADING nil ,(match-string 2 summary))
           (E-TYPE nil ,(match-string 1 summary)))
       `((HEADING nil ,summary)
         (E-TYPE nil nil)))
     (-ical-times-span event-properties zone-map)
     event-properties)))

(defun cal-sync--org-time-range (event-properties)
  (let ((e-type (get-prop event-properties 'E-TYPE))
        (start (get-prop event-properties 'START))
        (end (get-prop event-properties 'END)))
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
    (insert  "* " (get-prop event 'HEADING) "\n")
    (insert (cal-sync--org-time-range event) "\n")

    (if-let ((uid (get-prop event 'UID)))
        (org-set-property "ID" (url-unhex-string uid)))

    (if-let ((location (get-prop event 'LOCATION)))
        (and (org-string-nw-p location)
             (org-set-property
              "LOCATION"
              (replace-regexp-in-string "\n" ", "
                                        (icalendar--convert-string-for-import location)))))

    (if-let ((description (get-prop event 'DESCRIPTION)))
        (and (org-string-nw-p description)
             (insert (icalendar--convert-string-for-import description) "\n")))

    (if-let ((categories (get-prop event 'CATEGORIES)))
        (progn
          (org-back-to-heading)
          (org-set-tags (split-string categories "[ ,]+"))))

    (buffer-string)))

(let* ((buf (find-file-noselect "/tmp/agenda-36afc42317"))
       (events (cal-sync-convert-event buf)))
  (kill-buffer buf)
  (with-current-buffer (get-buffer-create "org-agen")
    (dolist (ev events)
      (insert (cal-sync--org-entry ev)))))
