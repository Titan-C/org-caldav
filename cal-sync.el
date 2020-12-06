
(require 'icalendar)

(defun cal-sync-convert-event (buffer)
  "Convert icalendar event buffer.
Returns a list '(start-d start-t end-d end-t summary description location)'
which can be fed into `cal-sync-insert-org-entry'."
  (with-current-buffer (icalendar--get-unfolded-buffer buffer)
    (goto-char (point-min))
    (let* ((calendar-date-style 'european)
           (ical-list (icalendar--read-element nil nil))
           (zone-map (icalendar--convert-all-timezones ical-list)))
      (mapcar (lambda (event)
                (cal-sync-create-org-entry event zone-map))
              (icalendar--all-events ical-list)))))

(defun cal-sync-create-org-entry (event zone-map)
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
      `((START nil ,dtstart-dec) (END nil ,dtend-dec))))

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

(let* ((buf (find-file-noselect "/tmp/agenda-ad095ce084"))
       (events (cal-sync-convert-event buf)))
  (kill-buffer buf)
  events)
