;;; cal-sync.el --- Pushing org events to caldav server -*- lexical-binding: t; -*-
;;
;; Version: 0.0.1
;; Homepage: https://github.com/titan-c/org-caldav
;; Package-Requires: ((emacs "27.1"))
;;
;;; Commentary:
;;
;; Export org entries that represent events to a caldav server.
;;
;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'subr-x)
(require 'icalendar)
(require 'org)
(require 'org-element)
(require 'ox-org)
(require 'url)

(defgroup cal-sync nil
  "Web Calendar configuration."
  :group 'external)

(defcustom cal-sync-url ""
  "Caldav server url.
For a nextcloud server it looks like this:
https://nextcloud-server-url/remote.php/dav/calendars/USERID"
  :type 'string)

(defcustom cal-sync-calendar-id ""
  "Calendar id."
  :type 'string)

(defun cal-sync-convert-event (buffer)
  "Convert icalendar event in BUFFER.
Returns a list '(start-d start-t end-d end-t summary description location)'
which can be fed into `cal-sync-insert-org-entry'."
  (with-current-buffer (icalendar--get-unfolded-buffer buffer)
    (goto-char (point-min))
    (let* ((ical-list (icalendar--read-element nil nil))
           (zone-map (icalendar--convert-all-timezones ical-list)))
      (--keep (unless (or (assq 'RRULE it) (assq 'RECURRENCE-ID it))
                (cal-sync-enrich-properties it zone-map))
              (mapcar 'caddr (icalendar--all-events ical-list))))))

(defun cal-sync-get-property (event property) ;; like icalendar--get-event-property
  "Get the correct PROPERTY from EVENT.
Wrapper around `alist-get' that understands EVENT structure."
  (-some-> (alist-get property event) (cadr)))

(defun cal-sync-get-properties (event property) ;; like icalendar--get-event-properties
  "Collect as comma separated string all occurrences of PROPERTY in EVENT."
  (mapconcat 'caddr
             (--filter (eq (car it) property) event)
             ","))

(defun cal-sync-get-attr (event property) ;; like icalendar--get-event-property-attributes
  "Get the correct attribute of PROPERTY from EVENT.
Wrapper around `alist-get' that understands EVENT structure."
  (-some-> (alist-get property event) (car)))

(defun cal-sync-ical-times (event time-property &optional zone-map)
  "Return the iso date string of TIME-PROPERTY from EVENT considering ZONE-MAP.
TIME-PROPERTY can be DTSTART, DTEND, DURATION"
  (--> (cal-sync-get-attr event time-property)
       (icalendar--find-time-zone it zone-map)
       (icalendar--decode-isodatetime
        (cal-sync-get-property event time-property)
        nil it)))

(defun cal-sync-ical-times-span (event summary &optional zone-map)
  "Calculate the start and end times of EVENT considering ZONE-MAP.
SUMMARY is for warning message to recognize event."
  (let ((dtstart-dec (cal-sync-ical-times event 'DTSTART zone-map))
        (dtend-dec (cal-sync-ical-times event 'DTEND zone-map)))
    (when-let ((duration (cal-sync-get-property event 'DURATION))
               (dtend-dec-d (icalendar--add-decoded-times
                             dtstart-dec
                             (icalendar--decode-isoduration duration))))
      (when (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
        (message "Inconsistent endtime and duration for %s" summary))
      (setq dtend-dec dtend-dec-d))
    `((START nil ,(encode-time dtstart-dec))
      (END nil ,(encode-time dtend-dec)))))

(defun cal-sync-enrich-properties (event-properties zone-map)
  "Add additional properties to EVENT-PROPERTIES considering ZONE-MAP."
  (let ((summary (icalendar--convert-string-for-import
                  (or (cal-sync-get-property event-properties 'SUMMARY)
                      "No Title"))))
    (append
     (if (string-match "^\\(?:\\(DL\\|S\\):\\s+\\)?\\(.*\\)$" summary)
         `((HEADING nil ,(match-string 2 summary))
           (E-TYPE nil ,(match-string 1 summary)))
       `((HEADING nil ,summary)
         (E-TYPE nil nil)))
     (cal-sync-ical-times-span event-properties summary zone-map)
     event-properties)))

(defun cal-sync--org-time-range (event-properties)
  "Construct `org-mode' timestamp range out of the EVENT-PROPERTIES."
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
  "Org block from given EVENT data."
  (cl-flet ((prop (symbol)
                  (-some-> (cal-sync-get-properties event symbol)
                    (org-string-nw-p)
                    (string-trim))))
    (with-temp-buffer
      (org-mode)
      (insert  "* " (cal-sync-get-property event 'HEADING) "\n")
      (insert (cal-sync--org-time-range event) "\n")

      (-some->> (prop 'UID)
        (url-unhex-string)
        (org-set-property "ID"))

      (-some->> (prop 'LOCATION)
        (icalendar--convert-string-for-import)
        (replace-regexp-in-string "\n" ", ")
        (org-set-property "LOCATION"))

      (-some--> (prop 'DESCRIPTION)
        (icalendar--convert-string-for-import it)
        (replace-regexp-in-string "\n " "\n" it)
        (insert it "\n"))

      (org-back-to-heading)
      (-some-> (prop 'CATEGORIES)
        (split-string "[ ,]+")
        (org-set-tags))

      (buffer-string))))

(defun cal-sync-events-url (server-url calendar-id)
  "Compose the events URL out of SERVER-URL and the CALENDAR-ID."
  (let ((url (file-name-as-directory server-url)))
    (file-name-as-directory
     (if (string-match ".*%s.*" url)
         (format url calendar-id)
       (concat url calendar-id)))))

;;; export

(defun cal-sync-entry (entry contents info)
  "Transcode ENTRY element into iCalendar format.

ENTRY is either a headline or an inlinetask.  CONTENTS is
ignored.  INFO is a plist used as a communication channel.

This cleans up the output of `org-icalendar-entry'."
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
  "Utility function to signal errors when communicating to server.
STATUS is the request response status.
BUFFER is the request buffer."
  (when (or (plist-get status :error)
            (with-current-buffer buffer
              (goto-char (point-min))
              (not (looking-at "HTTP.*2[0-9][0-9]"))))
    (display-buffer buffer)
    t))

(defun cal-sync-org-entry-action (action &optional obj)
  "Execute a request ACTION on server.
OBJ contains all data to send to server."
  (let ((url-request-method action)
        (url-request-data obj)
        (url-request-extra-headers '(("Content-type" . "text/calendar; charset=UTF-8")))
        (url (concat (cal-sync-events-url cal-sync-url cal-sync-calendar-id)
                     (org-id-get-create) ".ics")))
    (url-retrieve url (lambda (status action title)
                        (unless (cal-sync-error-handling status (current-buffer))
                          (message "%s: \"%s\" successful" action title)))
                  (list action (org-entry-get nil "ITEM")))))

(defun cal-sync-import-file (ics-file)
  "Import an ICS-FILE into the main agenda file."
  (interactive (list (read-file-name "Calendar ics file: ")))
  (with-temp-buffer
    (set-buffer-file-coding-system 'utf-8-unix)
    (insert-file-contents ics-file)
    (write-region
     (mapconcat #'cal-sync--org-entry (cal-sync-convert-event (current-buffer)) "")
     nil "~/org/caldav.org" t)))


(defun cal-sync-delete ()
  "Delete current org node on the server."
  (interactive)
  (cal-sync-org-entry-action "DELETE"))

(defun cal-sync-push ()
  "Push current org node to the server."
  (interactive)
  ;; Need id before processing, otherwise when pushing content server will create a new one
  ;; and there will be a conflict of file UID and event UID, that shows up after download.
  (org-id-get-create)
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
