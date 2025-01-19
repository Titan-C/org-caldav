;;; cal-sync.el --- Pushing org events to caldav server -*- lexical-binding: t; -*-
;;
;; Version: 0.0.1
;; Homepage: https://github.com/titan-c/org-caldav
;; Package-Requires: ((emacs "28.1"))
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

(cl-defstruct (cal-sync-calendar (:constructor cal-sync-calendar--create))
  "A calendar definition."
  url host user file)

(defun cal-sync-calendar-create (url user file)
  "Create caldav calendar connection.
URL is the caldav endpoint for the calendar.
  For a nextcloud server it looks like this:
  https://nextcloud-server-url/remote.php/dav/calendars/USERID
USER is to search in authinfo for login data.
FILE is where to calendar is locally stored."
  (unless (file-exists-p file) (user-error "File `%s' does not exist" file))
  (let ((host (url-host (url-generic-parse-url url))))
    (cal-sync-calendar--create :url (file-name-as-directory url)
                               :host host :user user :file file)))

(defun cal-sync--auth-header (calendar)
  "Generate the Basic authentication header for CALENDAR."
  (when-let ((auth (auth-source-search
                    :host (cal-sync-calendar-host calendar)
                    :user (cal-sync-calendar-user calendar))))
    (-let (((&plist :user :secret) (car auth)))
      (thread-last
        (format "%s:%s" user (funcall secret))
        (base64-encode-string)
        (concat "Basic ")))))

(defcustom cal-sync-connection nil
  "Connection to Caldav calendar.
Create with function `cal-sync-calendar-create'."
  :type 'cal-sync-calendar)

(defun cal-sync-parse (buffer)
  "Parse icalendar BUFFER. Return a list of all events."
  (with-current-buffer (icalendar--get-unfolded-buffer buffer)
    (goto-char (point-min))
    (let* ((ical-list (icalendar--read-element nil nil))
           (zone-map (icalendar--convert-all-timezones ical-list)))
      (thread-last
        (mapcan (lambda (e) (icalendar--get-children e 'VEVENT)) ical-list)
        (--keep
         (unless (or (assq 'RRULE (caddr it)) (assq 'RECURRENCE-ID (caddr it)))
           (cal-sync-enrich-properties (caddr it) zone-map)))))))

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
    (cl-flet ((org-time (time)
                (cl-destructuring-bind (sec min hour . rest) time
                  (org-timestamp-translate
                   (org-timestamp-from-time
                    (encode-time time)
                    (not (= 0 sec min hour)))))))
      `((ORG-TIME nil ,(concat (org-time dtstart-dec)
                               (when dtend-dec
                                 (concat "--" (org-time dtend-dec)))))))))

(defun cal-sync-enrich-properties (event-properties zone-map)
  "Add additional properties to EVENT-PROPERTIES considering ZONE-MAP."
  (let ((summary (icalendar--convert-string-for-import
                  (or (cal-sync-get-property event-properties 'SUMMARY)
                      "No Title")))
        (e-type-rx
         (rx bol
             (group (or (literal org-icalendar-deadline-summary-prefix)
                        (literal org-icalendar-scheduled-summary-prefix)))
             (group (+ any)) eol)))
    (append
     (if (string-match e-type-rx summary)
         `((HEADING nil ,(match-string 2 summary))
           (E-TYPE nil ,(match-string 1 summary)))
       `((HEADING nil ,summary)
         (E-TYPE nil nil)))
     (cal-sync-ical-times-span event-properties summary zone-map)
     event-properties)))

(defun cal-sync--org-time-range (event-properties)
  "Construct `org-mode' timestamp range out of the EVENT-PROPERTIES."
  (concat
   (pcase (cal-sync-get-property event-properties 'E-TYPE)
     ((pred (string-equal org-icalendar-deadline-summary-prefix)) "DEADLINE: ")
     ((pred (string-equal org-icalendar-scheduled-summary-prefix)) "SCHEDULED: ")
     (_ ""))
   (cal-sync-get-property event-properties 'ORG-TIME)))

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

      (buffer-substring-no-properties (point-min) (point-max)))))

;;; export

(defun cal-sync-entry (entry contents info)
  "Transcode ENTRY element into iCalendar format.

ENTRY is either a headline or an inlinetask.  CONTENTS is
ignored.  INFO is a plist used as a communication channel.

This cleans up the output of `org-icalendar-entry'."
  (cl-flet ((clean (pattern string) (replace-regexp-in-string pattern "" string nil nil 1)))
    (->> (org-icalendar-entry entry contents info)
         (clean (rx bol "UID:" (group (* space) (or "DL" "SC" "TS") (* digit) ?-)))
         (clean (rx (group (optional ?,) "???"))) ;; categories clean
         (clean (rx (group (regex org-ts-regexp-both) (? (+ ?-) (regex org-ts-regexp-both)) (* "\\n")))))))

(org-export-define-derived-backend 'caldav 'org
  :translate-alist '((clock . ignore)
                     (footnote-definition . ignore)
                     (footnote-reference . ignore)
                     (headline . cal-sync-entry)
                     (inlinetask . ignore)
                     (planning . ignore)
                     (section . ignore)
                     (inner-template . org-icalendar-inner-template)
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
    (:icalendar-use-scheduled nil nil org-icalendar-use-scheduled)
    (:icalendar-scheduled-summary-prefix nil nil org-icalendar-scheduled-summary-prefix)
    (:icalendar-deadline-summary-prefix nil nil org-icalendar-deadline-summary-prefix))
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
        (url-request-extra-headers
         `(("Content-type" . "text/calendar; charset=UTF-8")
           ("Authorization" . ,(cal-sync--auth-header cal-sync-connection))))
        (url (concat (cal-sync-calendar-url cal-sync-connection)
                     (org-id-get-create) ".ics")))
    (url-retrieve url (lambda (status action title)
                        (unless (cal-sync-error-handling status (current-buffer))
                          (message "%s: \"%s\" successful" action title)))
                  (list action (org-entry-get nil "ITEM")))))

(defun cal-sync-parse-file (ics-file)
  "Parse ICS-FILE into `org-mode' entries."
  (mapcar #'cal-sync--org-entry (cal-sync-parse (find-file-noselect ics-file))))

(defun cal-sync-import-file (ics-file)
  "Import an ICS-FILE into the main agenda file."
  (interactive (list (read-file-name "Calendar ics file: ")))
  (dolist (event (cal-sync-parse-file ics-file))
    (write-region event nil (cal-sync-calendar-file cal-sync-connection) t)))

(defun cal-sync-delete ()
  "Delete current org node on the server."
  (interactive)
  (cal-sync-org-entry-action "DELETE"))

(defun cal-sync-export-entry ()
  "Export current(on point) org node to ics buffer."
  (let ((content (buffer-substring-no-properties
                  (org-entry-beginning-position)
                  (org-entry-end-position))))
    (with-temp-buffer
      (insert content)
      (encode-coding-string
       (org-export-as 'caldav) 'utf-8))))

(defun cal-sync-push ()
  "Push current org node to the server."
  (interactive)
  ;; Need id before processing, otherwise when pushing content server will create a new one
  ;; and there will be a conflict of file UID and event UID, that shows up after download.
  (org-id-get-create)
  (unless (string-match-p "rrule" (or (org-entry-get nil "TAGS") ""))
    (cal-sync-org-entry-action "PUT" (cal-sync-export-entry))))

(provide 'cal-sync)
;;; cal-sync.el ends here
