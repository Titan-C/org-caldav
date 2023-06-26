;; Run it from the org-caldav directory like this:
;; ./makem.sh -vv --sandbox=testbox all

;;; Code:
(require 'ert)
(require 'cal-sync)

(ert-deftest cal-aget ()
  (should (equal (cal-sync-get-attr '((a . (5))) 'a) 5)))

(ert-deftest cal-time-span ()
  (should (equal
           '((ORG-TIME nil "<2022-11-05 Sat>"))
           (cal-sync-ical-times-span '((DTSTART (VALUE "DATE") "20221105T000000")) "test")))
  (should (equal
           '((ORG-TIME nil "<2022-11-05 Sat>--<2022-12-06 Tue 07:00>"))
           (cal-sync-ical-times-span '((DTSTART (VALUE "DATE") "20221105T000000")
                                       (DTEND   (VALUE "DATE") "20221206T070000")) "test"))))

(ert-deftest test-ical-to-org ()
  (let ((org-tags-column 0)
        (org-export-with-author nil)
        (org-icalendar-timezone "Europe/Berlin")
        (org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S")
        (org-icalendar-exclude-tags '("rrule"))
        (org-icalendar-categories '(local-tags))
        (org-agenda-default-appointment-duration nil)
        (input "BEGIN:VCALENDAR
BEGIN:VEVENT
UID:first
SUMMARY:Feed the dragons
LOCATION:forest
DTSTART;TZID=\"Europe/Berlin\":20200319T103000
DTEND;TZID=\"Europe/Berlin\":20200319T113000
DESCRIPTION:Take some meat\\nSearch for big
  animals in the forest
CATEGORIES:pets,dragons
CATEGORIES:hunting
END:VEVENT
BEGIN:VEVENT
CREATED:20230622T132317Z
DTSTAMP:20230622T132400Z
LAST-MODIFIED:20230622T132400Z
SEQUENCE:2
UID:922bd4ce-df85-4d69-a3f7-d56220c146a9
DTSTART;TZID=America/Guayaquil:20200824T070000
DTEND;TZID=Europe/Berlin:20200824T150000
STATUS:CONFIRMED
SUMMARY:Conversar
END:VEVENT
END:VCALENDAR")
        (result "* Feed the dragons :pets:dragons:hunting:
:PROPERTIES:
:ID:       first
:LOCATION: forest
:END:
<2020-03-19 Thu 10:30>--<2020-03-19 Thu 11:30>
Take some meat
Search for big animals in the forest
* Conversar
:PROPERTIES:
:ID:       922bd4ce-df85-4d69-a3f7-d56220c146a9
:END:
<2020-08-24 Mon 14:00>--<2020-08-24 Mon 15:00>\n")
        (exported-ics "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-////Emacs with Org mode//EN
CALSCALE:GREGORIAN
BEGIN:VEVENT\r
UID:first\r
DTSTART;TZID=Europe/Berlin:20200319T103000\r
DTEND;TZID=Europe/Berlin:20200319T113000\r
SUMMARY:Feed the dragons\r
LOCATION:forest\r
DESCRIPTION:\\nTake some meat\\\r
 nSearch for big animals in the forest\r
CATEGORIES:pets,dragons,hunting\r
END:VEVENT
BEGIN:VEVENT\r
UID:922bd4ce-df85-4d69-a3f7-d56220c146a9\r
DTSTART;TZID=Europe/Berlin:20200824T070000\r
DTEND;TZID=Europe/Berlin:20200824T150000\r
SUMMARY:Conversar\r
DESCRIPTION:\r
CATEGORIES:\r
END:VEVENT
END:VCALENDAR
"))
    (should (equal result
                   (with-temp-buffer
                     (insert input)
                     (mapconcat 'cal-sync--org-entry
                                (cal-sync-convert-event (current-buffer))
                                ""))))
    (should (equal exported-ics
                   (with-temp-buffer
                     (insert result)
                     (encode-coding-string
                      (->> (org-export-as 'caldav)
                           (replace-regexp-in-string "^DTSTAMP:.*?\n" "")
                           (replace-regexp-in-string "^X-WR-.*?\n" ""))
                      'utf-8))))))

(ert-deftest test-cal-sync-error-handler ()
  (with-temp-buffer
    (insert "HTTP/1.0 200 OK")
    (should (equal nil (cal-sync-error-handling nil (current-buffer))))
    (should (equal t (cal-sync-error-handling '(:error (error http 404)) (current-buffer))))))
