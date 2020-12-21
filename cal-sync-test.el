;; Run it from the org-caldav directory like this:
;; emacs --batch -Q -L . -l cal-sync-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cal-sync)

(ert-deftest cal-aget ()
  (should (equal (get-attr '((a . (5))) 'a) 5)))

(ert-deftest test-ical-to-org ()
  (let ((org-tags-column 0)
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
END:VCALENDAR")
        (result "* Feed the dragons :pets:dragons:hunting:
:PROPERTIES:
:ID:       first
:LOCATION: forest
:END:
<2020-03-19 Thu 10:30>--<2020-03-19 Thu 11:30>
Take some meat
Search for big animals in the forest\n"))
    (should (equal result
                   (with-temp-buffer
                     (insert input)
                     (mapconcat 'cal-sync--org-entry
                                (cal-sync-convert-event (current-buffer))
                                ""))))))
