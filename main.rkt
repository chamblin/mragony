#lang racket

(require net/url)
(require request)
(require json)
(require gregor)
(require gregor/period)
(require "slack.rkt")

(define headers (list "Accept: application/vnd.pagerduty+json;version=2"
                      (string-append "Authorization: Token token=" (getenv "PAGERDUTY_AUTH_TOKEN"))))
(define json-requester (wrap-requester-response (lambda (response) (string->jsexpr (http-response-body response))) http-requester))

(define (notification-user-id notification) (hash-ref (hash-ref notification 'user) 'id))
(define (notification-started-at notification) (hash-ref notification 'started_at))

; for now, "last night" is 300 UTC - 1400 UTC (7p-6a PDT, 10p-9a EDT)
; later we need to consider time zones per user, and DST as well
(define uri (let* ([today (today/utc)]
                   [since (~t today "YYYY-MM-dd'T'03:00:00'Z'")]
                   [until (~t today "YYYY-MM-dd'T'14:00:00'Z'")])
              (url "https"
                   #f
                   "api.pagerduty.com"
                   #f
                   #t
                   (list (path/param "notifications" '()))
                   (list (cons 'time_zone "UTC")
                         (cons 'since since)
                         (cons 'until until)
                         (cons 'filter "phone_notification"))
                   #f)))

(define response (get json-requester uri #:headers headers))
(define em-folks
  (hash "PE0UHDN" "Aish"
        "P9BL0FH" "Cees"
        "P6CGLLG" "Chris"
        "P6GC6YU" "Cory"
        "PL0090J" "Quattro"
        "PM6I8P0" "Ian"
        "PIZN5ML" "Ka"
        "PXYKSJR" "Mike"
        "P6EHOLT" "Peter"
        "PR7RE7K" "Priyam"
        "PYMRP2B" "Ryan"
        "P4K4BLO" "Shamil"))

(define (distant-notifications? notifications)
  (let* ([start-times-iso8601 (sort (map notification-started-at notifications) string<?)]
         [start-times (map iso8601->moment start-times-iso8601)]
         [first-time (first start-times)]
         [last-time (first (reverse start-times))]
         [period-between (period-between first-time last-time '(minutes))])
    (> (period-ref period-between 'minutes) 30)))

(define (was-a-crappy-night? notifications)
  ; a "crappy" night involves more than one notification, at least 30 minutes apart
  (cond
    [(< (length notifications) 2) #f]
    [else (distant-notifications? notifications)]))

(define (user-name-for-notification-set notifications)
  (hash-ref em-folks (notification-user-id (first notifications))))

(let* ([notifications (hash-ref response 'notifications)]
       [em-notifications (filter (lambda (n) (member (notification-user-id n) (hash-keys em-folks))) notifications)]
       [notifications-by-user (group-by notification-user-id em-notifications)]
       [notification-sets-that-were-crappy (filter was-a-crappy-night? notifications-by-user)]
       [users-that-had-a-crappy-night (map user-name-for-notification-set notification-sets-that-were-crappy)])
  (unless (empty? users-that-had-a-crappy-night)
    (send-message (string-append (string-join users-that-had-a-crappy-night ", ") " had a crappy on-call last night.  Please check on them and consider relieving them if they're on-call again tonight."))))