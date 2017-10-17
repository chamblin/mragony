#lang racket

(require net/url)
(require request)
(require json)
(require gregor)

(provide notifications-for-date
         notification-started-at
         notification-user-id)

(define headers (list "Accept: application/vnd.pagerduty+json;version=2"
                      (string-append "Authorization: Token token=" (getenv "PAGERDUTY_AUTH_TOKEN"))))
(define json-requester (wrap-requester-response (lambda (response) (string->jsexpr (http-response-body response))) http-requester))

(define (notification-user-id notification) (hash-ref (hash-ref notification 'user) 'id))
(define (notification-started-at notification) (hash-ref notification 'started_at))

; for now, "last night" is 300 UTC - 1400 UTC (7p-6a PDT, 10p-9a EDT)
; later we need to consider time zones per user, and DST as well
(define (uri-for-date date)
  (let* ([since (~t date "YYYY-MM-dd'T'03:00:00'Z'")]
         [until (~t date "YYYY-MM-dd'T'14:00:00'Z'")])
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

(define (notifications-for-date date)
    (get json-requester (uri-for-date date) #:headers headers))
