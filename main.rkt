#lang racket

(require gregor)
(require "slack.rkt")
(require "notifications.rkt")
(require "notifications-agony.rkt")

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

(define (user-name-for-notification-set notifications)
  (hash-ref em-folks (notification-user-id (first notifications))))

(define response (notifications-for-date (today/utc)))

(define (a-crappy-night? notifications)
  (symbol=? 'high (agony-level-for-notifications notifications)))

(let* ([notifications (hash-ref response 'notifications)]
       [em-notifications (filter (lambda (n) (member (notification-user-id n) (hash-keys em-folks))) notifications)]
       [notifications-by-user (group-by notification-user-id em-notifications)]
       [notification-sets-that-were-crappy (filter a-crappy-night? notifications-by-user)]
       [users-that-had-a-crappy-night (map user-name-for-notification-set notification-sets-that-were-crappy)])
  (if (empty? users-that-had-a-crappy-night)
    (send-message "Nobody had a crappy night")
    (send-message (string-append (string-join users-that-had-a-crappy-night ", ") " had a crappy on-call last night"))))
