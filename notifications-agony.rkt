#lang racket

(require gregor)
(require gregor/period)
(require "notifications.rkt")

(provide agony-level-for-notifications)

(define (distant-notifications? notifications)
  (let* ([start-times-iso8601 (sort (map notification-started-at notifications) string<?)]
         [start-times (map iso8601->moment start-times-iso8601)]
         [first-time (first start-times)]
         [last-time (first (reverse start-times))]
         [period-between (period-between first-time last-time '(minutes))])
    (> (period-ref period-between 'minutes) 30)))

(define (high-agony-level? notifications)
  ; a "crappy" night involves more than one notification, at least 30 minutes apart
  (cond
    [(< (length notifications) 2) #f]
    [else (distant-notifications? notifications)]))

(define (agony-level-for-notifications notifications)
  (cond
    [(high-agony-level? notifications) 'high]
    [(>= (length notifications) 1) 'medium]
    [else 'low]))