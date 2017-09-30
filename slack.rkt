#lang racket

(require net/url)
(require request)
(require json)

(provide send-message)

(define (send-message message)
  (post http-requester
        (string->url (getenv "SLACK_INCOMING_WEBHOOK_URL"))
        (jsexpr->bytes (hash 'text message))))

