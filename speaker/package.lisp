; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; CL-Speak package
; -------------------------------------------------------------
; file: package.lisp
; -------------------------------------------------------------
; 
; -------------------------------------------------------------

(require 'cffi)
(require 'bordeaux-threads)

(defpackage :speaker
  (:use #:cl #:cffi #:bordeaux-threads) 
  (:export
   #:main
   #:init-speaker
   #:speak
   #:start-speaking-string
   #:available-voices-count
   #:set-voice
   #:get-voice-name
   #:cleanup-speaker
   #:make-speaker
   #:speak-with
   #:set-voice-with
   #:cleanup-with
   #:register-will-speak-word-callback
   #:register-will-speak-phoneme-callback
   #:register-did-finish-speaking-callback
   #:register-did-recognize-command-callback
   #:make-listener
   #:start-listening
   #:stop-listening
   #:async-prefix
   #:enable-async-syntax
   #:disable-async-syntax
   #:*previous-readtables*
;   #:runloop-speaker
   #:runloop-thread-speaker
   #:runloop-call-thread-speaker
   #:runloop-listener
   #:runloop-thread-listener
   #:runloop-call-thread-listener
   #:stop-runloop-thread-listener
   #:speaking-p))
0
