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

(defpackage :speaker
  (:use #:cl #:cffi) 
  (:export
   #:init-with-speech
   #:speak
   #:start-speaking-string
   #:available-voices-count
   #:set-voice
   #:get-voice-name
   #:cleanup
   #:make-speaker
   #:speak-with
   #:set-voice-with
   #:cleanup-with
   #:register-will-speak-word-callback
   #:register-will-speak-phoneme-callback
   #:register-did-finish-speaking-callback))
