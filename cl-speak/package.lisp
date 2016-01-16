; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; CL-Speak package
; -------------------------------------------------------------
; file: package.lisp
; -------------------------------------------------------------
; 
; -------------------------------------------------------------

(in-package :common-lisp)

(require 'pcall)
(require 'cffi)

(defpackage :cl-sp
  (:use #:cl #:cffi #:pcall #:swank) 
  (:export
   #:init-with-speech
   #:speak
   #:start-speaking-string
   #:available-voices-count
   #:set-voice
   #:get-voice-name
   #:make-speaker
   #:speak-with
   #:set-voice-with
   #:register-will-speak-word-callback
   #:register-will-speak-phoneme-callback
   #:register-did-finish-speaking-callback))
