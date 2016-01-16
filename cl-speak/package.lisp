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
   #:set-voice-with))

;; void* make_speaker(char* speech);
;; void speak_with(void* speaker, char* text);
;; void set_voice_with(void* speaker, int index);
