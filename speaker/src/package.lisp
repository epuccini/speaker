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
   #:main
   #:init-speaker
   #:speak
   #:start-speaking-string
   #:available-voices-count
   #:set-voice
   #:available-languages-count
   #:set-language
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
   #:add-command
   #:mainloop-speaker
   #:mainloop-listener
   #:speaking-p
   #:spoken
   #:spoken-action
   #:add-commands
   #:add-commands-list))

