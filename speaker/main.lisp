; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Speaker library make and loader
; -------------------------------------------------------------
; file: make.lisp 
; -------------------------------------------------------------
; make - compile, load and run
; Compile this file and every other needed file gets compiled.
; On error check path in compile-files
; -------------------------------------------------------------
; Requirements: cffi
; -------------------------------------------------------------

(in-package :speaker)

;; Example application

(cffi:defcallback wsw-callback :void ((text :string))
	(print "Called back and word spoken: ~A!~%" text))

(cffi:defcallback wsp-callback :void ((op-code :short))
  (format t  "Called back and phoneme spoke (op-code: ~D)!~%" op-code))

(cffi:defcallback dfs-callback :void ()
  (print "Called back and did finish!"))


(defun main ()
  ;; -------------------------------
  ;;
  ;; Examples with plain c interface
  ;; 
  (init-speaker)

  ;(speaker:speak "One two three")
  ;(sleep 2)
  (format t "Available-voices: ~D~%" (speaker:available-voices-count))
  (format t "Get-voice(~D): ~A~%" (speaker:get-voice-name 6) "")
  (sleep 1)
  (speaker:speak "Guten Morgen.")
  (sleep 2)
  (speaker:set-voice 2)
  (speaker:speak "Hello world?")
  (speaker:cleanup)
  (sleep 2)
  ;(speaker:make-speaker nil)           ;; error test
  ;(speaker:speak-with nil "Text")      ;; "
  ;; -------------------------------
  ;;
  ;; Examples with wrapper of 
  ;; objective-c implementation
  ;;
  (let ((speaker (speaker:make-speaker)))
  	(speaker:register-will-speak-word-callback speaker (cffi:callback wsw-callback))
  	(speaker:register-will-speak-phoneme-callback speaker (cffi:callback wsp-callback))
  	(speaker:register-did-finish-speaking-callback speaker (cffi:callback dfs-callback))
  	(speaker:set-voice-with speaker 7)
  	(speaker:speak-with speaker "This is Voice 7.")
  	(sleep 3)
  	(speaker:set-voice-with speaker 8)
  	(speaker:speak-with speaker "Another Voice 8.")
	(speaker:cleanup-with speaker)))

(main)
