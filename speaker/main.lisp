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

(cffi:defcallback drc-callback :void ()
  (print "Called back and did recognizeh!"))


(defun main ()
  ;; -------------------------------
  ;;
  ;; Examples with plain c interface
  ;; 
  ;; (init-speaker)
  ;; (format t "Available-voices: ~D~%" (available-voices-count))
  ;; (format t "Get-voice(~D): ~A~%" (get-voice-name 6) "")
  ;; (sleep 1)
  ;; (speak "Guten Morgen.")
  ;; (sleep 2)
  ;; (set-voice 2)
  ;; (speak "Hello world?")
  ;; (cleanup-speaker)
  ;; (sleep 2)
  ;(make-speaker nil)           ;; error test
  ;(speak-with nil "Text")      ;; "
  ;; -------------------------------
  ;;
  ;; Examples with wrapper of 
  ;; objective-c implementation
  ;;
  (let ((speaker (make-speaker)))
  	(register-will-speak-word-callback speaker (cffi:callback wsw-callback))
  	(register-will-speak-phoneme-callback speaker (cffi:callback wsp-callback))
  	(register-did-finish-speaking-callback speaker (cffi:callback dfs-callback))
  	(set-voice-with speaker 7)
  	(speak-with speaker "This is Voice 7.")
  	(sleep 3)
  	(set-voice-with speaker 8)
  	(speak-with speaker "Another Voice 8.")
	(cleanup-with speaker))
  (let ((listener (make-listener)))
  	(register-did-recognize-command-callback listener (cffi:callback drc-callback))
	(print "Listener...")
	(add-command listener "hey")
	(add-command listener "run")
	(add-command listener "obey")
	(start-listening listener)
	(print "Start listening")
	(sleep 5)
	(print "End listening")
	(stop-listening listener)))

(main)
