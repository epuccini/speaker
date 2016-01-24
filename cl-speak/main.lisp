; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; CL-Speak library make and loader
; -------------------------------------------------------------
; file: make.lisp 
; -------------------------------------------------------------
; make - compile, load and run
; Compile this file and every other needed file gets compiled.
; On error check path in compile-files
; -------------------------------------------------------------
; Requirements: cffi
; -------------------------------------------------------------

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
  (cl-sp:init-with-speech "com.apple.speech.synthesis.voice.Alex")

  ;(cl-sp:speak "One two three)
  (sleep 2)
  (format t "Available-voices: ~D~%" (cl-sp:available-voices-count))
  (format t "Get-voice(~D): ~A~%" (cl-sp:get-voice-name 6) "")
  (cl-sp:set-voice 6)
  (cl-sp:speak "Hallo Andy! Bist am schwitzen?")
  (sleep 1)
  (cl-sp:speak "Guten morgen.")
  (cl-sp:cleanup)
  (sleep 2)
  ;(cl-sp:make-speaker nil)           ;; error test
  ;(cl-sp:speak-with nil "Text")      ;; "
  ;; -------------------------------
  ;;
  ;; Examples with wrapper of 
  ;; objective-c implementation
  ;;
  (let ((speaker (cl-sp:make-speaker  "com.apple.speech.synthesis.voice.anna")))
  	(cl-sp:register-will-speak-word-callback speaker (cffi:callback wsw-callback))
  	(cl-sp:register-will-speak-phoneme-callback speaker (cffi:callback wsp-callback))
  	(cl-sp:register-did-finish-speaking-callback speaker (cffi:callback dfs-callback))
  	(cl-sp:set-voice-with speaker 7)
  	(cl-sp:speak-with speaker "This delegate.")
  	(sleep 3)
  	(cl-sp:set-voice-with speaker 7)
  	(cl-sp:speak-with speaker "Next delegate.")
	(cl-sp:cleanup-with speaker)))

(main)
