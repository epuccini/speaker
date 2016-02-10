; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Speaker library make and loader
; -------------------------------------------------------------
; file: main.lisp 
; -------------------------------------------------------------
; make - compile, load and run
; Compile this file and every other needed file gets compiled.
; On error check path in compile-files
; -------------------------------------------------------------
; Requirements: cffi
; -------------------------------------------------------------

(in-package :speaker)

(asdf:load-system :trivial-main-thread)

(defvar *listener* nil);
(defvar *stop-flag* nil);

;; Example application
(defun speaker-test (speaker)
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
  (register-will-speak-word-callback speaker (cffi:callback wsw-callback))
;  (register-will-speak-phoneme-callback speaker (cffi:callback wsp-callback))
;  (register-did-finish-speaking-callback speaker (cffi:callback dfs-callback))
  (set-voice-with speaker 11)
  (speak-with speaker "Hallo Edward.")
  (sleep 2)
  (set-voice-with speaker 7)
  (speak-with speaker "This is number 7.")
  (sleep 2)
  (cleanup-with speaker))

(defun listener-setup (listener)
  (add-command listener "hey")
  (add-command listener "run")
  (add-command listener "test")
  (add-command listener "speak")
  (add-command listener "silence")
  (add-command listener "exit")
  (register-did-recognize-command-callback listener (cffi:callback drc-callback))
  (print "Start listening")
  (start-listening listener))

;;
;; CFFI Callbacks
;;
(cffi:defcallback wsw-callback :void ((text :string))
  (format t "Called back and spoke: ~A!~%" text))

(cffi:defcallback wsp-callback :void ((op-code :short))
  (format t  "Called back and spoke phoneme (op-code: ~D)!~%" op-code))

(cffi:defcallback dfs-callback :void ()
  (format t "Called back and did finish word!~%"))

(cffi:defcallback drc-callback :void ((text :string))
  (format t "Called back and recognize: ~A.~%" text)
  (cond ((equal text "exit")
		 (progn
		   (setq *stop-flag* t)
		   (stop-runloop-thread-listener *listener*)))
		((equal text "speak")
		 (let ((speaker (make-speaker)))
		   (stop-listening *listener*)
		   (speaker-test speaker)
		   (start-listening *listener*)))))

;;
;; Main
;;
(defun main ()
  "Main test program."
  (trivial-main-thread:with-body-in-main-thread ()
  (terpri)
  (let ((listener (make-listener))
		(speaker (make-speaker)))
	(setf *listener* listener)
	(listener-setup listener)
	(print "Entering mainloop...")
	(loop while (not *stop-flag*) do
		 (runloop-call-thread-speaker speaker)
		 (runloop-listener listener))
	(stop-listening listener)
	(print "End listening"))))

(main)

