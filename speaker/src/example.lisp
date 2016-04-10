; ------------------------------------------------------------
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

(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :speaker))

(require 'bordeaux-threads)
(require 'trivial-main-thread)

(defvar *speaker* nil);
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
  (add-command listener "do it")
  (add-command listener "test")
  (add-command listener "speak")
  (add-command listener "silence")
  (add-command listener "exit")
  (add-command listener "fuck")
  (add-command listener "fuck yourself")
  (add-command listener "you")
  (add-command listener "love")
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

(defmacro spoken (imperative response voice)
  "Check if commando is spoken and respond
with a response."
  `(cond ((equal text ,imperative)
		  (progn
			(stop-listening *listener*)
			(set-voice-with *speaker* ,voice)
			(speak-with *speaker* ',response)
		    (start-listening *listener*)))))

(cffi:defcallback drc-callback :void ((text :string))
  (format t "Called back and recognize: ~A.~%" text)
  (cond ((equal text "exit")
		 (progn
		   (stop-listening *listener*)
		   (set-voice-with *speaker* 20)
		   (speak-with *speaker* "bye bye")
		   (start-listening *listener*)
		   (setq *stop-flag* t))))
  (spoken "love" "love me" 2)
  (spoken "do it" "no" 2)
  (spoken "fuck" "Oh, dont curse" 2)
  (spoken "fuck yourself" "Yes master" 2)
  (spoken "you" "I am master blaster" 20)
  (spoken "speak" "What should i say" 2))

;;
;; Main
;;
(defun main ()
  "Main test program."
  (trivial-main-thread:with-body-in-main-thread () ;; main loop
	(terpri)
	(let ((listener (make-listener))
		  (speaker (make-speaker)))
	  (setf *speaker* speaker)
	  (setf *listener* listener)
	  (set-voice-with speaker 10)
	  (listener-setup listener)
	  (print "Entering mainloop...")
	  (setf *stop-flag* nil)
	  (loop while (not *stop-flag*) do 
		   (mainloop-speaker speaker))
;		   (mainloop-listener listener))
	  (setf *stop-flag* nil)
	  (stop-listening listener)
	  (print "End listening")
	  (cleanup-with *speaker*))))
