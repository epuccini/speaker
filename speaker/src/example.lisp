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

;;
;; Global objects
;;
(defvar *speaker* nil)
(defvar *listener* nil)
(defvar *stop-flag* nil)

;;
;; Utility macro
;;
(defmacro spoken (imperative response voice)
  "Check if commando is spoken and respond
with a response."
  `(cond ((equal text ,imperative)
		  (progn
			(stop-listening *listener*)
			(set-voice-with *speaker* ,voice)
			(speak-with *speaker* ',response)
		    (start-listening *listener*)))))

(defmacro spoken-action (imperative response voice)
  "Check if commando is spoken and respond
with a response."
  `(cond ((equal text ,imperative)
		  (progn
			(stop-listening *listener*)
			(set-voice-with *speaker* ,voice)
			(speak-with *speaker* ',response)
			',response
		    (start-listening *listener*)))))


;;
;; Callbacks
;;
(cffi:defcallback wsw-callback :void ((text :string))
  (format t "Called back and spoke: ~A!~%" text))

(cffi:defcallback wsp-callback :void ((op-code :short))
  (format t  "Called back and spoke phoneme (op-code: ~D)!~%" op-code))

(cffi:defcallback dfs-callback :void ()
  (format t "Called back and did finish word!~%"))

(cffi:defcallback drc-callback :void ((text :string))
  (format t "Called back and recognize: ~A.~%" text)		  
  (spoken "love" "love me" 7)
  (spoken "fuck" "Oh, dont curse" 7)
  (spoken "you" "I am master blaster" 7)
  (spoken "you stupid asshole" "I kill you" 7)
  (spoken "speak" "What should i say" 7)
  (spoken-action "exit" (setq *stop-flag* t) 7))

(defun listener-setup (listener)
  (add-commands listener
		"test" "speak" "exit" "fuck" "you" "you stupid asshole")
  (start-listening listener))

(defun speaker-test (speaker)
  ;; -------------------------------
  ;;
  ;; Examples with plain c interface
  ;; 
  (init-speaker)
  (format t "Available-voices: ~D~%" (available-voices-count))
  (format t "Get-voice(~D): ~A~%" (get-voice-name 6) "")
  (sleep 1)
  (speak "Guten Morgen.")
  (sleep 2)
  (set-voice 2)
  (speak "Hello world")
  (cleanup-speaker)
  (sleep 2)
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
  (set-voice-with speaker 9)
  (speak-with speaker "This is number 9.")
  (sleep 2)
  (cleanup-with speaker))

;;
;; Main
;;
(defun main ()
  "Main test program."
  (trivial-main-thread:with-body-in-main-thread () ;; main loop
	(terpri)
	(print "Start listening")
	(setf *speaker* (make-speaker))
	(setf *listener* (make-listener))
	(register-did-recognize-command-callback *listener* (cffi:callback drc-callback))
	(register-will-speak-word-callback *speaker* (cffi:callback wsw-callback))
	(set-voice-with *speaker* 7)
	(listener-setup *listener*)
	(speak-with *speaker* "Entering mainloop")
	(setf *stop-flag* nil)
	(loop while (not *stop-flag*) do 
		 (mainloop-speaker *speaker*))
		 ;;(mainloop-listener *listener*))
	(setf *stop-flag* nil)
	(stop-listening *listener*)
	(speak-with *speaker* "exit")
	(cleanup-with *speaker*)))



