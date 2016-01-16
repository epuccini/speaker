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

;; -----------------
;; External packages
;; -----------------

(require 'cffi)

;
; load and compile helper
;
(defmacro load-and-compile-set (directory &body forms)
"Macro for doing the same things in seuquence"
  `(progn
    ,@(loop for f in forms collect `(load (compile-file (merge-pathnames ,directory ,f))))))

;
; Compile all files on C-c C-c
;
(eval-when (:load-toplevel :compile-toplevel :execute)
  "Executed at compile time. Does load and compile
all necessary files including packages"
  
  (require 'cffi)
  
  (load-and-compile-set *default-pathname-defaults*
	"package.lisp"
	"speaker.lisp"))                

;; Example application

(cffi:defcallback wsw-callback :void ((text :string))
	(print "Called back and word spoken: ~A!~%" text))

(cffi:defcallback wsp-callback :void ((op-code :short))
  (format t  "Called back and phoneme spoke (op-code: ~D)!~%" op-code))

(cffi:defcallback dfs-callback :void ()
  (print "Called back and did finish!"))


(Defun main ()
  ;; -------------------------------
  ;;
  ;; Examples with plain c interface
  ;; 
  (cl-sp:init-with-speech  "com.apple.speech.synthesis.voice.Alex")

  (cl-sp:speak "One two three")
  (sleep 2)
  (format t "Available-voices: ~D~%" (cl-sp:available-voices-count))
  (format t "Get-voice(~D): ~A~%" (cl-sp:get-voice-name 6) 6)
  (cl-sp:set-voice 6)
  (cl-sp:speak "Hallo Eddie")
  (sleep 1)
  (cl-sp:speak "Guten morgen.")
  (sleep 2)
  (cl-sp:make-speaker nil)           ;; error test
  (cl-sp:speak-with nil "Text")      ;; "
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
	(cl-sp:speak-with speaker "Test delegate.")
	(sleep 3)
	(cl-sp:set-voice-with speaker 7)
	(cl-sp:speak-with speaker "Next delegate.")))

(main)
