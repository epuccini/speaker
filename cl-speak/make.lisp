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

(defun main ()
  ;; --------
  ;;
  ;; Examples
  ;;
  (cl-sp:init-with-speech  "com.apple.speech.synthesis.voice.Alex")

  (cl-sp:speak "One two three")
  (sleep 2)
  (format t "Available-voices: ~D~%" (cl-sp:available-voices-count))
  (format t "Get-voice(~D): ~A~%" (cl-sp:get-voice-name 6) 6)
  (cl-sp:set-voice 8)
  (cl-sp:speak "Two for one")
  (sleep 1)
  (cl-sp:set-voice 7)
  (cl-sp:speak "Three are gone")
  (let ((speaker (cl-sp:make-speaker  "com.apple.speech.synthesis.voice.anna")))
	(cl-sp:speak-with speaker "Hello object oriented scum.")
	(print speaker)))

(main)

