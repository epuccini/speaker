; -------------------------------------------------------------
; Edward Alan Puccini 19.01.2016
; -------------------------------------------------------------
; Kind of make for common lisp
; -------------------------------------------------------------
; file: make.lisp 
; -------------------------------------------------------------
; make - compile, load and run
; Compile this file and every other needed file gets compiled.
; On error check path in compile-files
; -------------------------------------------------------------

(eval-when (:load-toplevel :compile-toplevel :execute)
  "Executed at compile time. Does load and compile
all necessary files including packages.
Compile all files on C-c C-k in emacs/slime"

	(defvar *make-conf* "")
	(defvar *main-function* nil)
	(defvar *categories* nil)
	(defvar *sources* '())
	(defvar *make-filename* "make.conf")
	(defvar *app* "")

	; -------------------------------------------------------------

	(defmacro build-files (directory &rest forms)
	"Macro for load and compile. Just list some files and they
	get compiled and loaded."
	`(progn
		,@(loop for f in forms collect 
				`(load (compile-file (merge-pathnames ,directory ,f))))))


	(defun quickload-list (systems)
	  "Macro to quickload a system."
	  (handler-case
		  (progn
			(print "Quickload found...")
			(mapc (lambda (s)
					(ql:quickload s)
					(format t "~A..." s)) systems))
		(error (condition)
		  (write-line (format nil "Error in quickload-list! ~A." condition) 
					  common-lisp:*error-output*))))


	(defun require-list (packages)
	  "Function require a list of packages."
	  (handler-case
		  (progn
			(format t "~%Require packages...")
			(mapc #'(lambda (p) 
					  (require p)
					  (format t "~A..." p)) packages))
		(error (condition)
		  (write-line (format nil "Error in require-list! ~A." condition) 
					  common-lisp:*error-output*))))
	
	#+clisp
	(defun require-list (packages)n
	  (write-line "No need to require in CLisp" common-lisp:*error-output*))


	(defun load-config ()
	  (let ((content 
			 (with-open-file (stream *make-filename* :direction :input)
							 (read stream))))
		content))

	(defun setup ()
	  "This function has side effects and sets the
	globals with valid configuration properties."
	  (setq *make-filename* (merge-pathnames 
							 *default-pathname-defaults* "make.conf"))
	  (format t "filename '~A' set...~%" *make-filename*)
	  (setq *make-conf* (load-config))
	  (format t "Config read...~%")
	  (setq *categories* (getf *make-conf* :categories))
	  (mapc #'pprint *categories*)
	  (setq *main-function* (getf *make-conf* :main))
	  (setq *app* (getf *make-conf* :app)))


	(defun cbuild (category)
	  "Load and compile all files in given
	category."
	  (declare (keyword category))
	  (handler-case 
	   (progn
		 (mapc (lambda (file)
				 (format t "Compiling file ~A.~%" file)
				 (load (compile-file file)))
			   (getf *categories* category)) t)
		(error (condition)
		  (write-line (format nil "Error in cbuild! ~A." condition) 
					  common-lisp:*error-output*))))

	(defun build ()
	  "Load and compile all files in all categories."
	  (handler-case 
		  (progn
			(format t "~%Building ~A...~%" *app*)
			(loop for category in *categories* do
				 (cond ((getf *categories* category) 
						(format t "Building category ~A.~%" category)
						(cbuild category)))))
		(error (condition)
		  (write-line (format nil "Error in build! ~A." condition) 
					  common-lisp:*error-output*))))


	#+sbcl 
	(defun save-bin ()
	  "Save binary-file with sbcl."
	  (handler-case
		  (save-lisp-and-die  *app*
							  :executable t 
							  :compression t 
							  :toplevel *main-function*)
		(error (condition)
		  (write-line (format nil "Error in save-bin! ~A." condition) 
					  common-lisp:*error-output*))))

	#+cmu 
	(defun save-bin ()
	  "Save binary-image with cmucl."
	  (handler-case
		  (extensions:save-lisp  *app*
								 :executable t 
								 :init-function *main-function*)
		(error (condition)
		  (write-line (format nil "Error in save-bin! ~A." condition) 
					  common-lisp:*error-output*))))

	#+(or openmcl ccl) 
	(defun save-bin ()
	  "Save binary-file with clozure-lisp."
	  (handler-case
		  (progn
			;(require 'ccl)
			;(require :build-application)
			;;(require 'cocoa)
			 (ccl:save-application *app*
								   :purify t
								   :prepend-kernel t
								   :application-class 'ccl::application
								   :toplevel-function *main-function*))
		(error (condition)
		  (write-line (format nil "Error in save-bin! ~A." condition) 
					  common-lisp:*error-output*))))

	#+ecl 
	(defun save-bin ()
	  "Save binary-file with ecl."
	  (handler-case
		  (c:build-program *app*)
		(error (condition)
		  (write-line (format nil "Error in save-bin! ~A." condition) 
					  common-lisp:*error-output*))))


	#+clisp 
	(defun save-bin ()
	  "Save binary-image with clisp."
	  (handler-case
		  (ext:saveinitmem *app* 
						   :executable t 
						   :verbose t 
						   :init-function *main-function*)
		(error (condition)
		  (write-line (format nil "Error in save-bin! ~A." condition) 
					  common-lisp:*error-output*))))


	(defun execute-with (shell-call command)
	  "Executes a shell command in 'command'
with the function in 'shell-call'."
	  (handler-case
		  (progn
			(format t "~%Execute...~A~%" command)
			(format t "Command returned with ~A~%"
					(funcall shell-call command)))
		(error (condition)
		  (write-line (format nil "Error in execute-with! ~A." condition) 
					  common-lisp:*error-output*))))


	(defun execute-when (at)
	  (let ((commands (getf *make-conf* at)))
		(mapc (lambda (c)
				(format t "~%Try to execute ~A with ~A~%" at c)
				(cond (c
					   (execute c)))) commands)))


	#+sbcl
	(defun execute (command)
	  "Platform specific shell-call with 'command'."
	  (execute-with #'asdf:run-shell-command command))

	#+cmu
	(defun execute (command)
	  "Platform specific shell-call with 'command'."
	  (execute-with #'asdf:run-shell-command command))

	#+ecl
	(defun execute (command)
	  "Platform specific shell-call with 'command'."
	  (execute-with #'asdf:run-shell-command command))

	#+clisp
	(EXT:WITHOUT-PACKAGE-LOCK ("EXT")
	  (defun execute (command)
		"Platform specific shell-call with 'command'."
		(execute-with #'asdf:run-shell-command command)))

	#+(or openmcl ccl)
	(defun execute (command)
	  "Platform specific shell-call with 'command'."
	  (execute-with #'asdf:run-shell-command command))


	(defun run ()
	  (handler-case
		  (cond (*main-function*
			 (funcall (symbol-function 
					   (find-symbol (string-upcase *main-function*))))))
		(error (condition)
		  (write-line (format nil "Error in run! ~A." condition) 
					  common-lisp:*error-output*))))

  (print "Make startup...")
  ;; Load configuration
  (setup)
  ;; Quickload systems
  (quickload-list (getf *make-conf* :systems))  ;; Require packages
  (require-list (getf *make-conf* :packages))
  ;; Execute command if 'before'
  (execute-when :before)
  ;; compile everything
  (build)
  ;; Execute command if 'after'
  (execute-when :after)
  ;; End: run program if main-key set
  (format t "Building  complete. Starting ~A:" *app*)
  (cond ((and (getf *make-conf* :run) (equal (getf *make-conf* :run) t))
		 (run))))

