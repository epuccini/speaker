; -------------------------------------------------------------
; Edward Alan Puccini 19.01.2016
; -------------------------------------------------------------
; Kind of make for common lisp
; -------------------------------------------------------------
; file: spin.lisp 
; -------------------------------------------------------------
; spin - compile, load and run
; Compile this file and every other needed file gets compiled.
; On error check path in compile-files
; -------------------------------------------------------------

(eval-when (:load-toplevel :compile-toplevel :execute)
  "Executed at compile time. Does load and compile
all necessary files including packages.
Compile all files on C-c C-k in emacs/slime"

    ;; global parameter
	(defvar *app* "")
	(defvar *spin-conf* "")
	(defvar *main-function* nil)
	(defvar *categories* nil)
	(defvar *sources* '())

	;; do not touch
    (defvar *project-directory* "")
	(defvar *spin-filename* #P"./spin.conf")

	; -------------------------------------------------------------

	(defmacro build-files (directory &rest forms)
	"Macro for load and compile. Just list some files and they
	get compiled and loaded."
	`(progn
		,@(loop for f in forms collect 
				`(load (compile-file (merge-pathnames ,directory ,f))))))

	(defun reset-global-params ()
	  "Reset all global dynamic vars.
    Does not change *project-directory* and
    *spin-filename* ."
	  (setq *app* "")
	  (setq *spin-conf* "")
	  (setq *main-function* nil)
	  (setq *categories* nil)
	  (setq *sources* '()))
	
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

	(defun load-config-with (configuration-filepath)
	  "Load a configuration-file and return content."
	  (let ((content 
			 (with-open-file (stream configuration-filepath :direction :input)
							 (read stream))))
		content))

	(defun load-config ()
	  "Load configuration file with configured filepath."
	  (load-config-with *spin-filename*))

	(defun setup-with (configuration-filepath)
	  "This function has side effects and sets the
	globals with valid configuration properties."
	  (setq *spin-filename* (merge-pathnames 
							 *default-pathname-defaults* 
							 configuration-filepath))
	  (format t "configuration-file '~A' set...~%" configuration-filepath)
	  (setq *spin-conf* (load-config-with configuration-filepath))
	  (format t "Configuration read...")
	  (setq *categories* (getf *spin-conf* :categories))
	  (mapc #'pprint *categories*)
	  (setq *main-function* (getf *spin-conf* :main))
	  (setq *app* (getf *spin-conf* :app)))

	(defun setup ()
	  "This function has side effects and sets the
	globals with valid configuration properties."
	  (setup-with *spin-filename*))

	(defun category (category)
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
		  (write-line (format nil "Error in category! ~A." condition) 
					  common-lisp:*error-output*))))

	(defun build ()
	  "Load and compile all files in all categories."
	  (handler-case 
		  (progn
			(format t "~%Building ~A...~%" *app*)
			(loop for c in *categories* do
				 (cond ((getf *categories* c) 
						(format t "Building category ~A.~%" c)
						(category c)))))
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
			(format t "Command returned with ~A~%" (funcall shell-call command)))
		(error (condition)
		  (write-line (format nil "Error in execute-with! ~A." condition) 
					  common-lisp:*error-output*))))

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

	(defun execute-when (at)
	  "Execute all commands by calling 'execute'.
    Needed by execute-with."
	  (let ((commands (getf *spin-conf* at)))
		(mapc (lambda (c)
				(format t "~%Try to execute ~A with ~A~%" at c)
				(cond (c
					   (execute c)))) commands)))

	(defun run ()
	  (handler-case
		  (cond (*main-function*
				 (progn
				   (format t "~%Running ~A in ~A.~%" *main-function* *app*)
				   (funcall (symbol-function 
							 (find-symbol (string-upcase *main-function*)))))))
		(error (condition)
		  (write-line (format nil "Error in run! ~A." condition) 
					  common-lisp:*error-output*))))

	(defun touch (file)
	  "Create an empty file."
	  (handler-case
		  (let* ((file-exist (probe-file file)))
			(cond (file-exist (format t "File ~A exists.~%" file)))
			(cond ((not file-exist) 
				   (let ((stream (open file 
									   :direction :input 
									   :if-does-not-exist :create)))
					 (format t "File ~A created.~%" file)
					 (close stream)))))
		(error (condition)
		  (format t "Error in touch! ~A~%" condition))))

	(defun recreate ()
	  "Create missing directories and files which are configured
in the spin configuration-file."
	  (handler-case
		  (progn
			(format t "~%~%Recreate directories and files...~%")
			(loop for c in *categories* do
				 (let ((category-files (getf *categories* c)))
				   (cond (category-files
						  (format t ":~A.~%" c)
						(mapc (lambda (file)
								(format t "Ensure directory and file: ~A~%" file)  
								(common-lisp:ensure-directories-exist file)
								(touch file)) category-files))))))
		(error (condition)
		  (write-line (format nil "Error in recreate! ~A." condition) 
					  common-lisp:*error-output*))))

	(defun get-directory-from-file (filepath)
	  "Extract directory-path from filepath."
	  (let* ((filepath-string (format nil "~A" filepath))
			 (lastSlashPos (1+ (position #\/ filepath-string :from-end t)))
			 (path (subseq filepath-string 0 lastSlashPos)))
	    (merge-pathnames path)))

	(defun spin-with (configuration-filepath)
	  "Complete setup, recreate, build, run cycle configured
with given configuration filepath"
	  (setq *default-pathname-defaults* 
			(get-directory-from-file configuration-filepath)) 
										; change into folder of given spin.conf
	  (recreate) ; setup project directories and files
	  (quickload-list (getf *spin-conf* :systems))  ; Quickload systems
	  (require-list (getf *spin-conf* :packages))   ; Require packages
	  (execute-when :before) ; Execute command if 'before' build
	  (build) ; compile everything
	  (execute-when :after) ; Execute command if 'after'
	  ; End: run program if main-key set
	  (format t "Building  complete. Starting ~A:~%~%" *app*)
	  ; do we have a run flag and is it true?
	  (cond ((and (getf *spin-conf* :run) 
				  (equal (getf *spin-conf* :run) t))
			 (run)))) ; start the function defined in :main
	
	(defun spin ()
	  "Complete setup, recreate, build, run cycle configured
     with given configuration filepath. *spin-filename* must
     be set."
	  (spin-with *spin-filename*))

	(defun dependencies ()
	  "Spin (load-recreate-build-run) dependency-projects from current
     configuration."
	  (handler-case
		  (let ((dependencies (getf *spin-conf* :dependencies)))
			(cond (dependencies 
				   (mapc (lambda (configuration-filepath)
						   (format t "~%Spinning dependencies at ~A~%" 
								   configuration-filepath)
						   (reset-global-params)
						   (setup-with configuration-filepath) ; setup configuration
						   (spin-with configuration-filepath)) dependencies)) ; spin confs
				  ((not dependencies) ; no dependency
				   (print "No dependencies found..."))))
		(error (condition)
		  (format t "Error in dependencies! ~A~%" condition))))

	(print "Spin startup...")
	(setq *project-directory* *default-pathname-defaults*) ; save current directory-path
	(setup) ; load project-configuration
	(dependencies) ; spin dependent projects
	(reset-global-params) ; set back to default
	(setq *default-pathname-defaults* *project-directory*) ; restore project directory-path
	(setup) ; reload project-configuration for directly configured project
	(spin)) ; spin (load-, recreate-, build-, run) project

