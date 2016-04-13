(cl:in-package :cl-user)
(when (cl:find-package :ros.util)
  (pushnew :ros.util *features*))

(defpackage :ros.util
  (:use :cl)
  (:export :uname :uname-m :homedir :config :use :impl :which :list%
           :parse-version-spec :download :expand :sh :version :chdir
           :core-extention))

(in-package :ros.util)

(defun uname ()
  "Returns uname. Example: linux , darwin"
  (ros:roswell '("roswell-internal-use" "uname") :string t))

(defun uname-m ()
  "Returns the machine type as a string. Example: x86-64"
  (ros:roswell '("roswell-internal-use" "uname" "-m") :string t))

(defun homedir ()
  "Returns the user-level installation directory of roswell. Example: /home/user/.roswell"
  (ros:opt "homedir"))

(defun impl (&optional (imp ""))
  "Returns a full name/version of an implementation. Default: current system.
Example:
  (impl) -> sbcl/1.3.2 (current system)
  (impl \"ccl\") -> ccl/system (currently installed system) "
  (ros:roswell `("roswell-internal-use" "impl" ,imp) :string t))

(defun which (cmd)
  "equivalent to \"which\" command on shell"
  (let ((result (ros:roswell `("roswell-internal-use" "which" ,cmd) :string t)))
    (unless (zerop (length result))
      result)))

(defun download (uri file &key proxy)
  "Interface to curl4 in the roswell C binary"
  (declare (ignorable proxy))
  (ensure-directories-exist file)
  (ros:roswell `("roswell-internal-use" "download" ,uri ,file) :interactive nil))

(defun expand (archive dest &key verbose)
  "Interface to the roswell C binary"
  (ros:roswell `(,(if verbose "-v" "")"roswell-internal-use tar" "-xf" ,archive "-C" ,dest)
               (or #-win32 :interactive nil) nil))

(defun core-extention (&optional (impl (ros:opt "impl")))
  "Interface to the roswell C binary. Returns \"core\" on sbcl."
  (ros:roswell `("roswell-internal-use" "core-extention" ,impl) :string t))

(defun config (c)
  "Interface to roswell C binary."
  (ros:roswell `("config" "show" ,c) :string t))

(defun (setf config) (val item)
  "Interface to roswell C binary."
  (ros:roswell `("config" "set" ,item ,val) :string t)
  val)

(defun list% (&rest params)
  (string-right-trim #.(format nil "~A" #\Newline)
                     (ros:roswell `("list" ,@params) :string nil)))

(defun chdir (dir &optional (verbose t))
  (funcall (intern (string :chdir) :uiop/os) dir)
  (when verbose
    (format t "~&chdir ~A~%" dir)))

(defun sh ()
  (or #+win32
      (unless (ros:getenv "MSYSCON")
	(format nil "~A" (#+sbcl sb-ext:native-namestring #-sbcl namestring
				 (merge-pathnames (format nil "impls/~A/~A/msys~A/usr/bin/bash" (uname-m) (uname)
							  #+x86-64 "64" #-x86-64 "32") (homedir)))))
      (which "bash")
      "sh"))

(defun version (&optional (opt ""))
  "Interface to roswell C binary."
  (ros:roswell `("roswell-internal-use" "version"
					,(string-downcase opt)) :string t))

(defvar *version*
  `(
    :roswell ,(version)
	     :lisp ,(lisp-implementation-type)
	     :version ,(lisp-implementation-version)
	     :date ,(get-universal-time)))

(defun parse-version-spec (string)
  "Parse the given version specification string and returns a list of strings (LISP VERSION).
If it does not contain a version substring, VERSION becomes a null.
If it is a version string only (detected when it starts from digit-char), LISP becomes NIL.
Examples:
ccl-bin/1.11 -> (\"ccl-bin\" \"1.11\")
ccl-bin      -> (\"ccl-bin\" nil)
1.11         -> (nil \"1.11\")
"
  (let ((pos (position #\/ string)))
    (if pos
        `(,(subseq string 0 pos) ,(subseq string (1+ pos)))
        (if (digit-char-p (aref string 0))
            `(nil ,string)
            `(,string nil)))))

(defun use (arg)
  "Parse the lisp version string (such as ccl-bin/1.11) and set it to the correct config slot(s)"
  (when (and arg
             (ignore-errors
               (ros:roswell `("-L" ,arg "version=t" "run"))))
    (destructuring-bind (lisp version) (parse-version-spec arg)
      (cond ((and lisp version)
             (setf (config "default.lisp") lisp
                   (config (format nil "~A.version" lisp)) version))
            (lisp
             (setf (config "default.lisp")
                   lisp))
            (version
             (setf (config (format nil "~A.version" (config "default.lisp")))
                   version))))
    t))
