(defpackage "MACHINAE"
  (:use "COMMON-LISP"))

(in-package machinae)

(defun print-am0 (s stream depth)
  (declare (ignore depth))
  (format stream "#<~A, ~A, ~A, ~A, ~A>" (am0-ic s) (am0-stack s) (am0-mem s) (am0-inp s) (am0-out s)))

(defstruct (am0 (:print-function print-am0))
  (ic 1)
  (stack nil)
  (mem nil)
  (inp "")
  (out ""))

(defun dispatch (mnem)
  (intern (format nil "~A~A" 'am0- (string-upcase mnem))))

(defun instruction (prog idx)
  (let ((i (1- idx)))
    (if (eq -1 i)
	nil
      (nth i prog))))

(defun run (prog am0)
  (let* ((ic (am0-ic am0))
	 (i (instruction prog ic)))
    (if (null i)
	am0
      (run prog (if (consp i)
		    (funcall (dispatch (car i)) am0 (cadr i))
		  (funcall (dispatch i) am0))))))

(defun step-ic (am0)
  (incf (am0-ic am0))
  am0)

(defmacro opname (op)
  `(intern (format nil "~A~A" 'am0- ,op)))

(defmacro with-stack (am0 &key (bindings nil) body)
  `(setf (am0-stack ,am0)
	 (let* ((s (am0-stack ,am0))
		,@bindings)
	   ,body)))

(defmacro binary-op (op filter impl)
  `(defun ,(opname op) (am0)
     (with-stack am0
		 :bindings ((r (pop s))
			    (l (pop s)))
		 :body (push (,filter (,impl l r)) s))
  (step-ic am0)))

(defmacro binary-function (op impl)
  `(binary-op ,op truncate ,impl))

(defmacro predicate (op impl)
  `(binary-op ,op map-bool ,impl))

(defun map-bool (b)
  (if b 1
    0))

(defun neq (l r)
  (not (= l r)))

(binary-function add +)
(binary-function sub -)
(binary-function mul *)
(binary-function div /)
(binary-function mod mod)

(predicate gt >)
(predicate lt <)
(predicate eq =)
(predicate ge >=)
(predicate le <=)
(predicate ne neq)

(defun am0-lit (am0 val)
  (with-stack am0 :body (push val s))
  (step-ic am0))

(defun am0-jmp (am0 tgt)
  (setf (am0-ic am0) tgt)
  am0)

(defun am0-jmc (am0 tgt)
  (let ((c 1))
    (with-stack am0 :body (progn
			    (setf c (pop s))
                (push c s)
			    s))
    (if (zerop c)
	(am0-jmp am0 tgt)
      (step-ic am0))))

(defun get-mem (am0 idx)
  (nth (1- idx) (am0-mem am0)))

(defun set-mem (am0 idx val)
  (let* ((i (1- idx))
	 (lst (am0-mem am0))
	 (elt (nth i lst)))
    (if (null elt)
	(setf lst (append lst
			  (make-list (- i (length lst)))
			  (list val)))
      (setf (nth i lst) val))
    (setf (am0-mem am0) lst)))

(defun am0-read (am0 idx)
  (let* ((i (am0-inp am0))
	 (n (position #\  i)))
    (set-mem am0 idx (read-from-string i t "" :start 0 :end n))
    (setf (am0-inp am0) (if (null n)
			    ""
			  (subseq i (1+ n))))
    (step-ic am0)))

(defun am0-write (am0 idx)
  (setf (am0-out am0) (concatenate 'string (am0-out am0) (format nil "~A" (get-mem am0 idx))))
  (step-ic am0))

(defun am0-store (am0 idx)
  (with-stack am0 :bindings ((val (pop s)))
	      :body (progn
		      (setf (am0-mem am0) (set-mem am0 idx val))
		      s))
  (step-ic am0))

(defun am0-load (am0 idx)
  (with-stack am0 :bindings ((val (nth (1- idx) (am0-mem am0))))
	      :body (push val s))
  (step-ic am0))

(defun do-test ()
  (run '((lit 3)
         (lit 1)
         sub
         (jmc 6)
         (jmp 2))
       (make-am0)))
