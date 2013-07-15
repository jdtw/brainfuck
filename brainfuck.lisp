;;;; brainfuck.lisp

(in-package #:brainfuck)

;;;;
;; eval functions
;;;;

(defstruct bf-state
  (mem (make-array 30000 :element-type '(unsigned-byte 8) :initial-element 0))
  (ip 0)
  (ptr 0)
  (program ""))

(defmacro with-bf-slots ((mem ip ptr program) state &body body)
  (let ((s (gensym)))
    `(let ((,s ,state))
       (symbol-macrolet ((,mem (bf-state-mem ,s))
                         (,ip (bf-state-ip ,s))
                         (,ptr (bf-state-ptr ,s))
                         (,program (bf-state-program ,s)))
         ,@body))))


(defun find-bracket (str pos &optional backwards)
  (let ((open (if backwards #\] #\[))
        (close (if backwards #\[ #\])))
    (loop with depth = 0 for c = (char str pos) do
         (cond 
           ((eq c open)
            (incf depth))
           ((eq c close)
            (progn
              (decf depth)
              (when (zerop depth)
                (return pos)))))
         (if backwards (decf pos) (incf pos)))))

(defun bf-eval (state)
  (with-bf-slots (mem ip ptr program) state
    (let ((length (length program)))
      (loop while (< ip length) do 
           (case (char program ip)
             (#\> (incf ptr)
                  (incf ip))
             (#\< (decf ptr)
                  (incf ip))
             (#\+ (setf (aref mem ptr) (mod (1+ (aref mem ptr)) #x100))
                  (incf ip))
             (#\- (setf (aref mem ptr) (mod (1- (aref mem ptr)) #x100))
                  (incf ip))
             (#\. (write-char (code-char (aref mem ptr)))
                  (incf ip))
             (#\, (setf (aref mem ptr) (char-code (read-char)))
                  (incf ip))
             (#\[ (if (zerop (aref mem ptr))
                      (setf ip (find-bracket program ip))
                      (incf ip)))
             (#\] (if (not (zerop (aref mem ptr)))
                      (setf ip (find-bracket program ip t))
                      (incf ip)))
             (otherwise (incf ip)))))))

(defun bf-repl ()
  (loop do
       (princ "BRAINFUCK> ")
       (let ((line (read-line)))
         (if (equal line "")
             (return)
             (bf-eval (make-bf-state :program line))))))

;;;;
;; compile functions
;;;;

(defun bf-body (program in out)
  `(lambda ()
     (let ((ptr 0) (mem (make-array 30000 :element-type '(unsigned-byte 8) :initial-element 0)))
       ,@(loop
            with stack = (list nil)
            for c across program do
              (case c
                (#\> (push '(incf ptr) (car stack)))
                (#\< (push '(decf ptr) (car stack)))
                (#\+ (push '(setf (aref mem ptr) (mod (1+ (aref mem ptr)) #x100)) (car stack)))
                (#\- (push '(setf (aref mem ptr) (mod (1- (aref mem ptr)) #x100)) (car stack)))
                (#\. (push `(write-char (code-char (aref mem ptr)) ,out) (car stack)))
                (#\, (push `(setf (aref mem ptr) (char-code (read-char ,in nil #\Nul))) (car stack)))
                (#\[ (push nil stack))
                (#\] (push (make-bf-loop (nreverse (pop stack))) (car stack))))
            finally (return (if (= (length stack) 1)
                                (nreverse (car stack))
                                (error "Unmatched bracket"))))))) 

(defun make-bf-loop (body)
  `(loop while (not (zerop (aref mem ptr)))
        ,@(when body `(do ,@body))))

(defun bf-compile (program &key (in *standard-input*) (out *standard-output*))
  (time (compile nil (bf-body program in out))))
