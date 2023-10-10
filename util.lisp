(in-package :jin-scripts)

;;;; This file contains the basic utils and two huge components:
;;;; Linked Lists and Limited Caches. I should seperate the last
;;;; two into a different file.

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro make-thread-safe-hash-table (&rest args)
  "create thread safe hash table."
  `(make-hash-table ,@args
                    #+lispworks :single-thread
                    #+lispworks nil
                    #+sbcl :synchronized
                    #+sbcl t))

;;;; - Linked Lists -
;;;; This part provides the data structure (doubly) linked lists,
;;;; finite (doubly) linked lists, and FIFO (first in first out).
;;;; It is built upon the data structure "3-cells".

(defclass 3-cell ()
  ((storage :initarg :storage
            :accessor storage)
   (head :initform nil
         :accessor head)
   (tail :initform nil
         :accessor tail)))

(defmethod deep-copy! ((x 3-cell))
  "Produce a deep copy for the input 3-cell. Only the storage is
copied, while the head and tail are not."
  (let ((y (make-instance '3-cell)))
    (setf (storage y) (deep-copy! (storage x)))
    y))

(defmethod chain ((x 3-cell) (y 3-cell))
  "Chain the input 3-cells by mutating their slots."
  (setf (tail x) y)
  (setf (head y) x))

(defun chain-3-cells (&rest xs)
  ;; XS is a list of 3-cells.
  (when (> (length xs) 1)
    (chain (nth 0 xs) (nth 1 xs))
    (apply #'chain-3-cells (cdr xs))
    t))

(defmethod successors ((x 3-cell))
  (let ((pointer x))
    (iter
      (if (null (tail pointer))
          (progn
            (collect (storage pointer) into result)
            (return result))
          (progn
            (collect (storage pointer) into result)
            (setf pointer (tail pointer)))))))

(defmethod predecessors ((x 3-cell))
  (let ((pointer x))
    (iter
      (if (null (head pointer))
          (progn
            (collect (storage pointer) into result)
            (return result))
          (progn
            (collect (storage pointer) into result)
            (setf pointer (head pointer)))))))

(let ((x0 (make-instance '3-cell :storage 0))
      (x1 (make-instance '3-cell :storage 1))
      (x2 (make-instance '3-cell :storage 2)))
  (chain-3-cells x0 x1 x2)
  (assert (equal `(0 1 2) (successors x0)))
  (assert (equal `(0) (predecessors x0)))
  (assert (equal `(1 2) (successors x1)))
  (assert (equal `(1 0) (predecessors x1)))
  (assert (equal `(2 1 0) (predecessors x2))))

;;; Doubly linked list

(defclass linked-list ()
  ((head :initform nil
         :accessor head)
   (tail :initform nil
         :accessor tail)))

(defmethod print-object ((l linked-list) stream)
  (format stream "#<linked-list>"))

(defmethod null? ((ll linked-list))
  (null (head ll)))

(defmethod .length ((ll linked-list))
  (let ((length 0))
    (when (head ll)
      (iter
        (with node = (head ll))
        (incf length)
        (if (eq node (tail ll))
            (return)
            (setf node (tail node)))))
    length))

(defmethod .nth-cell ((n integer) (ll linked-list))
  "Return the nth cell. When n is negative, it counts from the
tail."
  (if (alexandria:non-negative-integer-p n)
      (when (< n (.length ll))
        (iter
          (with node = (head ll))
          (repeat n)
          (setf node (tail node))
          (finally (return node))))
      (progn
        (setf n (- (1+ n)))
        (when (< n (.length ll))
          (iter
            (with node = (tail ll))
            (repeat n)
            (setf node (head node))
            (finally (return node)))))))

(defmethod .nth ((n integer) (ll linked-list))
  "Return the nth cell (1st value) and its content (0th value).
When n is negative, it counts from the tail."
  (let* ((cell (.nth-cell n ll))
         (storage (when cell (storage cell))))
    (values storage cell)))

(defmethod 3-cells ((ll linked-list))
  (let ((pointer (head ll)))
    (when pointer
      (iter
        (collect pointer into result)
        (if (tail pointer)
            (setf pointer (tail pointer))
            (return result))))))

(defmethod .pop ((cell 3-cell) (ll linked-list))
  ;; This may slow down the system. How can I check this if and
  ;; only if SAFETY is set to 3?
  ;; (assert (find cell (3-cells ll)))
  (let ((storage (storage cell)))
    (when (find cell (3-cells ll))
      (if (head cell)
          (setf (tail (head cell)) (tail cell))
          (setf (head ll) (tail cell)))
      (if (tail cell)
          (setf (head (tail cell)) (head cell))
          (setf (tail ll) (head cell))))
    (values storage cell)))

(defmethod .pop ((n integer) (ll linked-list))
  (multiple-value-bind (nth-storage nth-cell)
      (.nth n ll)
    (declare (ignore nth-storage))
    (if nth-cell
        (.pop nth-cell ll)
        (error "Linked list ~a does not have the ~ath cell." ll n))))

(defmethod list<- ((l linked-list))
  (mapcar #'storage (3-cells l)))

(defmethod linked-list<- ((xs list))
  "Coerce the list XS into a linked list."
  (let ((ll (make-instance 'linked-list))
        (3-cells (iter
                   (for x in xs)
                   (collect (make-instance '3-cell :storage x)))))
    (setf (head ll) (car 3-cells))
    (setf (tail ll) (car (last 3-cells)))
    (apply #'chain-3-cells 3-cells)
    ll))

(defmethod deep-copy! ((x linked-list))
  (linked-list<- (mapcar #'deep-copy! (list<- x))))

(defmethod .append (x (l linked-list))
  "Appends X into the doubly linked list L. It returns two values:
X and the cell that is appended."
  (let ((3-cell (make-instance '3-cell :storage x)))
    (if (null? l)
        (progn
          (setf (tail l) 3-cell)
          (setf (head l) 3-cell))
        (progn
          (chain-3-cells (tail l) 3-cell)
          (setf (tail l) 3-cell)))
    (values x 3-cell)))

(defmethod .prepend (x (l linked-list))
  "Prepends X into the doubly linked list L. It returns two values:
X and the cell that is prepended."
  (let ((3-cell (make-instance '3-cell :storage x)))
    (if (null? l)
        (progn
          (setf (tail l) 3-cell)
          (setf (head l) 3-cell))
        (progn
          (chain-3-cells 3-cell (head l))
          (setf (head l) 3-cell)))
    (values x 3-cell)))

(defmethod clear ((l linked-list))
  "Clear the linked-list."
  (setf (tail l) nil
        (head l) nil)
  l)

(defmethod .tail-drop ((l linked-list))
  (when (tail l)
    (.pop (tail l) l)))

(defmethod .head-drop ((l linked-list))
  (when (head l)
    (.pop (head l) l)))

(let ((xs `(0 1 2 3 4 5 6 7 8 9))
      (ys `(0))
      (zs `()))
  (assert (equal xs (list<- (linked-list<- xs))))
  (assert (equal ys (list<- (linked-list<- ys))))
  (assert (equal zs (list<- (linked-list<- zs)))))

(let* ((xs `(0 1 2 3 4 5 6 7 8 9))
       (ll (linked-list<- xs)))
  (assert (= 10 (.length ll)))

  ;; .nth with nonnegative N.
  (assert (= 0    (.nth 0  (linked-list<- xs))))
  (assert (= 5    (.nth 5  (linked-list<- xs))))
  (assert (= 9    (.nth 9  (linked-list<- xs))))
  (assert (eq nil (.nth 10 (linked-list<- xs))))

  ;; .nth with negative N.
  (assert (= 9    (.nth -1  (linked-list<- xs))))
  (assert (= 5    (.nth -5  (linked-list<- xs))))
  (assert (= 1    (.nth -9  (linked-list<- xs))))
  (assert (= 0    (.nth -10 (linked-list<- xs))))
  (assert (eq nil (.nth -11 (linked-list<- xs))))

  ;; .nth - all values
  (multiple-value-bind (storage cell)
      (.nth 0 (linked-list<- xs))
    (assert (= 0 storage))
    (assert (typep cell '3-cell))
    (assert (= (storage cell) storage)))
  (multiple-value-bind (storage cell)
      (.nth 100 (linked-list<- xs))
    (assert (eq nil storage))
    (assert (eq nil cell))))

(let ((l (make-instance 'linked-list)))
  (assert (eq l l))
  (assert (null? l))
  (assert (equal `() (list<- l)))
  (assert (= 0 (.length l)))
  (assert (= 0 (.append 0 l)))
  (.append 1 l)
  (.append 2 l)
  (assert (equal `(0 1 2) (list<- l)))
  (.prepend -1 l)
  (.prepend -2 l)
  (assert (equal `(-2 -1 0 1 2) (list<- l)))
  (assert (= 5 (.length l))))

;; FIXME
(let ((l (linked-list<- `(0 1 2))))
  (.tail-drop l) (assert (equal `(0 1) (list<- l)))
  (.tail-drop l) (assert (equal `(0)   (list<- l)))
  (.tail-drop l) (assert (equal `()    (list<- l)))
  (.tail-drop l) (assert (equal `()    (list<- l)))
  )

;; FIXME
(let ((l (linked-list<- `(0 1 2))))
  (.head-drop l) (assert (equal `(1 2) (list<- l)))
  (.head-drop l) (assert (equal `(2)   (list<- l)))
  (.head-drop l) (assert (equal `()    (list<- l)))
  (.head-drop l) (assert (equal `()    (list<- l))))

;; FIXME
;; Tests for deep-copy.
;; (let ((l (linked-list<- `(0 1 2)))
;;       (l-))
;;   (setf l- (deep-copy! l))
;;   (.head-drop l)
;;   (assert (equal   `(1 2) (list<- l)))
;;   (assert (equal `(0 1 2) (list<- l-))))

;; Tests for tail-drop and head-drop.
(let ((l (make-instance 'linked-list)))
  (.append 0 l)
  (.append 1 l)
  (.append 2 l)

  ;; Drop the tail and test what #'.tail-drop returns.
  (multiple-value-bind (value storage)
      (.tail-drop l)
    (assert (= value 2))
    (assert (typep storage '3-cell))
    (assert (eq (head storage) (tail l)))
    (assert (equal (tail storage) nil)))
  (assert (equal `(0 1) (list<- l)))

  ;; Drop the head and test what #'.tail-drop returns.
  (multiple-value-bind (value storage)
      (.head-drop l)
    (assert (= value 0))
    (assert (typep storage '3-cell))
    (assert (eq (tail storage) (head l)))
    (assert (equal (head storage) nil)))
  (assert (equal `(1) (list<- l))))

(let ((l (make-instance 'linked-list)))
  (.append 0 l)
  (.pop 0 l)
  (assert (eq :error
              (handler-case (.pop 0 l)
                (error (c) (declare (ignore c)) :error)))))

(let ((l (make-instance 'linked-list)))
  (.append 0 l)
  (.append 1 l)
  (.append 2 l)
  (assert (equal `(0 1 2) (list<- l)))

  (.pop 1 l)
  (assert (equal `(0 2) (list<- l)))
  (assert (= (.nth 0 l) 0))
  (assert (= (.nth 1 l) 2))

  (.pop 1 l)
  (assert (equal `(0) (list<- l)))
  (assert (= (.nth 0 l) 0))

  (multiple-value-bind (storage cell)
      (.pop 0 l)
    (assert (= 0 storage))
    (assert (typep cell '3-cell)))
  (assert (equal `() (list<- l)))
  (assert (null (.nth 0 l))))

;; Tests for .append and its return values.
(let ((l (make-instance 'linked-list)))
  (multiple-value-bind (x 3-cell)
      (.append 0 l)
    (assert (= x 0))
    (assert (typep 3-cell '3-cell))
    (assert (= x (storage 3-cell)))))

;; Tests for .prepend and its return values.
(let ((l (make-instance 'linked-list)))
  (multiple-value-bind (x 3-cell)
      (.prepend 0 l)
    (assert (= x 0))
    (assert (typep 3-cell '3-cell))
    (assert (= x (storage 3-cell)))))

;;; Finite Linked List

(defclass finite-linked-list (linked-list)
  ((max-size :initarg :max-size
             :initform 10
             :accessor max-size)
   (%length :initform 0
            :accessor %length
            :documentation "Record of its current length for avoiding computation every time.")))

(defmethod print-object ((l finite-linked-list) stream)
  (format stream "#<finite-linked-list>"))

(defmethod finite-linked-list<- ((xs list))
  "Coerce the list XS into a finite linked list."
  (let ((ll (make-instance 'finite-linked-list))
        (3-cells (iter
                   (for x in xs)
                   (collect (make-instance '3-cell :storage x)))))
    (setf (max-size ll) (length xs))
    (setf (%length ll) (length xs))
    (setf (head ll) (car 3-cells))
    (setf (tail ll) (car (last 3-cells)))
    (apply #'chain-3-cells 3-cells)
    ll))

(defmethod .pop ((cell 3-cell) (ll finite-linked-list))
  (when (head ll)
    (decf (%length ll)))
  (call-next-method))

(defmethod .pop ((n integer) (ll finite-linked-list))
  (call-next-method))

(defmethod deep-copy! ((x finite-linked-list))
  (call-next-method))

;; FIXME
;; (let* ((ll (finite-linked-list<- `(0 1 2)))
;;        (ll- (deep-copy! ll)))
;;   (assert (eq (.length ll) (%length ll)))

;;   (.pop 1 ll)
;;   (assert (eq (.length ll) (%length ll)))
;;   (assert (equal `(0 2) (list<- ll)))

;;   (.pop 0 ll)
;;   (assert (eq (.length ll) (%length ll)))
;;   (assert (equal `(2) (list<- ll)))

;;   ;; Prove that ll-, as a deep copy, is not mutated.
;;   (assert (equal `(0 1 2) (list<- ll-))))

(defmethod .append (x (l finite-linked-list))
  "Append X into the finite doubly linked list L. It returns four
values: [0: x], [1: cell with x] [2: whether it pops something
out (T/NIL)] [3: the popped cell, if any, orelse NIL]."
  (let ((pop? nil))
    (cond
      ((< (%length l) (max-size l))
       (setf pop? nil)
       (incf (%length l))
       (multiple-value-bind (x 3-cell)
           (call-next-method)
         (let ((popped-3-cell nil))
           (values x 3-cell pop? popped-3-cell))))
      ((= (%length l) (max-size l))
       (setf pop? t)
       (multiple-value-bind (storage popped-3-cell)
           (.pop 0 l)
         (declare (ignore storage))
         (incf (%length l))
         (multiple-value-bind (x 3-cell)
             (call-next-method)
           (values x 3-cell pop? popped-3-cell)))))))

(let* ((ll (finite-linked-list<- `(0 1 2))))
  (assert (= 3 (%length ll)))

  (.append 3 ll)
  (assert (equal `(1 2 3) (list<- ll)))
  (assert (= 3 (%length ll)))

  (.append 4 ll)
  (assert (equal `(2 3 4) (list<- ll)))
  (assert (= 3 (%length ll)))

  (.append 5 ll)
  (assert (equal `(3 4 5) (list<- ll)))
  (assert (= 3 (%length ll)))

  (.pop 0 ll) (.pop 0 ll) (assert (equal `(5) (list<- ll)))

  (.append 6 ll) (assert (equal `(5 6)   (list<- ll)))
  (.append 7 ll) (assert (equal `(5 6 7) (list<- ll)))
  (.append 8 ll) (assert (equal `(6 7 8) (list<- ll)))
  (.append 9 ll) (assert (equal `(7 8 9) (list<- ll))))

;; An integration test to catch bug.
(let* ((ll (finite-linked-list<- `(0 1 2))))
  (assert (= 3 (%length ll))) (.pop 0 ll)
  (assert (= 2 (%length ll))) (.pop 0 ll)
  (assert (= 1 (%length ll))) (.pop 0 ll)
  (assert (= 0 (%length ll)))
  (.append 3 ll) (assert (equal `(3)     (list<- ll))) (assert (= 1 (%length ll)))
  (.append 4 ll) (assert (equal `(3 4)   (list<- ll))) (assert (= 2 (%length ll)))
  (.append 5 ll) (assert (equal `(3 4 5) (list<- ll))) (assert (= 3 (%length ll)))
  (.append 6 ll) (assert (equal `(4 5 6) (list<- ll))) (assert (= 3 (%length ll))))

(let* ((ll     ; NOTE LL is a finite linked list with max size 3.
         (finite-linked-list<- `(0 1 2))))
  ;; Popped and element to make room.
  (.pop 2 ll)
  ;; First case: When there is room.
  (multiple-value-bind (x 3-cell popped? popped-3-cell)
      (.append 2 ll)
    (assert (= 2 x))
    (assert (typep 3-cell '3-cell))
    (assert (= x (storage 3-cell)))
    (assert (eq nil popped?))
    (assert (eq nil popped-3-cell)))
  ;; Second case: When overflow.
  (multiple-value-bind (x 3-cell popped? popped-3-cell)
      (.append 3 ll)
    (assert (= 3 x))
    (assert (typep 3-cell '3-cell))
    (assert (= x (storage 3-cell)))
    (assert (eq t popped?))
    (assert (typep popped-3-cell '3-cell))
    (assert (= 0 (storage popped-3-cell)))))

(defmethod .prepend (x (l finite-linked-list))
  "Prepend X into the finite doubly linked list L. It returns four
values: [0: x], [1: cell with x] [2: whether it pops something
out (T/NIL)] [3: the popped cell, if any, orelse NIL]."
  (let ((pop? nil))
    (cond
      ((< (%length l) (max-size l))
       (incf (%length l))
       (multiple-value-bind (x 3-cell)
           (call-next-method)
         (let ((popped-3-cell nil))
           (values x 3-cell pop? popped-3-cell))))
      ((= (%length l) (max-size l))
       (setf pop? t)
       (multiple-value-bind (storage popped-3-cell)
           (.pop (1- (%length l)) l)
         (declare (ignore storage))
         (incf (%length l))
         (multiple-value-bind (x 3-cell)
             (call-next-method)
           (values x 3-cell pop? popped-3-cell)))))))

(let* ((ll (finite-linked-list<- `(2 1 0))))
  (.prepend 3 ll) (assert (equal `(3 2 1) (list<- ll)))
  (.prepend 4 ll) (assert (equal `(4 3 2) (list<- ll)))
  (.prepend 5 ll) (assert (equal `(5 4 3) (list<- ll)))

  (.pop 1 ll) (.pop 1 ll) (assert (equal `(5) (list<- ll)))

  (.prepend 6 ll) (assert (equal `(6 5) (list<- ll)))
  (.prepend 7 ll) (assert (equal `(7 6 5) (list<- ll)))
  (.prepend 8 ll) (assert (equal `(8 7 6) (list<- ll)))
  (.prepend 9 ll) (assert (equal `(9 8 7) (list<- ll))))

;; Test for .prepend and its returned values.
(let* ((ll ; NOTE LL is a finite linked list with max size 3.
         (finite-linked-list<- `(2 1 0))))
  ;; Popped and element to make room.
  (.pop 0 ll)
  ;; First case: When there is room.
  (multiple-value-bind (x 3-cell popped? popped-3-cell)
      (.prepend 2 ll)
    (assert (= 2 x))
    (assert (typep 3-cell '3-cell))
    (assert (= x (storage 3-cell)))
    (assert (eq nil popped?))
    (assert (eq nil popped-3-cell)))
  ;; Second case: When overflow.
  (multiple-value-bind (x 3-cell popped? popped-3-cell)
      (.prepend 3 ll)
    (assert (= 3 x))
    (assert (typep 3-cell '3-cell))
    (assert (= x (storage 3-cell)))
    (assert (eq t popped?))
    (assert (typep popped-3-cell '3-cell))
    (assert (= 0 (storage popped-3-cell)))))

(defmethod clear ((l finite-linked-list))
  "Clear the linked-list."
  (setf (%length l) 0)
  (call-next-method))

;; Test for #'clear.
(let* ((ll (finite-linked-list<- `(2 1 0))))
  (clear ll)
  (assert (equal `() (list<- ll)))
  (assert (= 0 (%length ll)))
  (assert (= 0 (.length ll)))
  (assert (= 3 (max-size ll))))

(defmethod .tail-drop ((l finite-linked-list))
  (call-next-method))

(defmethod .head-drop ((l finite-linked-list))
  (call-next-method))

;; Tests for #'.tail-drop and #'.head-drop
(let* ((ll (finite-linked-list<- `(2 1 0))))
  (assert (= 3 (%length ll)))
  (assert (= 3 (.length ll)))

  ;; .tail-drop: returned values and side-effects
  (multiple-value-bind (x 3-cell)
      (.tail-drop ll)
    (assert (= x 0))
    (assert (typep 3-cell '3-cell))
    (assert (= x (storage 3-cell))))
  (assert (equal `(2 1) (list<- ll)))
  (assert (= 2 (%length ll)))
  (assert (= 2 (.length ll)))
  (assert (= 3 (max-size ll)))

  ;; .head-drop: returned values and side-effects
  (multiple-value-bind (x 3-cell)
      (.head-drop ll)
    (assert (= x 2))
    (assert (typep 3-cell '3-cell))
    (assert (= x (storage 3-cell))))
  (assert (equal `(1) (list<- ll)))
  (assert (= 1 (%length ll)))
  (assert (= 1 (.length ll)))
  (assert (= 3 (max-size ll))))

;;; FIFO

(defclass fifo ()
  ((storage :initform (make-instance 'linked-list)
            :type linked-list
            :reader storage)
   (max-size :initform -1
             :initarg :max-size
             :type number
             :reader max-size
             :documentation "The maximal length of the fifo. -1 means infinitely long.")))

(defmethod .length ((fifo fifo))
  (.length (storage fifo)))

(defmethod .dequeue ((fifo fifo))
  (.tail-drop (storage fifo)))

(defmethod .enqueue (x (fifo fifo))
  (.prepend x (storage fifo))
  (when (and (not (= (max-size fifo) -1)) ; -1 means infinity
             (> (.length fifo) (max-size fifo)))
    (.dequeue fifo)))

;; Test a fifo with infinite max length.
(let ((fifo (make-instance 'fifo)))
  (assert (= 0 (.length fifo)))
  (.enqueue 1 fifo)
  (.enqueue 2 fifo)
  (.enqueue 3 fifo)
  (.enqueue 4 fifo)
  (.enqueue 5 fifo)
  (assert (= 5 (.length fifo)))
  (assert (equal `(5 4 3 2 1) (list<- (storage fifo))))
  ;; (assert (= 1 (.dequeue fifo)))            ; FIXME Make this correct by fixing the utils for linked list.
  (.dequeue fifo) (assert (equal `(5 4 3 2) (list<- (storage fifo))))
  (.dequeue fifo) (assert (equal `(5 4 3)   (list<- (storage fifo))))
  (.dequeue fifo) (assert (equal `(5 4)     (list<- (storage fifo))))
  (.dequeue fifo) (assert (equal `(5)       (list<- (storage fifo))))
  (.dequeue fifo) (assert (equal `()        (list<- (storage fifo))))
  (.dequeue fifo) (assert (equal `()        (list<- (storage fifo)))))

;; Test a fifo with finite max length.
(let ((fifo (make-instance 'fifo :max-size 3)))
  (assert (= 0 (.length fifo)))
  (.enqueue 1 fifo)
  (.enqueue 2 fifo)
  (.enqueue 3 fifo)
  (.enqueue 4 fifo)
  (.enqueue 5 fifo)
  (assert (= 3 (.length fifo)))
  (assert (equal `(5 4 3) (list<- (storage fifo))))
  (.dequeue fifo) (assert (equal `(5 4) (list<- (storage fifo))))
  (.dequeue fifo) (assert (equal `(5)   (list<- (storage fifo))))
  (.dequeue fifo) (assert (equal `()    (list<- (storage fifo))))
  (.dequeue fifo) (assert (equal `()    (list<- (storage fifo)))))

;;;; - Limited Cache -
;;;;
;;;; This part implements cache with limit. It depends on the
;;;; libraries local-time and bordeaux-thread. It depends on
;;;; Linked Lists.

(defvar *managed-limited-caches* nil)

(defconstant +limited-cache/lock+ (bt:make-recursive-lock "limited-cache main lock"))

(defconstant +limited-cache/default-timeout+ 60)

(defconstant +limited-cache/default-minimal-timeout+ 0.01)

(defconstant +limited-cache/manager-name+ "limited-cache manager")

;; NOTE Whenever the manager wakes up, it performs tasks on
;; various lists that could be resource-intensive. Therefore, if
;; the tasks are not critical, it is advisable to increase the
;; snooze time. However, it's important to note that longer
;; snooze times may result in less accurate clearing of expired
;; limited-cachesxpected timeframe.
(defparameter *manager-snooze-time* 10
  "You can change this dynamically and the manager will act accordingly.
Set log level to :debug to see its effect.")

(defconstant +limited-cache/default-minimal-snooze-time+ (/ +limited-cache/default-minimal-timeout+ 2))

(defclass limited-cache-table-value ()
  ((value :initform nil
          :initarg :value
          :accessor %value
          :documentation "The indended value.")
   (3-cell :initform (make-instance '3-cell)
           :initarg :3-cell
           :type 3-cell
           :accessor %3-cell
           :documentation "The associated 3-cell of the key; the 3-cell is maintained in the queue.")
   (last-access-time
    :initform (local-time:now)
    :initarg :last-access-time
    :type local-time:timestamp
    :accessor %last-access-time
    :documentation "The last access time of the entry.")))

;; The gauge for life-span. Limited cache cannot affort too short life-span.

(defclass limited-cache ()
  ((table :initarg :table
          :accessor table
          :documentation "
The hash table that records the cache
and other essential information of each entry.
The value of each key is an object of a limited-cache-table-value.")

   (queue :initform (make-instance 'linked-list)
          :type linked-list
          :accessor queue
          :documentation "
The queue that stores the keys.
It is a doubly linked list that keeps track of the order
of the last access time of the keys. This helps optimizing
the pruning process.")

   (life-span :initarg :life-span
              :reader life-span
              :documentation "
The life span (in second) for each entry in this cache.")

   (max-size :initform -1
             :initarg :max-size
             :type number
             :reader max-size
             :documentation "
The maximal amount of entries in the limited cache.
Negative one (-1) means infinitely large."))

  (:documentation "
Author: Jin-Cheng Guu (2023)

Limited cache is a data structure that is like a hash-table. The
user can specify its maximal size and the life span of each
entry. If the limited cache reaches its maximal size, then adding
a new entry clears the oldest entry in the cache. The maximal
size can be infinite. Each time an entry is initiated, accessed,
or updated, its latest-access-time gets updated.

We maintain a global thread for clearing caches that expire. The
global thread should be listed in all bordeaux-threads, and it
has name +limited-cache/manager-name+. Every snooze-time, the
manager wakes up and goes through the list of limited caches and
clear the expired entries (see #'%auto-prune). To get the list of
expired-keys, use #'expired-keys.

The user must initiate a limited-cache with #'make-limited-cache.
For example, it adds the newly created cache into the list
*managed-limited-caches* so the manager goes and kills its
expired entries.

Internally, a limited cache has a hash-table whose keys are the
intended keys, but whose values are the intended values and other
information. Each value (called cokey in the program) is of type
limited-cache-table-value, which has three slots: value, 3-cell,
and last-access-time. The value is the intended value. The
last-access-time is the last access time of the key; it gets
updated whenever the entry is initiated, accessed, or updated.
The 3-cell is a basic component of the queue of the limited
cache, it helps to maintain the order among the keys, from the
oldest to the newest."))

(defmethod expired-keys ((cache limited-cache))
  (let ((life-span (life-span cache))
        (table (table cache)))
    ;; TODO Use information in the queue to optimize this function.
    ;; Recall that the queue maintains the order of how new a key is.
    (iter (for (key value) in-hashtable table)
      (when (< life-span (local-time:timestamp-difference
                          (local-time:now)
                          (%last-access-time value)))
        (collect key)))))

(defun %auto-prune ()
  (when (< *manager-snooze-time* +limited-cache/default-minimal-snooze-time+)
    (log:debug "Setting snooze time to the default minimal.")
    (setf *manager-snooze-time* +limited-cache/default-minimal-snooze-time+))
  (iter
    (sleep *manager-snooze-time*)
    (log:debug "Manager: Waking up to kill.. at ~a~%" (local-time:now))
    (bt:with-recursive-lock-held (+limited-cache/lock+)
      (iter
        (for cache in *managed-limited-caches*)
        (iter
          (for key in (expired-keys cache))
          (log:debug "Killing key ~s of cache ~sa at ~a~%" key cache (local-time:now))
          (remove-cache cache key))))))

(defun %%reset-all-limited-caches ()
  "For development use. It should reset the limit-cache facility to its initial state."
  (%ensure-all-limited-cache-manager-are-dead)
  (%ensure-manager-for-limited-caches)
  (mapcar #'destroy-cache *managed-limited-caches*))

;; (%%reset-all-limited-caches)

(defun manager-for-limited-caches-alive-p ()
  (iter
    (for thread in (bt:all-threads))
    (thereis (string= (bt:thread-name thread) +limited-cache/manager-name+))))

;; Only one global manager to kill for all limited caches.
(defun %ensure-manager-for-limited-caches ()
  (unless (manager-for-limited-caches-alive-p)
    (log:debug "Making a manager thread at ~a..~%" (local-time:now))
    (bt:make-thread #'%auto-prune
                    :name +limited-cache/manager-name+)
    (iter (until (manager-for-limited-caches-alive-p)))
    t))

(defun %ensure-all-limited-cache-manager-are-dead ()
  "For ease of development only. The user should not kill the
limied cache manager manually."
  (mapcar #'bt:destroy-thread
          (remove-if-not
           (lambda (thread)
             (string= (bt:thread-name thread) +limited-cache/manager-name+))
           (bt:all-threads)))
  (iter (until (not (manager-for-limited-caches-alive-p)))))

(defun make-limited-cache (&key (test #'eql) (life-span +limited-cache/default-timeout+) (max-size -1))
  ;; NOTE We use lisp threads to kill expired keys. If the life-span is too short,
  ;; there may be bugs. Therefore we setup this form to prevent life-span that is too short.
  (when (< life-span +limited-cache/default-minimal-timeout+)
    (log:info "Setting life-span to the minimal possible value ~a." +limited-cache/default-minimal-timeout+)
    (setf life-span +limited-cache/default-minimal-timeout+))
  (let ((cache (make-instance 'limited-cache
                              :life-span life-span
                              :max-size max-size
                              :table (make-thread-safe-hash-table :test test))))
    ;; Initiate the doubly-linked-list.
    (cond ((= max-size -1)
           (setf (queue cache) (make-instance 'linked-list)))
          ((alexandria:non-negative-integer-p max-size)
           (setf (queue cache) (make-instance 'finite-linked-list :max-size max-size)))
          (t (error "Unsupported.")))
    (push cache *managed-limited-caches*) ; the central manager can thus prune this cache
    cache))

(defmethod set-cache ((cache limited-cache) key value)
  "Set the value for the field of the key in the cache."
  (bt:with-recursive-lock-held (+limited-cache/lock+)
    (%ensure-manager-for-limited-caches)
    (let ((now (local-time:now)))
      (multiple-value-bind (cokey boundp)
          (gethash key (table cache))
        (if boundp
            (progn
              ;; Update queue (linked list): pop and append.
              (.pop (%3-cell cokey) (queue cache))
              (multiple-value-bind (value- 3-cell)
                  (.append key (queue cache))
                (declare (ignore value-))
                ;; Use the new 3-cell to replace the one in the cokey.
                (setf (%3-cell cokey) 3-cell))
              ;; Update last access time.
              (setf (%last-access-time cokey) now)
              ;; Set new value.
              (setf (%value cokey) value))
            (progn
              (multiple-value-bind (value- 3-cell pop? popped-3-cell)
                  ;; Update queue (linked list).
                  (.append key (queue cache))
                (declare (ignore value-))
                ;; Set cokey (value, 3-cell, last-access-time).
                (setf (gethash key (table cache))
                      (make-instance 'limited-cache-table-value
                                     :value value
                                     :3-cell 3-cell
                                     :last-access-time now))
                (when pop?
                  (let* ((popped-key (storage popped-3-cell))
                         (equality-test (hash-table-test (table cache)))
                         (to-prune? (not (funcall equality-test key popped-key))))
                    (when to-prune?
                      (log:debug "Key ~a is removed from cache due to overflow." popped-key)
                      (remhash popped-key (table cache))))))
              value))))))

(defmethod get-cache (key (cache limited-cache))
  "Access to the value of the key in the cache."
  (bt:with-recursive-lock-held (+limited-cache/lock+)
   (%ensure-manager-for-limited-caches)
   (let ((now (local-time:now)))
     (multiple-value-bind (cokey boundp)
         (gethash key (table cache))
       (if boundp
           ;; FIXME The THEN branch is wrong. Fix it for .prepend too!
           ;; Add a test case to prevent this in the future.
           (progn
             ;; Update queue (linked list): pop and append.
             (.pop (%3-cell cokey) (queue cache))
             (multiple-value-bind (value- 3-cell)
                 (.append key (queue cache))
               (declare (ignore value-))
               ;; Use the new 3-cell to replace the one in the cokey.
               (setf (%3-cell cokey) 3-cell))
             ;; Update last access time.
             (setf (%last-access-time cokey) now)
             ;; Return
             (values (%value cokey) boundp))
           (values nil boundp))))))

(defsetf get-cache (key place) (value)
  `(set-cache ,place ,key ,value))

(defmethod remove-cache ((cache limited-cache) key)
  (bt:with-recursive-lock-held (+limited-cache/lock+)
   (%ensure-manager-for-limited-caches)
   (let* ((table (table cache))
          (cokey (gethash key table)))
     (when cokey
       (.pop (%3-cell cokey) (queue cache)))
     (remhash key table))))

(defmethod clear-cache ((cache limited-cache))
  (bt:with-recursive-lock-held (+limited-cache/lock+)
    (%ensure-manager-for-limited-caches)
    (clear (queue cache))
    (clrhash (table cache))))

(defmethod destroy-cache ((cache limited-cache))
  "To destroy a limited cache means to clear its content and remove
it from the management list. If not refered elsewhere, the
destroyed cache should be garbage collectable."
  (clear-cache cache)
  (setf *managed-limited-caches* (remove cache *managed-limited-caches*)))

(defmethod count-keys ((cache limited-cache))
  (hash-table-count (table cache)))

(defmacro with-reset-limited-cache-context (snooze-time &rest body)
  "WARNING - This kills the global limited cache manager!

Create a new limited cache manager with the designated
SNOOZE-TIME, and kill all other managers. At the end, kill the
manager just created, and create a manager with the old snooze
time. Note that there is only one global manager, so we have to
do it dynamically and globally.

It also creates a temporarily seperated list *manager-snooze-time*.

If SNOOZE-TIME is NIL, we use the original dynamic snooze-time."
  `(let* ((old-snooze-time *manager-snooze-time*)
          (tmp-list *managed-limited-caches*))
     (unwind-protect
          (progn
            (setf *managed-limited-caches* nil)
            (setf *manager-snooze-time* (if ,snooze-time
                                            (min ,snooze-time +limited-cache/default-minimal-snooze-time+)
                                            *manager-snooze-time*))
            (%ensure-all-limited-cache-manager-are-dead)
            (%ensure-manager-for-limited-caches)
            ,@body)
       (setf *managed-limited-caches* tmp-list)
       (setf *manager-snooze-time* old-snooze-time)
       (%ensure-all-limited-cache-manager-are-dead)
       (%ensure-manager-for-limited-caches))))

;;; Memoization using limited cache.
(defun limited-memoizing (fn &key (test #'equal) (life-span +limited-cache/default-timeout+) (max-size 10))
  (let ((cache (make-limited-cache :test test :life-span life-span :max-size max-size)))
    (values
     (lambda (&rest input)
       (multiple-value-bind (values boundp)
           (get-cache input cache)
         (if boundp
             (apply #'values values)
             (let ((values (multiple-value-list (apply fn input))))
               (setf (get-cache input cache) values)
               (apply #'values values)))))
     cache)))

(defun limited-memoize! (symbol &key (test #'equal) (life-span +limited-cache/default-timeout+) (max-size 10))
  "Symbol is expected to be the name of the function to be memoized."
  (multiple-value-bind (new-fn cache)
      (limited-memoizing (fdefinition symbol) :test test :life-span life-span :max-size max-size)
    (let ((old-fn (fdefinition symbol)))
      (setf (fdefinition symbol) new-fn)
      (values old-fn cache))))

;; TODO Seperate tests to another file, and run the test everytime it is compiled.

(def-suite limited-cache-suite
  :description "Suite of tests for limited cache.")

(in-suite limited-cache-suite)

;; Test the last-access is updated after setting.
(def-test last-access-after-setting ()
  (with-reset-limited-cache-context 0.01
    (let* ((life-span 0.05)
           (cache (make-limited-cache :life-span life-span)))
      (setf (get-cache :a cache) t)
      (setf (get-cache :b cache) t)
      (sleep 0.03)
      (setf (get-cache :a cache) t)
      (sleep 0.03)
      ;; The cache of :a is updated 0.03 second ago, so its cache should be there still.
      (is (eq (get-cache :a cache) t))
      ;; 0.03 + 0.03 > 0.05, so the cache for :b should have been gone.
      (is (eq (get-cache :b cache) nil)))))

;; Test the last-access is updated after accessing.
(def-test last-access-after-updating ()
  (with-reset-limited-cache-context 0.01
    (let* ((life-span 0.05)
           (cache (make-limited-cache :life-span life-span)))
      (setf (get-cache :a cache) t)
      (setf (get-cache :b cache) t)
      (sleep 0.03)
      (get-cache :a cache)
      (sleep 0.04)
      ;; The cache of :a is accessed 0.03 second ago, so its cache should be there still.
      (is (eq (get-cache :a cache) t))
      ;; 0.03 + 0.04 > 0.05, so the cache for :b should have been gone.
      (is (eq (get-cache :b cache) nil)))))

;; Test setting with setf.
(def-test setting-with-setf ()
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (setf (get-cache :a cache) t)
      (is (eq (get-cache :a cache) t)))))

;; Test set-cache.
(def-test setting-with-set-cache ()
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (set-cache cache :a t)
      (is (eq (get-cache :a cache) t)))))

;; Test get-cache: bound.
(def-test get-cache/bound ()
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (set-cache cache :a 1)
      (is (= 2 (length (multiple-value-list (get-cache :a cache)))))
      (multiple-value-bind (value boundp)
          (get-cache :a cache)
        (is (eq value 1))
        (is (eq boundp t))))))

;; Test get-cache: unbound.
(def-test get-cache/unbound ()
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (is (= 2 (length (multiple-value-list (get-cache :a cache)))))
      (multiple-value-bind (value boundp)
          (get-cache :a cache)
        (is (eq value nil))
        (is (eq boundp nil))))))

;; Test count-keys
(def-test count-keys ()
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (is (= 0 (count-keys cache)))
      (set-cache cache :a 1)
      (is (= 1 (count-keys cache)))
      (set-cache cache :b 1)
      (is (= 2 (count-keys cache)))
      (set-cache cache :b 2)
      (is (= 2 (count-keys cache)))
      (set-cache cache :c nil)
      (is (= 3 (count-keys cache))))))

;; Test clear-cache
(def-test clear-cache ()
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (set-cache cache :a 1)
      (set-cache cache :b 2)
      (is (= 2 (count-keys cache)))
      (clear-cache cache)
      ;; TODO test queue too
      (is (= 0 (count-keys cache))))))

;; Test destroy-cache
(def-test destroy-cache ()
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (destroy-cache cache)
      (is (not (find cache *managed-limited-caches*))))))

;; Test remove-cache.
(def-test remove-cache ()
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (set-cache cache :a 1)
      (set-cache cache :b 2)
      (set-cache cache :c nil)
      (is (= 3 (count-keys cache)))
      (remove-cache cache :a)
      (is (= 2 (count-keys cache)))
      (remove-cache cache :b)
      (is (= 1 (count-keys cache)))
      (remove-cache cache :c)
      (is (= 0 (count-keys cache))))))

(defun check-test-equality (test key-0 key-1 key-2)
  "This function test the limited cache whose testing function is
TEST. TEST is one of #'eq, #'eql, #'equal, and #'equals. Under
this notion of equality, key-0 must be equal to key-1, and key-0
must be unequal to key-2."
  (with-reset-limited-cache-context nil
    (let* ((cache (make-limited-cache :test test)))
      (assert (funcall test key-0 key-1))
      (assert (not (funcall test key-0 key-2)))
      (set-cache cache key-0 0)
      (set-cache cache key-1 1)
      (set-cache cache key-2 2)
      (assert (= 1 (get-cache key-0 cache)))
      (assert (= 1 (get-cache key-1 cache)))
      (assert (= 2 (get-cache key-2 cache)))
      t)))

(def-test test-equality ()
  (is (check-test-equality #'eq 0 0 1))
  (is (check-test-equality #'eql 0 0 0.0))
  (is (check-test-equality #'equal (list 0) (list 0) (list 1)))
  (is (check-test-equality #'equalp "a" "A" "b")))

;; NOTE Comment out because we no longer allow changing life-span
;; on the fly. This is because life-span that is too short will
;; cause trouble. And we currently only enforce life-span with
;; #'make-limited-cache.
;;
;; ;; Test various configs: life-span
;; (def-test config/life-span ()
;;   (with-reset-limited-cache-context 0.01
;;     (let ((cache (make-limited-cache :life-span 0.1)))
;;       (setf (get-cache :a cache) t)
;;       (sleep 0.05)
;;       (is (equal (multiple-value-list (get-cache :a cache)) `(t t)))
;;       (sleep 0.15)
;;       (is (equal (multiple-value-list (get-cache :a cache)) `(nil nil)))
;;       ;; Lengthen life-span to 0.5 sec.
;;       (setf (life-span cache) 0.5)
;;       (setf (get-cache :a cache) t)
;;       (sleep 0.05)
;;       (is (equal (multiple-value-list (get-cache :a cache)) `(t t)))
;;       (sleep 0.15)
;;       (is (equal (multiple-value-list (get-cache :a cache)) `(t t))))))

;; Test auto pruning expired entries.
(def-test auto-prune-expired-entires ()
  (with-reset-limited-cache-context 0.01
    (let* ((life-span 0.01)
           (cache (make-limited-cache :life-span life-span)))
      (setf (get-cache :a cache) t)
      (sleep 0.02)
      (is (equal (multiple-value-list (get-cache :a cache)) `(nil nil))))))

;; Test auto pruning excessive entries.
(def-test auto-prune-excessive-entries ()
  (with-reset-limited-cache-context 100
    ;; We don't want the manager to clear the cache in this test,
    ;; so we set the snooze time to be very long. At the end we
    ;; clear cache ourselves.
    (let* ((max-size 5)
           (cache (make-limited-cache :max-size max-size))
           (iteration 15))
      (iter (for i from 1 to iteration)
        (set-cache cache i i))
      (iter
        (for i from 1 to (- iteration max-size))
        (is (not (find i (alexandria:hash-table-keys (table cache))))))
      (iter
        (for i from (1+ (- iteration max-size)) to iteration)
        (is (find i (alexandria:hash-table-keys (table cache)))))
      (clear-cache cache))))

;; Test internal queue.
(def-test internal-queue ()
  (with-reset-limited-cache-context nil
    (let ((cache))
      (setf cache (make-limited-cache))
      (set-cache cache 0 t)
      (set-cache cache 1 t)
      (set-cache cache 2 t)
      (set-cache cache 3 t)
      (is (equal `(0 1 2 3) (list<- (queue cache))))
      ;; Setting updates the order.
      (set-cache cache 2 t)
      (is (equal `(0 1 3 2) (list<- (queue cache))))
      ;;
      (setf cache (make-limited-cache))
      (set-cache cache 0 t)
      (set-cache cache 1 t)
      (set-cache cache 2 t)
      (set-cache cache 3 t)
      (is (equal `(0 1 2 3) (list<- (queue cache))))
      ;; Getting updates the order.
      (get-cache 2 cache)
      (is (equal `(0 1 3 2) (list<- (queue cache)))))))

;; Test infinite max-size
(def-test infinite-max-size ()
  (with-reset-limited-cache-context 100
    ;; We don't want the manager to clear the cache in this test,
    ;; so we set the snooze time to be very long. At the end we
    ;; clear cache ourselves.
    (let ((cache (make-limited-cache))
          (large-number 100000))
      ;; #'expired-keys seems to throw restarts when large-number
      ;; #is too large, and when we repeat testing
      ;; #'infinite-max-size too many times: e.g. by (iter
      ;; #(repeat 100) (fiveam:run! 'infinite-max-size))
      ;; This seems ok though; it doesn't break the pipeline.
      (iter (for i from 1 to large-number)
        (set-cache cache i i))
      (is (= large-number (count-keys cache)))
      (clear-cache cache))))

;; Test finite max-size.
(def-test finite-max-size ()
  (with-reset-limited-cache-context 100
    ;; We don't want the manager to clear the cache in this test,
    ;; so we set the snooze time to be very long. At the end we
    ;; clear cache ourselves.
    (let* ((max-size 5)
           (cache (make-limited-cache :max-size max-size))
           (iteration 15))
      (iter (for i from 1 to iteration)
        (set-cache cache i i))
      (is (= max-size (count-keys cache)))
      (clear-cache cache))))

;; Stress test: very short life-span.
(def-test very-short-life-span ()
  (let* ((life-span +limited-cache/default-minimal-timeout+)
         (snooze-time life-span))
    (with-reset-limited-cache-context snooze-time
      (let ((cache (make-limited-cache :life-span life-span)))
        (set-cache cache :a t)
        (sleep (+ snooze-time life-span))
        (is (eq nil (get-cache :a cache)))))))

;; Stress test: getting the slots frequently.
(def-test frequent-accessing-to-slots ()
  "We test if the the last access times are updated."
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (set-cache cache :a t)
      (is (eq nil
              (iter
                (with access = nil)
                (repeat 1000)
                (get-cache :a cache)
                (let ((last-access (%last-access-time (gethash :a (table cache)))))
                  (setf access last-access)
                  (thereis (not (equal access last-access))))))))))

;; Stress test: setting the slots frequently.
(def-test frequent-setting-slots ()
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (set-cache cache :a t)
      (is (eq nil
              (iter
                (with access = nil)
                (for i from 1 to 1000)
                (set-cache cache :a i)
                (let ((last-access (%last-access-time (gethash :a (table cache)))))
                  (setf access last-access)
                  (thereis (or (not (equal access last-access))
                               (not (= i (get-cache :a cache))))))))))))

;; Stress test: clearing the slots frequently.
(def-test frequent-clearing-slots ()
  (with-reset-limited-cache-context nil
    (let ((cache (make-limited-cache)))
      (let ((flag nil))
        (iter (repeat 1000)
          (set-cache cache :a t)
          (unless (= 1 (count-keys cache))
            (setf flag t))
          (clear-cache cache)
          (unless (= 0 (count-keys cache))
            (setf flag t)))
        (is (eq flag nil))))))

;; Test racing condition.
(def-test racing-condition ()
  (let* ((errorp nil)
         (iteration 1000)
         (cache (make-limited-cache))
         (fn (lambda ()
               (handler-case
                   (iter
                     (repeat iteration)
                     (get-cache :a cache)
                     (set-cache cache :a t)
                     (remove-cache cache :a))
                 (error (c)
                   (declare (ignore c))
                   (setf errorp t)))))
         (t0 (bt:make-thread fn))
         (t1 (bt:make-thread fn)))
    (bt:join-thread t0)
    (bt:join-thread t1)
    ;; Make sure there is no error.
    (is (eq nil errorp))))

;; Test: get-cache, set-cache do not change length.
(def-test get-set-do-not-change-length ()
  (let ((cache (make-limited-cache :test #'equal :max-size 5)))
    (get-cache :a cache)
    (is (= 0 (%length (queue cache))))
    (setf (get-cache :a cache) t)
    (is (= 1 (%length (queue cache))))
    (iter (repeat 10) (get-cache :a cache))
    (is (= 1 (%length (queue cache))))
    (iter (repeat 10) (setf (get-cache :a cache) t))
    (is (= 1 (%length (queue cache))))))

;;; Tests for limited memoization

(def-test limited-memoize/basic ()
  (let* ((long-time 0.1)
         (short-time (/ long-time 10))
         ;; takes-tenth is a symbol. I do this because I don't want
         ;; any symbol to pollute the global space.
         (takes-tenth
           (defun #:takes-tenth (x y &key (z "lol"))
             ;; #:takes-tenth is uninterned, so we need to save it
             ;; #to a local variable to call later.
             "A test function that takes at least 0.1 second to complete."
             (sleep long-time)
             (values x (list y z)))))
    ;; Without memoization, the function call takes a long time to
    ;; return. Thus the form should return timeout error.
    (is
     (eq :gotcha
         (handler-case
             (bt:with-timeout (short-time)
               (funcall takes-tenth 1 2 :z 3))
           (bt:timeout (c)
             (declare (ignore c)) :gotcha))))
    takes-tenth
    (limited-memoize! takes-tenth)
    (limited-memoize! takes-tenth) ; it's ok to memoize multiple times
    (funcall takes-tenth 1 2 :z 3)
    ;; After memoization, the function takes much shorter to return.
    (is (equal
         (multiple-value-list
          (bt:with-timeout (short-time)
            (funcall takes-tenth 1 2 :z 3)))
         (multiple-value-list
          (funcall takes-tenth 1 2 :z 3))))))

;; Test: If a memoized function raises error, then do not memoize that input.
(def-test limited-memoize/error ()
  (let* ((takes-tenth
           (defun #:takes-tenth (x)
             ;; #:takes-tenth is uninterned, so we need to save it
             ;; #to a local variable to call later.
             "A test function that takes at least 0.1 second to complete."
             (sleep (/ 1 10))
             (error "input: ~a" x)))
         (input 12345))
    (multiple-value-bind (old-fn cache)
        (limited-memoize! takes-tenth)
      (declare (ignore old-fn))
      (handler-case (funcall takes-tenth input)
        (error (c) (format nil "Expected error: ~s" c)))
      (multiple-value-bind (value boundp)
          (get-cache input cache)
        (declare (ignore value))
        (is (eq nil boundp))))))

;; Test for max-size
(def-test limited-memoize/max-size ()
 (let* ((takes-tenth
          (defun #:takes-tenth (x)
            ;; #:takes-tenth is uninterned, so we need to save it
            ;; #to a local variable to call later.
            "A test function that takes at least 0.1 second to complete."
            (sleep (/ 1 10))
            x)))
   (limited-memoize! takes-tenth :max-size 5)
   (iter (for i from 1 to 10)
     (funcall takes-tenth i))
   ;; 1~5 should be kicked out from the cache due to overflow.
   (iter (for i in `(1 2 3 4 5))
     (is
      (eq :gotcha
          (handler-case
              (bt:with-timeout ((/ 1 100))
                (funcall takes-tenth i))
            (bt:timeout (c)
              (declare (ignore c)) :gotcha)))))
   ;; while 6~10 should not.
   (iter (for i in `(6 7 8 9 10))
     (is
      (equal
       (multiple-value-list
        (bt:with-timeout ((/ 1 100))
          (funcall takes-tenth i)))
       (multiple-value-list
        (funcall takes-tenth i)))))))

;; Test for life-span.
(def-test limited-memoize/life-span ()
  (let* ((life-span 0.2)
         (takes-tenth
           (defun #:takes-tenth (x)
             ;; #:takes-tenth is uninterned, so we need to save it
             ;; #to a local variable to call later.
             "A test function that takes at least 0.1 second to complete."
             (sleep (/ 1 10))
             x)))
    ;; Reset the limited cache context. This for example updates
    ;; the snooze time of the global limited cache manager.
    (with-reset-limited-cache-context 0.1
      (limited-memoize! takes-tenth :life-span life-span)
      (funcall takes-tenth 1)
      (is
       (equal
        (bt:with-timeout ((/ 1 100))
          (funcall takes-tenth 1))
        (funcall takes-tenth 1)))
      (sleep (+ life-span 0.01))
      (is
       (eq :gotcha
           (handler-case
               (bt:with-timeout ((/ 1 100))
                 (funcall takes-tenth 1))
             (bt:timeout (c)
               (declare (ignore c)) :gotcha)))))))
