(in-package :day19)

(defparameter +input+ 3012210)

(setf *print-circle* t)

(defun sol1 (&key (input +input+))
  (let ((presents (make-hash-table))
        (elves-not-out (loop for i below input collect i))
        (num-elves-not-out input)
        robber robbed)
    (setf (cdr (last elves-not-out)) elves-not-out) ;; make circular
    (loop for i below input do (setf (gethash i presents) 1)) ;; each elf brings one present
    (loop while (> num-elves-not-out 1) do ;; as long as there're more than one elf
         (setf robber (first elves-not-out)
               robbed (second elves-not-out))
         (anaphora:slet (gethash robbed presents)
           (incf (gethash robber presents) anaphora:it)
           (setf anaphora:it 0)
           (decf num-elves-not-out)
           (setf (cdr elves-not-out) (cddr elves-not-out)) ;; remove robbed elf
           (setf elves-not-out (cdr elves-not-out))))      ;; advance pointer
    (let ((remaining-elf (first elves-not-out)))
      (values (1+ remaining-elf) (gethash remaining-elf presents)))))

#|

(defstruct (node (:conc-name nd-))
  pos
  prev
  next)

(defstruct ptr
  num-elts
  hd)

(defun mk-circ-nodes (num-elts)
  (let ((ptr (make-ptr :num-elts num-elts)))
    (setf (ptr-hd ptr) (make-node :pos 0))
    (loop with curr = (ptr-hd ptr)
	  for i from 1 below num-elts do
	    (let ((new (make-node :pos i)))
	      (setf (nd-next curr) new
		    (nd-prev new) curr
		    curr new))
	  finally
	     (setf (nd-next curr) (ptr-hd ptr)
		   (nd-prev (ptr-hd ptr)) curr))
    ptr))

(defun advance-ptr (ptr)
  (setf (ptr-hd ptr) (nd-next (ptr-hd ptr)))
  ptr)

(defun get-nth-node (ptr n)
  (let ((remainder (mod n (ptr-num-elts ptr))))
    (loop with curr = (ptr-hd ptr)
	  repeat remainder do
	    (setf curr (nd-next curr))
	  finally
	     (return-from get-nth-node curr))))

(defun delete-node (node ptr)
  (let ((next (nd-next node))
	(prev (nd-prev node)))
    (setf (nd-prev next) prev
	  (nd-next prev) next
	  (nd-prev node) nil
	  (nd-next node) nil)
    (decf (ptr-num-elts ptr))
    prev))

(defun sol2 (&key (input +input+))
  (let ((elves (mk-circ-nodes input)))
    (loop while (> (ptr-num-elts elves) 1) do
      (let ((node-2-delete (get-nth-node elves (floor (ptr-num-elts elves) 2))))
	(delete-node node-2-delete elves)
	(advance-ptr elves)))
    (1+ (nd-pos (ptr-hd elves)))))

|#

;; Found by doing some examples and noticing a pattern
;; Not 100% sure it's correct; but it gives the correct answer...
(defun sol2 (&key (input +input+))
  (let* ((ary (make-array input :initial-contents (alexandria:iota input)))
	 (idx (floor input 2))
	 (counter input))
    (macrolet ((incf-to-next (idx &optional (step 1))
		 `(loop repeat ,step do
		   (setf ,idx (mod (1+ ,idx) input))
		   (loop with n = ,idx
			 while (null (aref ary n)) do
			   (setf n (mod (1+ n) input))
			 finally
			    (setf ,idx n))))
	       (cross-off (idx)
		 `(progn
		    (setf (aref ary ,idx) nil)
		    (decf counter)
		    (when (= 1 counter)
		      (return)))))
      (loop
	initially
	   (when (oddp input)
	     (cross-off idx)
	     (incf-to-next idx 2))
	while (> counter 1) do
	  (cross-off idx)
	  (incf-to-next idx)
	  (cross-off idx)
	  (incf-to-next idx 2)))
    (1+ (position-if (complement #'null) ary))))
