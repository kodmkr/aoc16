(in-package :day13)

(defparameter +fav-num+ 1358)

(defparameter +target+ #c(31 39))

(defparameter +start+ #c(1 1))

(defparameter +u+ #c(0 -1))
(defparameter +d+ #c(0 1))
(defparameter +l+ #c(1 0))
(defparameter +r+ #c(-1 0))

(defun poly (x y)
  (+ (* x x) (* 3 x) (* 2 x y) y (* y y)))

(defun closed-p (cpx &optional (fav-num +fav-num+))
  (oddp (logcount (+ (poly (realpart cpx)  (imagpart cpx))
                      fav-num))))

(defun nbrs (point)
  (cond ((zerop point)
         (list #c(0 1) #c(1 0)))
        ((and (zerop (imagpart point))  ;; on x axis
              (plusp (realpart point))) ;; not origin
         (list (+ point +l+) (+ point +d+) (+ point +r+)))
        ((and (zerop (realpart point))  ;; on y axis
              (plusp (imagpart point))) ;; not origin
         (list (+ point +u+) (+ point +r+) (+ point +d+)))
        (t (list (+ point +u+) (+ point +r+) (+ point +d+) (+ point +l+)))))

(defun distance (src tgt)
  (+ (abs (- (realpart tgt) (realpart src)))
     (abs (- (imagpart tgt) (imagpart src)))))

(defun nearness (p q)
  (<= (distance p +target+)
     (distance q +target+)))

(defun nexts (pt seens)
  (-<> (nbrs pt)
       (remove-if (lambda (x) (closed-p x)) <>)
       (remove-if (lambda (x) (or (minusp (imagpart x)) (minusp (realpart x)))) <>)
       (remove-if (lambda (x) (map-set:ms-member-p seens x)) <>)))

;; adapted from Cormen et al.
(defun search-paths (&optional (start +start+) (target +target+))
  (let* ((pred (make-hash-table)) ;; predecessors of nodes
         (dist (make-hash-table))
         (seens (map-set:make-map-set))
         q)
    (setf (gethash start pred) nil
          (gethash start dist) 0)
    (push start q)
    (loop while q do
         (let* ((min-node (pop q))
                (nexts (nexts min-node seens)))
           (map-set:ms-insert seens min-node)
           (cond ((member target nexts) ;; no early exit hear because of part 2 of the problem
                  (setf (gethash target pred) min-node
                        (gethash target dist) (1+ (gethash min-node dist))))
                 (t (flet ((n> (x y) ;; CAUTION: this expects y to always be a number
                             (unless x (return-from n> t))
                             (> x y)))
                      (loop for n in nexts do
                           (anaphora:slet (gethash n dist)
                             (when (n> anaphora:it (1+ (gethash min-node dist)))
                               (setf (gethash n dist) (1+ (gethash min-node dist))
                                     (gethash n pred) min-node)))))
                    (setf q (sort (append q nexts) #'nearness))))))
    (values pred dist)))

(defun build-path (preds &optional (start +start+) (target +target+))
  (let* (path
         (curr target)
         (next (gethash curr preds)))
    (loop while (/= next start) do
         (push next path)
         (setf curr next
               next (gethash curr preds))
       finally (push start path))
    path))

(defun sol1 ()
  (-<> (search-paths)
       (build-path <>)
       (length <>)))

(defun sol2 ()
  (multiple-value-bind (pred dist)
      (search-paths)
    (declare (ignore pred))
    (let ((distinct-locations (map-set:make-map-set)))
      (loop for d being the hash-value of dist using (hash-key loc)
         if (<= d 50) do
           (map-set:ms-insert distinct-locations loc))
      (map-set:ms-count distinct-locations))))
