(in-package :day07)

(defstruct (ipaddr (:conc-name "IA-"))
  supernet
  hypernet)

(defun read-input (inpath)
  (with-open-file (s inpath)
    (loop
       for line = (read-line s nil nil)
       while line
       collect line)))

(defun disentangle (data-str)
  (let (supernet hypernet)
    (loop
       with inside-hn = nil
       with d = nil with h = nil
       for c across data-str
       if (not inside-hn) do
         (cond ((char= c #\[)
                (push (coerce d 'string) supernet)
                (setf inside-hn t
                      d nil))
               (t (push c d)))
       else do
         (cond ((char= c #\])
                (push (coerce h 'string) hypernet)
                (setf inside-hn nil
                      h nil))
               (t (push c h)))
       finally
         (when d (push (coerce d 'string) supernet))
         (when h (push (coerce h 'string) hypernet))) ;; this is just a precaution
    (make-ipaddr :supernet supernet :hypernet hypernet)))

(defun mk-ipaddrs-from-strings (ip-strings)
  (loop for ip-str in ip-strings collect (disentangle ip-str)))

;; regex would be much easier: (\\w)(?!\\1)(\\w)\\2\\1
;; figured that out only after the function was written
(defun contains-abba (string)
  (loop
     with stack = nil
     for c across string
     with s = 0
     with prev ;; this is essential; there was a bug with strings of the form XXYYZZYY
     for i from 0
     do  ;; could probably implemented as a macro
       ;; (format t "[~d|~c|~a]~%" s c stack)
       (cond ((= s 0) ;; start state
              (push c stack)
              (setf s 1))
             ((= s 1) ;; opposite of state 2
              (cond ((char= c (car stack))
                     (push c stack))
                    (t
                     (push c stack)
                     (setf s 2))))
             ((= s 2) ;; top is different from second-to-top
              (cond ((char= c (car stack))
                     (setf prev (pop stack)
                           s 3))
                    (t
                     (push c stack)
                     (setf s 2))))
             ((= s 3) ;; inner match happened
              (cond ((char= c (car stack))
                     (return t))
                    (i
                     (push prev stack)
                     (push c stack)
                     (setf s 2
                           prev nil))))
             (t (error "CANT GET HERE")))))

(defun contains-abba-regex (string)
  (scan-to-strings "(\\w)(?!\\1)(\\w)\\2\\1" string))

(defun ipaddr-supp-tls-p (ipaddr &key (pred #'contains-abba))
  (and (some pred (ia-supernet ipaddr))
       (notany pred (ia-hypernet ipaddr))))

(defun count-ipaddrs (ipaddrs &key pred)
  (loop
     for ipaddr in ipaddrs
     if (funcall pred ipaddr) count 1))

(defun sol1 ()
  (-<> (read-input "./inputs/day07")
       (mk-ipaddrs-from-strings <>)
       (count-ipaddrs <> :pred #'ipaddr-supp-tls-p)))

(defun aba-p (str i j k)
  "Checks whether the characters at I, J, K in STR form an ABA"
  (let ((fc (char str i)))
         (and (char= fc (char str k))
              (char/= fc (char str j)))))

(defun all-abas (str)
  (let ((s-len (length str)))
    (loop
       for x upto (- s-len 3)
       if (aba-p str x (1+ x) (+ x 2))
       collect (subseq str x (+ x 3)))))

(defun bab-from-aba (aba)
  (coerce (list (char aba 1) (char aba 0) (char aba 1)) 'string))

(defun ipaddr-supp-ssl-p (ipaddr)
  (let ((abas (sort (loop for sn in (ia-supernet ipaddr)
                       nconc (all-abas sn))
                    #'string<=))
        (babs (sort (loop for sn in (ia-hypernet ipaddr)
                       nconc (loop for aba in (all-abas sn)
                                collect (bab-from-aba aba)))
                    #'string<=)))
    (intersection abas babs :test #'string=)))

(defun sol2 ()
  (-<> (read-input "./inputs/day07")
       (mk-ipaddrs-from-strings <>)
       (count-ipaddrs <> :pred #'ipaddr-supp-ssl-p)))
