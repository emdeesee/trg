(ql:quickload :ltk)

(defparameter *length* 500)
(defparameter *canvas-dim* 800)

(defun sqr (x) (* x x))

(defun equilateral-triangle (length)
  (let ((half-length (/ length 2)))
    (list (list 0 0)
          (list length 0)
          (list half-length
                (sqrt (- (sqr length) (sqr half-length)))))))

(defvar *vertices* (equilateral-triangle *length*))
(defvar *initial-point* (list (random *length*)
                              (random (cadar (last *vertices*)))))
(defun vec* (v s)
  (mapcar (lambda (e) (* e s)) v))

(defun vec+ (v0 v1)
  (mapcar #'+ v0 v1))

(defun vec- (v0 v1)
  (vec+ v0 (vec* v1 -1)))

;; Translate the triangle to a reasonable place when rendered.
(let* ((d (/ *canvas-dim* 2))
       (p (vec- (list d d)
                (list (/ *length* 2) (/ (cadar (last *vertices*)) 2)))))
  (setq *vertices*
        (mapcar (lambda (v) (vec+ v p)) *vertices*))
  (setq *initial-point* (vec+ *initial-point* p)))

(defun rand-nth (l) (nth (random (length l)) l))
(defun random-vertex ()
  (rand-nth *vertices*))

(defun midpoint (p0 p1)
  (vec+ p0
        (vec* (vec+ p1 (vec* p0 -1)) 1/2)))

(let ((current-point *initial-point*))
  (defun get-next-point ()
    (setq current-point (midpoint current-point (random-vertex)))))

(let ((points)
      (count 0)
      (max 4096))
  (defun retain-index (canvas i)
    (push i points)
    (if (< count max) (incf count)
        (progn
          (ltk:itemdelete canvas (car (last points)))
          (nbutlast points)))))

(defun choose-color ()
  (rand-nth '("cyan4" "cyan3" "cyan2")))

(defun render-point (canvas p)
  (destructuring-bind (x y) p
    (let ((index (ltk:create-rectangle canvas x y x y)))
      (ltk:itemconfigure canvas index :outline (choose-color))
      index)))

(defun update-points (canvas point)
  (retain-index canvas (render-point canvas point)))

(defun main ()
  (ltk:with-ltk ()
    (let* ((sc (make-instance 'ltk:scrolled-canvas))
           (c (ltk:canvas sc)))
      (ltk:configure c :background "black")
      (ltk:pack sc :expand 1 :fill :both)
      (ltk:scrollregion c 0 0 *canvas-dim* *canvas-dim*)
      (ltk:format-wish "wm geometry . \"~ax~a\"" *canvas-dim* *canvas-dim*)
      ;; mark triangle vertices
      (dolist (v *vertices*) (render-point c v))
      (loop do (update-points c (get-next-point))))))
