
(in-package :chess)

(defun empty= (x)
  (equalp 'empty x))

(defun not-empty= (x)
  (not (empty= x)))

(defun color= (a b)
  (when (and a b
             (typep a 'piece) (typep b 'piece))
      (equalp (color a) (color b))))

(defun color-field= (color field)
  (unless (empty= field) ; retornar nil caso o campo esteja vazio
    (equalp color (color field))))

(defun type= (a b)
  (equalp (type-of a) (type-of b)))

(defun pos (a b) ; 1-indexed 
  (cons a b)) 

(defun row (pos) (car pos))

(defun column (pos) (cdr pos))

(defun rel-pos (pos &key (row 0) (column 0))
  (pos (+ (row pos) row) 
       (+ (column pos) column))) 

(defun valid-pos-p (pos)
  (and (and (>= (row pos) 1) 	(<= (row pos) 8) )
       (and (>= (column pos) 1) (<= (column pos) 8))))

(defun pos-offset (pos off)
  (rel-pos pos :row (row off) :column (column off))) 

(defun pos= (a b) (equalp a b))

(defun access-board-form (state pos) 
  `(aref (get-board ,state) ,(1- (row pos)) ,(1- (column pos))))

(defun access-board (state pos)
  (when (valid-pos-p pos) ; retorna nil caso um campo esteja fora do tabuleiro
    (eval (access-board-form state pos)))) 

(defun rel-access-board (state pos &key (row 0) (column 0)) ;; ??
  (access-board state (rel-pos pos :row row :column column))) 

(defun set-access (state pos value)
  (eval `(setf ,(access-board-form state pos) ',value))) 

(defun pos-empty= (state pos)
  (equalp 'empty (access-board state pos)))

(defun not-pos-empty= (state pos)
  (not (pos-empty= state pos)))

(defun color-string (color)
  (if (equalp 'white color) "W" "B"))

(defun toggle (a b c)
  (if (equalp a b) c b))
