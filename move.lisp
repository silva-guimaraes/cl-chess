
(in-package :chess)

(defclass move ()
  ((moving-piece :accessor moving-piece 
                 :initarg :moving-piece)
   (moving-piece-pos :accessor moving-piece-pos 
                     :initarg :moving-piece-pos)
   (landing-pos :accessor landing-pos 
                :initarg :landing-pos)
   (can-capture :accessor can-capture 
                :initarg :can-capture
                :initform nil)))

(defclass pawn-move (move)
  ((new-rank :accessor new-rank :initarg :new-rank)
   (passant :accessor passant :initarg :passant)))

(defclass castling (move)
  ((rook :accessor rook :initarg :rook)
   (rook-landing-pos :accessor rook-landing-pos :initarg :rook-landing-pos)))

(defun make-move (moving-piece landing-pos &optional (can-capture nil)) 
  (make-instance 'move :moving-piece moving-piece
                                   :moving-piece-pos (get-pos moving-piece)
                                   :landing-pos landing-pos
                                   :can-capture can-capture))

(defun make-pawn-move (&key (moving-piece nil) (landing-pos nil) (new-rank nil) (can-capture nil) (passant nil))
  (let ((pawn-move (make-instance 'pawn-move :moving-piece moving-piece
                                             :landing-pos landing-pos
                                             :passant passant
                                             :new-rank new-rank
                                             :can-capture can-capture)))
    (when moving-piece (setf (moving-piece-pos pawn-move) (get-pos moving-piece)))
    ;;progn
    pawn-move))

(defun make-castling (moving-piece landing-pos rook rook-landing-pos)
  (make-instance 'castling :moving-piece moving-piece
                           :moving-piece-pos (get-pos moving-piece)
                           :landing-pos landing-pos
                           :rook rook
                           :rook-landing-pos rook-landing-pos))
