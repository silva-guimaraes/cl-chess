
;; todo:
;; upgrade do peão
;; check e checkmate

(in-package :chess)

(defclass state ()
  ((current-turn :accessor get-turn
         :initform 'white) 
   (board :accessor get-board 
          :initform (make-array '(8 8) :initial-element 'empty))
   (last-double-push :accessor last-double-push :initform nil )
   (last-move :accessor last-move :initform nil)
   (piece-refs :accessor piece-refs :initform nil)))


(defmethod clean-board ((state state))
  ;; (board-map state (lambda (x) (setf x 'empty)))
  (setf (get-board state) (make-array '(8 8) :initial-element 'empty)))

(defclass piece ()
  ((color  :initarg :color :accessor color)
   (pos :initarg :pos :accessor get-pos)
   (firstmove :accessor firstmove 
              :initform t)
   (string :initarg :string
           :initform "NIL"
           :accessor to-string))) 

(defclass pawn (piece)
  ((rank :accessor rank :initform 2))) 
(defclass rook (piece) nil)
(defclass bishop (piece) nil)
(defclass queen (rook bishop) nil)
(defclass knight (piece) nil)
(defclass king (piece)
  ((check :accessor check :initarg nil :initform nil)))

(defun setup-side (state color a b)
  (loop for x from 1 to 8
        for y :in '(rook knight bishop queen king bishop knight rook)
        for label :in '("RR" "RN" "RB" "Q" "K" "LB" "LN" "LR")
        for pawn = (set-access state (pos a x)
                               (make-instance 'pawn :color color
                                                    :string (concatenate 'string (color-string color) "P")
                                                    :pos (pos a x))) 
        for piece = (set-access state (pos b x)
                                (make-instance y :color color
                                                 :string (concatenate 'string (color-string color) label)
                                                 :pos (pos b x)))
        while x
        do (progn (push pawn (piece-refs state))
                  (push piece (piece-refs state)))
        ))

(defmethod setup-board ((state state))
  (setf (piece-refs state) nil
        (last-move state) nil)
  (clean-board state)
   (setup-side state 'black 2 1)
  (setup-side state 'white 7 8))

; catch all function pra qualquer peça que se mova em uma linha 
; reta (vertical, diagonal ou horizontal) continuamente, sem pular peças.
(defun straight-move (state piece direction)
  (loop with y = (get-pos piece) 
        for x = (pos-offset y direction)
        for access = (access-board state x)
        while (valid-pos-p x)
        if (equalp (get-pos piece) x) ;; não salvar a mesma posição da peça
          do (progn nil) ; continue
        else if (typep access 'piece)
          collect (make-move piece x x) into ret
          and do (loop-finish)
        else if (valid-pos-p x)
               collect (make-move piece x x) into ret
        do (setf y x)
        finally (return ret)))

(defun list-straight-moves-by-offset (state piece direction-list)
  (loop for direction in direction-list
        append (straight-move state piece direction)))

(defmethod list-moves ((rook rook) state opponent-moves)
  (declare (ignore opponent-moves))
  (list-straight-moves-by-offset state rook  
    '((-1 .  0)
      ( 0 .  1)
      ( 1 .  0)
      ( 0 . -1))))

(defmethod list-moves ((bishop bishop) state opponent-moves)
  (declare (ignore opponent-moves))
  (list-straight-moves-by-offset state bishop  
    '((-1 . -1)
      (-1 .  1)
      ( 1 .  1)
      ( 1 . -1)))) 

(defmethod list-moves ((queen queen) state opponent-moves)
  (declare (ignore opponent-moves))
  (list-straight-moves-by-offset state queen  
    '((-1 . -1)
      (-1 .  1)
      ( 1 .  1)
      ( 1 . -1)
      (-1 .  0)
      ( 0 .  1)
      ( 1 .  0)
      ( 0 . -1))))

(defmethod list-moves ((knight knight ) state opponent-moves)
  (declare (ignore opponent-moves))
  (loop for x :in '((-2 . -1)(-2 .  1)
                    (-1 .  2) (1 .  2)
                    ( 2 .  1) (2 . -1)
                    ( 1 . -2)(-1 .  -2))
        for off = (pos-offset (get-pos knight) x)
        for access = (access-board state off)
        if (valid-pos-p off)
          collect (make-move knight off off) into ret
        finally (return ret)
        )) 

(defun castle-move (offset state king rook-pos opponent-moves)
  (let ((rook (access-board state rook-pos)))
    (when (and rook
               (equalp 'rook (type-of rook))
               (color= rook king)
               (firstmove rook))
      (loop repeat 2
            with x = (get-pos king)
            for p = (rel-pos x :column offset)
            never (member p opponent-moves :test #'equalp :key #'can-capture)
            always (empty= (access-board state p))
            do (setf x p)
            finally (return (make-castling king p rook (rel-pos p :column (if (= +1 offset) -1 +1))))))))


(defmethod list-moves ((king king) state opponent-moves)
  (let ((free-moves
          (loop for i from -1 to 1
                append (loop for j from -1 to 1
                             for p = (rel-pos (get-pos king) :row i :column j)
                             for access = (access-board state p)
                             if (equalp (get-pos king) p)
                               do (progn nil) ;; continue
                             else if (valid-pos-p p) 
                                    collect (make-move king p p)))))
        ;; remover movimentos que resultariam em um check
        (setf free-moves (set-difference free-moves opponent-moves
                                     :test #'equalp :key #'can-capture))
    (unless (null opponent-moves)
      (with-slots (firstmove pos check) king
        (when (and firstmove (not check))
          (let ((queenside (castle-move -1 state king (pos (row pos) 1) opponent-moves))
                (kingside (castle-move +1 state king (pos (row pos) 8) opponent-moves)))
            (when queenside
              (push queenside free-moves))
            (when kingside
              (push kingside free-moves))))))
    ;;progn
    free-moves))

(defmethod list-moves ((pawn pawn) state opponent-moves)
  (declare (ignore opponent-moves))
  (let* ((ret nil)
         ;; os indices do tabuleiro começam do lado superior esquerdo e aqui a gente 
         ;; assume que as peças brancas estão no lado inferior do tabuleiro. como os 
         ;; peões brancos no nosso caso podem apenas se movimentar para cima, um offset
         ;; negativo para representar o movimento do peão branco e vice versa.
         (direction (if (equalp 'white (color pawn)) -1 1)) 
         (front (rel-pos (get-pos pawn) :row direction ))
         (front2 (rel-pos front :row direction)))
    (when (pos-empty= state front)
      (push (make-pawn-move :moving-piece pawn :landing-pos front :new-rank (1+ (rank pawn)))
            ret)
      (when (and (firstmove pawn) (pos-empty= state front2))
        (push (make-pawn-move :moving-piece pawn :landing-pos front2 :new-rank (+ 2 (rank pawn)))
              ret)))
    (defun pawn-capture (left-right)
      ;; peão é a unica peça que não captura uma outra peça onde ele aterrissa.
      ;; essa parte é chatinha.
      (let* ((lr-offset (if (equalp left-right 'left) -1 +1))
             (diagonal 	(rel-pos front :column lr-offset))
             (side 	(rel-pos (get-pos pawn) :column lr-offset))
             ;; (diagonal-field 	(access-board state diagonal))
             (side-field 	(access-board state side)))
        (when (valid-pos-p diagonal)
                (push (make-pawn-move :can-capture diagonal
                                      :moving-piece pawn
                                      :landing-pos diagonal
                                      :new-rank (1+ (rank pawn)))
                      ret))
        ;; en passant
        ;; https://en.wikipedia.org/wiki/En_passant
        ;; outro movimento que o peão não faria normalmente.
        (when (and (valid-pos-p diagonal)
                    (valid-pos-p side)
                    (equalp side-field (last-double-push state)))
                (push (make-pawn-move :can-capture side
                                      :moving-piece pawn
                                      :passant t
                                      :landing-pos diagonal
                                      :new-rank (1+ (rank pawn)))
                      ret))))
    ;; todo: separar movimentos e capturas em duas funções diferentes.
    (pawn-capture 'right) (pawn-capture 'left)
    ;; progn
    ret))

(defun execute-move (piece state move)
  (with-slots (moving-piece landing-pos can-capture) move
    (with-slots (firstmove pos) piece
      (set-access state pos 'empty)
      (when (and can-capture
                 (not (color= piece (access-board state can-capture))))
        (set-access state can-capture 'empty)
        (setf (piece-refs state)
              (remove can-capture (piece-refs state) :test #'equalp :key #'get-pos)))
      (set-access state landing-pos moving-piece)
      (setf firstmove nil
            pos landing-pos
            (last-move state) move))))

(defmethod move-piece ((piece piece) state move)
  (execute-move piece state move))

(defmethod move-piece ((king king) state (castling castling))
  (with-slots (moving-piece rook rook-landing-pos) castling
    (execute-move rook state (make-move rook rook-landing-pos))
    (execute-move king state castling)))

(defmethod move-piece ((pawn pawn) state pawn-move)
  (with-slots (firstmove rank) pawn
    ;; (when passant (setf passant nil))
    (when (and firstmove (= 4 (new-rank pawn-move)))
      (setf (last-double-push state) pawn))
    (setf rank (new-rank pawn-move))
    (execute-move pawn state pawn-move)))

(defmethod legal? ((x move))
  (not (color= (moving-piece x) (access-board *game* (can-capture x)))))

(defmethod legal? ((x castling)) t) ;; castle-move ja da conta disso

(defmethod legal? ((x pawn-move)) 
  (with-slots (moving-piece landing-pos can-capture) x 
    (if (not can-capture) t
      (with-slots (pos color) moving-piece
        (let ((can-capture-field (access-board *game* can-capture)))
          (when (and (not-empty= can-capture-field)
                     (not (color= can-capture-field moving-piece)))
            (or (not (= (row pos)
                        (row (get-pos can-capture-field))))
                (when (typep can-capture-field 'pawn)
                  (equalp can-capture-field (last-double-push *game*)))))))))) ;; inferno

(defun filter-illegal (move-list) 
  (remove-if-not #'legal? move-list))

(defun color-moves (state color)
  (with-slots (current-turn piece-refs) state
    (remove-if #'null ;; porque isso retornar null?
               (loop for x in (remove-if-not (lambda (x) (equalp color (color x))) piece-refs)
                     append (list-moves x state nil)))))

(defun opponent-moves (state)
  (remove-if #'null (color-moves state (if (equalp 'white (get-turn state)) 'black 'white))
             :key #'can-capture))

(defun turn ()
  (with-slots (current-turn last-double-push) *game*
    
    (setf current-turn (toggle current-turn 'white 'black))
    (when (and last-double-push
               (equalp current-turn (color last-double-push)))
      (setf last-double-push nil))
    (format t "~a~%" (last-double-push *game*))
    )
  )

(defvar *game* (make-instance 'state)) ;; instancia pricipal
(defvar *font-face* nil)

(defclass options ()
  ((show-opponent-moves :accessor show-opponent-moves :initform nil)
   (turn-highlight :accessor turn-highlight :initform t)))

(sketch:defsketch board ;; fr
    ((sketch:title "chess")
     (sketch:background sketch:+white+)
     (rect-size 80)
     (sketch:width (* 8 rect-size))
     (sketch:height (* 8 rect-size))
     (grey-select (sketch:gray 0.95))
     (selected-piece nil)
     (available-moves nil)
     (opponent-moves nil)
     (game-state :running)
     (options (make-instance 'options))) 
  (defun draw-square (fill i j)
    (sketch:with-pen (sketch:make-pen :fill fill :stroke sketch:+black+) 
      (sketch:rect (* i rect-size) (* j rect-size) rect-size rect-size)))
  (when (null opponent-moves)
    (setf opponent-moves (opponent-moves *game*)))
  (case game-state
    (:running
     ;; (when (equalp 'black (get-turn *game*)) ;; um amigo pra jogar contra =))))
     ;;   (let* ((moves (filter-illegal (color-moves *game* 'black)))
     ;;          (random-move (nth (random (length moves)) moves)))
     ;;     (when (and random-move (moving-piece random-move))
     ;;       (move-piece (moving-piece random-move)
     ;;                   *game*
     ;;                   random-move)
     ;;       (turn))))
     (loop for i from 0 to 7
           do (loop for j from 0 to 7
                    for field = (pos (1+ j) (1+ i))
                    for access = (access-board *game* field)
                    do (draw-square sketch:+white+ i j)
                    if (turn-highlight options)
                      if (and (not-empty= access)
                              (equalp (get-turn *game*) (color access)))
                        do (draw-square grey-select i j)
                    if (and (last-move *game*)
                            (pos= (moving-piece-pos (last-move *game*)) field))
                      do (draw-square sketch:+yellow+ i j)
                    if (and (last-move *game*)
                            (pos= (landing-pos (last-move *game*)) field))
                      do (draw-square sketch:+yellow+ i j)
                    do (cond ;; coloração de todas as casas
                         ((member field available-moves :test #'equalp :key #'landing-pos)
                          (draw-square sketch:+green+ i j))
                         ((and opponent-moves (show-opponent-moves options))
                          (when (member field opponent-moves :test #'equalp :key #'can-capture)
                            (draw-square sketch:+red+ i j))
                          (when (member field opponent-moves :test #'equalp :key #'moving-piece-pos)
                            (draw-square sketch:+orange+ i j))))
                    do (unless (empty= access) ;; representação em texto das peças (temporario)
                         (sketch:with-font (sketch:make-font :face *font-face* :size (/ rect-size 4) :color sketch:+black+) 
                           (sketch:text (to-string access)
                                        (* i rect-size) (* j rect-size))))
                    )))
    (:pause
     ;; (setf sketch:background sketch:+white+)
     (sketch:with-pen (sketch:make-pen :fill sketch:+white+ :stroke sketch:+black+) 
       (sketch:rect 0 0 sketch:width sketch:height))
     (sketch:with-font (sketch:make-font :face *font-face* :size (/ rect-size 4) :color sketch:+black+) 
       (sketch:text "menu"
                    50 50)))))

(defmethod kit.sdl2:mousebutton-event ((instance board) state ts b x y)
  (when (eq state :mousebuttondown)
    (with-slots (rect-size available-moves selected-piece opponent-moves) instance
      (let* ((cy (1+ (truncate (/ y rect-size))))
             (cx (1+ (truncate (/ x rect-size))))
             (p (pos cy cx))
             (access (access-board *game* p)))
        (defun clear-selection ()
          (setf selected-piece nil available-moves nil))
        (defun select ()
          (setf available-moves (filter-illegal (list-moves access *game* (opponent-moves *game*)))
                selected-piece access))
        (if selected-piece
            (let ((selected-move (find p available-moves :test #'equalp :key #'landing-pos)))
              (if selected-move
                  (progn (move-piece selected-piece *game* selected-move)
                         (clear-selection)
                         (setf opponent-moves nil)
                         (turn)) 
                  (if (and (not-empty= access)
                           (color= access selected-piece)) 
                      (select)
                      (clear-selection))))
            (when (and (not selected-piece) ; selecionar peça
                       (not-empty= access)
                       (equalp (color access) (get-turn *game*)))
              (select)))))))

(defmethod kit.sdl2:keyboard-event ((instance board) state timestamp repeat-p keysym)
  (declare (ignorable timestamp repeat-p keysym))
  (with-slots (options game-state available-moves selected-piece opponent-moves) instance
    (with-slots (show-opponent-moves turn-highlight) options
      (when (eql state :keydown)
        (cond
          ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-p)
           (setf game-state (if (equalp game-state :running) :pause :running)))
          ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
           (setf selected-piece nil
                 available-moves nil
                 opponent-moves nil
                 (get-turn *game*) 'white)
           (setup-board *game*))
          ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-1)
           (setf show-opponent-moves (if show-opponent-moves nil t)))
          ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-2)
           (setf turn-highlight (if turn-highlight nil t))))))))

(defmethod setup ((instance board) &key &allow-other-keys)
  (setf *font-face* (sketch:load-resource (sketch::relative-path "Comic Sans MS 400.ttf" 'board))))

(defun start-board ()
  (setup-board *game*)
  (make-instance 'board))
