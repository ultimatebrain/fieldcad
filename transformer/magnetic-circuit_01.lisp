;;; classical transformer computation
;;;;;;;;;;;;;;;;;;;;
;;; Michail Titan aka w0rm, hack.tty
;;;;;;;;;;;;;;;;;;;;;;

;;;
(defun mlst (&rest lst)
  lst) ;;; Возвращаем список, склеенный из переданных аргументов
;;;
(defun genXArr (Nshiht S Sshiht X0 X &optional XArr) ;;; Создаём массив X`сов для Шихт
      (cond ((Eq X0 0) ;; при X0 = 0, таким образом мы обозначаем вход в функцию
	 (progn
	   (setf (aref XArr (- Nshiht 1)) (mlst X0 X)) ;; Заполняем первую ячейку (нумерация ячеек
	   ;; массива с нуля! поэтому ячейки N не существует)
	   (genXArr (- Nshiht 2) S Sshiht (+ X S)  (+ X (+ S Sshiht )) XArr) ;;; вызываем себя же, с
	   ;; новыми параметрами 
	   ))
	((< Nshiht 0) XArr) ;; В случае когда посчитали все шихты, вернём массив XArr
	(t ;; В случае не первого входа в функцию
	 (progn
	   (setf (aref XArr  Nshiht) (mlst X0 X)) ;; заполняем ячейку массива
	   (genXArr (- Nshiht 1) S Sshiht (+ X S)  (+ X (+ S Sshiht)) XArr) ;;; вызываем себя
	   )
	 )
	)
      )
(defun gen-XArr (Nshiht S Sshiht) 
  (reverse ;;; т.к. наполняем массив с последней ячейки, реверснем ;]
   (genXArr Nshiht S Sshiht 0 Sshiht
	    (let ((XArr (make-array Nshiht :initial-element NIL))) XArr) ;;; Во избежании глобальных переменных
  ;;; производим вызов, передав в качестве аргумента локальную переменную массива
  ;;; который и вернём
	    )
    )
  )
;;;
;;;;;;;;;;;;;; Пластина №1, в Локальной системе координат
(defun genBlockB0 (Sshiht Hshiht lsh1) ;;; точки D D' A A' B B' E E'
  (let ( (B0 (make-array 8 :initial-element NIL)) ) ;; локальный массив, возвращаемый функцией
    (progn
      (setf (aref B0 0) (mlst 0 0 0))             ;;; D
      (setf (aref B0 1) (mlst Sshiht 0 0))        ;;; D'
      (setf (aref B0 2) (mlst 0 Hshiht 0))        ;;; A
      (setf (aref B0 3) (mlst Sshiht Hshiht 0))   ;;; A'
      (setf (aref B0 4) (mlst 0 Hshiht lsh1))     ;;; B
      (setf (aref B0 5) (mlst Sshiht Hshiht lsh1));;; B'
      (setf (aref B0 6) (mlst 0 0 lsh1))          ;;; E
      (setf (aref B0 7) (mlst Sshiht 0 lsh1))     ;;; E'
      )
    B0 ;;; функция возвращает массив координат 
    )
  )
;;
(defun genBlockB1 (Sshiht Hshiht hsh2 lsh1 lsh2) ;;; точки C C' F F' G G' H H'
  (let ( (B1 (make-array 8 :initial-element NIL)) )
    (progn
      (setf (aref B1 0) (mlst 0 hsh2 lsh1))        ;;; C
      (setf (aref B1 1) (mlst Sshiht hsh2 lsh1))   ;;; C'
      (setf (aref B1 2) (mlst 0 0 lsh2))           ;;; F
      (setf (aref B1 3) (mlst Sshiht 0 lsh2))      ;;; F'
      (setf (aref B1 4) (mlst 0 hsh2 lsh2))        ;;; G
      (setf (aref B1 5) (mlst Sshiht hsh2 lsh2))   ;;; G'
      (setf (aref B1 6) (mlst 0 Hshiht lsh2))      ;;; H
      (setf (aref B1 7) (mlst Sshiht Hshiht lsh2)) ;;; H'
      )
    B1 ;;; ну ясно ;]
    )
  )
;;
(defun genBlockB2 (Hshiht Sshiht hsh2 lsh3 lsh4) ;;; K K' P P' R R' S S'
  (let ( (B2 (make-array 8 :initial-element NIL)))
    (progn
      (setf (aref B2 0) (mlst 0 Hshiht lsh3))      ;;; K
      (setf (aref B2 1) (mlst Sshiht Hshiht lsh3)) ;;; K'
      (setf (aref B2 2) (mlst 0 hsh2 lsh3))        ;;; P
      (setf (aref B2 3) (mlst Sshiht hsh2 lsh3))   ;;; P'
      (setf (aref B2 4) (mlst 0 0 lsh3))           ;;; R
      (setf (aref B2 5) (mlst Sshiht 0 lsh3))      ;;; R'
      (setf (aref B2 6) (mlst 0 Hshiht lsh4))      ;;; S
      (setf (aref B2 7) (mlst Sshiht Hshiht lsh4)) ;;; S'
      )
    B2 ;;;
    )
  )
;;
(defun genBlockB3 (Hshiht Sshiht hsh2 lsh4 Lshiht) ;;; точки Z Z' Q Q' O O' W W'
  (let ( (B3 (make-array 8 :initial-element NIL)) )
    (progn
      (setf (aref B3 0) (mlst 0 hsh2 lsh4))          ;;; Z
      (setf (aref B3 1) (mlst Sshiht hsh2 lsh4))     ;;; Z'
      (setf (aref B3 2) (mlst 0 0 lsh4))             ;;; Q
      (setf (aref B3 3) (mlst Sshiht 0 lsh4))        ;;; Q'
      (setf (aref B3 4) (mlst 0 Hshiht Lshiht))      ;;; O
      (setf (aref B3 5) (mlst Sshiht Hshiht Lshiht)) ;;; O'
      (setf (aref B3 6) (mlst 0 0 Lshiht))           ;;; W
      (setf (aref B3 7) (mlst Sshiht 0 Lshiht))      ;;; W'
      )
    B3 ;;;
    )
  )
;;;;;;;;;;;;;; Пластина №2
(defun genBlockB0-2 (Sshiht Hshiht lsh1) ;;; A A' D D' E E' B B'
  (genBlockB0 Sshiht Hshiht lsh1)
  )
(defun genBlockB1-2 ( Sshiht Hshiht hsh2 lsh1 lsh2) ;;; точки C C' H H' G G' F F'
  (let ( (B1-2 (make-array 8 :initial-element NIL)) )
    (progn
      (setf (aref B1-2 0) (mlst 0  ( + hsh2 (- (/ Hshiht 2) hsh2)) lsh1))        ;;; C
      (setf (aref B1-2 1) (mlst Sshiht ( + hsh2 (- (/ Hshiht 2) hsh2)) lsh1))    ;;; C'
      (setf (aref B1-2 2) (mlst 0 0 lsh2))                                       ;;; H
      (setf (aref B1-2 3) (mlst Sshiht 0 lsh2))                                  ;;; H'
      (setf (aref B1-2 4) (mlst 0 ( + hsh2 (- (/ Hshiht 2) hsh2)) lsh2))         ;;; G
      (setf (aref B1-2 5) (mlst Sshiht ( + hsh2 (- (/ Hshiht 2) hsh2)) lsh2))    ;;; G'
      (setf (aref B1-2 6) (mlst 0 Hshiht lsh2))                                  ;;; F
      (setf (aref B1-2 7) (mlst Sshiht Hshiht lsh2))                             ;;; F'
      )
    B1-2 ;;; ну ясно ;]
    )
  )
;;
(defun genBlockB2-2 (Hshiht Sshiht hsh2 lsh3 lsh4) ;;; R R' P P' K K' Q Q'
  (let ( (B2-2 (make-array 8 :initial-element NIL)))
    (progn
      (setf (aref B2-2 0) (mlst 0 Hshiht lsh3))                              ;;; R
      (setf (aref B2-2 1) (mlst Sshiht Hshiht lsh3))                         ;;; R'
      (setf (aref B2-2 2) (mlst 0 ( + hsh2 (- (/ Hshiht 2) hsh2)) lsh3))     ;;; P
      (setf (aref B2-2 3) (mlst Sshiht ( + hsh2 (- (/ Hshiht 2) hsh2)) lsh3));;; P'
      (setf (aref B2-2 4) (mlst 0 0 lsh3))                                   ;;; K
      (setf (aref B2-2 5) (mlst Sshiht 0 lsh3))                              ;;; K'
      (setf (aref B2-2 6) (mlst 0 Hshiht lsh4))                              ;;; Q
      (setf (aref B2-2 7) (mlst Sshiht Hshiht lsh4))                         ;;; Q'
      )
    B2-2 ;;;
    )
  )
;;
(defun genBlockB3-2 (Hshiht Sshiht hsh2 lsh4 Lshiht) ;;; точки Z Z' S S' W W' O O'
  (let ( (B3-2 (make-array 8 :initial-element NIL)) )
    (progn
      (setf (aref B3-2 0) (mlst 0 ( + hsh2 (- (/ Hshiht 2) hsh2)) lsh4))     ;;; Z
      (setf (aref B3-2 1) (mlst Sshiht ( + hsh2 (- (/ Hshiht 2) hsh2)) lsh4));;; Z'
      (setf (aref B3-2 2) (mlst 0 0 lsh4))                                   ;;; S
      (setf (aref B3-2 3) (mlst Sshiht 0 lsh4))                              ;;; S'
      (setf (aref B3-2 4) (mlst 0 Hshiht Lshiht))                            ;;; W
      (setf (aref B3-2 5) (mlst Sshiht Hshiht Lshiht))                       ;;; W'
      (setf (aref B3-2 6) (mlst 0 0 Lshiht))                                 ;;; O
      (setf (aref B3-2 7) (mlst Sshiht 0 Lshiht))                            ;;; O'
      )
    B3-2 ;;;
    )
  )
(defun getEdgesShiht(Lshiht Sshiht Hshiht hsh2 lsh1 lsh2 lsh3 lsh4)
       ;;; получим координаты одной шихты в ЛСК
       ;;; для пластины №1
  (let ( (Edges (make-array 4 :initial-element NIL)) )
    (progn
      (setf (aref Edges 0) (genBlockB0 Sshiht Hshiht lsh1))             ;;; Block0
      (setf (aref Edges 1) (genBlockB1 Sshiht Hshiht hsh2 lsh1 lsh2))   ;;; Block1
      (setf (aref Edges 2) (genBlockB2 Hshiht Sshiht hsh2 lsh3 lsh4))   ;;; Block2  
      (setf (aref Edges 3) (genBlockB3 Hshiht Sshiht hsh2 lsh4 Lshiht)) ;;; Block3
      )
    Edges ;;;
    )
  )
;;
(defun getEdgesShiht-2(Lshiht Sshiht Hshiht hsh2 lsh1 lsh2 lsh3 lsh4)
       ;;; получим координаты одной шихты в ЛСК
       ;;; для пластины №2
  (let ( (Edges-2 (make-array 4 :initial-element NIL)) )
    (progn
      (setf (aref Edges-2 0) (genBlockB0-2 Sshiht Hshiht lsh1))             ;;; Block0
      (setf (aref Edges-2 1) (genBlockB1-2 Sshiht Hshiht hsh2 lsh1 lsh2))   ;;; Block1
      (setf (aref Edges-2 2) (genBlockB2-2 Hshiht Sshiht hsh2 lsh3 lsh4))   ;;; Block2  
      (setf (aref Edges-2 3) (genBlockB3-2 Hshiht Sshiht hsh2 lsh4 Lshiht)) ;;; Block3
      )
    Edges-2 ;;;
    )
  )
;;;;

