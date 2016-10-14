;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;@@@@@@@@@@@@@@@@@@@@@@@@@PART 1@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;***********CYCLE****ok***********
(defun cycle (n lis)
    (setq newList lis)
    (if (null lis)
        (return-from cycle nil)
        (progn 
            (dotimes (x n)
                (setq first (list (car newList)))
                (setq newList (cdr newList))
                (setq newList (append newList first))
            )
        )
    )
    (return-from cycle newList)
)



;;;;***********Split-list***********
(defun split-list (delim lis)
    (setq mainlist (list))
    (setq templist (list))
    (setq x 0)

    (if (null lis)
        (return-from split-list nil)
        (progn (dolist (n lis)
                    (setq x (+ x 1))
                    (if (and (equal n delim) (not (= x (length lis))))
                        (progn
                            (if (not (null templist))
                                (progn
                                    (setq templist (reverse templist))
                                    (setq mainlist (cons templist mainlist))
                                    (setq templist (list))
                                )
                            )
                        )
                        (if (not (equal n delim))
                            (setq templist (cons n templist)))
                    )
                )
                (setq templist (reverse templist))
                (setq mainlist(cons templist mainlist))
                (setq mainlist (reverse mainlist))
                (if (member nil mainlist) (setq mainlist nil))
                (return-from split-list mainList)
        )
    )
    
)




;***********RANGE**ok*********
(defun range (l)
    (cond
        ((equal (length l) 0) nil)
        (t (list (apply 'min l) (reduced-frac l) (apply 'max l)))
    )
)

(defun reduced-frac (l)
    (cond
        ((equal (length l) 0) nil)
        ((not(equal (length l) 0)) (/ (apply '+ l) (length l)))
    )
)



;***********Search-count**ok***********Use the hints in TA slices
(defun search-count (x l)
    (cond
        ((null l) 0)
        ( (equal x (car l)) (1+ (search-count x (cdr l))) )
        (t (search-count x (cdr l)))
    )
)



;***********pivot**ok*********: reused concept from search-count
(defun pivot (n l)
    (list (lessList n l) (greaterList n l))
)

(defun lessList (n l)
    (cond
        ((null l) nil)
        ( (> n (car l)) (cons (car l) (lessList n (cdr l))) )
        (t (lessList n (cdr l)))
    )
)

(defun greaterList (n l)
    (cond
        ((null l) nil)
        ( (or (< n (car l))(equal n (car l))) (cons (car l)(greaterList n (cdr l))))
        (t (greaterList n (cdr l)))
    )
)


;***********quicksort***********:reused concept from search count and pivot
(defun quicksort (l)
    (cond
        ((null l) nil)
        ((cons (apply 'min l) (quicksort (remove (apply 'min l) l))))
    )
)


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ PART 2 @@@ DONE @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;using TA slices' algorithm - see slices for details

(defun match (list1 list2)
    (cond   
        ((or (equal list1 nil)(equal list2 nil)) (equal list1 list2))               ;list1 or list2 null       
        ( (equal (car list1) '?)(match (cdr list1)(cdr list2)) )                    ;list1's head is ?
        ( (and (equal (car list1) '!)(match (cdr list1)(cdr list2)) ) (not nil) )   ;list1's head is ! and match the rest of both lists
        ( (equal (car list1) '!)(match list1 (cdr list2)) )                         ;list1's head is !
        ((equal (car list1) (car list2)) (match (cdr list1)(cdr list2)) )           ;list1's head equals to list2's head
    )
)


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ PART 3 @@@@ DONE @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;*************matrix-add*** ok ***************
(defun matrix-add (list1 list2)
    (cond
        ((and (null list1)(null list2)) nil)
        ( (and (not(null list1)) (not(null list2)))
            (append 
                (list (mapcar #' + (car list1)(car list2)))
                (matrix-add(cdr list1)(cdr list2))
            )
        )
        (t (matrix-add(cdr list1)(cdr list2)))
    )
)


;*************matrix-transpose** ok ****************
(defun matrix-transpose (l)
   (apply #'mapcar #'list l)

)



;************helper functions for matrix-multiply****************

;***********flatten**********************
(defun flatten (lis)
    (setq mainlist (list))
    (dolist (n lis)
        (setq mainlist (append mainlist n))
    )
    (return-from flatten mainlist)
)

;***********atomatindex**********************
(defun atomatindex (index lis)
    (setq templist lis)
    (dotimes (n (- index 1))
        (setq templist (cdr templist))   
    )
    (return-from atomatindex (car templist))

)

;**************even***************************
(defun even (n)
    (setq bool nil)
    (if (= (mod n 2) 0)
        (setq bool t)
    )
    (return-from even bool)
)



;;****************matrix-mulitply****************
(defun matrix-multiply (list1 list2)
 (if (null list1)
  (return-from matrix-multiply nil)
  (progn
    ;;get the size of the matrices
    (setq l1rows (length list1))
    (setq l1columns (length (car list1)))
    (setq l2rows (length list2))
    (setq l2columns (length (car list2)))
     ;;flatten both lists
    (setq newlist1 (flatten list1))
    (setq newlist2 (matrix-transpose list2))
    (setq newlist2 (flatten newlist2))
    ;;;for test purposes;;;
    ;(write-line "newlist1")
    ;(print newlist1)
    ;(terpri)
    ;(write-line "newlist2")
    ;(print newlist2)
    ;(terpri)
    ;;;;;;;;;;;;;;;;;;;;;
    ;;find multiplication terms and add them to a temporary list
    (setq templist (list)) ;has the multiplied terms
    (setq m1index 0)
    (setq m2index 0)
    (dolist (n newlist1)
        (setq m1index (+ m1index 1))
        (dolist (m newlist2)
            (setq m2index (+ m2index 1))
            (if (equal (even m1index) (even m2index))
                (setq templist (cons (* n m) templist))
            )
        )
    )
    (setq templist (reverse templist))
    ;;;for test purposes;;;
    ;(write-line "templist")
    ;(print templist)
    ;(terpri)
    ;;;;;;;;;;;;;;;;;;;;;
    ;;divide templist into two adjacent lists
    (setq templist1 (list))
    (setq templist2 (list))
    (dotimes (x (* 2 l1columns))
        (dotimes (y l2columns)
            (if (even x)
                (progn
                    (setq templist1 (cons (car templist) templist1))
                    (setq templist (cdr templist))
                )
                (progn 
                    (setq templist2 (cons (car templist) templist2))
                    (setq templist (cdr templist))
                )
            )
        )
    )

    (setq templist1 (reverse templist1))
    (setq templist2 (reverse templist2))
    ;;;for test purposes;;;
    ;(write-line "templist1")
    ;(print templist1)
    ;(terpri)
    ;(write-line "templist2")
    ;(print templist2)
    ;(terpri)
    ;;;;;;;;;;;;;;;;;;;;
    
    ;;sum up the appropriate product terms and add them to a list
    (setq templist3 (list))
    (dotimes (n (length templist1))
        (setq templist3 (cons (+ (car templist1) (car templist2)) templist3))
        (setq templist1 (cdr templist1))
        (setq templist2 (cdr templist2))
    )
    (setq templist3 (reverse templist3))
    ;;;for test purposes;;;
    ;(print templist3)
    ;(terpri)
    ;;;;;;;;;;;;;;;;;;;;;;
    
    ;;unflatten the matrix list returning it to matrix form (rows and columns)
    (setq templist4 (list))
    (setq templist (list))
    (dotimes (a l1rows)
        (dotimes (z l2columns)
            (setq templist4 (cons (car templist3) templist4))
            (setq templist3 (cdr templist3))
        )
        (setq templist4 (reverse templist4))
        (setq templist (cons templist4 templist))
        (setq templist4 (list))
    )
    (setq templist (reverse templist))
    ;;;for test purposes;;;
    ;(print templist)
    ;;;;;;;;;;;;;;;;;;;;;;;
    
    
    ;;Last case
    (if (and (= l1rows 1) (= l2columns 1))
        (progn
            (setq templis 0)
            (dotimes (n (length newlist1))
                (setq templis (+ (* (car newlist1) (car newlist2)) templis))
                (setq newlist1 (cdr newlist1))
                (setq newlist2 (cdr newlist2))
            )
            (setq templist (list (list templis)))
            
        )
        (return-from matrix-multiply templist)
    )
  )
 )
    
)

