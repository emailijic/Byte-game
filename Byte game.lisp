(load "Inference_engine.cl")

(defun dimenzije() (progn
                    (format t "~%")
                    (format t "~CUnesi dimenziju table: " #\return)
                    (setq n (read))
                    (if (or (equalp (pocetniBrojFiguraPoIgracu n) 0) (not (equalp (mod (* (pocetniBrojFiguraPoIgracu n) 2) 8) 0))) 
                        (progn (format t "~C Dimenzije nisu validne " #\return) (dimenzije))))
)

(defun igra_prvi() (setq na_potezu 'x)
                   (progn
                    (format t "~C Da li zelite da igrate prvi? (DA/NE): " #\return)

							(let ((odg (read))) (cond ((equal odg 'DA)
                                                        (progn
                                                        (setq covek 'X)
                                                        (setq racunar 'O)
                                                        (format t "~C Izbor uspesan. Prvi igrate Vi! ~%" #\return)))
                                                      ((equal odg 'NE)
                                                        (progn
                                                        (setq covek 'O)
                                                        (setq racunar 'X)
                                                        (format t "~C Izbor uspesan.Prvi igra racunar! ~%" #\return)))
                                                      (t (progn (format t "~C Neuspesno.Ponovo izaberite! ~%" #\return) (igra_prvi))))	
							)           
					)                  
)

(defun vrati_broj (el)                                  
  (case el
    ('A 1)
    ('B 2)
    ('C 3)
    ('D 4)
    ('E 5)
    ('F 6)
    ('G 7)
    ('H 8)
	('I 9)
    ('J 10)
    ('K 11)
    ('L 12)
    ('M 13)
    ('N 14)
    ('O 15)
    ('P 16)
    ('Q 17)
    ('R 18)
    ('S 19)
    ('T 20)
    ('U 21)
    ('V 22)
    ('W 23)
    ('X 24)
    ('Y 25)
    ('Z 26 )
    ))

(defun  pocetneKoord (i)
  (cond ((<= i 0) '())
        (t(append (pocetneKoord (- i 2)) (list i)))
   )
)

(defun  pocetneKoordKolona (igrac i)
  
  (if (eq igrac 'x) 
        (cond ((<= i 0) '())
                (t(append (pocetneKoordKolona igrac (- i 2)) (list i)))
        ) 
            (cond ((<= (1- i) 0) '())
                (t(append (pocetneKoordKolona igrac (-  i 2)) (list (1- i))))
        ) 
    )
)

(defun  pocetneKoordVrsta (igrac)
 (if (eq igrac 'y) (cdr (pocetneKoord (1- n))) (remove n (pocetneKoord n)) ) 
)  

(defun vezi (el lista)
	
	(if (not (null lista))  
	(append (list el (car lista))  (vezi el (cdr lista)))
	)
)

(defun vezi2 (listaV listaK)
	
	(if  (not (null listaV)) 
		( append (vezi (car listaV) listaK) (vezi2 (cdr listaV) listaK)
		
	))
)

(defun dodaj (lista elem)   

    (if (not (listp elem))
	(if (null lista) '()
		
		(cons (list (list (car lista) (cadr lista)) (list elem)) (dodaj (cddr lista) elem))
	)

    (if (null lista) '()
		
		(cons (list (list (car lista) (cadr lista))  elem) (dodaj (cddr lista) elem))
	)
    )
)

(defun mapiranjeKoordinata (i j)
   (if (or (<= i 0) (<= j 0) (>= i (1+ n)) (>= j (1+ n)) )
     '()
      (1- (+(* (1- i) n) j))
   )
)

(defun mapiranjeKoordinata2 (i j)
    
    (if (or (<= i 0) (<= j 0) (>= i (1+ n)) (>= j (1+ n)) )
    '()
  (1- (+(* (1- i) n) j)))

)

(defun setListaZaKoordinatu (i j lista tabla)

      (setf (nth 1 (nth (mapiranjeKoordinata i j) tabla) ) lista)
)
(defun setListeZaKoordinatuSaKoordinate (ip jp ik jk lista tabla)
       (setListaZaKoordinatu ik jk lista tabla)
       (vratiStanjeNaKoordinati ip jp tabla)

)

(defun appendListaZaKoordinatu (i j lista tabla)

      (setf (nth 1 (nth (mapiranjeKoordinata i j) tabla) )  (append (nth 1 (nth (mapiranjeKoordinata i j) tabla) ) lista))
)

(defun pocetniBrojFiguraPoIgracu (dimenzija)
   (/ (* (- dimenzija 2) (/ dimenzija 2)) 2)
)

(defun vratiListu_1_do_n (pocKoord)

    (if (<= pocKoord n) 

        (append (list pocKoord) (vratiListu_1_do_n (1+ pocKoord)))
    )
)

(defun praznaTabla (lista_1_do_n)

    (dodaj (vezi2 lista_1_do_n lista_1_do_n) '() )

)

(defun pocetneKoordinate  (igrac)

    (vezi2 (pocetneKoordVrsta igrac) (pocetneKoordKolona igrac n))

)

(defun initTablaPocetakIgrac (listaKooX)

    (if (null listaKooX) '()

    (progn
        (setListaZaKoordinatu (car listaKooX) (cadr listaKooX) '(x) TRENUTNATABLA)
        (initTablaPocetakIgrac  (cddr listaKooX))
    )
    )
)

(defun initTablaPocetakKomp (listaKooY)

    (if (null listaKooY) '()

    (progn
        (setListaZaKoordinatu (car listaKooY) (cadr listaKooY) '(o) TRENUTNATABLA)
        (initTablaPocetakKomp  (cddr listaKooY))
    )
    )
)


(defun initTablaPocetakIgre ()

(setq TRENUTNATABLA (praznaTabla (vratiListu_1_do_n 1)))
(initTablaPocetakKomp (pocetneKoordinate 'y ))
(initTablaPocetakIgrac (pocetneKoordinate 'x) )
(stampajBrojeve 1)
(stampajCeluTablu 1 1 TRENUTNATABLA)
(setq listaStekova '())
(setq scoreList '((X 0)(O 0)))

)

(defun stampajBrojeve (i)

	(cond 
   ((> i n) (format t "~%"))
   ((= i 1) (progn
              (format t "    ~a  " i)
              (stampajBrojeve (+ i 1))))
   (t (progn
              (format t "    ~a  " i)
              (stampajBrojeve (+ i 1))
))
   )

)

(defun vratiStanjeNaKoordinati (i j tabla)

    (nth 1 (nth (mapiranjeKoordinata i j) tabla) )
)

(defun trecinaStanjaNaKoordinati (listaStanjaNaKoo trecina)

    (case trecina
        (1 (list (nth 0 listaStanjaNaKoo) (nth 1 listaStanjaNaKoo) (nth 2 listaStanjaNaKoo) ))
        (2 (list (nth 3 listaStanjaNaKoo) (nth 4 listaStanjaNaKoo) (nth 5 listaStanjaNaKoo) ))
        (3 (list (nth 6 listaStanjaNaKoo) (nth 7 listaStanjaNaKoo) (nth 8 listaStanjaNaKoo) ))
    )
)

(defun staviZvezdice (lista)

   (cond
   
        ((null lista) '())
        ((if (null (car lista)) (append (list '*) (staviZvezdice (cdr lista)))
        (append (list (car lista)) (staviZvezdice (cdr lista)))
        ))
   )
)

(defun stampaj1Red (i j tabla trecina)
	(cond 
		( (= j (+ n 1)) 	(format t "~%")) 
		
		( (= (mod i 2) (mod j 2))
			(progn
			
				(format t "~a " (staviZvezdice (trecinaStanjaNaKoordinati (vratiStanjeNaKoordinati i j tabla) trecina)))
				(stampaj1Red i (1+ j) tabla trecina)))
			
		 (t (progn 
            (format t "      ")
				(stampaj1Red i (1+ j) tabla trecina)))
	)
)

(defun stampaj3Reda (i j tabla)

(progn 
	(format t " ")
	(stampaj1Red i j tabla 1)
	(format t "~a" (vrati_slovo i))
	(stampaj1Red i j tabla 2)
	(format t " ")
	(stampaj1Red i j tabla 3)

))

(defun stampajCeluTablu (i j tabla)

	(cond
		((= i (+ n 1)) (format t "~%"))
	(t
		(progn 
		
			(stampaj3Reda i j tabla)
			(stampajCeluTablu (1+ i) j tabla)
			
		)
	)
	)
)

(defun vrati_slovo (i)                                  
  (case i
    (1 'A)
    (2 'B)
    (3 'C)
    (4 'D)
    (5 'E)
    (6 'F)
    (7 'G)
    (8 'H)
	(9 'I)
    (10 'J)
    (11 'K)
    (12 'L)
    (13 'M)
    (14 'N)
    (15 'O)
    (16 'P)
    (17 'Q)
    (18 'R)
    (19 'S)
    (20 'T)
    (21 'U)
    (22 'V)
    (23 'W)
    (24 'X)
    (25 'Y)
    (26 'Z)
    ))

(defun removeStack (i j lista tabla)
       (setListaZaKoordinatu i j '() tabla)
       (setq listaStekova (append listaStekova (list lista)))
       (addToScoreList lista)   
)

(defun removeFullStack (i j tabla)
       (if (and (equalp i (caaar tabla)) (equalp j (cadaar tabla)))
            (cons (list (list i j) '()) (cdr tabla))
            (cons (car tabla) (removeFullStack i j (cdr tabla)))
       )
)

(defun addToScoreList (lista) 
       (changeAsocList (if (equalp (nth (1- (length lista)) lista) 'X) 'X 'O))
       (printPoint (if (equalp (nth (1- (length lista)) lista) 'X) 'X 'O))          
)

(defun printPoint (el)
      (if (equalp el 'X) (format t "~C Igrac X je osvojio jedan poen ~%" #\return ) (format t "~C Igrac O je osvojio jedan poen  ~%" #\return ))
)

(defun changeAsocList (el)
       (setq scoreList (if (equalp el 'X) (append (list (list (car (car scoreList)) (1+ (nth 1 (car scoreList))))) (cdr scoreList))
       (append (list (car scoreList)) (list (list (car (nth 1 scoreList)) (1+ (nth 1 (nth 1 scoreList))))))
       ))
)

(defun checkScore (num)
       (if (or (equalp (nth 1 (car scoreList)) num) (equalp (nth 1 (nth 1 scoreList)) num)) t nil)
       

)

(defun isThereStack (i j tabla)
       (cond ((> i n) '())
       (t (cond ((AND (= (mod i 2) (mod j 2)) (equalp (length (nth 1 (nth (mapiranjeKoordinata i j) tabla))) 8))
       (removeStack i j (nth 1 (nth (mapiranjeKoordinata i j) tabla)) tabla)
       (princ "Koordinate stekova su:")
       (print i)
       (print j)
       )
       )
       (if (equalp j n) (isThereStack (+ i 1) 1 tabla) (isThereStack i (+ j 1) tabla))
       ))
)

(defun isThereFullStack (i j tabla)
    (if (equalp (length (nth 1 (nth (mapiranjeKoordinata i j) tabla))) 8) t '())
)

(defun DaLiJePraznoPolje (ik jk tabla)
    (if (or (<= ik 0) (> ik n) (<= jk 0) (> jk n))
        '()
        (if (null (vratiStanjeNaKoordinati ik jk tabla)) t     
            nil
        )
    )
)

(defun daLiJeMojStek (lista igracNaPotezu)
    (if (null lista) '()
      (if (not (equalp igracNaPotezu (car lista))) nil t)
    )
)

(defun GetSvaValidnaPolja (ip jp tabla)
   
       (if (AND 
                (if (ivicep (- ip 1) (- jp 1)) (DaLiJePraznoPolje (- ip 1) (- jp 1) tabla) t ) 
                (if (ivicep (- ip 1) (1+ jp)) (DaLiJePraznoPolje (- ip 1) (+ jp 1) tabla) t ) 
                (if (ivicep (1+ ip) (- jp 1)) (DaLiJePraznoPolje (+ ip 1) (- jp 1) tabla) t ) 
                (if (ivicep (1+ ip) (1+ jp)) (DaLiJePraznoPolje (+ ip 1) (+ jp 1) tabla) t ) 
            )
             t 
             nil
       )
)

(defun !xor (a b)
  (not (equalp a b))
)

(defun !jedanKomsa (ip jp tabla)
        (let
            (
                (a (DaLiJePraznoPolje (- ip 1) (- jp 1) tabla))
                (b (DaLiJePraznoPolje (- ip 1) (1+ jp) tabla))
                (c (DaLiJePraznoPolje (1+ ip) (- jp 1) tabla))
                (d (DaLiJePraznoPolje (1+ ip) (1+ jp) tabla))
            )
            (or (and a b (!xor c d)) (and c d (!xor a b)))
        )
)

(defun !viseKomsaod1 (ip jp tabla)
        (if (and (null (!jedanKomsa ip jp tabla)) (null (GetSvaValidnaPolja ip jp tabla))) t '())
)

(defun validnoPolje (ip jp ik jk)
       (if (OR (AND (= ik (- ip 1)) (= jk (- jp 1))) (AND (= ik (- ip 1)) (= jk (1+ jp)))
       (AND (= ik (1+ ip)) (= jk (- jp 1))) (AND (= ik (1+ ip)) (= jk (1+ jp))))
       t 
       nil
       )
)

(defun MogucePremestanjeCelogSteka (ip jp ik jk brPlocica tabla)
       (if (and (GetSvaValidnaPolja ip jp tabla) (= (length (vratiStanjeNaKoordinati ip jp tabla)) brPlocica)) t 
       nil
       )
)

(defun RezultujuciStek8 (ik jk lista tabla)
       (if (<= (+ (length (vratiStanjeNaKoordinati ik jk tabla)) (length lista)) 8)
       t 
       nil
       )
)

(defun IsRightStek (ip jp brplocica tabla)              
       (if (>= (length (vratiStanjeNaKoordinati ip jp tabla)) brplocica) t 
       nil
       )
)

(defun NapraviListuOdBrojaPlocica (brplocica ip jp tabla)
       (last (vratiStanjeNaKoordinati ip jp tabla) brplocica)
)

(defun without-last (list)
    (reverse (cdr (reverse list)))
)

(defun SkloniZadnjihNPlocica (listiic br)
     (if (equalp br 1) (without-last listiic)
     (SkloniZadnjihNPlocica (without-last listiic) (- br 1)))
)

(defun validnaVisinaZaSpajanje (ip jp ik jk brojPlocica tabla)
   (if
        (> (length (vratiStanjeNaKoordinati ik jk tabla)) (- (length (vratiStanjeNaKoordinati ip jp tabla))  brojPlocica )) t
       nil
    )
)

(defun appendListaZaKoordinatuSaKoordinate (ip jp ik jk lista tabla) 
    (setListaZaKoordinatu ip jp (SkloniZadnjihNPlocica (vratiStanjeNaKoordinati ip jp tabla) (length lista)) tabla)
    (appendListaZaKoordinatu ik jk lista tabla)
)

(defun setListaZaKoordinatuSaKoordinate (ip jp ik jk lista tabla) 
    (setListaZaKoordinatu ip jp (SkloniZadnjihNPlocica (vratiStanjeNaKoordinati ip jp tabla) (length lista)) tabla)
    (setListaZaKoordinatu ik jk lista tabla)
)

(defun contains (el list)
    (cond
        ((null list) '())
        ((equalp el (car list)) t)
        (t (contains el (cdr list)))
    )
)

(defun moveValidp (is js id jd table)
    (let* ((minL (minLeval(listOfNeighbors is js 0 0 is js 1 table) n))
            (K2 (listOfClosestNeighbors2 id jd is js is js (1- minL) table)))
        
             (if (null K2) '() t)          
    )
)

(defun listOfClosestNeighbors (i j GLOBALI GLOBALJ table)
    (let* ((komsije (listOfNeighbors i j 0 0 GLOBALI GLOBALJ 1 table)))
        (pureNeighbors (arangeNeighbors komsije (minLeval komsije n) )))       
)

(defun listOfClosestNeighbors2 (i j parenti parentj GLOBALI GLOBALJ level table)
    (let* ((komsije (listOfNeighbors i j parenti parentj GLOBALI GLOBALJ 1 table)))
        (pureNeighbors (arangeNeighbors komsije level )))
)

(defun minLeval (neighbors min)
  (cond 
    ((null neighbors) min)
    ( (< (car (car neighbors)) min) (minLeval (cdr neighbors) (car (car neighbors) ) ))
    (t (minLeval (cdr neighbors) min))
    )
)

(defun arangeNeighbors (neighbors minLevel) 
    (cond 
        ((null neighbors) '())
        ( (equal (car (car neighbors)) minLevel ) (cons (car neighbors) (arangeNeighbors (cdr neighbors) minLevel) ) ) 
        (t (arangeNeighbors (cdr neighbors ) minLevel))
    )
)

(defun pureNeighbors (neighbors)
    (cond ((null neighbors) '())
    (t (append  (car (cdr (car neighbors))) (pureNeighbors (cdr neighbors) )))
))

(defun removeDuplicates (list)
    (cond 
        ((null list) list)
        ((contains (car list) (cdr list)) (removeDuplicates(cdr list)) )
        (t (cons (car list) (removeDuplicates(cdr list)))
    )
))

(defun listOfNeighbors (i j parenti parentj GLOBALI GLOBALJ leval table )
    (if (< (1- n) leval ) '()
  ( progn (setq position (mapiranjeKoordinata2 i j))
    (if (null position) '()
        (let ((neighbors (getNeighbors i j parenti parentj GLOBALI GLOBALJ table ))) 
            (if (not (null neighbors)) (list (list leval neighbors))                
                (append (if (and (not (and (equalp parenti (1- i)) (equalp parentj (1- j)))) (not (and (equalp GLOBALI (1- i)) (equalp GLOBALJ (1- j)))) (checkCoords (1- i) (1- j)))   (listOfNeighbors  (1- i) (1- j)  i j GLOBALI GLOBALJ (+ 1 leval) table) )
                   (if (and (not (and (equalp parenti (1- i)) (equalp parentj (1+ j)))) (not (and (equalp GLOBALI (1- i)) (equalp GLOBALJ (1+ j))))  (checkCoords (1- i) (1+ j)))   (listOfNeighbors (1- i) (1+ j) i j GLOBALI GLOBALJ (+ 1 leval)  table))
                     (if (and (not (and (equalp parenti (1+ i)) (equalp parentj (1- j)))) (not (and (equalp GLOBALI (1+ i)) (equalp GLOBALJ (1- j)))) (checkCoords (1+ i) (1- j)))  (listOfNeighbors (1+ i) (1- j)  i j GLOBALI GLOBALJ (+ 1 leval) table) )
                    (if (and (not (and (equalp parenti (1+ i)) (equalp parentj (1+ j))))  (not (and (equalp GLOBALI (1+ i)) (equalp GLOBALJ (1+ j)))) (checkCoords (1+ i) (1+ j)))   (listOfNeighbors (1+ i) (1+ j) i j GLOBALI GLOBALJ (+ 1 leval) table)  )
                ) 
    ))))
))

(defun getNeighbors(i j parenti parentj GLOBALI GLOBALJ tabla)
    (append  (if (and (not (and (equalp parenti (1- i)) (equalp parentj (1- j)))) (not (and (equalp GLOBALI (1- i)) (equalp GLOBALJ (1- j)))) (checkCoords (1- i) (1- j)))  (vratiStanjeNaKoordinati2  (1- i) (1- j) tabla))
               (if (and (not (and (equalp parenti (1- i)) (equalp parentj (1+ j)))) (not (and (equalp GLOBALI (1- i)) (equalp GLOBALJ (1+ j))))  (checkCoords (1- i) (1+ j)))  (vratiStanjeNaKoordinati2  (1- i) (1+ j) tabla))
               (if (and (not (and (equalp parenti (1+ i)) (equalp parentj (1- j)))) (not (and (equalp GLOBALI (1+ i)) (equalp GLOBALJ (1- j)))) (checkCoords (1+ i) (1- j)))  (vratiStanjeNaKoordinati2  (1+ i) (1- j) tabla))
               (if (and (not (and (equalp parenti (1+ i)) (equalp parentj (1+ j)))) (not (and (equalp GLOBALI (1+ i)) (equalp GLOBALJ (1+ j)))) (checkCoords (1+ i) (1+ j)))  (vratiStanjeNaKoordinati2  (1+ i) (1+ j) tabla))
        )
)

(defun checkCoords (i j)
    (if (null i) '())
    (if (null j) '())
    (if (not (or (<= i 0) (<= j 0) (>= i (1+ n)) (>= j (1+ n)) ))
    t nil
)
)

(defun vratiStanjeNaKoordinati2 (i j tabla)
    (setq position (mapiranjeKoordinata i j))
    (if (null position) '()
            (cond 
                ((< position 0) '())
                ((< (* n n) position ) '())
                (t 
                    (if (not (null (nth 1 (nth position tabla) ))) (list ( list i j (nth 1 (nth position tabla)) )) )
                )
        )
    )
) 

(defun validMoves (is js neighbors)
    (if (null neighbors) '()
        (append (moveCoords is js (car (car neighbors)) (cadr (car neighbors))) (validMoves is js (cdr neighbors)) )
    )
)

(defun moveCoords (is js id jd) 
    (let* ((i (if (< is id) (1+ is) (if (> is id) (1- is) '() ) ))
        (j (if (< js jd ) (1+ js) (if (> js jd) (1- js) '() ))))
        (cond 
            ((null i) (if (null j) (list '())   (list (if (checkCoords (1+ is) j ) (list (1+ is) j)) (if (checkCoords (1- is) j )(list (1- is) j)) ))) 
            ((null j) (if (null i) (list '())   (list (if (checkCoords  i (1+ js)) (list i (1+ js))) (if (checkCoords  i (1- js)) (list i (1- js)) ) ))) 
            (t (if (checkCoords i j) (list (list i j)) '()))
        )
    )   
)

(defun Potez2 (ip jp ik jk brojPlocica tabla)
    (setq igracevaLista (NapraviListuOdBrojaPlocica brojPlocica ip jp tabla))

    (if  (not (and  (validnoPolje ip jp ik jk) (daLiJeMojStek igracevaLista na_potezu) (IsRightStek ip jp brojPlocica tabla) (ivicep ik jk) ))
        (progn 
        (format t "~C Uneli ste nevalidan potez za igraca " #\return)
        (printMove na_potezu ip jp ik jk brojPlocica)
        '()
        )
        (progn    
            (cond
                 ((and (not (DaLiJePraznoPolje ik jk tabla)) (or (not (validnaVisinaZaSpajanje ip jp ik jk brojPlocica tabla))  (not (RezultujuciStek8 ik jk igracevaLista tabla)) ))
                            (progn    
                                    (format t "~C Uneli ste nevalidan potez za igraca ~%" #\return)
                                    (printMove na_potezu ip jp ik jk brojPlocica)
                                '()
                            )          
                    )
                ((and (DaLiJePraznoPolje ik jk tabla) (or (not (MogucePremestanjeCelogSteka ip jp ik jk brojPlocica tabla)) (not (moveValidp ip jp ik jk tabla)))) 
                        (progn 
                                    (format t "~C Uneli ste nevalidan potez  ~%" #\return)
                                    (printMove na_potezu ip jp ik jk brojPlocica)
                            '()
                        )
                    )
            (t (progn
                   (format t "~%")
                   (princ "Validan potez za igraca ")
                   (printMove na_potezu ip jp ik jk brojPlocica)               
                   (appendListaZaKoordinatuSaKoordinate ip jp ik jk igracevaLista tabla)
                   (if (equalp (length (vratiStanjeNaKoordinati ik jk tabla)) 8) 
                    (removeStack ik jk (vratiStanjeNaKoordinati ik jk tabla) tabla))
                   t
            ))
            )
        )
    )
)

(defun printMove (napotezu ip jp ik jk brojPlocica)
    (write na_potezu)
    (princ " sa koordinata ")
    (write (list ip jp))
    (princ " na koordinate ")
    (write (list ik jk))
    (princ " za broj plocica: ")
    (write brojPlocica)
    (format t "~%")
)

(defun PotezNaNovojTabli (ip jp ik jk brojPlocica tabla igracNaPotezu)
    (setq igracevaLista (NapraviListuOdBrojaPlocica brojPlocica ip jp tabla))
    (if igracNaPotezu (setq koIgra 'X) (setq koIgra 'O))
    (if  (not (and (validnoPolje ip jp ik jk) (daLiJeMojStek igracevaLista koIgra) (IsRightStek ip jp brojPlocica tabla) (ivicep ik jk) ))
        '()
    ;else --> validan za sada
        (progn
            (cond
                   ((and (not (DaLiJePraznoPolje ik jk tabla)) (or (not (validnaVisinaZaSpajanje ip jp ik jk brojPlocica tabla))  (not (RezultujuciStek8 ik jk igracevaLista tabla)) ))
                    ;znaci hoce da spoji stekove, proveravamo validnu visinu i visinu rezultujuceg steka
                            '()   
                    )
                ((and (DaLiJePraznoPolje ik jk tabla) (or (not (MogucePremestanjeCelogSteka ip jp ik jk brojPlocica tabla)) (not (moveValidp ip jp ik jk tabla)))) 
                ;znaci da hoce premestanje celog steka na prazno polje
                            '()
                    )
            (t (progn
                 (if igracNaPotezu (setq koIgra 'O) (setq koIgra 'X))
                 
            (let ((novatabla (kopirajTablu tabla))) 
                (appendListaZaKoordinatuSaKoordinate ip jp ik jk igracevaLista novatabla)
                novatabla
            )
             
            )
            )
        )
    )
 )
 )
 
 (defun ValidanPotezNaNovojTabli (ip jp ik jk brojPlocica tabla)
    (setq igracevaLista (NapraviListuOdBrojaPlocica brojPlocica ip jp tabla))
    (let ((novatabla (kopirajTablu tabla))) 
                (appendListaZaKoordinatuSaKoordinate ip jp ik jk igracevaLista novatabla)
                novatabla
    )            
)

(defun proveriKordinatuBroj(koor)
    (if (null koor) '()
                    (if (and (> koor 0) (<= koor n)) t '())
    )
)

(defun proveriKordinatuSlovo (slovo)
    (cond
        ((null (vrati_broj slovo)) '())
        ((> (vrati_broj slovo) n) '() )
        (t t)
    )
)

(defun proveriUnos (ipocetno jpocetno ikrajnje jkrajnje brPlocicaa)
    (cond
        ((null (proveriKordinatuSlovo ipocetno)) (progn (format t "~C Uneto neispravno slovo pocetne vrste ~%" #\return ) '()))
        ((null (proveriKordinatuBroj jpocetno)) (progn (format t "~C Unet neispravan broj pocetne kolone ~%" #\return ) '())) 
        ((null (proveriKordinatuSlovo ikrajnje)) (progn (format t "~C Uneto neispravno slovo krajnje vrste ~%" #\return ) '()))
        ((null (proveriKordinatuBroj jkrajnje)) (progn (format t "~C Unet neispravan broj krajnje kolone ~%" #\return ) '()))
        ((or (not (numberp brPlocicaa)) (> 0 brPlocicaa) (< 8 brPlocicaa)) (progn (format t "~C Unet nevalidan broj plocica ~%" #\return ) '()))
        (t t)
    )
)

(defun UnesiPotez()
    
    (let* (
        (player (if (equalp na_potezu 'x) t '() ))
        (potezi (GenerateListSvihPoteza 1 1 TRENUTNATABLA player))

         )
         (if (null potezi) (promena) 
    
                            (let*
                                (
                                    (ipocetno (progn (format t "~C Unesite slovo pocetne vrste: ~%" #\return )  (read)) )
                                    (jpocetno (progn (format t "~C Unesite broj pocetne kolone: ~%" #\return )  (read)))
                                    (ikrajnje (progn (format t "~C Unesite slovo krajnje vrste: ~%" #\return )  (read)) )
                                    (jkrajnje (progn (format t "~C Unesite broj krajnje kolone: ~%" #\return )  (read)))
                                    (brPlocicaa (progn (format t "~C  Unesite broj plocica: ~%" #\return )  (read)))
                                )
                                (if (null (proveriUnos ipocetno jpocetno ikrajnje jkrajnje brPlocicaa))
                                    (progn (format t "~C Unesi potez: Uneli ste nevalidan potez ! ~%" #\return ) (UnesiPotez))
                                    (if (null (Potez2 (vrati_broj ipocetno) jpocetno (vrati_broj ikrajnje) jkrajnje brPlocicaa TRENUTNATABLA)) (UnesiPotez))

                                )
                            )))
)

(defun UnosiPoteze()
     (if (checkScore(ceiling (/ (* (pocetniBrojFiguraPoIgracu n) 2) 8) 2)) (progn (princ "Igra gotova") (print scoreList) (print "gotovo"))
     (progn 
     (format t "~%")
     (princ "Na potezu je")
     (format t " ~a " na_potezu)
      (format t "~%") 
      (if (equalp na_potezu racunar)
            (igraRobot)
            (igraRobot)
      )
     (format t "~%") 
     (stampajBrojeve 1) 
     (stampajCeluTablu 1 1 TRENUTNATABLA)
     (if (equalp na_potezu 'X) (setq na_potezu 'O) (setq na_potezu 'X))
     (UnosiPoteze)
     ))
)

(defun promena()
     (princ "Nema mogucih poteza za igraca: " )
     (write na_potezu)
     (if (equalp na_potezu 'X) (setq na_potezu 'O) (setq na_potezu 'X))
     (UnosiPoteze)
)

(defun igraRobot()

    (let* ( 
            (player (if(equalp na_potezu 'x) t '()) )
            (move (car (minimax TRENUTNATABLA NIL -10000 10000 depth player '())))
          )
            (if (null move) (promena) 
            (progn 
            (Potez2 (caar move) (cadar move) (caadr move) (cadadr move) (caddr move) TRENUTNATABLA)
            (print move)))
    )
)

(defun Init()
      (dimenzije)
      (initTablaPocetakIgre)
      (setq na_potezu 'X)
      (igra_prvi)
      (UnosiPoteze)
      (print "game over")
)

(defun kopirajTablu (tabla)
       (if (null tabla) '()  (cons (copy-list (car tabla)) (kopirajTablu (cdr tabla))))
)

(defun ivicep (i j)
    (COND 
        ((< i 1) '())
        ((> i n) '())
        ((< j 1) '())
        ((> j n) '())
        (t t)
    )
) 

(defun potezp (ip jp ik jk brojPlocica tabla igracNaPotezu)
    (setq igracevaLista (NapraviListuOdBrojaPlocica brojPlocica ip jp tabla))
    (if igracNaPotezu (setq koIgra1 'X) (setq koIgra1 'O))
    (if  (not (and (validnoPolje ip jp ik jk) (daLiJeMojStek igracevaLista koIgra1) (IsRightStek ip jp brojPlocica tabla) (ivicep ik jk)))
       
        '()
        
        (progn
            (cond
                   ((and (not (DaLiJePraznoPolje ik jk tabla)) (or (not (validnaVisinaZaSpajanje ip jp ik jk brojPlocica tabla))  (not (RezultujuciStek8 ik jk igracevaLista tabla)) ))       
                                '()        
                   )
                ((and (DaLiJePraznoPolje ik jk tabla) (or (not (MogucePremestanjeCelogSteka ip jp ik jk brojPlocica tabla)) (not (moveValidp ip jp ik jk tabla)))) 
                            '()
                    )
            (t  t)
            )))
)

(defun GenerateList2 (i j tabla)
  (cond ((> i n) '()) 
       ((AND (= (mod i 2) (mod j 2)))
     (append (list (list i j) (MoguceSituacijeNaKoord i j (vratiStanjeNaKoordinati i j tabla) tabla))  (if (equalp j n) (GenerateList2 (+ i 1) 1 tabla) (GenerateList2 i (+ j 1) tabla)))
       )
       (t 
           (if (equalp j n) (GenerateList2 (+ i 1) 1 tabla) (GenerateList2 i (+ j 1) tabla)) 
       )
  )
)

(defun GenerateListStanja (i j tabla)
  (cond ((> i n) '())
       
       ((AND (= (mod i 2) (mod j 2)))
     (append  (ListaStanjaZaKoordRazlicitBrPlocica i j (vratiStanjeNaKoordinati i j tabla) tabla) (if (equalp j n) (GenerateListStanja (+ i 1) 1 tabla) (GenerateListStanja i (+ j 1) tabla)))
       )
       (t 
           (if (equalp j n) (GenerateListStanja (+ i 1) 1 tabla) (GenerateListStanja i (+ j 1) tabla)) 
       )
  )
)

(defun MoguceSituacijeNaKoord (ip jp lista tabla)
       (if (null lista) '() (append (listaPoteza ip jp (length lista) tabla na_potezu) (MoguceSituacijeNaKoord ip jp (cdr lista) tabla)))
)

(defun ListaStanjaZaKoordRazlicitBrPlocica (ip jp lista tabla)
       (if (null lista) '() (append (listaStanja ip jp (length lista) tabla) (ListaStanjaZaKoordRazlicitBrPlocica ip jp (cdr lista) tabla)))
)

(defun listaPoteza (ip jp brPl tabla igracNaPotezu)
    (append 
            (if (potezp ip jp (1- ip) (1- jp) brPl tabla igracNaPotezu) (list (list (LIST IP jp) (list (1- ip) (1- jp)) brPl ) ))
            (if (potezp ip jp (1- ip) (1+ jp) brPl tabla igracNaPotezu)  (list (list (LIST IP jp) (list (1- ip) (1+ jp)) brPl) ))
            (if (potezp ip jp (1+ ip) (1- jp) brPl tabla igracNaPotezu)  (list (list (LIST IP jp) (list (1+ ip) (1- jp)) brPl) ))
            (if (potezp ip jp (1+ ip) (1+ jp) brPl tabla igracNaPotezu)  (list (list (LIST IP jp) (list (1+ ip) (1+ jp)) brPl) ))
      ) 
)

(defun listaStanja (ip jp brPl tabla)
     (append  
            (if (potezp ip jp (1- ip) (1- jp) brPl tabla)  (list (ValidanPotezNaNovojTabli ip jp (1- ip) (1- jp) brPl tabla) ) )
            (if (potezp ip jp (1- ip) (1+ jp) brPl tabla)  (list (ValidanPotezNaNovojTabli ip jp (1- ip) (1+ jp) brPl tabla) ) ) 
            (if (potezp ip jp (1+ ip) (1- jp) brPl tabla)  (list (ValidanPotezNaNovojTabli ip jp (1+ ip) (1- jp) brPl tabla) ) )
            (if (potezp ip jp (1+ ip) (1+ jp) brPl tabla)  (list (ValidanPotezNaNovojTabli ip jp (1+ ip) (1+ jp) brPl tabla) ) )
    ) 
)

(defun GenerateListSvihPoteza (i j tabla igracNaPotezu)
  (cond ((> i n) '())
       
       ((AND (= (mod i 2) (mod j 2)))
     (append  (ListaPotezaZaKoordRazlicitBrPlocica i j (vratiStanjeNaKoordinati i j tabla) tabla igracNaPotezu) (if (equalp j n) (GenerateListSvihPoteza (+ i 1) 1 tabla igracNaPotezu) (GenerateListSvihPoteza i (+ j 1) tabla igracNaPotezu)))
       )
       (t 
           (if (equalp j n) (GenerateListSvihPoteza (+ i 1) 1 tabla igracNaPotezu) (GenerateListSvihPoteza i (+ j 1) tabla igracNaPotezu)) 
       )
  )
)

(defun ListaPotezaZaKoordRazlicitBrPlocica (ip jp lista tabla igracNaPotezu)
       (if (null lista) '() (append (listaPoteza ip jp (length lista) tabla igracNaPotezu) (ListaPotezaZaKoordRazlicitBrPlocica ip jp (cdr lista) tabla igracNaPotezu)))
)

(SETQ depth 3)

(defun minimax (tabla move alpha beta currentDepth isMyMove fullStackFlag)
    (cond
        ((zerop currentDepth) (list move (heuristics isMyMove tabla) ))
        (t
            (let*
                (
                    (evaluateState (heuristics isMyMove tabla))
                    (tabla (if (car fullStackFlag) 
                                (removeFullStack (caadr fullStackFlag) (cadadr fullStackFlag) tabla) 
                                tabla
                            )
                    )
                    (newMovesList (GenerateListSvihPoteza 1 1 tabla isMyMove))
                    (result
                        (if isMyMove
                            (maxPlay newMovesList '() currentDepth alpha beta isMyMove tabla )
                            (minPlay newMovesList '() currentDepth alpha beta isMyMove tabla )
                        ) 
                    )
                )
                (cond
                    ((null newMovesList) (list  move evaluateState ))
                    ((equalp currentDepth depth)(list (car result) (+ evaluateState (cadr result)))) 
                    (t (list move (+ evaluateState (cadr result))))
                 
                )
            )
        )       
    )
)
(defun maxPlay (movesList bestMove depth alpha beta isMyMove prethodnaTabla)
     
    (cond 
       
        ((null movesList) (list bestMove alpha))
        (t 
            (let*
                 (                
                    (previousMoveState (PotezNaNovojTabli (caaar movesList) (cadaar movesList) (caadar movesList)  (car (cdadar movesList)) (caddar movesList) prethodnaTabla isMyMove))
                    (fullStackFlag (isThereFullStack (caadar movesList) (car (cdadar movesList)) previousMoveState) )
                    (minMove (minimax previousMoveState (car movesList)  alpha beta (1- depth) (not isMyMove) (list fullStackFlag (list  (caadar movesList) (car (cdadar movesList))) )))
                   
                    (newMove  (if (>= alpha (cadr minMove)) (list bestMove alpha) minMove))
                )
                (if  (or (<= beta (cadr newMove)) (null movesList))                
                       (list  bestMove (cadr newMove) )
                       (maxPlay (cdr movesList) (car newMove) depth (cadr newMove) beta isMyMove prethodnaTabla)
                
            )
        )
    )
)
)

(defun minPlay (movesList bestMove depth alpha beta isMyMove prethodnaTabla)
    (cond 
        ((null movesList) (list bestMove beta))
        (t 
            (let*
               (    
                    (previousMoveState (PotezNaNovojTabli (caaar movesList) (cadaar movesList) (caadar movesList)  (car (cdadar movesList)) (caddar movesList) prethodnaTabla isMyMove))          
                    (fullStackFlag (isThereFullStack (caadar movesList) (car (cdadar movesList)) previousMoveState) )

                    (maxMove (minimax previousMoveState (car movesList) alpha beta (1- depth) (not isMyMove) (list fullStackFlag (list  (caadar movesList) (car (cdadar movesList)) )) ))
                   
                    (newMove (if (<= beta (cadr maxMove)) (list bestMove beta) maxMove))
                )
                (if (or (<=  (cadr newMove) alpha )(null  movesList))
                        (list bestMove  (cadr newMove))
                        (minPlay (cdr movesList) (car newMove) depth alpha (cadr newMove) isMyMove prethodnaTabla)
                )   
        )
    )
)
)

(defun evaluate (move  tabla)
 (cond
        
        (t (+ 10 (random 71)) ))
)

(defun bottomChecker (stek)
       (nth 0 stek)
)

(defun !mojTop (stek igrac)
    (if (null stek)
        '()
        (if (equalp (nth 7 stek) igrac) t '())
    )
)

(defun =getHeuristicsState ()
    heuristicsState
)

(defun stekoviKontrolise (tabla)
    (cond ((null tabla) '())
       ((null (nth 1 (car tabla))) (stekoviKontrolise (cdr tabla)))
       (t (cons (list 'kontrolise (bottomChecker (nth 1 (car tabla))) (caaar tabla) (nth 1 (caar tabla))) (stekoviKontrolise (cdr tabla)))))
)

(defun =isThereStackFullNovo (tabla)
       (cond 
        ((null tabla) '())
       ((equalp (length (nth 1 (car tabla))) 8)
       (append (nth 1 (car tabla)) (=isThereStackFullNovo (cdr tabla))))
       (t (=isThereStackFullNovo (cdr tabla)))
       )
)

(defun usamljeniSstekovi (listaPolja tabla )
    (cond
        ((null listaPolja) '())
        ((null (nth 1 (car listaPolja))) (usamljeniSstekovi (cdr listaPolja) tabla))
        ((GetSvaValidnaPolja (caaar listaPolja) (nth 1 (caar listaPolja)) tabla) (cons (list 'usamljenStek (bottomChecker (nth 1 (car listaPolja)))) (usamljeniSstekovi (cdr listaPolja) tabla)))   
        (t ( usamljeniSstekovi (cdr listaPolja) tabla ))
    )
)
(defun equal* (&rest arguments)
  (or (endp arguments)
      (let ((x (first arguments)))
        (every (lambda (y)
                 (equal x y))
               (rest arguments))))
)

(defun triIstePlocice (stek igrac)
    (cond
        ((null stek) '())
        ((>= (length stek) 3) (if (equal* (car stek) (cadr stek) (caddr stek) igrac) t (triIstePlocice (cdr stek) igrac) ))
        (t (triIstePlocice (cdr stek) igrac))
    )
)

(defun triPlocice (stekovi igrac)
    (cond
        ((null stekovi) '())
        ((<= (length (car stekovi)) 3)  (triPlocice (cdr stekovi) igrac) )
        ((triIstePlocice (car stekovi) igrac)  (cons (list 'trizaredom igrac) (triPlocice (cdr stekovi) igrac) ))
        (t (triPlocice (cdr stekovi) igrac))
    )
)

(defun svistekovi (tabla)
    (cond 
        ((null tabla) '())
        ((not (null (nth 1 (car tabla)))) (append (list (nth 1 (car tabla))) (svistekovi (cdr tabla))))
        (t (svistekovi (cdr tabla)))
    )
)

(defparameter *T1-RULES* 
    '(
        (if (stekod8 ?a) then (fullstack ?a))
        (if (usamljenStek x) then (lonelyStackx x))
        (if (usamljenStek o) then (lonelyStacko o))
        (if (and (kontrolise x ?a ?b) (!jedanKomsa ?a ?b (=getHeuristicsState))) then (verygoodx a b ) )
        (if (and (kontrolise o ?a ?b) (!jedanKomsa ?a ?b (=getHeuristicsState))) then (verygoodo a b ) )
        (if (and (kontrolise x ?a ?b) (!viseKomsaod1 ?a ?b (=getHeuristicsState))) then (goodx a b ) )
        (if (and (kontrolise o ?a ?b) (!viseKomsaod1 ?a ?b (=getHeuristicsState))) then (goodo a b ) )
        (if (trizaredom x) then (treeAdjx  x))
        (if (trizaredom o) then (treeAdjo o))
    )
)

(defun genFacts(tabla)
    (append 
        (if (!mojTop (=isThereStackFullNovo tabla ) 'X) (list '(stekod8 X)) '()) 
        (if (!mojTop (=isThereStackFullNovo tabla ) 'O) (list '(stekod8 o)) '())
        (usamljeniSstekovi tabla tabla)
        (stekoviKontrolise tabla)
        (triPlocice (svistekovi tabla) 'x)
        (triPlocice (svistekovi tabla) 'o)
    )
)

(defun heuristics (isMyMove tabla)
    (let* 
        (
            (*T1-FACTS* (genFacts tabla))  
        )  
        (progn
            (setq heuristicsState tabla)
            (prepare-knowledge *T1-RULES* *T1-FACTS* 10)
            
            (let*
                (
                    (acc1x (if (/= (count-results '(fullstack x)) 0) 100 '0))
                    (acc1o (if (/= (count-results '(fullstack o)) 0) -100 '0))                   
                    (acc2x (* (count-results '(lonelyStackx ?x)) 15))                   
                    (acc2o (* (count-results '(lonelyStacko ?o)) -15))
                    (acc3x (* (count-results '(verygoodx ?a ?b)) 5 ))                   
                    (acc3o (* (count-results '(verygoodo ?a ?b)) -5 ))
                    (acc4x (* (count-results '(goodx ?a ?b)) 2 ))                   
                    (acc4o (* (count-results '(goodo ?a ?b)) -2 ))                  
                    (acc5x (* (count-results '(treeAdjx ?a)) 20 ))                  
                    (acc5o (* (count-results '(treeAdjo ?a)) -20 ))
                    (acc (+ acc2x acc2o acc1x acc1o acc3x acc3o acc4x acc4o acc5x acc5o 10 ))  
                )         
             acc
            )
        )
    )
)
;(trace heuristics)
;; (setListaZaKoordinatu 3 3 '(O o o x x x) TRENUTNATABLA)
;; (setListaZaKoordinatu 3 5 '(x x o o o o) TRENUTNATABLA)
;; (setListaZaKoordinatu 4 4 '(x x ) TRENUTNATABLA)
;; (stampajCeluTablu 1 1 TRENUTNATABLA)
;; ;print (heuristics t TRENUTNATABLA))
;; (print (minimax TRENUTNATABLA '(1 1) -80 80 3 t '()))
;(trace minimax)
;;  (trace potez2)

;; (setq n 8)
;; (initTablaPocetakIgre)
;;  (print TRENUTNATABLA)
;; (format t "~%")
;; (setq na_potezu 'X)

;;  (setListaZaKoordinatu 2 2 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 2 4 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 2 6 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 2 8 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 3 1 '(o x o x) TRENUTNATABLA)
;; (setListaZaKoordinatu 3 3 '(x o x o x x) TRENUTNATABLA)
;;  (setListaZaKoordinatu 3 5 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 3 7 '(o x x) TRENUTNATABLA)
;;  (setListaZaKoordinatu 4 2 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 4 4 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 4 6 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 4 8 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 5 1 '(x o x o o o x) TRENUTNATABLA)
;;   (setListaZaKoordinatu 5 3 '() TRENUTNATABLA)
;;    (setListaZaKoordinatu 5 5 '() TRENUTNATABLA)
;;       (setListaZaKoordinatu 5 7 '() TRENUTNATABLA)

;; (setListaZaKoordinatu 6 2 '() TRENUTNATABLA) 
;; (setListaZaKoordinatu 6 4 '() TRENUTNATABLA)
;; (setListaZaKoordinatu 6 6 '(x o o o) TRENUTNATABLA)
;; (setListaZaKoordinatu 6 8 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 7 1 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 7 3 '() TRENUTNATABLA)
;;   (setListaZaKoordinatu 7 5 '() TRENUTNATABLA)
;;  (setListaZaKoordinatu 7 7 '() TRENUTNATABLA)
;; ;; ;;   (trace minimax)
;; ;; ;;  (trace maxPlay)
;; ;; ;;  (trace minPlay)
;; ;; ;;  (trace heuristics)
 
;; ;; ;; (trace !mojTop)
;; ;; ;; (trace usamljeniSstekovi)
;; ;; ;; (trace =isThereStackFullNovo)
;; ;; ;; (trace stekoviKontrolise)
;; ;; ;; (trace triPlocice)
;; ;; ;; (trace GetSvaValidnaPolja)
;; (stampajCeluTablu 1 1 TRENUTNATABLA)
;; ;; ;; (trace DaLiJePraznoPolje)
;; ;;  ;(minimax TRENUTNATABLA '() -8000 8000 3 '() '())
;; ;; ;;  (setq scoreList '((x 1) (o 1)))
;; ;;  (trace potez2)
;; ;;  (trace MogucePremestanjeCelogSteka)
;; ;;  (trace moveValidp)
;; ;;  (trace ivicep)
;; ;;  (trace DaLiJePraznoPolje)
;; ;; ;; (setq na_potezu 'o)
;; ;; ;; (setq racunar 'x)
;; ;; ;; (UnesiPotez)

;;  (Potez2 6 6 5 5 4 TRENUTNATABLA)

 ;(print (vrati_broj 'WW))
 
;;  (trace validnoPolje)
;; (trace daLiJeMojStek)
;; (trace IsRightStek)
;; (trace moveValidp)
;; (trace GetSvaValidnaPolja)
;; (trace MogucePremestanjeCelogSteka)

;(potez2 4 8 3 7 2 TRENUTNATABLA)
(Init)
 ;(print (pocetniBrojFiguraPoIgracu 2))
;(potez2 6 8 7 7 1 TRENUTNATABLA)