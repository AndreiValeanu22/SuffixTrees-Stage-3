#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix w1 w2)
  (common-prefix-h w1 w2 null)
  )

(define (common-prefix-h w1 w2 L)
  (cond ((null? w1) (if (null? w2)
                        (cons L (cons '() (cons '() '())))
                        (cons L (cons '() (cons w2 '())))))
        (else (if (null? w2)
                  (append (list L) (list w1) (list '()))
                  (if (equal? (car w1) (car w2))
                      (common-prefix-h (cdr w1) (cdr w2) (append L (list (car w1))))
                      (cons L (cons w1 (cons w2 '()))))))))


; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.



(define (longest-common-prefix-of-list words)
  
      
  ;(shortest-word words null)
  (common-prefix-helper words (shortest-word words null) null (shortest-word words null) null)
  )

(define (shortest-word words wshort)
  (if (null? words)
      wshort
      (if (null? wshort)
          (shortest-word (cdr words) (car words))
          (if (< (length (car words)) (length wshort))
              (shortest-word (cdr words) (car words))
              (shortest-word (cdr words) wshort)))))

(define (common-prefix-helper words w1 w2 wshort prefix)
  (if (null? words)
      (if (null? prefix)
          (car (common-prefix-h w1 w2 null))
          (car (common-prefix-h w2 prefix null)))
      (if (null? w1)
          (common-prefix-helper (cdr words) prefix (car words) wshort null)
          (if (null? w2)
              (common-prefix-helper (cdr words) wshort (car words) wshort prefix)
              (if (equal? (car w1) (car w2))
                  (common-prefix-helper words (cdr w1) (cdr w2) wshort (append prefix (list (car w1))))
                  (common-prefix-helper (cdr words) prefix (car words) prefix null))))))



;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.

(define (match-pattern-with-label st pattern)
  (if (null? st)
      (append (list #f) (list '()))
     (let* (
            [ramura (car st)]
            [eticheta (car (car st))]
            [subarbore_eticheta (cdr (car st))]
            )
       (if (equal? eticheta pattern)
           #t
           (if (equal? (egalitate_eticheta_sablon eticheta pattern null) #t)
               #t
               (if (null? (egalitate_eticheta_sablon eticheta pattern null))
                   (match-pattern-with-label (cdr st) pattern)
                   (if (equal? (car (egalitate_eticheta_sablon eticheta pattern null)) '(5))
                       (append (list eticheta) (list (car (cdr (cdr (egalitate_eticheta_sablon eticheta pattern null))))) (list subarbore_eticheta))
                       (if  (not (equal? (car (egalitate_eticheta_sablon eticheta pattern null)) '(5)))
                               
                          (append (list #f) (list (egalitate_eticheta_sablon eticheta pattern null)))
                           
                           (match-pattern-with-label (cdr st) pattern)))))))))



(define (egalitate_eticheta_sablon eticheta sablon copie)
  (if (and (null? eticheta) (not (null? sablon))) ;daca sunt pe cazul in care trebuie eticheta e mai mica decat sablonul
      (append (list '(5)) (list copie) (list sablon))
     
      (if (and (null? sablon) (not (null? eticheta)))  ;daca sablonul este cuprin total in prefixul etichetei
          #t
          (if (equal? (car eticheta) (car sablon))
              (egalitate_eticheta_sablon (cdr eticheta) (cdr sablon) (append copie (list (car eticheta))))
              copie))))




; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.


(define (st-has-pattern? st pattern)
  (if (null? st)
      #f
    (let* ([ramura (car st)]
            [eticheta (car (car st))]
            [subarbore_eticheta (cdr (car st))])
       (if (equal? eticheta pattern)
           #t
           (if (null? (verificare_interior eticheta pattern pattern null))
               (st-has-pattern? (cdr st) pattern)
               (if (equal? (verificare_interior eticheta pattern pattern null) pattern)
                   #t
                   (st-has-pattern? (cdr st) pattern)))))))
                   


(define (verificare_interior eticheta sablon copie-sablon copie)
    (if (or (null? eticheta) (null? sablon))
        copie
        (if (equal? (car eticheta) (car sablon))
            (verificare_interior (cdr eticheta) (cdr sablon) copie-sablon (append copie (list (car eticheta))))
            (verificare_interior (cdr eticheta) copie-sablon copie-sablon null))))






