#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (st-has-pattern? ((text->st  cst-func) text) pattern))

; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)

  (let sufixe ([arbore ((text->st  cst-func) text1)]
               [sufix (get-suffixes text2)]
               [potriviri '()])
    (if (null? sufix)
        (let cel-mai-lung ([potriviri potriviri]
                           [cuvant  (car potriviri)])
          (if (null? potriviri)
              cuvant
              (if (> (length (car potriviri)) (length cuvant))
                     (cel-mai-lung (cdr  potriviri) (car potriviri))
                     (cel-mai-lung (cdr  potriviri) cuvant))))
        (sufixe arbore (cdr sufix) (append potriviri (list (gaseste-sufix  arbore (car sufix))))))))
        


(define (gaseste-sufix  arbore sufix) ;pentru fiecare sufix caut cea mai lunga potrivire
  (let* ([rezultat (match-pattern-with-label arbore sufix)])
    (if (equal? #t rezultat)
        sufix
        (let* ([continut-list1 (car rezultat)]
                   [list2 (cdr rezultat)]
                   [continut-list2 (car list2)])
          (if (equal? #f continut-list1)
              continut-list2
              (append continut-list1 (gaseste-sufix  (car (cdr list2)) continut-list2)))))))

                                                 


; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.



(define (dfs arbore ramura subarbore parinte len sufix)
  (cond
   
    ((and (st-empty? arbore) (st-empty? ramura))'())
    ((st-empty? (get-branch-subtree (first-branch arbore))) (dfs (other-branches arbore) null null null len null ))
    ((st-empty? ramura) (dfs (get-branch-subtree arbore) (first-branch  arbore)
                             (append (list (get-branch-label (first-branch  arbore )))
                                     (other-branches (other-branches (first-branch  arbore ))))
                             null len null ))
    ((st-empty? (cdr ramura)) '()) 
    (else
     (let* ([informatie (get-branch-label ramura)]
           [sub-informatie (get-branch-subtree ramura)]
           [cuvant (append parinte informatie)])
       (let verifica ([sub-informatie sub-informatie]
                  [rezultat sufix])
         (cond
           ((>= (length cuvant) len) cuvant)
           ((not (null? rezultat)) rezultat)
           ((and (st-empty? sub-informatie) (not (null? (cdr subarbore))))
            (dfs arbore subarbore (append (list (get-branch-label subarbore))
                                          (other-branches (other-branches subarbore))) null len  null))
           ((st-empty? sub-informatie) rezultat)
           (else (verifica (other-branches sub-informatie) (dfs arbore (first-branch sub-informatie) subarbore cuvant len null )))))))))


          
               

(define (repeated-substring-of-given-length text len)
  (let* ([arbore ((text->st  cst-func) text)]
         [sufix (dfs arbore null null null len null)])
    
    (if (null? sufix)
        #f
        (if (equal? (length sufix) len)
            sufix
            (let ia-suf ([suf sufix] [len len] [cuv '()])
              (if (= 0 len)
                  cuv
                  (ia-suf (cdr suf) (- len 1) (append cuv (list (car suf))))))))))

 
    
 