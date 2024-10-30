#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-help w1 w2 listCom)
  (let* ([afisare  (list listCom  w1  w2)])
  (if (or (null? w1) (null? w2))
     afisare
     (if (equal? (car w1) (car w2))
         (longest-common-help (cdr w1) (cdr w2) (append listCom (list (car w1))))
         afisare))))


  (define (longest-common-prefix w1 w2)
    (longest-common-help w1 w2 null))



; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection


;; (define (cel-mai-scurt cuvant flux)
;;   (if (collection-empty?  flux)
;;       cuvant
;;       (if (> (length cuvant) (length (collection-firs flux)))
;;           (cel-mai-scurt (collection-firs flux) (collection-rest flux))
;;           (cel-mai-scurt cuvant (collection-rest flux)))))
  
;//functie
(define (longest-common-prefix-of-collection-help cuvant flux-cuvinte)
  (if (collection-empty? flux-cuvinte)
      cuvant
  (let* ([sufix (car (longest-common-prefix cuvant (collection-firs flux-cuvinte)))])
  (if (> (length cuvant) (length sufix))
      (longest-common-prefix-of-collection-help sufix (collection-rest flux-cuvinte))
      (longest-common-prefix-of-collection-help cuvant (collection-rest flux-cuvinte))))))
         


(define (longest-common-prefix-of-collection words)
   (longest-common-prefix-of-collection-help (collection-firs words) (collection-rest words)))






;//functie
(define (match-pattern-with-label st pattern)
      (let ([ST (get-ch-branch st (car pattern))])
        (if (equal? ST #f)
            (append (list #f)  (list '()))
            (let ([eticheta (get-branch-label ST)])
            (if (equal? eticheta  pattern)
                #t
                (let ([pref-lung (longest-common-prefix eticheta  pattern)])
                  (if (not (null? (car (cdr pref-lung))))
                      (append (list #f) (list (car pref-lung)))
                      (append (list eticheta ) (list (car (cdr (cdr pref-lung)))) (list (get-branch-subtree ST))))))))))





;//functie
(define (st-has-pattern? st pattern)
 
  (let ([afisare (match-pattern-with-label st pattern)])
    (if (equal? afisare #t)
        #t
        (if (equal? (car afisare) #f)
            #f
          (st-has-pattern? (cdr (cdr afisare)) (car (cdr afisare)))))))




;//functie
(define (get-suffixes text)
  (if (collection-empty? text)
    empty-collection
       (collection-cons  text (get-suffixes (collection-rest text)))))




;//functie
(define (get-ch-words words ch)
  (collection-filter (λ (cuvinte)
         (if (null? cuvinte)
             #f
             (if (equal? (car cuvinte) ch)
                #t
                 #f))) words))




;//functie
 (define (ast-func suffixes)
    (let ([prima-litera   (car (collection-firs suffixes))])
  (cons   (list prima-litera) (collection-map (λ (cuvant) (car (cdr (longest-common-prefix cuvant (list prima-litera)))))  suffixes))))





(define (cst-func suffixes)
 (let* ([sufix_lung (longest-common-prefix-of-collection suffixes)])
   (cons sufix_lung (collection-map (λ (cuvant) (car (cdr (longest-common-prefix cuvant sufix_lung))))  suffixes))))
    


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)


(define (suffixes->st labeling-func suffixes alphabet)

  (collection-map (λ (vector)
                      (if (null? (cdr vector))
                          vector
                         (cons  (car vector) 
                               (suffixes->st labeling-func (cdr vector) alphabet ))))

                  (collection-map  labeling-func
                                   (collection-filter (λ (lista)
                                                        (if (collection-empty? lista)
                                                            #f
                                                            #t))
                                                      (collection-map (λ (litera)
                                                                        (get-ch-words suffixes litera))
                                                                      alphabet)))))





; nu uitați să convertiți alfabetul într-un flux
(define (list->stream L)
  (if (null? L)
      empty-collection
      (collection-cons (car L) (list->stream (cdr L)))))




;; (define ( text->st  labeling-func)
;;   (λ (text)
;;     (let* ([cuvant (append text '(#\$))])
;;      (suffixes->st labeling-func (get-suffixes cuvant)  (list->stream (remove-duplicates (sort cuvant char<?)))))))
(define (text->st functie)
  (λ (string)
    (suffixes->st functie (get-suffixes (append string '(#\$))) (list->stream (remove-duplicates (sort (append '(#\$) string) char<?))))))


(define text->ast 
   (text->st ast-func)
   )

(define text->cst 
  (text->st  cst-func)
   )


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let* ([verificare-pattern (substring-help text pattern  pattern null)])
   (if (null? text)
      #f
      (if (equal? text pattern)
          #t
          (if (null? verificare-pattern)
              #f
              (if (equal? verificare-pattern pattern)
                  #t
                  #f))))))
 




(define (substring-help text pattern pattern1 coppy_pattern)
  (if (null? text)
      coppy_pattern
      (if (null? pattern)
          coppy_pattern
          (if (equal? (car text) (car pattern))
              (substring-help (cdr text) (cdr pattern) pattern1 (append coppy_pattern (list (car text))))
              (substring-help  (cdr text)  pattern1 pattern1 null)))))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.

                        
          
               

(define (repeated-substring-of-given-length text len)
 'f)
       