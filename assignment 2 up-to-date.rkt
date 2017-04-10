#lang racket

(require srfi/1)
(require srfi/13)
;; Defining items that will be accessible throughout the game
(define objects '((1 "a zanpakuto")
                  (2 "a bow")
                  (1 "a gold coin")))

(define beasts '((1 "soul reaper")
                  (2 "quincy")
                  (1 "hollow")))

;; Different areas for the user to explore
(define descriptions '((1 "you are in Soul Society.")
                      (2 "you are in the Hidden Leaf Village.")
                      (3 "you are in East Blue.")
                      (4 "you are in the Village Hidden in the Mist.")
                      (5 "you are in the Dragons Den.")
                      (6 "you are in Hueco Mundo.")
                      (7 "you are in the Sky Island.")
                      (8 "you are at Hells Gate.")
                      (9 "you are in the Valley of the Dead.")
                      (10 "you are in the Forst of Deadly Hollows.")
                      (11 "your are in the the kingdom of the elves.")))

;; commands and their decision tables that are avaliable to the users
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define fight '(((fight) fight) ((kill) fight) ((destroy) fight)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag)  inventory)))
(define upgrade '(((update) upgrade) ((evolution)  upgrade)))
(define actions `(,@look ,@quit ,@pick ,@fight ,@put ,@inventory))

;; decision tables for the areas
(define decisiontable `((1 ((Hidden Leaf Village) 2) ((East Blue) 3) ,@actions)
                        (2 ((Dragons Den) 5) ((Village Hidden in the Mist) 4),@actions)
                        (3 ((East Blue) 4) ((Heuco Mundo) 6),@actions)
                        (4 ((Village Hidden in the Mist) 7)((Hells Gate) 8) ,@actions)
                        (5 ((Hueco Mundo) 6) ,@actions)
                        (6 ,@actions)))

(define objectdb (make-hash))
(define inventorydb (make-hash))
(define upgradedb (make-hash))
(define beastdb (make-hash))

(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

(define (add-objects db)
  (for-each
   (lambda (r) 
     (add-object db (first r) (second r))) objects))



(add-objects objectdb)

;; monster db 
(define (add-beast db id beast)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons beast record)))
      (hash-set! db id (cons beast empty))))

(define (add-beasts db)
  (for-each
   (lambda (r) 
     (add-beast db (first r) (second r))) beasts))



(add-beasts beastdb)

(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'npcc)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))

(define (remove-object-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item) 
             (printf "I don't see that item in the room!\n"))
            (else
             (printf "Added ~a to your bag.\n" (first item))
             (add-object inventorydb 'bag (first item))
             (hash-set! db id result))))))

(define (remove-object-from-inventory db id str)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "You are not carrying that item!\n"))
            (else
             (printf "Removed ~a from your bag.\n" (first item))
             (add-object objectdb id (first item))
             (hash-set! db 'bag result))))))

;;Beast Db Functions

(define (display-beasts db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'npc)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))

(define (remove-beast-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (npcc (lset-difference equal? record result)))
      (cond ((null? npcc) 
             (printf "I don't see that npc in the room!\n"))
            (else
             (printf "~a has been killed" (first npcc))
             (add-object beastdb 'npc (first npcc))
             (hash-set! db id result))))))

            
(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

(define (kill-beast id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-beast-from-room beastdb id item)))

(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

(define (display-inventory)
  (display-objects inventorydb 'bag))

(define (slist->string l)
  (string-join (map symbol->string l)))

(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-description id)
  (car (assq-ref descriptions id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))


(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))

(define (display-description id)
  (printf "~a\n" (get-description id)))

(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (when description
      (display-description id)
      (display-beasts beastdb id)
      (display-objects objectdb id))
    
    (printf "> ")
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (printf "huh? I didn't understand that!\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #t))
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              ((eq? response 'fight)
               (kill-beast id input)
               (loop id #f))
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))              
              ((eq? response 'quit)
               (printf "So Long, and Thanks for All the Fish...\n")
               (exit)))))))


(startgame 1)