#lang typed/racket



(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

(require (only-in typed/racket/gui/base put-file get-file))

;; Data definitions

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Stone
  (U 'black 'white))

(define-struct LogicalLoc
  ([col : Integer]
   [row : Integer]))

(define-type Board
  (Vectorof (Vectorof (Optional Stone))))


(define-struct Go
  ([board : Board]
   [next-to-play : Stone]
   [history : (Listof Board)]
   [last-turn-place : (Optional LogicalLoc)]
   [last-turn-opp-captures : (Listof LogicalLoc)]
   [last-turn-self-captures : (Listof LogicalLoc)]
   [consecutive-passes : Integer]))



(define-struct PhysicalLoc
  ([x : Integer]
   [y  : Integer]))

(define-struct BoardSpec
  ([background-color : Image-Color]
   [cell-size-pixels : Integer]
   [margin-pixels : Integer]
   [stone-radius-pixels : Integer]))


(define-struct Outcome
  ([black  : Integer]
   [white  : Integer]
   [winner : (U Stone 'draw)]))


(define-struct World
  ([spec : BoardSpec]
   [game : Go]
   [status-message : String]
   [black-tenths : Integer]
   [white-tenths : Integer]
   [hover : (Optional LogicalLoc)]))



;; A function that converts a LogicalLoc into a PhysicalLoc.
;; The integer argument is the dimension (locations per side) of the board.
(: logical->physical : LogicalLoc Integer BoardSpec -> PhysicalLoc)

(define (logical->physical logLoc dim spec)
  (match* (logLoc spec)
    [((LogicalLoc col row)
      (BoardSpec _ c m _))
     (PhysicalLoc (+ m (* col c))
                  (+ m (* (+ dim -1 (- 0 row)) c)))]))

(check-expect (logical->physical (LogicalLoc 0 0) 4
                                 (BoardSpec 'green 12 16 4))
              (PhysicalLoc 16 52))


;; A distance function for Physical Locations.
(: dist : PhysicalLoc PhysicalLoc -> Real)
(define (dist physLoc1 physLoc2)
  (match* (physLoc1 physLoc2)
    [((PhysicalLoc x1 y1) (PhysicalLoc x2 y2))
     (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2))))])) 

(check-expect (dist (PhysicalLoc 2 3) (PhysicalLoc 8 11)) 10)


;; A function that takes in a dimension and computes all
;; Logical locations in the Go grid.
(: logical-grid : Integer Integer Integer -> (Listof LogicalLoc))
(define (logical-grid dim ncol nrow)
  (cond [(and (< ncol dim) (< nrow dim))
         (cons (LogicalLoc ncol nrow)
               (logical-grid dim ncol (+ 1 nrow)))]
        [(< ncol dim)        
               (logical-grid dim (+ 1 ncol) 0)]
        [else '()]))

(check-expect (logical-grid 2 0 0) (list (LogicalLoc 0 0)
                                         (LogicalLoc 0 1)
                                         (LogicalLoc 1 0)
                                         (LogicalLoc 1 1)))


;; A function that converts the logical grid to a physical grid. 
(: physical-grid : Integer BoardSpec  -> (Listof PhysicalLoc))
(define (physical-grid dim spec)
  (listLog->listPhys (logical-grid dim 0 0) dim spec))

(check-expect (physical-grid 2 (BoardSpec 'tan 12 16 4))
              (list (PhysicalLoc 16 28)
                    (PhysicalLoc 16 16)
                    (PhysicalLoc 28 28)
                    (PhysicalLoc 28 16)))



;; A function that checks whether any PhysicalLoc in
;; the physical grid is within a stone's radius of the
;; given PhysicalLoc. If there is such a PhysicalLoc
;; in the grid, we return it. Otherwise, we return 'None.

(: closest-physical : (Listof PhysicalLoc)
   PhysicalLoc BoardSpec Integer -> (Optional PhysicalLoc))
(define (closest-physical PPs physLoc spec dim)
  (match* (PPs spec)
    [('() _) 'None]
    [((cons head tail) (BoardSpec _ _ _ r))
     (if (< (dist head physLoc) r) (Some head)
         (closest-physical tail physLoc spec dim))]))

(check-expect (closest-physical
 (physical-grid 2 (BoardSpec 'tan 12 16 4))
 (PhysicalLoc 16 16) (BoardSpec 'tan 12 16 4) 9)
              (Some (PhysicalLoc 16 16)))

(check-expect (closest-physical
 (physical-grid 2 (BoardSpec 'tan 12 16 4))
 (PhysicalLoc 0 0) (BoardSpec 'tan 12 16 4) 9)
              'None)





;; A function that converts PhysicalLocs into
;; a LogicalLoc if it is within an accepted range.

(: physical->logical : PhysicalLoc Integer BoardSpec -> (Optional LogicalLoc))
(define (physical->logical phys dim spec)
  (match* (phys spec)
    [((PhysicalLoc x y)(BoardSpec b c m s))
     (match (closest-physical
             (physical-grid dim spec)
             (PhysicalLoc x y) (BoardSpec b c m s) dim)
       ['None 'None]
       [(Some (PhysicalLoc a b))
        (Some (LogicalLoc (round (/ (- a m) c))
                          (- (- dim 1) (round (/ (- b m) c)))))])]))

(check-expect (physical->logical (PhysicalLoc 18 17) 9
                                 (BoardSpec 'tan 12 16 4))
              (Some (LogicalLoc 0 8)))

(check-expect (physical->logical (PhysicalLoc 0 0) 9 (BoardSpec 'tan 12 16 4))
              'None)






;; Convert logical locations to strings such as "A1", "B3", etc.
;; Note the letter "I" is skipped in Go labeling.
;; When you get a column past "Z", use "AA", then "BB", then "CC", etc.
;; When you get past "ZZ", use "AAA", then "BBB", etc.
(: logical->string : LogicalLoc -> String)

(define (logical->string logLoc)
  (match logLoc
    [(LogicalLoc x y)
     (string-append
      (make-string (+ 1 (quotient x 25))
                   (list-ref alphabet (remainder x 25)))
      (number->string (+ y 1)))]))
  

(check-expect (logical->string (LogicalLoc 12 19))
              "N20")
(check-expect (logical->string (LogicalLoc 56 35))
              "GGG36")



;; A function for the empty board.
(: empty-board : Integer -> Board)
(define (empty-board dim)
  (build-vector dim
                (lambda ([i : Integer])
                  (make-vector dim (cast 'None (Optional Stone))))))

(check-expect (empty-board 2) (vector (vector 'None 'None)
                                      (vector 'None 'None)))





;; ALL TIME FUNCTIONS. 
;; given tenths of a second, build string that shows seconds
(: format-sec (Integer -> String))
(define (format-sec t)
  (match (quotient t 600)
    [mins
     (cond [(< (quotient (remainder t 600) 10) 10)
            (string-append (number->string mins) ":0"
                           (number->string
                            (quotient (remainder t 600) 10))
                           "."
                           (number->string
                            (remainder (remainder t 600) 10)))]
           [else (string-append (number->string mins) ":"
                           (number->string
                            (quotient (remainder t 600) 10))
                           "."
                           (number->string
                            (remainder (remainder t 600) 10)))])]))
                           
       
(check-expect (format-sec 1700) "2:50.0")
(check-expect (format-sec 1250) "2:05.0")



;; A function that reacts to change in time.
(: react-to-tick : World -> World)
(define (react-to-tick W)
  (match W
    [(World spec go msg b-tenths w-tenths hov)
     (if (two-passes? go)
         (World spec go msg b-tenths w-tenths hov)
     (match (Go-next-to-play go)
       ['white (World spec go msg b-tenths (add1 w-tenths) hov)]
       ['black (World spec go msg (add1 b-tenths) w-tenths hov)]))]))
       



;; A function that takes in an image, and places three timers
;; next to it.
(: place-timers : Integer Integer Image -> Image)
(define (place-timers black-time white-time bkg)
  (beside bkg
          (above (overlay (above (text "Black Time: " 20 'black)
                                 (text (format-sec black-time)
                                       20 'black))
                          (rectangle (round (* 1/3 (image-width bkg)))
                                       (round (* 1/3 (image-height bkg)))
                                       'solid 'white))
                 (overlay (above (text "White Time: " 20 'black)
                                 (text (format-sec white-time)
                                       20 'black))
                          (rectangle (round (* 1/3 (image-width bkg)))
                                       (round (* 1/3 (image-height bkg)))
                                       'solid 'white))
                 (overlay (above (text "Total Time: " 20 'black)
                                 (text (format-sec (+ black-time
                                                          white-time))
                                       20 'black))
                          (rectangle (round (* 1/3 (image-width bkg)))
                                       (round (* 1/3 (image-height bkg)))
                                       'solid 'white)
                          ))))


;; END OF TIME FUNCTIONS





;; A function that takes in a list of logial locations
;; and a logical location, checks
;; whether the given logLoc is in the list.
(: is-in-list? : LogicalLoc (Listof LogicalLoc) -> Boolean)
(define (is-in-list? logLoc lls)
  (match* (lls logLoc)
    [('() _) #f]
    [((cons (LogicalLoc x1 y1) tail) (LogicalLoc x y))
     (if (and (= x1 x) (= y1 y)) #t (is-in-list? logLoc tail))]))

(check-expect (is-in-list? (LogicalLoc 0 0)
                           (list (LogicalLoc 0 1) (LogicalLoc 1 0))) #f)
(check-expect (is-in-list? (LogicalLoc 0 0)
                           (list (LogicalLoc 0 1) (LogicalLoc 0 0))) #t)
     




;; A function that takes in a logLoc and a Board, and
;; returns 'None if unoccupied, or some
;; stone if not. 
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref go logLoc)
  (match* (logLoc go)
    [((LogicalLoc x y) (Go board _ _ _ _ _ _))
     (vector-ref (vector-ref board x) y)]))

(check-expect (board-ref (Go (empty-board 3) 'white '() 'None '() '() 0)
                         (LogicalLoc 1 2))
              'None)
(check-expect (board-ref (Go (vector (vector (Some 'white) 'None)
                                     (vector 'None 'None))'white '()
                                                          'None '() '() 0)
                         (LogicalLoc 0 0))
              (Some 'white))


;; Same as board-ref, but takes in a board rather than a
;; Go.
(: bd-ref : Board LogicalLoc -> (Optional Stone))
(define (bd-ref board logLoc)
  (match logLoc
    [(LogicalLoc x y) (vector-ref (vector-ref board x) y)]))

(check-expect (bd-ref (empty-board 3) (LogicalLoc 0 0))
              'None)
(check-expect (bd-ref (vector (vector (Some 'white) 'None)
                                     (vector 'None 'None))
                         (LogicalLoc 0 0))
              (Some 'white))




;; A function that makes an independent copy of
;; a board-vector.
(: board-copy : Board -> Board)
(define (board-copy board)
  (local {(: board-copy-help : Board Board Integer -> Board)
          (define (board-copy-help board copy n)
            (match board
              ['#() '#()]
              [_ (cond
                   [(or (< n 0) (> n (- (vector-length board) 1)))
                    copy]
                   [(not (= (vector-length board)
                       (vector-length copy)))
                     (error "wrong dimension")]
                   [else                    
                     (board-copy-help
                      board
                      (mod-vec copy n
                                          (vector-copy2
                                           (vector-ref board n))) 
                      (add1 n))])]))}
    (board-copy-help board
                     (make-vector (vector-length board)
                                        (vector-ref board 0)) 0)))



(check-expect (board-copy (empty-board 3)) (empty-board 3))
(check-expect (board-copy (vector (vector (Some 'white) 'None)
                                     (vector 'None 'None)))
              (vector (vector (Some 'white) 'None)
                                     (vector 'None 'None)))


;; A function that takes vector-set! and returns the
;; modified vector. I use this on occasion for a cleaner
;; code, instead of (begin (vector-set! ))
(: mod-vec : All (A) (Vectorof A) Integer A  -> (Vectorof A))
(define (mod-vec vector n elem)
  (match (vector-set! vector n elem)
    [_ vector]))

(check-expect (mod-vec (vector 1 2 3 4 5) 3 18)
              (vector 1 2 3 18 5))




;; My version of vector-copy.
(: vector-copy2 : (Vectorof (Optional Stone)) -> (Vectorof (Optional Stone)))
(define (vector-copy2 vector)
  (local {(: help : (Vectorof (Optional Stone))
             (Vectorof (Optional Stone)) Integer ->
             (Vectorof (Optional Stone)))
          (define (help vector copy n)
            (cond
              [(not (= (vector-length copy)
                      (vector-length vector))) (error "wrong dimension")]
              [(or (< n 0)
                       (< (- (vector-length vector) 1) n))
                   copy]
              [else (help vector
                          (begin (vector-set! copy n
                                              (vector-ref vector n)) copy)
                          (add1 n))]))}
    (help vector
          (make-vector (vector-length vector)
                       (Some 'black)) 0)))

(check-expect (vector-copy2 (vector (Some 'white)))
              (vector (Some 'white)))



;; A function that checks for equality between Vectors
;; of (Optional Stone)?
(: vector-eq? : (Vectorof (Optional Stone)) (Vectorof (Optional Stone))
   -> Boolean)
(define (vector-eq? vec1 vec2)
  (local {(: help : (Vectorof (Optional Stone))
             (Vectorof (Optional Stone)) Integer -> Boolean)
          (define (help vec1 vec2 n)
            (cond [(not (= (vector-length vec1) (vector-length vec2))) #f]
                  [(or (< n 0) (> n (- (vector-length vec2) 1))) #t]
                  [else (match* ((vector-ref vec1 n) (vector-ref vec2 n))
                          [(A A)
                           (help vec1 vec2 (add1 n))]
                          [(_ _) #f])]))}
    (help vec1 vec2 0)))


(check-expect (vector-eq? (vector 'None (Some 'white))
                          (vector 'None (Some 'white))) #t)
(check-expect (vector-eq? (vector 'None 'None)
                          (vector 'None (Some 'white))) #f)


;; A function that checks for equality between boards.
(: board=? : Board Board -> Boolean)
(define (board=? board1 board2)
  (local {(: help : Board Board Integer -> Boolean)
          (define (help board1 board2 n)
            (cond
              [(not (= (vector-length board1)
                 (vector-length board2))) #f]
              [(or (< n 0) (> n (- (vector-length board2) 1))) #t]
              [(vector-eq? (vector-ref board1 n)
                           (vector-ref board2 n))
               (help board1 board2 (add1 n))]
              [else #f]))}
    (help board1 board2 0)))
              
(check-expect (board=? (vector (vector (Some 'black) (Some 'white))
                                (vector (Some 'white) (Some 'white)))
           (vector (vector (Some 'black) (Some 'white))
                   (vector (Some 'white) (Some 'white)))) #t)

(check-expect (board=? (vector (vector 'None (Some 'white))
                                (vector (Some 'white) (Some 'white)))
           (vector (vector (Some 'black) (Some 'white))
                   (vector (Some 'white) (Some 'white)))) #f)

(check-expect (board=? (vector (vector 'None (Some 'white))
                               (vector 'None 'None))
                       (vector (vector 'None 'None)
                               (vector 'None 'None))) #f)
(check-expect (board=? (vector (vector 'None (Some 'white))
                               (vector 'None (Some 'white)))
                       (vector (vector 'None (Some 'white))
                               (vector 'None (Some 'white)))) #t)
                       


;; A function that modifies a board to store the specified stone
;; (or no stone at all) at the given location.
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! go logLoc stone?)
  (match* (go logLoc)
    [((Go board _ _ _ _ _ _) (LogicalLoc x y))
     (vector-set! (vector-ref board x) y stone?)]))



;; Example board 1.
(: b1 : Board)
(define b1
  (vector (vector (Some 'black) (Some 'white) 'None)
                      (vector (Some 'black) (Some 'white) (Some 'black))
                      (vector 'None (Some 'white) (Some 'black))))


(board-set! (Go b1 'white '() 'None '() '() 0)
            (LogicalLoc 0 2) (Some 'white))
(check-expect (bd-ref b1 (LogicalLoc 0 2)) (Some 'white))





;; Same as before, but with a board instead of a Go
(: bd-set! : Board LogicalLoc (Optional Stone) -> Void)
(define (bd-set! board logLoc stone?)
  (match  logLoc
    [(LogicalLoc x y)
     (vector-set! (vector-ref board x) y stone?)]))


;; Example board 2.
(: b2 : Board)
(define b2
  (vector (vector (Some 'black) (Some 'white) (Some 'white))
                      (vector (Some 'black) (Some 'white) (Some 'black))
                      (vector 'None (Some 'white) (Some 'black))))


(bd-set! b2 (LogicalLoc 2 0) (Some 'white))
(check-expect (bd-ref b2 (LogicalLoc 2 0)) (Some 'white))









                      

;; A function that verfies whether the BoardSpec is
;; valid or not.
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? spec)
  (match spec
    [(BoardSpec _ c m r)
     (cond [(or (<= r 0) (<= m 0) (<= c 0)) #f]
           [(and (< r (* 0.5 c)) (< r m)) #t]
           [else #f]
    )]))

(check-expect (valid-board-spec? (BoardSpec 'green 3 11 4)) #f)
(check-expect (valid-board-spec? (BoardSpec 'green 6 11 2)) #t)












;; A function that draws the Go-grid.
(: grid : Integer Integer -> Image)
(define (grid dim c)
  (foldr beside
             empty-image
             (make-list (- dim 1)
                        (foldr above
                               empty-image
                               (make-list (- dim 1)
                                          (square c 'outline 'black))))))



;; The function that draws the World.
(: draw : World -> Image)
(define (draw w) 
  (match w
    [(World spec
            (Go board next _ LTP LTOC LTSC CP)
            message BT WT hover)
     (local {(define dim (vector-length board))}
     (match spec
       [(BoardSpec bkgcol cell marg r)
        (put-image LTSC "purple" (round (/ r 2)) dim spec
        (put-image LTOC "blue" (round (/ r 2)) dim spec
        (put-image-2 LTP "red" (round (/ r 3)) dim spec #f          
        (put-image-2 hover next r dim spec #t
                     
        (place-timers BT WT
     (above/align "left"
              (draw-circles 'white (listLog->listPhys
                                    (piece-list board 'white)
                                    (vector-length board) spec) r 
                            (draw-circles 'black
                                          (listLog->listPhys
                                           (piece-list board 'black)
                                           (vector-length board) spec)
                                          r  (beside/align "top"
                                                           (above/align "left"                   
                    (overlay (grid (vector-length board) cell)
                     (square (+ (* 2 marg)
                                (* cell (- (vector-length board) 1)))
                             'solid bkgcol))
                    (row-alphabet (vector-length board) 0
                                  (BoardSpec bkgcol cell marg r)))
                            (column-num (vector-length board)
                                        (vector-length board)
                                        (BoardSpec bkgcol cell marg r)))))
             
                                      (rectangle 15 10 'solid 'white)
                   (overlay (text message 15 'black)
                            (rectangle (+ (* 2 marg)
                                (* cell (- (vector-length board) 1)))
                              (* 1/6 (+ (* 2 marg)
                                (* cell (- (vector-length board) 1))))
                              'solid bkgcol)))
     )))))]))]))



;; A function that converts a list of logical locs
;; into a list of physLocs.
(: listLog->listPhys : (Listof LogicalLoc) Integer BoardSpec  ->
   (Listof PhysicalLoc))
(define (listLog->listPhys LLs dim spec)
  (match LLs
    ['() '()]
    [(cons head tail)
     (cons (logical->physical head dim spec)
           (listLog->listPhys tail dim spec))]
    ))

(check-expect (listLog->listPhys
               (list (LogicalLoc 0 0) (LogicalLoc 1 1) (LogicalLoc 4 3))
               9 (BoardSpec 'tan 12 16 4))
              (list (PhysicalLoc 16 112)
                    (PhysicalLoc 28 100)
                    (PhysicalLoc 64 76)))





;; A function that takes in a List of LogicalLoc, a radius, a color,
;; and a background image, and draws circles of said radius on that
;; background.
(: put-image : (Listof LogicalLoc) Image-Color Integer
   Integer BoardSpec Image -> Image)
(define (put-image LLs color r dim spec bkg)
  (match LLs
    ['() bkg]
    [(cons head tail)
     (put-image tail color r dim spec (place-image (circle r 'solid color)
                             (PhysicalLoc-x (logical->physical head
                                                               dim spec))
                             (PhysicalLoc-y (logical->physical head
                                                               dim spec))
                              bkg))]))
                  

;; A function that takes in a List of LogicalLoc, a radius, a color,
;; and a background image, and draws circles of said radius on that
;; background.
(: put-image-2 : (Optional LogicalLoc) Image-Color Integer
   Integer BoardSpec Boolean Image -> Image)
(define (put-image-2 logLoc? color r dim spec hover? bkg)
  (match logLoc?
    ['None bkg]
    [(Some logLoc)
     (if hover?
     (place-image (circle r 128 color)
                             (PhysicalLoc-x (logical->physical logLoc
                                                               dim spec))
                             (PhysicalLoc-y (logical->physical logLoc
                                                               dim spec))
                              bkg)
     (place-image (circle r 'solid color)
                             (PhysicalLoc-x (logical->physical logLoc
                                                               dim spec))
                             (PhysicalLoc-y (logical->physical logLoc
                                                               dim spec))
                              bkg))
                ]))




;; A function that draws circles at Physical Locations
;; given a list of Physical locations, the dimension, a background,
;; and the color.
(: draw-circles : Image-Color (Listof PhysicalLoc) Integer Image -> Image)
(define (draw-circles color physLocs rad bkgimg)
  (match physLocs
    ['() bkgimg]
    [(cons (PhysicalLoc x y) tail)
     (place-image (circle rad 'solid color) x y
                  (draw-circles color tail rad bkgimg))]))




;; A function that takes in a board and
;; returns the list of logical locations
;; of the given piece.
(: piece-list : Board Stone -> (Listof LogicalLoc))
(define (piece-list board stone)
  (local {(define len (vector-length board))
          (: help : Board Stone Integer Integer -> (Listof LogicalLoc))
          (define (help board stone N M)
            (match board
              ['#() '()]
              [_ (cond
                   [(or (< N 0) (< M 0)
                     (> N (- len 1))) '()]
                   [(> M (- len 1)) (help board stone (add1 N) 0)]
                   [else (match* ((vector-ref (vector-ref board N) M) stone)
                           [((Some stone) stone)
                            (cons (LogicalLoc N M)
                                  (help board stone N (add1 M)))]
                           [(_ _) (help board stone N (add1 M))])])]))}
    (help board stone 0 0)))
              
(check-expect (piece-list (vector (vector (Some 'black)
                                          (Some 'white) (Some 'black))
                      (vector (Some 'black) (Some 'white) (Some 'black))
                      (vector 'None (Some 'white) (Some 'black))) 'white)
              (list (LogicalLoc 0 1) (LogicalLoc 1 1) (LogicalLoc 2 1)))









;; The function that changes the world in accordance with
;; screen clicks.
(: react-to-click : World Integer Integer Mouse-Event -> World)
(define (react-to-click w x y e)
  (match e
    ["button-down"
        (match w
       [(World spec (Go board stone history LTP LTOC
                        LTSC CP) _ BT WT hov)
        (local {(define copy (board-copy board))
                (define go (Go board stone history LTP LTOC
                        LTSC CP))
                (: opposite : Stone -> Stone)
             (define (opposite stone)
               (match stone
                 ['black 'white]
                 ['white 'black]))}
        (match
            (physical->logical
             (PhysicalLoc x y) (vector-length board) spec)
          ['None w]
          [(Some (LogicalLoc a b))

           (cond [(not (legal-move? go
                               (LogicalLoc a b)))
                  (World spec (Go copy stone history LTP LTOC
                        LTSC CP)
                         (string-append (string-titlecase
                                         (symbol->string stone))
                                        " attempted an illegal move. "
                                        "It is still "
                                        (string-titlecase
                                            (symbol->string stone))
                                           "'s turn.") BT WT hov)]
                 [else
                  (match (bd-ref board (LogicalLoc a b))
                    ['None (World spec
                           (apply-move go (LogicalLoc a b))
                           (string-append (string-titlecase
                                           (symbol->string stone)) " moved to "
                                          (logical->string
                                                 (LogicalLoc a b))
                                          ". It is now " 
                                           (string-titlecase
                                            (symbol->string (opposite stone)))
                                           "'s turn." ) BT WT hov)])])]))])]               
       
    ["move" (match w
       [(World spec myGo
               msg BT WT hov)
        (match myGo
          [(Go board next history LTP LTOC LTSC CP)
        (match
            (physical->logical
             (PhysicalLoc x y) (vector-length board) spec)
          ['None (World spec myGo
               msg BT WT 'None)]
          [(Some logLoc)
           (if (legal-move? myGo logLoc)
               (World spec myGo msg BT WT (Some logLoc))
               (World spec myGo
               msg BT WT 'None))])])])] 
    [_ w]))



(check-expect (react-to-click
               (World (BoardSpec 'tan 12 16 4)
                      (Go (empty-board 2) 'black '()
                          'None '() '() 0) "Hjello" 0 0 'None)
               16 16 "button-down")
              (World (BoardSpec 'tan 12 16 4)
                     (Go (vector (vector 'None (Some 'black))
                                 '#(None None)) 'white (list
                                                        (vector
                                               (vector 'None (Some 'black))
                                                            '#(None None)))
                                         (Some (LogicalLoc 0 1)) '() '() 0)
                 "Black moved to A2. It is now White's turn." 0 0 'None))
          




;;; The function for passing on a move.
(: react-to-key : World String -> World)
(define (react-to-key w key)
  (match key
    ["p"
     (match w
       [(World spec go _ BT WT hover)
        (if (two-passes? go)
            (World spec go "The game is over. You can't pass again."
                   BT WT hover)
            (match go
              [(Go board 'white history LTP LTOC LTSC CP)
               (cond [(one-pass? go)
                      (World spec (Go board 'black
                        (cons (board-copy board) history)
                        'None '() '() (add1 CP))
               (who-won (outcome go)) BT WT hover)]
                     [else
               (World spec (Go board 'black
                        (cons (board-copy board) history)
                        'None '() '() (add1 CP))
               "White passed. It is Black's turn." BT WT hover)])]
               
              [(Go board 'black history LTP LTOC LTSC CP)
               (cond [(one-pass? go)
                      (World spec (Go board 'white
                        (cons (board-copy board) history)
                        'None '() '() (add1 CP))
               (who-won (outcome go)) BT WT hover)]
                     [else 
               (World spec (Go board 'white
                        (cons (board-copy board) history)
                        'None '() '() (add1 CP))
               "Black passed. It is White's turn."
               BT WT hover)])]))])]
    ["s" (begin (save-game! w) w)]
    ["l" (match w
           [(World spec (Go bd1 _ _ _ _ _ _) _ _ _ _)
           (match (load-game spec)
             [(World _ (Go bd2 _ _ _ _ _ _) _ _ _ _)
              (if (= (vector-length bd2) (vector-length bd1))
                  (load-game spec)
                  (error (string-append "the dimensions of the current "
                                        "board do not match those "
                                        "of the loaded one.")))])])]
    [_ w]))

(check-expect (react-to-key
               (World (BoardSpec 'tan 12 16 4)
                      (Go (empty-board 2) 'black '()
                          'None '() '() 0) "Hjello" 0 0 'None) "p")
              (World (BoardSpec 'tan 12 16 4)
                     (Go '#(#(None None) #(None None))
                         'white '(#(#(None None) #(None None)))
                         'None '() '() 1)
                     "Black passed. It is White's turn." 0 0 'None))





;; A function that determines whether two consecutive passes
;; have occured.
(: two-passes? : Go -> Boolean)
(define (two-passes? go)
  (match go
    [(Go _ _ _ _ _ _ 2) #t]
    [_ #f]))

(check-expect (two-passes? (Go (empty-board 2) 'black '()
                          'None '() '() 0)) #f)


;; Same but with one pass.
(: one-pass? : Go -> Boolean)
(define (one-pass? go)
  (match go
    [(Go _ _ _ _ _ _ 1) #t]
    [_ #f]))

(check-expect (two-passes? (Go (empty-board 2) 'black '()
                          'None '() '() 0)) #f)


;; A function that evaluates the legality of a
;; proposed move.
(: legal-move? : Go LogicalLoc -> Boolean)
(define (legal-move? go logLoc)
  (if (two-passes? go) #f
  (match (board-ref go logLoc)
    ['None
   (match go
    [(Go board stone history _ _ _ _)
     
     (local {(define copy (board-copy board))
             (: board=?-help : Board -> Boolean)
             (define (board=?-help board1)
               (match (apply-move (Go copy stone '() 'None '() '() 0) logLoc)
                 [(Go bd _ _ _ _ _ _)
               (board=? bd
                        board1)]))
             (define bd1 (Go-board (apply-move (Go copy stone '()
                                                   'None '() '() 0) logLoc)))}
     
           (not (is-board-in-list? bd1 history)) )
     ])]
    [_ #f])))

(check-expect (legal-move? (Go b3 'white '()
                               'None '() '() 0) (LogicalLoc 0 2)) #f)




;; A function that checks whether a given board is in a history list. 
(: is-board-in-list? : Board (Listof Board) -> Boolean)
(define (is-board-in-list? board history)
  (match history
    ['() #f]
    [(cons head tail)
     (if (board=? board head) #t
         (is-board-in-list? board tail))]))


(check-expect (is-board-in-list? b3 (list b3 b1)) #t)
(check-expect (is-board-in-list? b3 (list b1)) #f)






;; A broken version of board-ref that returns (Some 'black)
;; if the given logical location is outside the bounds of
;; the board:
(: bd-ref-broken : Board LogicalLoc -> (Optional Stone))
(define (bd-ref-broken board logLoc)
  (local {(define len (- (vector-length board) 1))}
  (match logLoc
    [(LogicalLoc x y)
     (cond [(and (<= 0 x)
                 (<= x len)
                 (<= 0 y)
                 (<= y len)) (bd-ref board logLoc)]
           [else (Some 'black)])])))

(check-expect (bd-ref-broken (empty-board 3) (LogicalLoc 1 4))
              (Some 'black))
(check-expect (bd-ref-broken (empty-board 3) (LogicalLoc 1 2))
              (bd-ref (empty-board 3) (LogicalLoc 1 2)))
                 


;; The function that checks if a location has liberties.
(: liberties? : Board LogicalLoc -> Boolean)
(define (liberties? board logLoc)
    (match logLoc
      [(LogicalLoc x y)
       (match* ((bd-ref-broken board
                        (LogicalLoc (add1 x) y))
                (bd-ref-broken board
                        (LogicalLoc (sub1 x) y))
                (bd-ref-broken board
                        (LogicalLoc x (add1 y)))
                (bd-ref-broken board
                        (LogicalLoc x (sub1 y))))
         [((Some a) (Some b) (Some c) (Some d)) #f]
         [(_ _ _ _) #t])]))


(check-expect (liberties? b3 (LogicalLoc 2 2)) #f)

                  
          

;; A function that takes in a Logical Location
;; and returns a list of its neighbors.
(: neighbors : Board LogicalLoc -> (Listof LogicalLoc))
(define (neighbors board logLoc)
  (local {(define len (- (vector-length board) 1))
           (: valid-neighbors : LogicalLoc -> Boolean)
           (define (valid-neighbors logLoc)
             (match logLoc
               [(LogicalLoc x y)
                (and (<= x len) (<= 0 x)
                     (<= y len) (<= 0 y))]))}
  (match logLoc
    [(LogicalLoc x y)
            (filter valid-neighbors (list (LogicalLoc (add1 x) y)
                  (LogicalLoc (sub1 x) y)
                  (LogicalLoc x (add1 y))
                  (LogicalLoc x (sub1 y))))])))
    
(check-expect (neighbors b3 (LogicalLoc 2 2))
              (list (LogicalLoc 1 2) (LogicalLoc 2 1)))



;; The function that returns a list of neighbors
;; of the same color.
(: color-neighbors : Board LogicalLoc
   -> (Optional (Listof LogicalLoc)))
(define (color-neighbors board logLoc)
  (match (bd-ref board logLoc)
    ['None 'None]
    [(Some stone)
     (local {(: of-same-color? : LogicalLoc -> Boolean)
             (define (of-same-color? logicalLoc)
               (match* ((bd-ref board logicalLoc) (Some stone))
                 [(A A) #t]
                 [(_ _) #f]))}
       (Some (filter of-same-color? (neighbors board logLoc))))]))

             
(check-expect (color-neighbors b3 (LogicalLoc 0 0 )) 'None)
(check-expect (color-neighbors b3 (LogicalLoc 1 1))
              (Some (list (LogicalLoc 2 1))))
(check-expect (color-neighbors b3 (LogicalLoc 2 1))
              (Some (list (LogicalLoc 1 1))))
(check-expect (color-neighbors b3 (LogicalLoc 0 2))
              (Some '() ))




;; A helper-function that provies a list of logical locations
;; of the "unmarked" neighbors of the same color.
(: unmarked-neighbors : Board LogicalLoc (Listof LogicalLoc)
   -> (Optional (Listof LogicalLoc)))
(define (unmarked-neighbors board logLoc marked)
  (match (color-neighbors board logLoc)
    ['None 'None]
    [(Some List)
  (local {(: isnt-in-marked? : LogicalLoc -> Boolean)
          (define (isnt-in-marked? logicalLoc)
            (not (is-in-list? logicalLoc marked)))}
    (Some (filter isnt-in-marked? List)))])) 

(check-expect (unmarked-neighbors b3 (LogicalLoc 2 2) '())
              (Some (list (LogicalLoc 1 2))))



;; The function that identifies any potential chains.
(: identify-chain : Board (Listof LogicalLoc) (Listof LogicalLoc)
   -> (Optional (Listof LogicalLoc)))
(define (identify-chain board to-explore marked)
  (match to-explore
    ['() (Some marked)]
    [(cons head tail)
     (cond [(liberties? board head) 'None]
           [else (match (unmarked-neighbors board head marked)
                   ['None 'None]
                   [(Some unmk-neigh)
                    (local {(: isnt-head? : LogicalLoc -> Boolean)
                            (define (isnt-head? logicalLoc)
                              (match* (logicalLoc head)
                                [(A A) #f]
                                [(_ _) #t]))}
                      (identify-chain board
                                      (append unmk-neigh
                                              (filter isnt-head? to-explore))
                                      (append unmk-neigh marked)))])])]))


(check-expect (identify-chain b3 (list (LogicalLoc 2 2))
                              (list (LogicalLoc 2 2)))
              (Some (list (LogicalLoc 1 2) (LogicalLoc 2 2))))


;; A function that deletes all 'None appearances in a
;; list of (Optional (Listof LogicalLoc))
(: no-nones : (Listof (Optional (Listof LogicalLoc))) ->
   (Listof (Listof LogicalLoc)))
(define (no-nones xs)
  (match xs
    ['() '()]
    [(cons head tail)
     (match head
       ['None (no-nones tail)]
       [(Some A) (cons A (no-nones tail))])]))


(check-expect (no-nones (list 'None 'None (Some (list (LogicalLoc 1 1)))))
              (list (list (LogicalLoc 1 1))))



;; A function that deletes repetions of a (Listof LogialLoc)
;; from a list of chains.
(: no-repetitions :  (Listof (Listof LogicalLoc)) ->
   (Listof (Listof LogicalLoc)))
(define (no-repetitions xs)
  (local {(: is-in-listlists? : LogicalLoc
             (Listof (Listof LogicalLoc)) -> Boolean)
          (define (is-in-listlists? logLoc xxs)
            (match xxs
              ['() #f]
              [(cons head tail)
               (if (is-in-list? logLoc head) #t
                   (is-in-listlists? logLoc tail))]))}
    (match xs
      ['() '()]
      [(cons head tail)
       (if (is-in-listlists? (first head) tail)
           (no-repetitions tail)
           (cons head (no-repetitions tail)))])))

(check-expect (no-repetitions
               (list (list (LogicalLoc 1 2))
                     (list (LogicalLoc 1 2))
                     (list (LogicalLoc 2 4)
                           (LogicalLoc 4 5))))
              (list (list (LogicalLoc 1 2))
                    (list (LogicalLoc 2 4)
                          (LogicalLoc 4 5))))



;; A function that takes in a Stone, a Board, a list of
;; chains (Listof LogicalLoc) and selects those
;; chains of given color.
(: color-chains : Stone Board (Listof (Listof LogicalLoc)) ->
   (Listof (Listof LogicalLoc)))
(define (color-chains stone board xs)
  (match xs
    ['() '()]
    [(cons head tail)
     (match* ((bd-ref board (first head)) (Some stone))
       [(A A) (cons head (color-chains stone board tail))]
       [(_ _) (color-chains stone board tail)])]))

(check-expect (color-chains 'white b3 (list (list (LogicalLoc 1 2)
                                               (LogicalLoc 2 2))))
              '())
(check-expect (color-chains 'black b3 (list (list (LogicalLoc 1 2)
                                               (LogicalLoc 2 2))))
              (list (list (LogicalLoc 1 2) (LogicalLoc 2 2))))





;; A function that takes a Board and a chain(s)
;; and changes all those spots on the board to 'None.
(: chain-eraser : Board (Listof LogicalLoc) -> Board)
(define (chain-eraser board LLs)
  (match LLs
    ['() board]
    [(cons head tail)
     (chain-eraser
      (begin (bd-set! board head 'None) board) tail)]))

(check-expect (chain-eraser (vector (vector (Some 'white) (Some 'white))
                                    (vector (Some 'black) (Some 'black)))
                            (list (LogicalLoc 0 0) (LogicalLoc 0 1)))
              (vector (vector 'None 'None)
                      (vector (Some 'black) (Some 'black))))



;; A function that takes a list of lists and
;; appends them all into one giant list.
(: append-all : All (A) (Listof (Listof A)) -> (Listof A))
(define (append-all xs)
  (match xs
    ['() '()]
    [(cons head tail)
     (append head (append-all tail))]))


(check-expect (append-all (list (list 1 2) (list 3 4)))
              (list 1 2 3 4))




;; The function that applies the move.
(: apply-move : Go LogicalLoc -> Go)
(define (apply-move go logLoc)
  (match go
    [(Go board stone history LTP LTOC LTSC CP)
     (local {(define copy (board-copy board))
             (: opposite : Stone -> Stone)
             (define (opposite stone)
               (match stone
                 ['black 'white]
                 ['white 'black]))
             (define moved-board
               (begin (bd-set! board logLoc (Some stone)) board))
             (define moved-copy (board-copy moved-board))
             (: id-chain-help : LogicalLoc ->
                (Optional (Listof LogicalLoc)))
             (define (id-chain-help logicalLoc)
               (identify-chain moved-board
                             (list logicalLoc)
                            (list logicalLoc)))}
              
       (match (no-repetitions
               (no-nones
                (map id-chain-help
                     (cons logLoc (neighbors board logLoc)))))
            ['() (Go moved-board
                     (opposite stone) (cons moved-copy history)
                     (Some logLoc) '() '() 0)]
            [chains
             (match (append-all (color-chains (opposite stone) board chains))
               ['()
                (match (append-all (color-chains stone moved-board chains))
                  [chain (Go (chain-eraser moved-board chain)
                             (opposite stone)
                             (cons (chain-eraser moved-copy chain) history)
                             (Some logLoc)
                             '()
                             chain
                             0)])]
               [chain-col
                (Go (chain-eraser moved-board chain-col)
                    (opposite stone) (cons (chain-eraser moved-copy chain-col)
                                           history)
                    (Some logLoc)
                    chain-col
                    '()
                    0)])]))]))




(check-expect (apply-move (Go b4 'white '() 'None '() '() 0) (LogicalLoc 2 0))
              (Go
 (vector (vector 'None (Some 'white) 'None)
         (vector 'None (Some 'white) (Some 'black))
         (vector (Some 'white) (Some 'white) (Some 'black)))
 'black
 (list (vector (vector 'None (Some 'white) 'None)
               (vector 'None (Some 'white) (Some 'black))
               (vector (Some 'white) (Some 'white) (Some 'black))))
 (Some (LogicalLoc 2 0))
 (list (LogicalLoc 0 0) (LogicalLoc 1 0))
 '()
 0))

                                                                                                                                                                                






;; The function that returns a list of neighbors
;; that are empty intersections.
(: empty-neighbors : Board LogicalLoc
   -> (Listof LogicalLoc))
(define (empty-neighbors board logLoc)
     (local {(: empty? : LogicalLoc -> Boolean)
             (define (empty? logicalLoc)
               (match (bd-ref board logicalLoc)
                 ['None #t]
                 [_ #f]))}
       (filter empty? (neighbors board logLoc))))
    
(check-expect (empty-neighbors b4 (LogicalLoc 0 0)) (list (LogicalLoc 1 0)))


;; A helper-function that provies a list of logical locations
;; of the "unmarked" neighbors of the same color.
(: empty-unmarked-neighbors : Board LogicalLoc (Listof LogicalLoc)
   -> (Listof LogicalLoc))
(define (empty-unmarked-neighbors board logLoc marked)
  (match (empty-neighbors board logLoc)
    ['() '()]
    [List
  (local {(: isnt-in-marked? : LogicalLoc -> Boolean)
          (define (isnt-in-marked? logicalLoc)
            (not (is-in-list? logicalLoc marked)))}
    (filter isnt-in-marked? List))])) 


(check-expect (empty-unmarked-neighbors b4 (LogicalLoc 0 0)
                                        (list (LogicalLoc 0 0 )))
              (list (LogicalLoc 1 0)))



;; A function that identifies all chains of empty intersections.
;; This may include the location itself, even though it is not empty.
;; THIS ISSUE WILL BE RESOLVED LATER.
(: empty-chain : Board (Listof LogicalLoc) (Listof LogicalLoc)
   -> (Listof LogicalLoc))
(define (empty-chain board to-explore marked)
  (match to-explore
    ['() marked]
    [(cons head tail)
     (match (empty-unmarked-neighbors board head marked)
       [emp-unmk
        (local {(: isnt-head? : LogicalLoc -> Boolean)
                (define (isnt-head? logicalLoc)
                  (match* (logicalLoc head)
                    [(A A) #f]
                    [(_ _) #t]))}
          (empty-chain board
                          (append emp-unmk
                                  (filter isnt-head? to-explore))
                          (append emp-unmk marked)))])]))


(check-expect (empty-chain (vector (vector 'None (Some 'black))
                                   (vector 'None 'None))
                           (list (LogicalLoc 0 0)) (list (LogicalLoc 0 0)))
              (list (LogicalLoc 1 1) (LogicalLoc 1 0) (LogicalLoc 0 0)))
(check-expect (empty-chain (vector (vector 'None (Some 'black))
                                   (vector (Some 'black) 'None))
                           (list (LogicalLoc 0 0)) (list (LogicalLoc 0 0)))
              (list (LogicalLoc 0 0)))



;; Issue of marking itself?


;; A function that takes in a LogicalLoc and
;; a list of (Listof LogicalLocs) and confirms
;; whether the given LogicalLoc is in any of the
;; lists.
(: is-in-listoflists? : LogicalLoc (Listof (Listof LogicalLoc))
   -> Boolean)
(define (is-in-listoflists? logLoc LLLs)
  (match LLLs
    ['() #f]
    [(cons head tail)
     (if (is-in-list? logLoc head) #t
         (is-in-listoflists? logLoc tail))]))

(check-expect (is-in-listoflists? (LogicalLoc 0 0) '()) #f)





;; A function that goes through a board and returns each
;; empty chain. 
(: all-empty-chains : Board -> (Listof (Listof LogicalLoc)))
(define (all-empty-chains board)
(local {(define len (vector-length board))
          (: loop : Integer Integer (Listof (Listof LogicalLoc))
             -> (Listof (Listof LogicalLoc)))
          (define (loop col row acc)
            (cond [(= col len) acc]
                  [(= row len) (loop (add1 col) 0 acc)]
                  [else (match (bd-ref board (LogicalLoc col row))
                          ['None
                           (if (is-in-listoflists? (LogicalLoc col row) acc)
                               (loop col (add1 row) acc)
                               (loop col (add1 row)
                                     (cons (empty-chain board
                                                  (list (LogicalLoc col row))
                                            (list (LogicalLoc col row))) acc))
                                     )]
                        [_ (loop col (add1 row) acc) ])]))}
    (loop 0 0 '() )
  ))


(check-expect (all-empty-chains (vector (vector 'None (Some 'black))
                                (vector 'None 'None)))
              (list (list (LogicalLoc 1 1)
                          (LogicalLoc 1 0)
                          (LogicalLoc 0 0))))




;; A function that takes in an (empty) LogicalLoc and a stone,
;; and asks whether the Loc has a neighbor of the opposite
;; color.
(: opponent-stone? : LogicalLoc Stone Board -> Boolean)
(define (opponent-stone? logLoc stone board)
  (local {(: opposite : Stone -> Stone)
          (define (opposite stn)
            (match stn
              ['black 'white]
              ['white 'black]))
    (: helper : (Listof LogicalLoc) -> Boolean)
    (define (helper neighbs)
    (match neighbs 
      ['() #f]
      [(cons head tail)
       (match* ((bd-ref board head) (opposite stone))
         [((Some A) A) #t]
         [(_ _) (helper tail)])]))}
    (helper (neighbors board logLoc))))


(check-expect (opponent-stone? (LogicalLoc 1 1) 'white
                               (vector (vector 'None 'None)
                                       (vector 'None 'None))) #f)
(check-expect (opponent-stone? (LogicalLoc 1 1) 'white
                               (vector (vector 'None 'None)
                                       (vector (Some 'white) 'None))) #f)
(check-expect (opponent-stone? (LogicalLoc 1 1)
                               'white
                               (vector (vector 'None 'None)
                                       (vector (Some 'black) 'None))) #t)



;; A function that takes in a list of empty-chains and a color.
;; Each chain that is the color's territory becomes the number
;; of LogicalLocs in said chain. Those that aren't, are neutral,
;; and become zero.
(: territory-score : (Listof (Listof LogicalLoc)) Stone Board -> Integer)
(define (territory-score empty-chains stone board)
  (match empty-chains
    ['() 0]
    [(cons head tail)
     (local {(: help : (Listof LogicalLoc) -> Boolean)
             (define (help LLs)
               (match LLs
                 ['() #f]
                 [(cons h t)
                  (if (not (opponent-stone? h stone board))
                      (help t) #t)]))}
     (if (not (help head))
         (+ (length head)
            (territory-score tail stone board))
         (territory-score tail stone board)))]))

(check-expect (territory-score (list (list (LogicalLoc 0 0))) 'white b4) 1)


;; A function that calculates the number of stones that a
;; player has on the board.
(: number-of-stones : Board Stone -> Integer)
(define (number-of-stones board stone)
  (local {(define len (vector-length board))
          (: loop : Integer Integer -> Integer)
          (define (loop col row)
            (cond [(= col len) 0]
                  [(= row len) (loop (add1 col) 0)]
                  [else
                   (match* ((bd-ref board (LogicalLoc col row))
                          stone) 
                        [((Some A) A)
                         (add1 (loop col (add1 row)))]
                        [(_ _) (loop col (add1 row)) ])]))}
    (loop 0 0)))

(check-expect (number-of-stones b4 'black) 2)


;; A function that determines the outcome of a game.
(: outcome : Go -> Outcome)
(define (outcome go)
  (match go
    [(Go board _ _ _ _ _ _)
     (if (board=? board (empty-board (vector-length board)))
         (Outcome 0 0 'draw)
        (local {(define white-pts
                  (+ (number-of-stones board 'white)
                     (territory-score (all-empty-chains board)
                                      'white board)))
                (define black-pts
                  (+ (number-of-stones board 'black)
                     (territory-score (all-empty-chains board)
                                      'black board)))}
          (cond [(< white-pts black-pts)
                 (Outcome black-pts white-pts 'black)]
                [(< black-pts white-pts)
                 (Outcome black-pts white-pts 'white)]
                [else
                 (Outcome black-pts white-pts 'draw)])))]))

(check-expect (outcome (Go b3 'white '() 'None '() '() 0))
              (Outcome 2 7 'white))


  
;; A function that writes the outcome of a game.
(: who-won : Outcome -> String)
(define (who-won outcome)
  (match outcome
    [(Outcome bl wh winner)
     (match winner
       ['draw (string-append "The game ended in a draw. "
                            "Both black and white finished with "
              (number->string bl) " points.")]
       [_ (string-append (string-titlecase (symbol->string winner))
                         " won the game. Black has "
          (number->string bl) " points, whereas White has "
          (number->string wh) ".")])]))


(check-expect (who-won (Outcome 2 7 'white))
              "White won the game. Black has 2 points, whereas White has 7.")


;;; The function that initiates the World.
(: play : Integer BoardSpec -> World)
(define (play dim spec)
  (cond [(or (< dim 2)
             (not (valid-board-spec? spec))) (error "valid inputs please")]
  [else (big-bang
            (World spec (Go (empty-board dim) 'black
                            (list (empty-board dim))
                            'None '() '() 0) "Welcome to Go!"
                                                      0 0 'None) : World
    [on-tick react-to-tick 1/10]
    [to-draw draw]
    [on-key  react-to-key]
    [on-mouse react-to-click]
          )]))

(check-error (play 9 (BoardSpec 'tan 1 1 4)) "valid inputs please")





;;;;; SAVING/LOADING GAMES ;;;;;



;; A function that turns a board into a string.
(: board->string : Board -> String)
(define (board->string board)
  (local {(define len (vector-length board))
          (: loop : Integer Integer -> String)
          (define (loop col row)
            (cond [(= col len) ""]
                  [(and (= col (sub1 len))
                        (= row len)) (loop (add1 col) 0)]
                  [(= row len) (string-append "|"
                                              (loop (add1 col) 0))]
                  [else
                   (match (bd-ref board (LogicalLoc col row))
                     [(Some 'white)
                      (string-append "o" (loop col (add1 row)))]
                     [(Some 'black)
                      (string-append "*" (loop col (add1 row)))]
                     ['None
                      (string-append "_" (loop col (add1 row)))])]))}
    (loop 0 0)))


(check-expect (board->string (vector (vector (Some 'white) 'None 'None)
                         (vector 'None (Some 'black) 'None)
                         (vector (Some 'black) 'None 'None)))
              "o__|_*_|*__")



;; A function that converts a history list into a string.
(: history->string : (Listof Board) -> String)
(define (history->string boards)
  (match boards
    ['() ""]
    [(cons head '())
     (board->string head)]
    [(cons hd tl)
     (string-append
      (board->string hd) "!"
      (history->string tl))]))

(check-expect (history->string (list
                                (vector (vector (Some 'white) 'None 'None)
                         (vector 'None (Some 'black) 'None)
                         (vector (Some 'black) 'None 'None))
                         (vector (vector (Some 'white) 'None 'None)
                         (vector 'None 'None 'None)
                         (vector (Some 'black) 'None 'None))))
              "o__|_*_|*__!o__|___|*__")


;; A function that converts a Go into a string.
(: go->string : Go -> String)
(define (go->string go)
  (match go
    [(Go board next history _ _ _ CP)
     (match next
       ['white (string-append "o~"
                (board->string board) "~"
                (history->string history) "~"
                (number->string CP))]
       ['black (string-append "*~"
                (board->string board) "~"
                (history->string history) "~"
                (number->string CP))])]))


(check-expect (go->string (Go (vector (vector (Some 'white) 'None 'None)
                         (vector 'None (Some 'black) 'None)
                         (vector (Some 'black) 'None 'None)) 'white
                                                             (list
                                (vector (vector (Some 'white) 'None 'None)
                         (vector 'None (Some 'black) 'None)
                         (vector (Some 'black) 'None 'None))
                         (vector (vector (Some 'white) 'None 'None)
                         (vector 'None 'None 'None)
                         (vector (Some 'black) 'None 'None)))
                                                             'None '() '() 1))
             "o~o__|_*_|*__~o__|_*_|*__!o__|___|*__~1")


;; A function that takes a world and outputsa string.
(: world->string : World -> String)
(define (world->string w)
  (match w
    [(World _ go _ BT WT _)
     (string-append (number->string BT) "@"
                    (number->string WT) "@"
                    (go->string go))]))

(check-expect (world->string (World (BoardSpec 'moccasin 24 14 7)
                        (Go (vector (vector (Some 'white) 'None 'None)
                         (vector 'None (Some 'black) 'None)
                         (vector (Some 'black) 'None 'None)) 'white
                                                             (list
                                (vector (vector (Some 'white) 'None 'None)
                         (vector 'None (Some 'black) 'None)
                         (vector (Some 'black) 'None 'None))
                         (vector (vector (Some 'white) 'None 'None)
                         (vector 'None 'None 'None)
                         (vector (Some 'black) 'None 'None)))
                                                             'None '() '() 1)
                        "Hello"
                        10 10 'None))
              "10@10@o~o__|_*_|*__~o__|_*_|*__!o__|___|*__~1")




;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) 
        (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

(check-expect (string->integer "1") 1)


;; A function that takes a string and returns a column
;; on the board. This only takes in string representations
;; of columns as constructed before. Garbage in, garbage out.
(: string->column : String -> (Vectorof (Optional Stone)))
(define (string->column str)
  (local {(define len (string-length str))
          (: loop : Integer (Vectorof (Optional Stone)) ->
             (Vectorof (Optional Stone)))
          (define (loop n vector)
            (cond [(= n len) vector]
                  [else (match (string-ref str n)
                          [#\o (loop (add1 n)
                                     (begin (vector-set! vector n
                                                  (Some 'white)) vector)
                                     )]
                          [#\* (loop (add1 n)
                                     (begin (vector-set! vector n
                                                  (Some 'black))
                                            vector))]
                          [#\_ (loop (add1 n)
                                     (begin (vector-set! vector n
                                                  'None)
                                            vector))]
                          [_
                           (error "string->column: not a valid column string")]
                          )]))}
    (loop 0 (make-vector len (Some 'black)))))
          

(check-expect (string->column "__*")
              (vector 'None 'None (Some 'black)))
(check-error (string->column "__h")
             "string->column: not a valid column string")
(check-expect (string->column "___")
              '#(None None None))





;; A function that takes a string and outputs a board.
;; ISSUE: This could produce boards that are not N x N.
(: string->board : String -> Board)
(define (string->board str)
  (local {(define SS (string-split str "|"))
          (define len (length SS))
          (: correct-dim? : (Listof String) -> Boolean)
          (define (correct-dim? strings)
            (match strings
              ['() #t]
              [(cons head tail)
               (if (= len (string-length head))
                   (correct-dim? tail)
                   #f)]
              ))    
          (: loop : Integer (Listof String) Board -> Board)
          (define (loop n strings bd)
            (if (= n (length strings)) bd
                (loop (add1 n) strings
                      (begin (vector-set! bd n
                                   (string->column (list-ref strings n)))
                             bd))))}
    (if (correct-dim? SS)
    (loop 0 SS
          (empty-board len))
    (error "string->board : incorrect board dimensions"))))


(check-expect (string->board "___|_*_|_oo")
              (vector '#(None None None)
                      (vector 'None (Some 'black) 'None)
                      (vector 'None (Some 'white) (Some 'white))))
(check-error (string->board "____|___|*o_")
             "string->board : incorrect board dimensions")
(check-error (string->board "*___|___|o*|o")
             "string->board : incorrect board dimensions")



;; A function that takes in a string and produces a history list.
(: string->history : String -> (Listof Board))
(define (string->history str)
  (map string->board (string-split str "!")))

(check-expect (string->history
               "o__|_*_|*__!o__|___|*__")
              (list (vector (vector (Some 'white) 'None 'None)
                            (vector 'None (Some 'black) 'None)
                            (vector (Some 'black) 'None 'None))
                    (vector (vector (Some 'white) 'None 'None)
                            '#(None None None)
                            (vector (Some 'black) 'None 'None))))




;; A function that takes in a string and produces a Go.
(: string->go : String -> Go)
(define (string->go str)
  (match (string-split str "~")
    [(list next board history CP)
     (match next
       ["*" (Go (string->board board)
                'black
                (string->history history)
                'None '() '() (string->integer CP))]
       ["o" (Go (string->board board)
                'white
                (string->history history)
                'None '() '() (string->integer CP))]
       [_ (error "string->go : incorrect Go format")])]
    [_ (error "string->go : incorrect Go format")]))
                

(check-expect (string->go "o~o__|_*_|*__~o__|_*_|*__!o__|___|*__~1")
              (Go (vector (vector (Some 'white) 'None 'None)
                         (vector 'None (Some 'black) 'None)
                         (vector (Some 'black) 'None 'None)) 'white
                                                             (list
                                (vector (vector (Some 'white) 'None 'None)
                         (vector 'None (Some 'black) 'None)
                         (vector (Some 'black) 'None 'None))
                         (vector (vector (Some 'white) 'None 'None)
                         (vector 'None 'None 'None)
                         (vector (Some 'black) 'None 'None)))
                                                             'None '() '() 1))

(check-error (string->go "o~___|_*_|___~___|___|___")
              "string->go : incorrect Go format")
(check-error (string->go "x~___|_*_|___~___|___|___~1")
              "string->go : incorrect Go format")



;; A function that takes in a string, and returns a world.
(: string->world : BoardSpec String -> World)
(define (string->world spec str)
  (match (string-split str "@")
    [(list BT WT Go)
     (World spec (string->go Go) "Welcome back!"
            (string->integer BT)
            (string->integer WT)
            'None)]
    [_ (error "string->world : incorrect World format")] ))


(check-expect (string->world (BoardSpec 'moccasin 24 14 7)
                             "0@0@*~___|___|___~___|___|___~0")
              (World (BoardSpec 'moccasin 24 14 7)
                     (Go '#(#(None None None)
                            #(None None None)
                            #(None None None))
                         'black
                         '(#(#(None None None)
                             #(None None None)
                             #(None None None)))
                         'None '() '() 0) "Welcome back!" 0 0 'None))




;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (local
          {(define op : Output-Port (open-output-file path))}
          (begin (write-string (world->string w) op)
                 (close-output-port op)))
        (void))))


;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided BoardSpec to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : BoardSpec -> World)
(define (load-game bs)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
        (local
          {(define ip : Input-Port (open-input-file path))
           (define w : World
             (string->world bs (port->string (open-input-file path))))}
          (begin (close-input-port ip) w))
        (error "load-game: user cancelled"))))








;;;;;;;;; END OF SAVE/LOAD FUNCTIONS ;;;;;;;



;; The column-image of the numbers marking the
;; vertical axis.
(: column-num : Integer Integer BoardSpec -> Image)
(define (column-num dim I board)
  (match board
    [(BoardSpec b c m s)
     (if (<= 10 c)
         (if (> I 0)
             (place-image
              (text (number->string I) 10 "black")
              (/ c 2) (+ (* (- dim I) c) m)
              (column-num dim (- I 1) board))
             (rectangle c (+ (* dim c) m) "outline" 'white))
         (error "column-num: size of cell is too small for chosen font"))]))



;; The row-image of the alphabet marking
;; the horizontal axis.
(: row-alphabet : Integer Integer BoardSpec -> Image)
(define (row-alphabet dim I board)
  (match board
    [(BoardSpec b c m s)
     (if (<= 10 c)
         (if (< I dim)
             (place-image
              (text (list-ref (marked-x-axis (x-axis dim)) I) 10 "black")
              (+ (* I c) m) (/ c 2)
              (row-alphabet dim (+ I 1) board))
             (rectangle (+ (* (- dim 1) c) m m) c "outline" 'white))
         (error "row-alphabet: size of cell is too small for chosen font"))]))






;; A list of Latin characters in alphabetical order
;; (without the letter I).
(: alphabet : (Listof Char))
  (define alphabet
    (list #\A #\B #\C #\D #\E
          #\F #\G #\H #\J #\K
          #\L #\M #\N #\O #\P
          #\Q #\R #\S #\T #\U
          #\V #\W #\X #\Y #\Z))





;; Function that takes in the dimension, and makes
;; a list of logical locations on the x-axis
(: x-axis : Integer -> (Listof LogicalLoc))
(define (x-axis dim)
  (cond
    [(<= dim 0) '()]
    [else (cons (LogicalLoc (- dim 1) 0)
                (x-axis (- dim 1)))]))

(check-expect (x-axis 9)
              (list
 (LogicalLoc 8 0)
 (LogicalLoc 7 0)
 (LogicalLoc 6 0)
 (LogicalLoc 5 0)
 (LogicalLoc 4 0)
 (LogicalLoc 3 0)
 (LogicalLoc 2 0)
 (LogicalLoc 1 0)
 (LogicalLoc 0 0)))
              


;; A function that takes in a list of logical locations
;; and creates a list of alphabetically ascending strings without "1"
(: marked-x-axis : (Listof LogicalLoc) -> (Listof String))
(define (marked-x-axis lls)
  (reverse (map trim-1 (map logical->string lls))))

(check-expect (marked-x-axis (x-axis 9))
              (list "A" "B" "C" "D" "E" "F" "G" "H" "J"))


;; A function that deletes "1" out of a string
(: trim-1 : String -> String)
(define (trim-1 str)
  (string-trim str "1"))

(check-expect (trim-1 "AA1") "AA")



;; Example board 1.
(: b4 : Board)
(define b4
  (vector (vector (Some 'black) (Some 'white) 'None)
                      (vector (Some 'black) (Some 'white) (Some 'black))
                      (vector 'None (Some 'white) (Some 'black))))



;; Example board 2.
(: b3 : Board)
(define b3
  (vector (vector 'None 'None (Some 'white))
                      (vector 'None (Some 'white) (Some 'black))
                      (vector 'None (Some 'white) (Some 'black))))

;; Eyeball tests.
(grid 12 6)
(draw-circles 'black (physical-grid 10 (BoardSpec 'tan 10 10 5))
              5 (square 100 'solid 'tan))
(draw (World (BoardSpec 'tan 12 16 4) (Go (empty-board 19)
                                          'white '()
                                          'None '() '() 0) "Hello"
                                                           0 0 'None))
(column-num  19 19 (BoardSpec 'tan 10 10 4))
(row-alphabet  19 0 (BoardSpec 'tan 10 10 4))
(put-image (list (LogicalLoc 0 0)) "red" 2
   2 (BoardSpec 'moccasin 24 14 7) (square 10 'solid 'white))
(put-image-2 'None "red" 2
   2 (BoardSpec 'moccasin 24 14 7) #t (square 10 'solid 'white))
(play 19 (BoardSpec 'moccasin 24 14 7))



(test)
