#lang racket/gui

; LOGIC
(struct Pos (x y) #:transparent)

(define board-size 8)
(define radix 10)
(define board (build-vector board-size (λ (n) (build-vector board-size (λ (m) 0)))))
(define board-list (build-list board-size (λ (n) (build-list board-size (λ (m) (Pos m n))))))
(define empties (apply append board-list))
(define (set-cell x y v [b board]) (vector-set! (vector-ref b y) x v))
(define (get-cell x y [b board]) (vector-ref (vector-ref b y) x))
(define (pset p v) 
  (when (not (equal? v (pget p)))
    (when (zero? (pget p)) (set! empties (remove p empties)))
    (set-cell (Pos-x p) (Pos-y p) v)
    (when (zero? v) (set! empties (cons p empties)))))
(define (pget p) (get-cell (Pos-x p) (Pos-y p)))
(define (print-board) (for-each displayln (vector->list board)))

(define (spawn) 
  (if (empty? empties)
      (displayln "Game over") ; turn this into an actual check
      (let ([e (list-ref empties (random (length empties)))])
        (pset e 2))))

(define (slidepush p slider)
  (let ([p1 (slider p)])
    (if (or (< (Pos-x p1) 0) (>= (Pos-x p1) board-size)
            (< (Pos-y p1) 0) (>= (Pos-y p1) board-size)
            (> (pget p1) 0))
        p
        (slidepush p1 slider))))

(define (slide d)
  (let ([travers (match d
                   ['up (drop board-list 1)]
                   ['down (drop (reverse board-list) 1)]
                   ['left (map (λ (bl) (drop bl 1)) board-list)]
                   ['right (map (λ (bl) (drop (reverse bl) 1)) board-list)])]
        [slider (match d
                  ['up (λ (p) (Pos (Pos-x p) (sub1 (Pos-y p))))]
                  ['down (λ (p) (Pos (Pos-x p) (add1 (Pos-y p))))]
                  ['left (λ (p) (Pos (sub1 (Pos-x p)) (Pos-y p)))]
                  ['right (λ (p) (Pos (add1 (Pos-x p)) (Pos-y p)))])])
    (map 
     (λ (r) 
       (map 
        (λ (p) 
          (let ([pn (slidepush p slider)])
            (when (not (equal? pn p))
              (pset pn (pget p))
              (pset p 0))))
        r)) 
     travers)))

(define (shift d)
  (slide d)
  ; Collapse
  (let ([travers (match d
                   ['up (drop-right board-list 1)]
                   ['down (drop-right (reverse board-list) 1)]
                   ['left (map (λ (bl) (drop-right bl 1)) board-list)]
                   ['right (map (λ (bl) (drop-right (reverse bl) 1)) board-list)])]
        [scouter (match d
                   ['up (λ (p) (Pos (Pos-x p) (add1 (Pos-y p))))]
                   ['down (λ (p) (Pos (Pos-x p) (sub1 (Pos-y p))))]
                   ['left (λ (p) (Pos (add1 (Pos-x p)) (Pos-y p)))]
                   ['right (λ (p) (Pos (sub1 (Pos-x p)) (Pos-y p)))])])
    (map (λ (r)
           (map (λ (p)
                  (let* ([p1 (scouter p)]
                         [pv (pget p)]
                         [p1v (pget p1)])
                    (when (and (not (zero? pv)) (equal? pv p1v))
                      (pset p (* 2 pv))
                      (pset p1 0))))
                r))
         travers))
  (slide d))

; COLOUR
(define colours (make-hash `((0 . ,(make-color 200 200 200)))))

(define (get-clr n)
  (if (hash-has-key? colours n)
      (hash-ref colours n)
      (let ([c (clr n)])
        (hash-set! colours n c)
        c)))

(define (wrapmod n m)
  (abs (- (modulo n (- (* 2 m) 2)) m -1)))
(define (clr n)
  #;(let* ([l2n (inexact->exact (round (/ (log n) (log 2))))]
           [flr 64] [cei (- 256 16 flr)]
           [r (+ flr (modulo (* l2n 89) cei))] 
           [g (+ flr (modulo (* l2n 11) cei))] 
           [b (+ flr (modulo (* l2n 19) cei))])
      (make-color r g b))
  (let* ([l2n (inexact->exact (round (/ (log n) (log 2))))]
         [flr 127] [cei (- 256 32 flr)]
         [r (+ flr (wrapmod (* l2n 21) cei))] 
         [g (+ flr (- cei (wrapmod (* l2n 13) cei)))] 
         [b (+ flr (wrapmod (* l2n 7) cei))])
    (make-color r g b)))

; DRAW
(define (digits n)
  (inexact->exact (ceiling (/ (log n) (log radix)))))

(define (get-fon cellsize dgt)
  (let* ([fact (cond [(equal? dgt 0) 2]
                     [(equal? dgt 1) 2]
                     [(equal? dgt 2) 5/2]
                     [(equal? dgt 3) 11/4]
                     [else (* 2/3 dgt)])]
         [siz (floor (/ cellsize fact))])
    (send the-font-list find-or-create-font 
          siz "Trebuchet MS" 'modern 'normal 'normal #f 'smoothed #t)))

(define (draw-grid dc)
  (let*-values
      ([(width height) (send dc get-size)]
       [(smaller-bound) (if (< width height) width height)]
       [(cellsize) (/ smaller-bound (+ 1/16 board-size))]
       [(cellborder) (/ cellsize 16)]
       [(startx) (+ (/ cellsize 32) (/ (- width smaller-bound) 2))]
       [(starty) (+ (/ cellsize 32) (/ (- height smaller-bound) 2))])
    (send dc set-background (make-color 240 240 240))
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-pen (send dc get-background) cellborder 'solid)
    (send dc set-text-foreground "white")
    (map (λ (r)
           (map (λ (p)
                  (let* ([px (+ startx (* cellsize (Pos-x p)))] 
                         [py (+ starty (* cellsize (Pos-y p)))]
                         [pv (pget p)])
                    (send dc set-brush (get-clr pv) 'solid)
                    (send dc draw-rounded-rectangle px py cellsize cellsize cellborder)
                    (when (> pv 0)
                      (send dc set-font 
                            (get-fon cellsize (digits pv)))
                      (let*-values ([(ps) (number->string pv radix)]
                                    [(w h bas ver) (send dc get-text-extent ps)]
                                    [(tx) (+ px (- (/ cellsize 2) (/ w 2)))]
                                    [(ty) (+ py (- (/ cellsize 2) (/ h 2)))])
                        (send dc draw-text ps tx ty)))))
                r))
         board-list)
    ))

; FRAME
(define frame (new frame% [label "2048"] [width 900] [height 920]))
(define canvas-p (new vertical-panel% [parent frame] [alignment '(center center)]))
(define horizon (new horizontal-panel% [parent canvas-p] [alignment '(center center)]
                     [stretchable-height #f]))

; CANVAS
(define my-canvas%
  (class canvas%
    (define/override (on-char ch)
      (let ([k (send ch get-key-code)])
        (when (member k '(up down left right))
          (shift k)
          (spawn)))
      (send canvas refresh))
    (super-new)))

(define canvas (new my-canvas% [parent canvas-p]
                    [paint-callback (λ (canvas dc) (draw-grid dc))]))

(spawn)
#|(define next-power
  (let ([p 1])
    (λ () (set! p (* 2 p))
      p)))
(for-each (λ (p) (pset p (next-power))) (apply append board-list))|#
(send frame show #t)
