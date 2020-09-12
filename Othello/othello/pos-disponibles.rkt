#lang racket

(require "funciones-aux.rkt")

(provide (all-defined-out))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su fila por la izquierda
PRE:
|#
(define (pos-disponibles-fila-izq othello pos)
  (cond
    [(equal? -1 (car pos)) -1]
    [(equal? 0 (get-elem (car pos) (last pos) othello)) pos]
    [else (pos-disponibles-fila-izq othello (list (- (car pos) 1) (last pos)))]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su fila por la derecha
PRE:
|#
(define (pos-disponibles-fila-der othello pos)
  (cond
    [(equal? 8 (car pos)) -1]
    [(equal? 0 (get-elem (car pos) (last pos) othello)) pos]
    [else (pos-disponibles-fila-der othello (list (+ (car pos) 1) (last pos)))]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su fila
PRE:
|#
(define (pos-disponibles-fila othello pos)
  (cond
    [(equal? (car pos) 0)
     (cond
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (+ (car pos) 1) (last pos) othello)) (list (pos-disponibles-fila-der othello pos))])]
    [(equal? (car pos) 7)
     (cond
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (- (car pos) 1) (last pos) othello)) (list (pos-disponibles-fila-izq othello pos))])]
    [else
     (cond
       [(and (equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (- (car pos) 1) (last pos) othello)) (equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (+ (car pos) 1) (last pos) othello))) (list (pos-disponibles-fila-izq othello pos) (pos-disponibles-fila-der othello pos))]
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (- (car pos) 1) (last pos) othello)) (list (pos-disponibles-fila-izq othello pos))]
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (+ (car pos) 1) (last pos) othello)) (list (pos-disponibles-fila-der othello pos))])]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su columna por arriba
PRE:
|#
(define (pos-disponibles-columna-arriba othello pos)
  (cond
    [(equal? -1 (last pos)) -1]
    [(equal? 0 (get-elem (car pos) (last pos) othello)) pos]
    [else (pos-disponibles-columna-arriba othello (list (car pos) (- (last pos) 1)))]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su columna por abajo
PRE:
|#
(define (pos-disponibles-columna-abajo othello pos)
  (cond
    [(equal? 8 (last pos)) -1]
    [(equal? 0 (get-elem (car pos) (last pos) othello)) pos]
    [else (pos-disponibles-columna-abajo othello (list (car pos) (+ (last pos) 1)))]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su columna
PRE:
|#
(define (pos-disponibles-columna othello pos)
  (cond
    [(equal? 0 (last pos))
     (cond
      [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (car pos) (+ (last pos) 1) othello)) (list (pos-disponibles-columna-abajo othello pos))])]
    [(equal? 7 (last pos))
     (cond
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (car pos) (- (last pos) 1) othello)) (list (pos-disponibles-columna-arriba othello pos))])]
    [else
     (cond
       [(and (equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (car pos) (- (last pos) 1) othello)) (equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (car pos) (+ (last pos) 1) othello))) (list (pos-disponibles-columna-arriba othello pos) (pos-disponibles-columna-abajo othello pos))]
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (car pos) (+ (last pos) 1) othello)) (list (pos-disponibles-columna-abajo othello pos))]
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (car pos) (- (last pos) 1) othello)) (list (pos-disponibles-columna-arriba othello pos))])]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su diagonal principal por abajo/derecha
PRE:
|#
(define (pos-disponibles-diag-abajo-der othello pos)
  (cond
    [(or (equal? 8 (car pos)) (equal? 8 (last pos))) -1]
    [(equal? 0 (get-elem (car pos) (last pos) othello)) pos]
    [else (pos-disponibles-diag-abajo-der othello (list (+ (car pos) 1) (+ (last pos) 1)))]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su diagonal principal por arriba/izquierda
PRE:
|#
(define (pos-disponibles-diag-arriba-izq othello pos)
  (cond
    [(or (equal? -1 (car pos)) (equal? -1 (last pos))) -1]
    [(equal? 0 (get-elem (car pos) (last pos) othello)) pos]
    [else (pos-disponibles-diag-arriba-izq othello (list (- (car pos) 1) (- (last pos) 1)))]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su diagonal principal 
PRE:
|#
(define (pos-disponibles-diagonal othello pos)
  (cond
    [(or (and (equal? 7 (car pos)) (equal? 0 (last pos))) (and (equal? 0 (car pos)) (equal? 7 (last pos)))) -1]
    [(or (equal? 0 (car pos)) (equal? 0 (last pos)))
     (cond
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (+ (car pos) 1) (+ (last pos) 1) othello)) (list (pos-disponibles-diag-abajo-der othello pos))])]
    [(or (equal? 7 (car pos)) (equal? 7 (last pos)))
     (cond
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (- (car pos) 1) (- (last pos) 1) othello)) (list (pos-disponibles-diag-arriba-izq othello pos))])]
    [else
     (cond
        [(and (equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (- (car pos) 1) (- (last pos) 1) othello)) (equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (+ (car pos) 1) (+ (last pos) 1) othello))) (list (pos-disponibles-diag-arriba-izq othello pos) (pos-disponibles-diag-abajo-der othello pos))]
        [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (+ (car pos) 1) (+ (last pos) 1) othello)) (list (pos-disponibles-diag-abajo-der othello pos))]
        [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (- (car pos) 1) (- (last pos) 1) othello)) (list (pos-disponibles-diag-arriba-izq othello pos))])]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su diagonal secundaria por abajo/izquierda
PRE:
|#
(define (pos-disponibles-diag-secun-abajo-izq othello pos)
  (cond
    [(or (equal? -1 (car pos)) (equal? 8 (last pos))) -1]
    [(equal? 0 (get-elem (car pos) (last pos) othello)) pos]
    [else (pos-disponibles-diag-secun-abajo-izq othello (list (- (car pos) 1) (+ (last pos) 1)))]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su diagonal secundaria por arriba/derecha
PRE:
|#
(define (pos-disponibles-diag-secun-arriba-der othello pos)
  (cond
    [(or (equal? 8 (car pos)) (equal? -1 (last pos))) -1]
    [(equal? 0 (get-elem (car pos) (last pos) othello)) pos]
    [else (pos-disponibles-diag-secun-arriba-der othello (list (+ (car pos) 1) (- (last pos) 1)))]))

#| 
othello, lista -> lista
OBJ: determinar las posiciones disponibles de una pieza a lo largo de su diagonal secundaria
PRE:
|#
(define (pos-disponibles-diagonal-secun othello pos)
  (cond
    [(or (and (equal? 7 (car pos)) (equal? 7 (last pos))) (and (equal? 0 (car pos)) (equal? 0 (last pos)))) -1]
    [(or (equal? 7 (car pos)) (equal? 0 (last pos)))
     (cond
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (- (car pos) 1) (+ (last pos) 1) othello)) (list (pos-disponibles-diag-secun-abajo-izq othello pos))])]
    [(or (equal? 0 (car pos)) (equal? 7 (last pos)))
     (cond
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (+ (car pos) 1) (- (last pos) 1) othello)) (list (pos-disponibles-diag-secun-arriba-der othello pos))])]
    [else
     (cond
       [(and (equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (- (car pos) 1) (+ (last pos) 1) othello)) (equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (+ (car pos) 1) (- (last pos) 1) othello))) (list (pos-disponibles-diag-secun-arriba-der othello pos) (pos-disponibles-diag-secun-abajo-izq othello pos))]
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (- (car pos) 1) (+ (last pos) 1) othello)) (list (pos-disponibles-diag-secun-abajo-izq othello pos))]
       [(equal? (get-pieza-contraria (get-elem (car pos) (last pos) othello)) (get-elem (+ (car pos) 1) (- (last pos) 1) othello)) (list (pos-disponibles-diag-secun-arriba-der othello pos))])]))


