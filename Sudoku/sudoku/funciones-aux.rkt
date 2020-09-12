#lang racket

(provide (all-defined-out))

#|
numero, numero -> numero
OBJ: devolver el valor de una posicion del sudoku
PRE: 0 < x-coor and y-coor < 9
|#
(define (get-elem x-coor y-coor sudoku)
  (list-ref(list-ref sudoku x-coor) y-coor))

#|
numero, numero, lista -> numero
OBJ: obtiene un numero de una lista dada
PRE: indice < len(lista)
|#
(define (get-num-columna indice lista)
  (cond
    [(equal? indice 0) (car lista)]
    [else (get-num-columna (- indice 1) (cdr lista))]))

#|
numero, numero, sudoku -> lista
OBJ: obtener una columna determinada del sudoku
PRE: columna < len(lista)
|#
(define (get-columna columna sudoku)
  (cond
    [(empty? sudoku) '()]
    [else (cons (get-num-columna columna (car sudoku)) (get-columna columna (cdr sudoku)))]))


#|
sudoku, numero, numero -> lista
OBJ: devolver las filas a partir de un numero
PRE: fila and columna < 9
|#
(define(get-filas sudoku fila columna)
  (cond
    [(< columna fila) (cons (list-ref sudoku columna) (get-filas sudoku (+ columna 1) fila) )]))

#|
sudoku, numero, numero -> lista
OBJ: obtener un cuadrante del sudoku
PRE: num-fila y num-columna deben ser 0 o 1 o 2
|#
(define (get-cuadrante sudoku num-fila num-columna)
  (list (list-ref (car(get-filas sudoku (+ (* num-fila 3) 3) (* num-fila 3))) (* num-columna 3))
        (list-ref (car(get-filas sudoku (+ (* num-fila 3) 3) (* num-fila 3))) (+ (* num-columna 3) 1))
        (list-ref (car(get-filas sudoku (+ (* num-fila 3) 3) (* num-fila 3))) (+ (* num-columna 3) 2))
        (list-ref (car(get-filas sudoku (+ (* num-fila 3) 3) (+ (* num-fila 3) 1))) (* num-columna 3))
        (list-ref (car(get-filas sudoku (+ (* num-fila 3) 3) (+ (* num-fila 3) 1))) (+ (* num-columna 3) 1))
        (list-ref (car(get-filas sudoku (+ (* num-fila 3) 3) (+ (* num-fila 3) 1))) (+ (* num-columna 3) 2))
        (list-ref (car(get-filas sudoku (+ (* num-fila 3) 3) (+ (* num-fila 3) 2))) (* num-columna 3))
        (list-ref (car(get-filas sudoku (+ (* num-fila 3) 3) (+ (* num-fila 3) 2))) (+ (* num-columna 3) 1))
        (list-ref (car(get-filas sudoku (+ (* num-fila 3) 3) (+ (* num-fila 3) 2))) (+ (* num-columna 3) 2))))