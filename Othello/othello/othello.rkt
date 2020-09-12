#lang racket/gui

(require "funciones-aux.rkt")
(require "pos-disponibles.rkt")
(require 2htdp/batch-io)

#|
othello -> null
OBJ: imprimir el tablero del juego
PRE:
|#
(define (imprimir-othello othello)
  (cond
    [(empty? othello) (display "")]
    [else (imprimir-lista (car othello)) (display "\n") (imprimir-othello (cdr othello))]))

#|
lista -> number
OBJ: calcular el tamanno de una lista
PRE: 
|#
(define (len lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))]))

#|
num, lista -> boolean
OBJ: determinar si un numero esta en una lista
PRE: 
|#
(define (member? item seq)
  (sequence-ormap (lambda (x)
                    (equal? item x))
                  seq))

#|
lista -> null
OBJ: imprimir una lista dada
PRE:
|#
(define (imprimir-lista lista)
  (cond
    [(empty? lista) (display "")]
    [else (display (car lista)) (display " ") (imprimir-lista (cdr lista))]))

#|
lista -> lista
OBJ: eliminar los negativos de una lista
PRE:
|#
(define (eliminar-negativos lista)
  (cond
    [(empty? lista) '()]
    [(pair? (car lista)) (cons (car lista) (eliminar-negativos (cdr lista)))]
    [else (eliminar-negativos (cdr lista))]))

#|
lista -> lista
OBJ: eliminar los ceros de una lista
PRE:
|#
(define (eliminar-ceros lista)
  (cond
    [(empty? lista) '()]
    [(equal? (car lista) 0) (eliminar-ceros (cdr lista))]
    [else (cons (car lista) (eliminar-ceros (cdr lista)))]))

#|
othello -> boolean
OBJ: determinar si el othello esta lleno
PRE:
|#
(define (othello-lleno? othello)
  (cond
    [(empty? othello) true]
    [(member 0 (car othello)) false]
    [else (othello-lleno? (cdr othello))]))

#|
othello -> null
OBJ: determinar el ganador del juego
PRE:
|#
(define (determinar-ganador othello)
  (cond
    [(> (contar-pieza othello 2) (contar-pieza othello 1)) (display "Gana la ficha 2")]
    [(< (contar-pieza othello 2) (contar-pieza othello 1)) (display "Gana la ficha 1")]
    [else (display "Empate")]))

#|
lista -> lista
OBJ: eliminar las listas innecesarias
PRE: len lista > 9
|#
(define (delete-lists lista)
  (cond
    [(equal? (len lista) 8) lista]
    [(member ";|" (car lista)) (delete-lists (append (cdr lista) (list (car lista))))]
    [else (delete-lists (cdr lista))]))

#|
lista -> lista
OBJ: limpiar una lisra para su posterior lectura
PRE: len lista > 9
|#
(define (limpiar-lista-dos lista)
  (cond
    [(equal? (len lista) 8) lista]
    [(or (equal? "|" (car lista))(equal?  ";|" (car lista))) (limpiar-lista-dos (cdr lista))]
    [else (limpiar-lista-dos (append (cdr lista) (list (string->number(car lista)))))]))

#|
lista, number -> lista
OBJ: limpiar un othello completo
PRE: 
|#
(define (limpiar-othello lista rep)
  (cond
    [(equal? rep 0) lista]
    [else (limpiar-othello (append (cdr lista) (list (limpiar-lista-dos (car lista)))) (- rep 1))]))

#|
othello, numero -> numero
OBJ: contar el numero de apariciones de una pieza en el tablero
PRE:
|#
(define (contar-pieza othello pieza)
  (cond
    [(empty? othello) 0]
    [else (+ (contar-pieza-lista (car othello) pieza) (contar-pieza (cdr othello) pieza))]))

#|
lista, numero -> numero
OBJ: contar el numero de apariciones de una pieza
PRE: 
|#
(define (contar-pieza-lista lista pieza)
  (cond
    [(empty? lista) 0]
    [(equal? (car lista) pieza) (+ 1 (contar-pieza-lista (cdr lista) pieza))]
    [else (contar-pieza-lista (cdr lista) pieza)]))

#| 
othello, numero -> lista
OBJ: devolver una lista sin ceros y sin fichas sueltas
PRE: pieza = 1/2
|#
(define (limpiar-lista lista pieza)
  (cond
    [(and (equal? (length lista) 1) (equal? (car lista) pieza)) (list pieza)]
    [(equal? (length lista) 1) '()]
    [(and (equal? (car lista) (get-pieza-contraria pieza)) (equal? (car (cdr lista)) (get-pieza-contraria pieza))) (limpiar-lista (cdr lista) pieza)]
    [else (cons (car lista) (limpiar-lista (cdr lista) pieza))]
    ))

#|
othello -> boolean
OBJ: determinar si un tablero es valido, un tablero solo sera valido si contiene unicamente ceros, unos y doses
PRE: len(othello) = 8 x 8 and  -1 < othello[i][j] < 10
|#
(define (othello-valido? othello)
  (cond
    [(empty? othello) true]
    [(or (member 3 (car othello)) (member 4 (car othello)) (member 5 (car othello)) (member 6 (car othello)) (member 7 (car othello)) (member 8 (car othello)) (member 9 (car othello))) false]
    [else (othello-valido? (cdr othello))]))

#| 
othello, number -> lista
OBJ: devolver las apariciones de una pieza en el tablero
PRE: pieza = 1/2
|#
(define (apariciones-pieza othello pieza)
  (for*/list ([i 8]
              [j 8]
              #:when (equal? pieza (get-elem i j othello))) (list i j)))

#| 
othello, lista -> lista
OBJ: determinar las posibilidades que tiene una pieza en concreto
PRE:
|#
(define (pos-disponibles-pieza othello pos)
  (delete-voids (list
                 (pos-disponibles-fila othello pos)
                 (pos-disponibles-columna othello pos)
                 (pos-disponibles-diagonal othello pos)
                 (pos-disponibles-diagonal-secun othello pos))))

#| 
othello, lista -> lista
OBJ: determinar las posibilidades que tiene un jugador de colocar su proxima pieza
PRE:
|#
(define (posibilidades-jugador othello lista-apariciones-pieza)
  (cond
    [(empty? lista-apariciones-pieza) '()]
    [else (append (pos-disponibles-pieza othello (car lista-apariciones-pieza)) (posibilidades-jugador othello (cdr lista-apariciones-pieza)))]))

#| 
othello, lista -> null
OBJ: imprimir el tablero marcando las posibilidades que tiene un jugador de colocar su proxima ficha
PRE:
|#
(define (imprimir-tablero-posibles othello lista-posibles)
  (cond
    [(empty? lista-posibles) (imprimir-othello othello)]
    [else (imprimir-tablero-posibles (set-elem (car (car lista-posibles)) (car (cdr (car lista-posibles))) othello "X") (cdr lista-posibles))]))

#| 
lista -> boolean
OBJ: determinar si existe una posible expansion de ficha
PRE:
|#
(define (posible-expansion? lista pieza)
  (cond
    [(< (length lista) 3) false]
    [(and (equal? (list-ref lista 0) pieza) (equal? (list-ref lista 1) (get-pieza-contraria pieza)) (equal? (list-ref lista 2) pieza)) true]
    [else (posible-expansion? (cdr lista) pieza)]))

#| 
lista, numero -> numero
OBJ: calcular el numero de fichas que se pueden expandir
PRE:
|#
(define (puntuar-expansion lista numero)
  (cond
    [(equal? (len lista) 1) lista]
    [(or (equal? (list-ref lista 0) 0) (equal? (list-ref lista 0) (get-pieza-contraria numero))) (puntuar-expansion (cdr lista) numero)]
    [(and (equal? (list-ref lista 0) numero) (or (equal? (list-ref lista 1) numero) (equal? (list-ref lista 1) 0))) (puntuar-expansion (cdr lista) numero)]
    [else lista]))

#| 
othello, lista -> numero
OBJ: calcular la expansion de una posicion en fila
PRE: pos debe estar dentro de los limites del tablero
|#
(define (puntuar-expansion-fila othello pos)
  (cond
    [(member? 0 (reverse (puntuar-expansion (reverse (puntuar-expansion (get-fila (set-elem (car pos) (last pos) othello 1) (last pos)) 1)) 1))) 1]
    [else (len (reverse (puntuar-expansion (reverse (puntuar-expansion (get-fila (set-elem (car pos) (last pos) othello 1) (last pos)) 1)) 1)))]))

#| 
othello, lista -> numero
OBJ: calcular la expansion de una posicion en columna
PRE: pos debe estar dentro de los limites del tablero
|#
(define (puntuar-expansion-columna othello pos)
  (cond
    [(member? 0 (reverse (puntuar-expansion (reverse (puntuar-expansion (get-columna (set-elem (car pos) (last pos) othello 1) (car pos)) 1)) 1))) 1]
    [else (len (reverse (puntuar-expansion (reverse (puntuar-expansion (get-columna (set-elem (car pos) (last pos) othello 1) (car pos)) 1)) 1)))]))

#| 
othello, lista -> numero
OBJ: calcular la expansion de una posicion en diagonal
PRE: pos debe estar dentro de los limites del tablero
|#
(define (puntuar-expansion-diagonal othello pos)
  (cond
    [(member? 0 (reverse (puntuar-expansion (reverse (puntuar-expansion (get-diagonal (set-elem (car pos) (last pos) othello 1) (car pos) (last pos)) 1)) 1))) 1]
    [else (len (reverse (puntuar-expansion (reverse (puntuar-expansion (get-diagonal (set-elem (car pos) (last pos) othello 1) (car pos) (last pos)) 1)) 1)))]))

#| 
othello, lista -> numero
OBJ: calcular la expansion de una posicion en diagonal secundaria
PRE: pos debe estar dentro de los limites del tablero
|#
(define (puntuar-expansion-diagonal-secundaria othello pos)
  (cond
    [(member? 0 (reverse (puntuar-expansion (reverse (puntuar-expansion (get-diagonal-secundaria (set-elem (car pos) (last pos) othello 1) (car pos) (last pos)) 1)) 1))) 1]
    [else (len (reverse (puntuar-expansion (reverse (puntuar-expansion (get-diagonal-secundaria (set-elem (car pos) (last pos) othello 1) (car pos) (last pos)) 1)) 1)))]))

#| 
othello, lista -> boolean
OBJ: determina cual es la mejor expansion de una posicion
PRE: pos debe estar dentro de los limites del tablero
|#
(define (puntuar-expansion-pos othello pos)
  (cond
    [(and (> (puntuar-expansion-fila othello pos) (puntuar-expansion-columna othello pos)) (> (puntuar-expansion-fila othello pos) (puntuar-expansion-diagonal othello pos)) (> (puntuar-expansion-fila othello pos) (puntuar-expansion-diagonal-secundaria othello pos))) (list (puntuar-expansion-fila othello pos) "fila" pos)]
    [(and (> (puntuar-expansion-columna othello pos) (puntuar-expansion-fila othello pos)) (> (puntuar-expansion-columna othello pos) (puntuar-expansion-diagonal othello pos)) (> (puntuar-expansion-columna othello pos) (puntuar-expansion-diagonal-secundaria othello pos))) (list (puntuar-expansion-columna othello pos) "columna" pos)]
    [(and (> (puntuar-expansion-diagonal othello pos) (puntuar-expansion-columna othello pos)) (> (puntuar-expansion-diagonal othello pos) (puntuar-expansion-fila othello pos)) (> (puntuar-expansion-diagonal othello pos) (puntuar-expansion-diagonal-secundaria othello pos))) (list (puntuar-expansion-diagonal othello pos) "diagonal" pos)]
    [(and (> (puntuar-expansion-diagonal-secundaria othello pos) (puntuar-expansion-fila othello pos)) (> (puntuar-expansion-diagonal-secundaria othello pos) (puntuar-expansion-columna othello pos)) (> (puntuar-expansion-diagonal-secundaria othello pos) (puntuar-expansion-diagonal othello pos))) (list (puntuar-expansion-diagonal-secundaria othello pos) "diagonal-secundaria" pos)]))

#| 
lista, lista, numero -> lista
OBJ: expandir una pieza  alo largo de una lista
PRE: lista-aux = '() and pieza = 1/2
|#
(define (mejor-pos-expandir othello lista-max lista-posibles)
  (cond
    [(empty? lista-posibles) lista-max]
    ;[(display (puntuar-expansion-pos othello (car lista-posibles))) (display "\n")(mejor-pos-expandir othello lista-max (cdr lista-posibles))]
    [(> (car (puntuar-expansion-pos othello (car lista-posibles))) (car lista-max)) (mejor-pos-expandir othello (puntuar-expansion-pos othello (car lista-posibles)) (cdr lista-posibles))]
    [else (mejor-pos-expandir othello lista-max (cdr lista-posibles))]))

#| 
lista, lista, numero -> lista
OBJ: expandir una pieza  alo largo de una lista
PRE: lista-aux = '() and pieza = 1/2
|#
(define (expandir-pieza lista-aux lista pieza)
  (cond
    [(empty? lista) lista-aux]
    [(and (equal? (car lista) (get-pieza-contraria pieza)) (equal? (last lista-aux) pieza) (and (> (length lista) 1)) (or (equal? (car (cdr lista)) (get-pieza-contraria pieza)) (equal? (car (cdr lista)) pieza))) (expandir-pieza (append lista-aux (list pieza)) (cdr lista) pieza)]
    [(or (equal? (car lista) 0) (equal? (car lista) pieza) (equal? (car lista) (get-pieza-contraria pieza))) (expandir-pieza (append lista-aux (list (car lista))) (cdr lista) pieza)]
    [else (expandir-pieza lista-aux (cdr lista) pieza)]))

#| 
othello, lista, string -> othello
OBJ: realiza la expansion del jugador
PRE:
|#
(define (realizar-expansion othello pos)
  (cond
    [(posible-expansion? (limpiar-lista (get-fila othello (last pos)) (get-elem (car pos) (last pos) othello)) (get-elem (car pos) (last pos) othello)) (set-fila othello (last pos) (cdr (expandir-pieza '(0) (get-fila othello (last pos)) (get-elem (car pos) (last pos) othello))))]
    [(posible-expansion? (limpiar-lista (get-columna othello (car pos)) (get-elem (car pos) (last pos) othello)) (get-elem (car pos) (last pos) othello)) (set-columna othello (car pos) (cdr (expandir-pieza '(0) (get-columna othello (car pos)) (get-elem (car pos) (last pos) othello))))]
    [(posible-expansion? (limpiar-lista (get-diagonal othello (car pos) (last pos)) (get-elem (car pos) (last pos) othello)) (get-elem (car pos) (last pos) othello)) (set-diagonal othello (car (get-primera-pos-diagonal (car pos) (last pos))) (last (get-primera-pos-diagonal (car pos) (last pos))) (cdr (expandir-pieza '(0) (get-diagonal othello (car pos) (last pos)) (get-elem (car pos) (last pos) othello))))]
    [(posible-expansion? (limpiar-lista (get-diagonal-secundaria othello (car pos) (last pos)) (get-elem (car pos) (last pos) othello)) (get-elem (car pos) (last pos) othello)) (set-diagonal-secundaria othello (car (get-primera-pos-diagonal-secun (car pos) (last pos))) (last (get-primera-pos-diagonal-secun (car pos) (last pos))) (cdr (expandir-pieza '(0) (get-diagonal-secundaria othello (car pos) (last pos)) (get-elem (car pos) (last pos) othello))))]
    [else (display "No hay expansion?")]))

#| 
othello, lista, string -> othello
OBJ: realiza la expansion de la maquina
PRE:
|#
(define (realizar-expansion-minmax othello pos direccion)
  (cond
    [(equal? direccion "fila") (set-fila othello (last pos) (cdr (expandir-pieza '(0) (get-fila othello (last pos)) (get-elem (car pos) (last pos) othello))))]
    [(equal? direccion "columna") (set-columna othello (car pos) (cdr (expandir-pieza '(0) (get-columna othello (car pos)) (get-elem (car pos) (last pos) othello))))]
    [(equal? direccion "diagonal") (set-diagonal othello (car (get-primera-pos-diagonal (car pos) (last pos))) (last (get-primera-pos-diagonal (car pos) (last pos))) (cdr (expandir-pieza '(0) (get-diagonal othello (car pos) (last pos)) (get-elem (car pos) (last pos) othello))))]
    [(equal? direccion "diagonal-secundaria") (set-diagonal-secundaria othello (car (get-primera-pos-diagonal-secun (car pos) (last pos))) (last (get-primera-pos-diagonal-secun (car pos) (last pos))) (cdr (expandir-pieza '(0) (get-diagonal-secundaria othello (car pos) (last pos)) (get-elem (car pos) (last pos) othello))))]))

#| 
othello, lista, numero -> othello
OBJ: colocar una pieza en una determinada posicion del tablero
PRE: pieza = 1/2
|#
(define (colocar-pieza othello pos pieza)
  (set-elem (car pos) (last pos) othello pieza))

#| 
othello, numero -> othello
OBJ: realiza el turno del jugador
PRE: jugador = 1/2
|#
(define (realizar-turno-jugador othello jugador)
  (imprimir-tablero-posibles othello (eliminar-negativos (posibilidades-jugador othello (apariciones-pieza othello jugador))))
  (display "Determina la columna donde colocar tu pieza")
  (define columna (read-line (current-input-port) 'any))
  (display "Determina la fila donde colocar tu pieza")
  (define fila (read-line (current-input-port) 'any))
  (display "----------------------- \n")
  (realizar-expansion (colocar-pieza othello (list (string->number columna) (string->number fila)) jugador) (list (string->number columna) (string->number fila))))

#| 
othello-> othello
OBJ: realiza el turno de la maquina
PRE:
|#
(define (realizar-turno-maquina othello)
  (realizar-expansion-minmax (colocar-pieza othello (last (mejor-pos-expandir othello (list 0 "" '(0 0)) (eliminar-negativos (posibilidades-jugador othello (apariciones-pieza othello 1))))) 1) (last (mejor-pos-expandir othello (list 0 "" '(0 0)) (eliminar-negativos (posibilidades-jugador othello (apariciones-pieza othello 1))))) (second (mejor-pos-expandir othello (list 0 "" '(0 0)) (eliminar-negativos (posibilidades-jugador othello (apariciones-pieza othello 1)))))))

#| 
othello, numero -> othello
OBJ: representa una partida humano vs humano
PRE: turno-jugador = 1/2
|#
(define (jugar-othello-humano othello turno-jugador)
  (cond
    [(othello-lleno? othello) (determinar-ganador othello)]
    [(equal? turno-jugador 1) (display "JUGADOR 1 \n")(jugar-othello-humano (realizar-turno-jugador othello turno-jugador) 2)]
    [else (display "JUGADOR 2 \n") (jugar-othello-humano (realizar-turno-jugador othello turno-jugador) 1)]))

#| 
othello, numero -> othello
OBJ: representa una partida humano vs maquina
PRE:
|#
(define (jugar-othello-maquina othello turno)
  (cond
    [(othello-lleno? othello) (determinar-ganador othello)]
    [(equal? turno 1) (display "MAQUINA \n") (jugar-othello-maquina (realizar-turno-maquina othello) 2)]
    [else (display "JUGADOR \n") (jugar-othello-maquina (realizar-turno-jugador othello 2) 1)]))

#|
nil -> nil
OBJ: interfaz de usuario
PRE: 
|#
(define (iniciar-othello)
  (display "-----BIENVENIDO AL JUEGO DE OTHELLO----- \n")
  (display "Que modo de juego deseas jugar humano vs humano o humano vs maquina \n")
  (define modo-juego (read-line (current-input-port) 'any))
  (display "Indique el tablero con el que quiere comenzar a jugar (othello1, othello2, ..., othello5)")
  (define tablero (read-line (current-input-port) 'any))
  (cond
    [(equal? modo-juego "humano vs humano") (jugar-othello-humano (limpiar-othello (delete-lists (read-words/line (string-append "./test/" tablero ".txt"))) 8) 2)]
    [(equal? modo-juego "humano vs maquina") (jugar-othello-maquina (limpiar-othello (delete-lists (read-words/line (string-append "./test/" tablero ".txt"))) 8) 2)]
    [else "Debe seleccionar un modo de juego correcto"]))













(define test1
  '((2 2 2 2 2 2 0 2)
    (0 0 0 2 2 0 0 0)
    (2 0 2 0 0 2 2 0)
    (0 2 0 2 0 2 2 1)
    (2 0 2 2 2 0 1 2)
    (0 0 1 0 0 0 0 0)
    (1 0 0 0 2 0 0 0)
    (0 0 0 0 0 0 0 0)))

;(eliminar-negativos (posibilidades-jugador test1 (apariciones-pieza test1 1)))
;(imprimir-tablero-posibles test1 (eliminar-negativos (posibilidades-jugador test1 (apariciones-pieza test1 1))))

;(mejor-pos-expandir test1 (list 0 "" '(0 0)) (eliminar-negativos (posibilidades-jugador test1 (apariciones-pieza test1 1))))
;(imprimir-othello (realizar-expansion-minmax (colocar-pieza test1 (last (mejor-pos-expandir test1 (list 0 "" '(0 0)) (eliminar-negativos (posibilidades-jugador test1 (apariciones-pieza test1 1))))) 1) (last (mejor-pos-expandir test1 (list 0 "" '(0 0)) (eliminar-negativos (posibilidades-jugador test1 (apariciones-pieza test1 1))))) (second (mejor-pos-expandir test1 (list 0 "" '(0 0)) (eliminar-negativos (posibilidades-jugador test1 (apariciones-pieza test1 1)))))))
;(display (realizar-turno-maquina test1))


;(jugar-othello-maquina test1 2)

;(realizar-turno-jugador test1 2)
;(jugar-othello test1 2)


(iniciar-othello)






