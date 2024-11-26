data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud ArbolVacio = 0
longitud (Raiz _ izq der) = 1 + longitud izq + longitud der 

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz _ izq der) = 1 + max (profundidad izq) (profundidad der)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho ArbolVacio = 0
ancho ( Raiz _ ArbolVacio ArbolVacio ) = 1
ancho (Raiz _ izq der )=  ancho izq + ancho der 

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz x izq der) InOrder = recorrido izq InOrder ++ [x] ++ recorrido der InOrder
recorrido (Raiz x izq der) PreOrder = [x] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz x izq der) PosOrder = recorrido izq PosOrder ++ recorrido der PosOrder ++ [x]

-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz raiz ArbolVacio ArbolVacio) = [[raiz]]
niveles (Raiz raiz arbolIzquierdo arbolDerecho) = [[raiz]] ++
	combinarNiveles (niveles arbolIzquierdo) (niveles arbolDerecho)

combinarNiveles :: [[a]] -> [[a]] -> [[a]]
combinarNiveles [] ys = ys
combinarNiveles xs [] = xs
combinarNiveles (x:xs) (y:ys) = (x ++ y) : combinarNiveles xs ys

-------------------- EJERCICIO 6 --------------------
minimo :: Arbol a -> a
minimo (Raiz x ArbolVacio _) = x
minimo (Raiz _ izq _) = minimo izq
-------------------- EJERCICIO 7 --------------------
maximo :: Arbol a -> a
maximo (Raiz x _ ArbolVacio) = x
maximo (Raiz _ _ der) = maximo der
-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio elemento =
	error "No se puede eliminar en un árbol vacío."

eliminar (Raiz x ArbolVacio arbolDerecho) elemento =
	if x == elemento then
		arbolDerecho
	else
		error "El elemento no existe en el árbol."

eliminar (Raiz x arbolIzquierdo ArbolVacio) elemento =
	if x == elemento then
		arbolIzquierdo
	else
		error "El elemento no existe en el árbol."

eliminar (Raiz x arbolIzquierdo arbolDerecho) elemento =
	if elemento < x then
		Raiz x (eliminar arbolIzquierdo elemento) arbolDerecho
	else if elemento > x then
		Raiz x arbolIzquierdo (eliminar arbolDerecho elemento)
	else (Raiz (minimo arbolDerecho) arbolIzquierdo (eliminar arbolDerecho (minimo arbolDerecho)))


