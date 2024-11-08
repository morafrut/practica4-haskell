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
niveles arbol = nivelesAux [arbol]

nivelesAux :: [Arbol a] -> [[a]]
nivelesAux [] = []
nivelesAux xs = [obtenerValores xs] ++ nivelesAux (obtenerHijos xs)

obtenerValores :: [Arbol a] -> [a]
obtenerValores [] = []
obtenerValores (ArbolVacio:xs) = obtenerValores xs
obtenerValores ((Raiz valor _ _):xs) = valor : obtenerValores xs

obtenerHijos :: [Arbol a] -> [Arbol a]
obtenerHijos [] = []
obtenerHijos (ArbolVacio:xs) = obtenerHijos xs
obtenerHijos ((Raiz _ izq der):xs) = izq : der : obtenerHijos xs

-- Prueba
-- niveles (Raiz 5 (Raiz 3 ArbolVacio ArbolVacio) (Raiz 6 ArbolVacio ArbolVacio))

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
eliminar ArbolVacio _ = ArbolVacio
eliminar (Raiz x izq der) n
  | n < x     = Raiz x (eliminar izq n) der
  | n > x     = Raiz x izq (eliminar der n)
  | n == x    = eliminarRaiz (Raiz x izq der)
  where
    eliminarRaiz (Raiz _ ArbolVacio der) = der
    eliminarRaiz (Raiz _ izq ArbolVacio) = izq
    eliminarRaiz (Raiz _ izq der)        = Raiz minDer izq (eliminar der minDer)
      where minDer = minimo der



