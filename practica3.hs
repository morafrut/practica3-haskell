data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud (Node _ list) = 1 + longitud list

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void _ = False
estaContenido (Node x rest) elem = 
    if elem == x
    then True
    else estaContenido rest elem
    
convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista Void  = []
convertirALista (Node x rest ) = x : (convertirALista rest )  

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x rest) = 
    if estaContenido rest x
    then conjunto rest 
    else Node x (conjunto rest)
    
eliminarIndice :: List a -> Int -> List a
eliminarIndice Void _ = error "Indice fuera del rango permitido."
eliminarIndice lista n = 
    if n < 0 
    then error "Indice fuera del rango permitido."
    else if n >= longitud lista 
         then error "Indice fuera del rango permitido."
         else if n == 0 
              then case lista of 
                      Node _ rest -> rest
              else case lista of 
                      Node x rest -> Node x (eliminarIndice rest (n-1))
		      
insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void n nuevoElem = 
    if n == 0 
    then Node nuevoElem Void 
    else error "Indice fuera del rango permitido."
insertarIndice (Node x rest) n nuevoElem = 
    if n < 0 
    then error "Indice fuera del rango permitido."
    else if n > longitud (Node x rest) 
         then error "Indice fuera del rango permitido."
         else if n == 0 
              then Node nuevoElem (Node x rest)
              else Node x (insertarIndice rest (n-1) nuevoElem)

recorrerLista :: List a -> Int -> List a
recorrerLista Void _ = Void 
recorrerLista lista 0 = lista 
recorrerLista (Node x rest) n = recorrerLista (insertarIndice rest (longitud rest) x) (n-1)



