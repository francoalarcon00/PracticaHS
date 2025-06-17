type Barrio = String        
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
 ambientes :: Int,
 superficie :: Int,
 precio :: Int,
 barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
   mail :: Mail,
   busquedas :: [Busqueda]
}

deptosDeEjemplo = [
 Depto 3 80 7500 "Palermo",
 Depto 1 45 3500 "Villa Urquiza",
 Depto 2 50 5000 "Palermo",
 Depto 1 45 5500 "Recoleta"]


ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
 (ordenarSegun criterio . filter (not . criterio x)) xs ++
 [x] ++
 (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior



{-Se pide desarrollar las siguientes funciones y consultas de modo que se aprovechen tanto como sea posible los conceptos de orden superior, 
aplicación parcial y composición.


Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar esa función sobre 
el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.
-}

-- mayor recibe 3 parametros y evaluamos cada valor con la funcion definida
mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor f valor1 valor2 = f valor1 > f valor2

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor f valor1 valor2 = f valor1 < f valor2

{-Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.-}

ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1","estevaasereltercero","dos"]

{-Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
 - ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista.
 - cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser aplicada con el departamento 
se encuentra entre los dos valores indicados.-}

ubicadoEn :: [Barrio] -> (Depto -> Bool)
ubicadoEn barrios = flip elem barrios . barrio -- dado un depto, me fijo si el barrio de ese depto esta en la lista de barrios
                                               -- el flip me cambia el orden de parametros ya que la lista viene en el segundo slot de parametro

-- otra forma de escribir esto puede ser
ubicadoEn' :: [Barrio] -> (Depto -> Bool)
ubicadoEn' barrios = (`elem` barrios) . barrio -- es lo mismo que antes salvo que aplico infijo elem dado barrios


cumpleRango :: Ord a => (Depto -> a) -> a -> a -> (Depto -> Bool)
cumpleRango f cotaInf cotaSup = between cotaInf cotaSup . f

{-Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.-}

cumpleBusqueda :: Depto -> (Busqueda -> Bool)
cumpleBusqueda depto = all (\requisito -> requisito depto)

{-Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y una lista de departamentos 
retorne todos aquellos que cumplen con la búsqueda ordenados en base al criterio recibido.-}
buscar :: Busqueda -> (Depto -> Depto -> Bool) -> ([Depto]->[Depto])
buscar busqueda criterio = ordenarSegun criterio . filter (`cumpleBusqueda` busqueda)


{-Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas retorne los mails de las personas 
que tienen alguna búsqueda que se cumpla para el departamento dado.-}

mailsDePersonasInteresadas :: Depto -> ([Persona] -> [Mail])
mailsDePersonasInteresadas depto = map mail . filter (any (cumpleBusqueda depto) . busquedas)