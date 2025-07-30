-- Tipos para las propiedades del departamento
type Ambientes = Int
type Superficie = Int
type Precio = Int
type Barrio = String

-- Definimos los requisitos y la busqueda
type Requisito = Departamento -> Bool
type Busqueda = [Requisito]

-- Definimos el tipo Mail
type Mail = String

-- Definimos el dato Departamento con sus propiedades (ambiente, superficie, precio, barrio)
data Departamento = Departamento{
    ambientes :: Ambientes,
    superficie :: Superficie,
    precio :: Precio,
    barrio :: Barrio
} deriving Show


-- Definimos el dato Persona con sus propiedades (mail, busquedas)
data Persona = Persona{
    mail :: Mail,
    busquedas :: [Busqueda]
}

-- Datos de ejemplo
deptosDeEjemplo = [
    Departamento 3 80 7500 "Palermo",
    Departamento 1 45 3500 "Villa Urquiza", 
    Departamento 2 50 5000 "Palermo", 
    Departamento 1 45 5500 "Recoleta"]


ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio.filter (not.criterio x)) xs ++ [x] ++ (ordenarSegun criterio.filter (criterio x)) xs

between x y z = 
    x <= z && y >= z

{-
Se pide desarrollar las siguientes funciones y consultas de modo que se aprovechen tanto como sea posible los conceptos de orden superior, aplicación parcial y composición.

    a- Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar esa función 
    sobre el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.

    b- Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.

-}
--a
{- La fucion va de un tipo a otro (que no sabemos) pero los dos inputs que tenemos tienen que ser del mismo tipo que el input de la funcion anterior, y como ese "b" 
debe ser ordenable, lo aclaramos con el Ord -}
mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor f x y = f x > f y

{- Mismo caso que el anterior, unicamente cambia el simbolo "<" para comprar -}
menor :: Ord b => (a -> b) -> a -> a -> Bool
menor f x y = f x < f y

--b
{- Debemos aplicar ordenarSegun, entonces, siguiendo con el orden de sus parametros:
    - Primero necesitamos poner la funcion criterio que queremos usar. En este caso necesitamos saber la longitud de cada elemento de la lista de strings, entonces, 
    dada longitud de mis strings, quero compararlos para saber el menor.
    - Como segundo paso, necesitamos pasarle la lista que va a recorrer
-}
ordenarLista = ordenarSegun (menor length) ["Primero", "Segundo", "que", "Hola como estas?"]

{-
Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
    a- ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista.
    b- cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser aplicada con el departamento 
    se encuentra entre los dos valores indicados.
-}

{-
    Para resolver estos tipos de ejercicio, hay que buscar las palabras claves:
        - nos estan indicando que dado una lista de barrios, queremos saber si el barrio del departamento se encuentra dentro de esta lista (elem)
        - primero obtenemos el barrio del departamento (barrio departamento) y despues con elem le pasasmos ese barrio y  la lista de barrios
        - esto nos va a devolver true o false, dependiendo si esta o no presente en esa lista
-}
ubicadoEn :: [Barrio] -> (Departamento -> Bool)
ubicadoEn barrios = (`elem` barrios) . barrio


cumpleRango :: Ord a => (Departamento -> a) -> a -> a -> (Departamento -> Bool)
cumpleRango f cotaInf cotaSup = between cotaInf cotaSup . f


{-
    a- Definir la función cumpleBusqueda :: Depto -> Busqueda -> Bool que se cumple si todos los requisitos de la búsqueda se verifican para ese departamento.
    b- Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y una lista de departamentos retorne todos aquellos que cumplen 
    con la búsqueda ordenados en base al criterio recibido.
    c- Mostrar un ejemplo de uso de buscar para obtener los departamentos que se encuentren en Recoleta o Palermo de 1 a 2 ambientes que se alquilen 
    a menos de $6000 por mes, de modo que el resultado se encuentre ordenado por mayor superficie con la lista de departamentos de ejemplo dada.
-}

--a
cumpleBusqueda :: Departamento -> Busqueda -> Bool
cumpleBusqueda departamento = all (\requisito -> requisito departamento)
-- cumpleBusqueda' departamento = all ($ departamento) 

--b
type Departamentos = [Departamento]

buscar :: Busqueda -> (Departamento -> Departamento -> Bool) -> Departamentos -> Departamentos
buscar busqueda criterio = ordenarSegun criterio . filter ( `cumpleBusqueda` busqueda )

{-
Definir la función mailsDePersonasInteresadas que a partir de un departamento y 
una lista de personas retorne los mails de las personas que tienen alguna búsqueda que se cumpla para el departamento dado.
-}

mailsDePersonasInteresadas :: Departamento -> [Persona] -> [Mail] 
mailsDePersonasInteresadas departamento = map mail . filter (any(cumpleBusqueda departamento) . busquedas)

