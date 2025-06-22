type Valoracion = String
type Trabajo = String

data Reparacion = Reparacion{
    duracionDias :: Int,
    trabajo :: [Trabajo]
}deriving Show

data Atraccion = Atraccion {
    nombre :: String,
    alturaMinima :: Int,
    duracion :: Int,
    opiniones :: [Valoracion],
    mantenimiento :: Bool,
    reparaciones :: [Reparacion]
}deriving Show


{-
Punto 1: Más bueno que … (2 puntos)
Queremos saber qué tan buena es una atracción. 
Para eso utilizamos un sistema de scoring que tiene un modo muy particular para calcularlo: Si la atracción tiene una duración prolongada 
(es decir que dura más de 10 minutos) valen 100 puntos. Si no es así, pero tiene menos de 3 órdenes de reparaciones el puntaje es 10 puntos por cada letra del nombre 
más 2 puntos por cada opinión que tiene. Caso contrario es 10 veces la altura mínima requerida.

-}

scoring :: Atraccion -> Int
scoring atraccion | duracion atraccion > 10 = 100
                  | (length . reparaciones) atraccion < 3 = 10 * length (nombre atraccion)
                  | otherwise = 10 * alturaMinima atraccion


{-
Punto 2: Iguana fissss (4 puntos)
Los técnicos tienen diversos tipos de trabajo que pueden desarrollar en cada reparación sobre las atracciones. 
Algo muy importante a tener en cuenta es que luego de que realizan cualquier trabajo siempre ocurren dos cosas: 
    - se elimina la última reparación de la lista (no importa cual fuere)
    - y se verifica que no tenga reparaciones pendientes. Si quedan pendientes debe mantener el indicador que está en mantenimiento, caso contrario no. 

Los posibles trabajos son :
    - ajusteDeTornillería que como le refuerza aún más estructura a la atracción, prolonga su duración en 1 minuto por cada tornillo apretado pero no pudiendo 
    superar los 10 minutos porque no es rentable. Es decir que si una atracción dura 3 minutos y ajusta 4 tornillos la misma pasa a durar 7 minutos. 
    Pero sí una atracción dura 8 minutos y el técnico logra apretar 5 tornillos pasa a durar solamente 10 minutos.
    - engrase que vuelve más veloz al entretenimiento, por lo tanto aumenta en 0,1 centímetros la altura mínima requerida por cada gramo de grasa utilizada 
    en el proceso y le agrega la opinión “para valientes”. La cantidad de grasa requerida puede variar según el criterio del técnico.
    - mantenimientoElectrico repara todas las bombitas de luz y su cableado. Como es un lavado de cara y una novedad para la gente, solo se queda con 
    las dos primeras opiniones y el resto las descarta.
    - mantenimientoBásico que consiste en ajustar la tornillería de 8 tornillos y hacer un engrase con 10 gramos de grasa. 
-}
