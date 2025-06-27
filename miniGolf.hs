-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


{-
Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro 
que se compone por velocidad, precisión y altura.
El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
-}
putter :: Habilidad -> Tiro
putter habilidad = UnTiro 10 (2 * precision habilidad) 0

{-
La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
-}

madera :: Habilidad -> Tiro
madera habilidad = UnTiro 100 (precisionJugador habilidad `div` 2) 5

{-
Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual 
a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). 
Modelarlos de la forma más genérica posible.
-}

hierros :: Int -> Habilidad -> Tiro
hierros n habilidad = UnTiro (fuerzaJugador habilidad * n) (precisionJugador habilidad `div` n) (max 0 (n - 3))

--Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.
palos :: [Habilidad -> Tiro]
palos = [putter, madera] ++ map (`hierros` ) [1 .. 10]  --aplicacion parcial de hierros con la lista fininta del 1 al 10

{-
Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese 
palo con las habilidades de la persona.
Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.
-}

type Palo = Habilidad -> Tiro

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador) -- aplicación parcial de palo con la habilidad del jugador

{-
Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, 
y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. 
En principio necesitamos representar los siguientes obstáculos:

    Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
independientemente de la velocidad del tiro. 
    Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.
-}

type Obstaculo = Tiro -> Tiro



tunelConRampita :: Obstaculo
tunelConRampita tiro
                | (precision tiro > 90)  && (altura tiro == 0) = UnTiro (2 * velocidad tiro) 100 0
                | otherwise = UnTiro 0 0 0 -- tiro nulo si no se supera el obstáculo

{-
Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros.
Luego de superar una laguna el tiro llega con la misma velocidad y precisión,   
pero una altura equivalente a la altura original dividida por el largo de la laguna.
-}

laguna :: Int -> Obstaculo
laguna largo tiro
                | (velocidad tiro > 80) && between 1 5 (altura tiro) = UnTiro (velocidad tiro) (precision tiro) (altura tiro `div` largo)
                | otherwise = UnTiro 0 0 0 -- tiro nulo si no se supera el obstáculo

{-
Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo
con una precisión mayor a 95. 
Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.
-}

hoyo :: Obstaculo
hoyo _ = UnTiro 0 0 0 -- tiro nulo si no se supera el obstáculo

{-
Definir palosUtiles que dada una persona y un obstáculo, permita determinar 
qué palos le sirven para superarlo.
-}

esTiroNulo :: Tiro -> Bool
esTiroNulo Untiro 0 0 0 = True
esTiroNulo _ = False

paloSirve :: Palo -> Jugador -> Obstaculo -> Bool
paloSirve palo jugador obstaculo = not (esTiroNulo (obstaculo(golpe jugador palo)))


palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (paloSirve jugador obstaculo) palos


{-
Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, 
y una lista con dos túneles con rampita seguidos de un hoyo, el resultado sería 2 ya que 
la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo.
BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función 
takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.
-}

contar :: (Int, Tiro) -> Obstaculo -> (Int, Tiro)
contar (n, t) obst
  | esTiroNulo t            = (n, t)               -- ya estaba muerto ➜ nada cambia
  | esTiroNulo (obst t)     = (n, obst t)          -- falla aquí ➜ no suma
  | otherwise               = (n + 1, obst t)      -- lo supera ➜ suma 1

-- Punto 4 b
superarObstaculos :: Tiro -> [Obstaculo] -> Int
superarObstaculos tiro obstaculos =
  fst (foldl contar (0, tiro) obstaculos)

