-- Declaramos los tipos
type Nombre = String
type Velocidad = Int
type Habilidad = Int
type Puesto = String
type Partidos = ([Int, Int])

-- Declaramos la informacion del jugador
data Jugador = Jugador{
    nombre :: Nombre,
    velocidad = Velocidad,
    habilidad :: Habilidad,
    puesto :: Puesto,
    partidos :: Partidos
} deriving Show


{-
Queremos saber los nombres de los jugadores de un equipo que jugaron todos los partidos 
al menos una cantidad de minutos, donde esa cantidad de minutos sea parametrizable. 
Por ejemplo, si elegimos que la cantidad de minutos sea 45, puede haber jugado 
45, 46 ó los 90, eso alcanza.
-}
type Minutos = Int

jugaronTodosLosPartidos :: Minutos -> [Jugador] -> [String]
jugaronTodosLosPartidos minutos = map nombre . filter (\jug -> all (\(m, _) -> m >= minutos) (partidos jug))

type Letra = Char
empiezaConP :: Letra -> [Jugador] -> Bool
empiezaConP letra jugadores = any ((==letra) . head . nombre)


--Punto 2 -> Tecnicos
{-
Bielsa le agrega un 50% de velocidad a los jugadores pero también baja 10 puntos su habilidad 
(todo no se puede)
-}
type Equipo = [Jugador]

bielsa :: Equipo -> Equipo
bielsa = map (\jugador -> jugador{
    velocidad = velocidad jugador * 3 `div` 2,
    habilidad = habilidad jugador - 10
})

{-
Menotti le agrega un "Mr. " al nombre del jugador y aumenta una cantidad de puntos su habilidad, 
donde esa habilidad se puede parametrizar

-}

menotti :: Habilidad -> Jugador -> Jugador
menotti = jugador{
    nombre = "Mr. " ++ nombre jugador,
    habilidad = habilidad jugador + habilidad
}

-- Bertolotti hace lo mismo que Menotti, solo que aumenta siempre 10 su habilidad
bertolotti :: Jugador -> Jugador
bertolotti = menotti 10

-- Van Gaal no afecta a los jugadores, siguen jugando igual que siempre
vanGaal :: Jugdaor -> Jugador
vanGaal = id

-- Punto 3

{-
Sabemos que un jugador es bueno si tiene más habilidad que velocidad, o bien si es volante.

Queremos saber si un técnico mejora a un equipo, esto se da si después de entrenar a todos los 
jugadores de un equipo tiene más jugadores buenos que antes de entrenarlos.
-}

-- usamos guardas
jugadorBueno :: Jugador -> Bool
jugadorBueno | habilidad jugador > velocidad jugador = True
             | posicion jugador == "Volante" = True
             | otherwise = False 

type Tecnico = Jugador -> Jugador

mejoraEquipo :: Tecnico -> Equipo -> Bool
mejoraEquipo tecnico equipo =
     length (filter jugadorBueno (map tecnico equipo))
  >  length (filter jugadorBueno equipo)

{-
Queremos saber si un jugador es imparable. Esto se da si con el correr de los partidos va metiendo 
la misma cantidad de goles o más.

Por ejemplo un jugador que en el primer partido no metió goles, en el segundo metió 2, 
en el tercero metió 2 y en el cuarto metió 4 es imparable.

En el caso de un jugador que en el primer partido no metió goles, en el segundo hizo un gol y 
en el tercero no metió goles, no es imparable.

-}

jugadorImparable :: Jugador -> Bool
jugadorImparable jugador =
    all id $                                    -- verificamos que todas son true
        zipWith (>=)                            --produce una lista de Bool comparando cada elemento de goles con su siguiente
            map snd (partidos jugador)          --obtengo los partidos del jugador 
            tail (map snd (partidos jugador))   --obtengo los partidos del jugador en una lista y descarto el primer partido