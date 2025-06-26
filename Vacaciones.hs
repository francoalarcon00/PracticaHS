type Cansancio = Int
type Stress = Int
type Idioma = String

data Turista = Turista {
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
}deriving Show


type Excursion = Turista -> Turista

{-
Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
-}
irALaPlaya :: Excursion
irALaPlaya turista | viajaSolo turista = turista {cansancio = cansancio turista - 5}
                   | otherwise = turista { stress = stress turista - 1 }

{-
Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 
-}
apreciarPaisaje :: String -> Excursion
apreciarPaisaje aprecia turista = turista { stress = stress turista - length aprecia}


{-
Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.
-}
hablarIdioma :: Idioma -> Excursion
hablarIdioma idioma turista = turista {
                                    idiomas = idiomas turista ++ [idioma],
                                    viajaSolo = False}  -- hay que tener en cuenta que dice que sigue acompañado, por ende no viaja solo

{-
Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos en la misma cantidad. 
El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.
-}

type Tiempo = Int

caminar :: Tiempo -> Excursion
caminar tiempo turista = turista {
                                cansancio = cansancio turista + tiempo `div` 4,
                                stress = stress turista - tiempo `div` 4}


{-

Paseo en barco: depende de cómo esté la marea
                si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
                si está moderada, no pasa nada.
                si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.

-}
-- defino un data para hacer PatterMatching
data Marea = Fuerte | Moderada | Tranquila 
            deriving Show

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte turista = turista { 
                                cansancio = cansancio turista + 10,
                                stress = stress turista + 6
                                }

paseoEnBarco Moderada turista = id turista

paseoEnBarco Tranquila turista = (hablarIdioma "aleman" . apreciarPaisaje "mar" . caminar 10) turista


{-
Nos avisaron que es común que, cada cierto tiempo, se vayan actualizando las excursiones que ofrecen, en base a las nuevas demandas que surgen en el mercado turístico. 
Se pide
    Crear un modelo para los turistas y crear los siguientes tres ejemplos:
    Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
    Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15 unidades de cansancio y stress.

-}

ana :: Turista
ana = Turista { cansancio = 0,
                stress = 21,
                viajaSolo = False,
                idiomas = ["Español"]
            }

beto :: Turista
beto = Turista {
                cansancio = 15,
                stress = 15,
                viajaSolo = True,
                idiomas = ["Aleman"]
            }

cathi :: Turista
cathi = Turista {
                cansancio = 15,
                stress = 15,
                viajaSolo = True,
                idiomas = ["Aleman", "Catalan"]
            }


{-
Modelar las excursiones anteriores de forma tal que para agregar una excursión al sistema no haga falta modificar las funciones existentes. 
Además:
    - Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.
-}
reducirStress :: Excursion
reducirStress turista = turista{stress = stress turista - stress turista `div` 10}

hacerExcursion :: Excursion -> Excursion
hacerExcursion excursion = reducirStress . excursion

{-
Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice después de que 
el turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.
    Por ejemplo, si “stress” es la función que me da el stress de un turista:
    > deltaExcursionSegun stress ana irALaPlaya
        -3     -- porque al ir a la playa Ana queda con 18 de estrés (21 menos 1 menos 10% de 20)
-}

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = (Turista -> Int)

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista


{-
Usar la función anterior para resolver cada uno de estos puntos:
    Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
    Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.
-}

cantIdiomas :: Turista -> Int
cantIdiomas = length . idiomas

esEducativa :: Turista -> Excursion -> Bool
esEducativa turista excursion = deltaExcursionSegun cantIdiomas turista excursion > 0

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista excursion = deltaExcursionSegun stress turista excursion <= (-3)


excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (esDesestresante turista)


{-

Para mantener a los turistas ocupados todo el día, la empresa vende paquetes de excursiones llamados tours.
 Un tour se compone por una serie de excursiones.
    Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, 
    y finaliza con una salida con gente local que habla "melmacquiano".
-}

type Tour = [Excursion] --definimos tour para mas simpllcidad de codigo

completo :: Tour
completo = [apreciarPaisaje "cascada" . caminar 20, irALaPlaya . caminar 40, hablarIdioma "melmacquiano"] --podria escribirse al revez en cada caso para tener una mejor coherencia a al hora de leer el codigo

{-
Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. 
    Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, 
    luego realiza la excursión elegida 
    y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.
-}

ladoB :: Excursion -> Tour -- dada una excursion elegida, me arma un toour donde la excursion elegida es la 2da en la lista
ladoB excursionElegida = [paseoEnBarco Tranquila, excursionElegida, caminar 120]

{-
Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. 
Esta excursión depende de cómo esté la marea al llegar a la otra isla: 
    si está fuerte se aprecia un "lago", sino se va a una playa.

En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, 
luego llevar a cabo dicha excursión, 
y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.

-}

excursionDeterminada :: Marea -> Excursion -- dada una marea determinada, me devuelve una excursion
excursionDeterminada Fuerte = apreciarPaisaje "lago"
excursionDeterminada _ = irALaPlaya

islaVecina :: Marea -> Tour
islaVecina marea = [paseoEnBarco marea, excursionDeterminada marea, paseoEnBarco marea]


{-
Modelar los tours para:
- Hacer que un turista haga un tour. 
    Esto implica, primero un aumento del stress en tantas unidades como cantidad de excursiones tenga el tour, y luego realizar las excursiones en orden.
-}

hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour = foldl (flip hacerExcursion) (turista{stress = stress turista + length tour}) tour

{-
Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. 
Esto significa que el tour tiene alguna excursión desestresante la cual, además, deja al turista acompañado luego de realizarla.
-}

esConvincente :: Turista -> [Tour] -> Bool
esConvincente turista = any 
                            ( any (\exc -> esDesestresante turista exc && not (viajaSolo (hacerExcursion exc turista ))))


{-
Saber la efectividad de un tour para un conjunto de turistas. 
Esto se calcula como la sumatoria de la espiritualidad recibida de cada turista a quienes les resultó convincente el tour. 
La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.
-}

efectividadTour :: Tour -> [Turista] -> Int
efectividadTour tour =
  sum 
  . map (\t -> stress t - stress (hacerTour t tour) + cansancio t - cansancio (hacerTour t tour) )
  . filter (\t -> esConvincente t [tour])


{-
Implementar y contestar en modo de comentarios o pruebas por consola
Construir un tour donde se visiten infinitas playas.
 1- ¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
 - ¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.
-}

infinitasPlayas :: Tour
infinitasPlayas = repeat irALaPlaya

{-

1)  ¿El tour es convincente para Ana?
> esConvincente ana [playasInf]
True  

> esConvincente beto [playasInf]
Nunca termina

2) ¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.
Solo se puede conocer cuando no tengamos ningun turista, si tenemos al menos un turista, nunca termina de recorrer las excursiones

-}
