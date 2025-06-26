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
