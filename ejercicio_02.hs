data Persona = Persona {
  nombrePersona :: String,
  suerte :: Int,
  inteligencia :: Int,
  fuerza :: Int
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)


{-Dada una persona definir las siguientes funciones para cuantificar sus niveles de suerte, inteligencia y fuerza sin repetir código:
    - sumaDeNiveles que suma todos sus niveles.
    - diferenciaDeNiveles es la diferencia entre el nivel más alto y más bajo.
    - nivelesMayoresA n, que indica la cantidad de niveles de la persona que están por encima del valor dado.-}

niveles :: Persona -> [Int]
niveles persona = [suerte persona, inteligencia persona, fuerza persona]

sumaDeNiveles :: Persona -> Int
sumaDeNiveles = sum . niveles

diferenciaDeNiveles :: Persona -> Int
diferenciaDeNiveles persona = maximoNivel persona - minimoNivel persona

maximoNivel = maximum . niveles

minimoNivel = minimum . niveles

nivelesMayoresA :: Int -> (Persona -> Int)
nivelesMayoresA num = length . filter (>num) . niveles -- leyendo de izquierda a derecha, dado un conjunto de niveles, 
                                                       -- me quedo con los que cumplen > num (filter me de vuelve una lista) y cuento la cantidad de elementos


{-Definir la función efectosDePocion que dada una poción devuelve una lista con los efectos de todos sus ingredientes.-}

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion = concatMap efectos . ingredientes

{-Dada una lista de pociones, consultar:
    - Los nombres de las pociones hardcore, que son las que tienen al menos 4 efectos.
    - La cantidad de pociones prohibidas, que son aquellas que tienen algún ingrediente cuyo nombre figura en la lista de ingredientes prohibidos.
    - Si son todas dulces, lo cual ocurre cuando todas las pociones de la lista tienen algún ingrediente llamado “azúcar”.-}

pocionesHarcore :: [Pocion] -> [String]
pocionesHarcore = map nombrePocion . filter ( (>=4) . length . efectosDePocion)

pocionesProhibidas :: [Pocion] -> Int
pocionesProhibidas = length . filter esProhibida

esProhibida :: Pocion -> Bool
esProhibida = any (flip elem nombresDeIngredientesProhibidos . nombreIngrediente ) . ingredientes

pocionesDulces :: [Pocion] -> Bool
pocionesDulces = all(any (("azucar"==) . nombreIngrediente) . ingredientes)


{-Definir la función tomarPocion que recibe una poción y una persona, y devuelve como quedaría la persona después de tomar la poción. 
Cuando una persona toma una poción, se aplican todos los efectos de esta última, en orden.-}

tomarPocion :: Pocion -> Persona -> Persona
tomarPocion pocion persona =  (foldl (\persona efecto -> efecto persona) persona . efectosDePocion) pocion


{-Definir la función esAntidotoDe que recibe dos pociones y una persona, y dice si tomar la segunda poción revierte los cambios que se producen en la persona al tomar la primera.-}

esAntidoto :: Pocion -> Pocion -> Persona -> Bool
esAntidoto pocion antidoto persona = ((==persona) . tomarPocion antidoto . tomarPocion pocion) persona


{-Definir la función personaMasAfectada que recibe una poción, una función cuantificadora (es decir, una función que dada una persona retorna un número) 
y una lista de personas, y devuelve a la persona de la lista que hace máxima el valor del cuantificador. 
Mostrar un ejemplo de uso utilizando los cuantificadores definidos en el punto 1.-}

personaMasAfectada :: Pocion -> (Persona -> Int) -> ([Persona] -> Persona)
personaMasAfectada pocion criterio = maximoSegun (criterio . tomarPocion pocion)