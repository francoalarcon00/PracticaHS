{-
Se desea desarrollar un sistema para un popular gimnasio que permita calcular el efecto de las rutinas de ejercicios que realizan sus socios.

De cada gimnasta nos interesa saber su peso y su coeficiente de tonificación.

Los profesionales del gimnasio preparan rutinas de ejercicios pensadas para las necesidades de cada gimnasta. 

Una rutina es una lista de ejercicios que el gimnasta realiza durante unos minutos para quemar calorías y tonificar sus músculos.

-}

type Peso = Int
type Tiempo = Int
type Velocidad = Int
type Grados = Int


-- Creo el gimnasta con su peso y tonificacion
data Gimnasta = Gimnasta{
    peso :: Int,
    tonificacion :: Int
} deriving Show


data Rutina = Rutina{
    nombre :: String,
    duracion :: Tiempo,
    ejercicios :: [Ejercicio]
} 


{-1. Modelar a los Gimnastas y las operaciones necesarias para hacerlos ganar tonificación y quemar calorías considerando que por cada 500 calorías quemadas 
se baja 1 kg de peso.-}

-- En casos donde no es necesario
tonificar :: Int -> Gimnasta -> Gimnasta
tonificar n gimnasta = gimnasta{tonificacion = tonificacion gimnasta + n}

quemarCalorias :: Int -> Gimnasta -> Gimnasta
quemarCalorias n gimnasta = gimnasta{peso = peso gimnasta - n `div` 500}

{-
    Modelar los siguientes ejercicios del gimnasio:
    La cinta es una de las máquinas más populares entre los socios que quieren perder peso. Los gimnastas simplemente corren sobre la cinta y queman calorías 
en función de la velocidad promedio alcanzada (quemando 10 calorías por la velocidad promedio por minuto).
    La cinta puede utilizarse para realizar dos ejercicios diferentes:
    La caminata es un ejercicio en cinta con velocidad constante de 5 km/h.
El pique arranca en 20 km/h y cada minuto incrementa la velocidad en 1 km/h, con lo cual la velocidad promedio depende de los minutos de entrenamiento.-}

type Ejercicio = Tiempo -> Gimnasta -> Gimnasta

cinta :: Velocidad -> Ejercicio
cinta velocidad tiempo = quemarCalorias(velocidad * tiempo * 10)

caminata :: Ejercicio
caminata tiempo = cinta tiempo 5

pique :: Ejercicio
pique tiempo = cinta tiempo (velocidadPromedio tiempo)

velocidadPromedio :: Tiempo -> Velocidad
velocidadPromedio tiempo = (20 + tiempo) `div` 2


{-
Las pesas son el equipo preferido de los que no quieren perder peso, sino ganar musculatura. 
Una sesión de levantamiento de pesas de más de 10 minutos hace que el gimnasta gane una tonificación equivalente a los kilos levantados. 
Por otro lado, una sesión de menos de 10 minutos es demasiado corta, y no causa ningún efecto en el gimnasta.-}

pesas :: Peso -> Ejercicio
pesas peso tiempo gimnasta | tiempo > 10 = tonificar peso gimnasta
                           | otherwise = gimnasta

{-
La colina es un ejercicio que consiste en ascender y descender sobre una superficie inclinada y quema 2 calorías por minuto 
multiplicado por la inclinación con la que se haya montado la superficie.-}

colina :: Grados -> Ejercicio
colina tiempo inclinacion = quemarCalorias (2 * tiempo * inclinacion)

{-
Los gimnastas más experimentados suelen preferir otra versión de este ejercicio: 
la montaña, que consiste en 2 colinas sucesivas (asignando a cada una la mitad del tiempo total), 
donde la segunda colina se configura con una inclinación de 5 grados más que la inclinación de la primera. 
Además de la pérdida de peso por las calorías quemadas en las colinas, la montaña incrementa en 3 unidades la tonificación del gimnasta.
-}

montaña :: Grados -> Ejercicio
montaña inclinacion tiempo = tonificar 3 . colina (tiempo `div` 2) (inclinacion + 5). colina (tiempo `div` 2) inclinacion

{-

Dado un gimnasta y una Rutina de Ejercicios, representada con la siguiente estructura:

data Rutina = Rutina {
    nombre :: String,
    duracionTotal :: Int,
 ejercicios :: [Ejercicio]
}

Implementar una función realizarRutina, que dada una rutina y un gimnasta retorna el gimnasta resultante de realizar todos los ejercicios de la rutina, 
repartiendo el tiempo total de la rutina en partes iguales. Mostrar un ejemplo de uso con una rutina que incluya todos los ejercicios del punto anterior.-}

realizarRutina :: Rutina -> Gimnasta -> Gimnasta
realizarRutina rutina gimnasta = foldl (\gimnasta ejercicio -> ejercicio (tiempoPorEjercicio rutina) gimnasta) gimnasta (ejercicios rutina)

tiempoPorEjercicio :: Rutina -> Tiempo
tiempoPorEjercicio rutina = duracion rutina `div` length (ejercicios rutina)


{-
Definir las operaciones necesarias para hacer las siguientes consultas a partir de una lista de rutinas:
¿Qué cantidad de ejercicios tiene la rutina con más ejercicios?
¿Cuáles son los nombres de las rutinas que hacen que un gimnasta dado gane tonificación?
¿Hay alguna rutina peligrosa para cierto gimnasta? Decimos que una rutina es peligrosa para alguien si lo hace perder más de la mitad de su peso.-}

mayorCantidadEjercicios :: [Rutina] -> Int
mayorCantidadEjercicios rutina = maximum . map (length . ejercicios rutina)

nombresRutinasTonificantes :: [Rutina] -> Gimnasta -> [String]
nombresRutinasTonificantes rutina gimnasta = map nombre . filter ((> tonificacion gimnasta) . tonificacion . realizarRutina gimnasta ) 
