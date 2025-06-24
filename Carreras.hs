data Auto = Auto{
    color :: String,
    velocidad :: Int,
    distanciaRecorrida :: Int
}deriving (Show, Eq)

data Carrera = Carrera{
    autos :: [Auto]
}

{-
Saber si un auto está cerca de otro auto, que se cumple si son autos distintos 
y la distancia que hay entre ellos (en valor absoluto) es menor a 10.
-}

estaCerca :: Auto -> (Auto -> Bool)
estaCerca auto1 auto2 = color auto1 /= color auto2 && abs(distanciaRecorrida auto1 - distanciaRecorrida auto2) < 10


{-
Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca 
y les va ganando a todos (por haber recorrido más distancia que los otros).
-}

-- | Devuelve la lista de rivales (todos menos el propio auto).
rivales :: Auto -> Carrera -> [Auto]
rivales auto = filter ((/= color auto) . color) . autos

-- | 1) No hay ningún rival a menos de 10 m
sinAutosCerca :: Auto -> Carrera -> Bool
sinAutosCerca auto = all (not . estaCerca auto) . rivales auto

-- | 2) Le gana en distancia a todos los rivales
lesGanaATodos :: Auto -> Carrera -> Bool
lesGanaATodos auto =
  all ((< distanciaRecorrida auto) . distanciaRecorrida) . rivales auto

-- | Punto 1 b: un auto va tranquilo si se cumplen las dos condiciones
vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = sinAutosCerca auto carrera && lesGanaATodos auto carrera


{-
Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que le van ganando.
-}
puesto :: Auto -> Carrera -> Int
puesto auto carrera = 1 + length (filter((> distanciaRecorrida auto) . distanciaRecorrida) (rivales auto carrera))


{-
Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo indicada, 
la distancia recorrida por el auto debería ser equivalente a: 
    la distancia que llevaba recorrida + ese tiempo * la velocidad a la que estaba yendo.

-}
type Tiempo = Int

corra :: Tiempo -> Auto -> Auto
corra tiempo auto = auto {distanciaRecorrida = distanciaRecorrida auto + tiempo * velocidad auto}



{-
A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto de modo que 
    su velocidad final sea la resultante de usar dicho modificador con su velocidad actual.
    Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad indicada de modo que se le reste a la velocidad actual la cantidad indicada, 
    y como mínimo quede en 0, ya que no es válido que un auto quede con velocidad negativa.

-}

type Velocidad = Int

modificarVelocidad :: (Velocidad -> Velocidad) -> Auto -> Auto
modificarVelocidad f auto = auto {velocidad = f (velocidad auto)}

bajarVelocidad :: Velocidad -> Auto -> Auto
bajarVelocidad n = modificarVelocidad (\v -> max 0 (v - n))


{-
Inicialmente queremos poder representar los siguientes power ups, pero debería ser fácil incorporar más power ups a futuro para enriquecer nuestro programa:
terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan su velocidad en 50.
-}

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista


terremoto :: Auto -> Carrera -> Carrera
terremoto auto carrera = carrera {autos = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50) (autos carrera)}

{-
miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad 
los autos que se vean afectados por su uso. Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando.

-}

leGanaA :: Auto -> Auto -> Bool
leGanaA auto rival =
  color auto /= color rival
  && distanciaRecorrida auto > distanciaRecorrida rival

miguelitos :: Velocidad-> Auto -> Carrera -> Carrera
miguelitos velocidad auto carrera = carrera {autos = afectarALosQueCumplen (leGanaA auto) (bajarVelocidad velocidad) (autos carrera)}


{-
jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. 
El jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.
Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el tiempo indicado 
y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo.
-}

esElMismoAuto :: Auto -> Auto -> Bool
esElMismoAuto auto = (== color auto) . color 

aplicarJetPack :: Tiempo -> Auto -> Auto
aplicarJetPack tiempo auto =
  (corra tiempo (auto { velocidad = velocidad auto * 2 })) { velocidad = velocidad auto }



jetPack :: Tiempo -> Auto -> Carrera -> Carrera
jetPack tiempo auto carrera = carrera { autos = afectarALosQueCumplen (esElMismoAuto auto) (aplicarJetPack tiempo) (autos carrera) }


{-

A partir de todo lo construido hasta ahora queremos finalmente simular una carrera, para lo cual se provee una lista de eventos, 
que son funciones que permiten ir de un estado de la carrera al siguiente, y el estado inicial de la carrera a partir del cual se producen dichos eventos. Con esta información buscamos generar una “tabla de posiciones”, que incluye la información de en qué puesto quedó cada auto asociado al color del auto en cuestión.

Se pide:
Desarrollar la función:
simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
que permita obtener la tabla de posiciones a partir del estado final de la carrera, el cual se obtiene produciendo 
cada evento uno detrás del otro, partiendo del estado de la carrera recibido.

Desarrollar las siguientes funciones de modo que puedan usarse para generar los eventos que se dan en una carrera:
c   orrenTodos que hace que todos los autos que están participando de la carrera corran durante un tiempo indicado.
    usaPowerUp que a partir de un power up y del color del auto que gatilló el poder en cuestión, encuentre el auto correspondiente dentro del estado actual de la carrera para usarlo y produzca los efectos esperados para ese power up.

-}

correnTodos :: Tiempo -> Carrera -> Carrera
correnTodos tiempo carrera = carrera { autos = map (corra tiempo) (autos carrera) } 


type PowerUp = Auto -> Carrera -> Carrera
type Color = String

usaPowerUp :: PowerUp -> Color -> Carrera -> Carrera
usaPowerUp powerUp colorObjetivo carrera = powerUp (head (filter ((== colorObjetivo) . color) (autos carrera))) carrera



