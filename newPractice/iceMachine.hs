{-
    Los helados son de un único gusto (Ej.: "frutilla", "durazno", etc) y de cada uno se conoce su
    temperatura y la proporción de agua respecto de la fruta.
-}

type Sabor = String
type Temperatura = Int
type Proporcion = Float

type Helado = (Nombre, Temperatura, Proporcion)

{-
    1. Saber si un helado salió mal: Esto sucede cuando no se llegó a congelar (los helados
deben estar a 0 grados o menos), o bien la proporción de agua no es correcta.
    Que la proporción de agua sea correcta depende además de la fruta que se utilice:
        ● los helados de frutilla deben tener siempre una proporción de 0.4
        ● los de durazno deben estar entre 0.2 y 0.6
        ● los demás sabores deben guardar relación con la cantidad de letras de su nombre.
Si la fruta es uva, por ejemplo, su proporción deberá ser de 0.3. Pero en caso de
que el nombre supere las 8 letras, como “mandarina”, la proporción deberá guardar
relación con la cantidad de vocales del nombre, en este caso 0.4.
-}

salioMal :: Helado -> Bool
salioMal (sabor,temperatura,proporcion)  | temperatura > 0 
                                                = True
                                          | sabor == "Frutilla" && proporcion /= 0.4 
                                                = True
                                          | sabor == "Durazno" && (proporcion < 0.2 || proporcion > 0.6) 
                                                = True
                                          | length sabor <= 8 && proporcion /= (length sabor `div` 10) 
                                                = True
                                          | length nsabor > 8 && proporcion /= (cantidadVocales sabor `div` 10) 
                                                = True
                                          | otherwise 
                                                = False


{-
2. Para poder producir el helado se necesita materia prima y máquinas que lo procesen.
Modelar las siguientes máquinas y dar el tipo de cada una de ellas:
-}

{-
    a. heladera: Enfría a una cierta cantidad de grados a un helado.
-}

heladera :: Int -> (Helado -> Helado)
heladera grados (sabor,temperatura,proporcion) = (sabor,temperatura - grados,proporcion)

{-
    b. batidora: Toma un cajón de fruta y un bidón de agua y los mezcla, transformándolo en
un helado del sabor de la fruta del cajón, la temperatura del agua y la proporción de agua
que surge (litros por kg.).
-}

type Peso = Int
type Cajon = (Sabor, Peso)

batidora :: Cajon-> Temperatura -> Helado
batidora (sabor,peso) temperatura = (sabor,temperatura,temperatura `div` peso)

{-
    c. exprimidora:Toma un cajón de fruta natural y lo transforma en un nuevo cajón de fruta
exprimida, perdiendo el 50% de su peso.
-}

exprimidora :: Cajon -> Cajon
exprimidora (sabor,peso) = (sabor,peso `div` 2)


{-
    d. mixturadora: Toma dos helados de frutas diferentes y obtiene uno nuevo, de gusto
combinado, con la menor temperatura y la proporción promedio.
-}

mixturadora :: Helado -> Helado -> Helado
mixturadora (saborA, temperaturaA, proporcionA) 
            (saborB, temperaturaB, proporcionB)
            = (saborA ++ " con " ++ saborB, temperaturaA `min` temperaturaB, (proporcionA + proporcionB) / 2)
