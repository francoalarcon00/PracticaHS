{-
El fondo monetario internacional nos solicitó que modelemos su negocio, basado en realizar préstamos a países en apuros financieros. 
Sabemos de cada país el "ingreso per cápita" que es el promedio de lo que cada habitante necesita para subsistir, también conocemos 
la población activa en el sector público y la activa en el sector privado, la lista de recursos naturales (ej: "Minería", "Petróleo", "Industria pesada") 
y la deuda que mantiene con el FMI.

-}

-- modelamos el pais
data Pais = Pais{
    ingresoPerCapita :: Float,
    poblacionActiva_sectorPublico :: Int,
    poblacionActiva_sectorPrivado :: Int,
    recursosNaturales :: [Recurso],
    deuda :: Float
}deriving Show

-- vamos a crear las funciones que modifican al pais si acude al FMI

-- prestar dinero
type Monto = Float
prestarDinero :: Monto -> (Pais -> Pais)
prestarDinero monto pais = pais{ deuda = deuda pais + monto*2.5 }

argentina = Pais 10000 2000000 8000000 ["Soja"] 50.0

{-
Reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público y 
además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario
-}

type Cantidad = Int
reducirPuestosdeTrabajo :: Cantidad -> (Pais -> Pais)
reducirPuestosdeTrabajo puestos pais = pais {
    poblacionActiva_sectorPublico = poblacionActiva_sectorPublico pais - puestos,
    ingresoPerCapita = ingresoPerCapita pais * (1 - disminucionPerCapita puestos)
}

disminucionPerCapita :: Cantidad -> Float
disminucionPerCapita cantidad | cantidad > 100 = 0.2
                              | otherwise = 0.15

{-
darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares 
la deuda que el país mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. 
No considerar qué pasa si el país no tiene dicho recurso.

-}
type Recurso = String
darRecursosNaturales :: Recurso -> (Pais -> Pais)
darRecursosNaturales recurso pais = pais{
    deuda = deuda pais - 2000000,
    recursosNaturales = filter (/= recurso) (recursosNaturales pais)
}

{-
establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno 
(que se calcula como el ingreso per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo) 
y reducir 500 puestos de trabajo del sector público. Evitar la repetición de código.

-}

blindaje :: (Pais -> Pais)
blindaje pais = prestarDinero ((ingresoPerCapita pais * fromIntegral (poblacionActiva_sectorPrivado pais + poblacionActiva_sectorPublico pais)) / 2)
                (reducirPuestosdeTrabajo 500 pais)


namibia = Pais 4140 400000 650000 ["Mineria","Ecoturismo"] 50000000

estrategiaNueva :: (Pais -> Pais)
estrategiaNueva pais = prestarDinero 200000000 (darRecursosNaturales "Mineria" pais)

{-
Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales.

-}

zafan :: [Pais] -> [Pais]
zafan paises = filter (\pais -> elem "Petroleo" (recursosNaturales pais)) paises --podria aplicarse 


{-
Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
-}

tieneAFavor :: [Pais] -> Float
tieneAFavor paises = sum (map (\pais -> deuda pais) paises)
-- tieneAFavor paises = sum (map deuda paises)


{-
Indicar en dónde apareció cada uno de los conceptos (solo una vez) y justificar qué ventaja tuvo para resolver el requerimiento.
-}


{-
dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, 
en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor. 
Recordamos que el Producto Bruto Interno surge de multiplicar el ingreso per cápita por la población activa (privada y pública). 
-}

type Estrategia = Pais -> Pais

-- Helper top-level para calcular el PBI
pbi :: Pais -> Float
pbi pais =
  ingresoPerCapita pais
  * fromIntegral
      ( poblacionActiva_sectorPublico pais
      + poblacionActiva_sectorPrivado  pais
      )

-- Función principal
ordenadasDePeorAMejor :: Pais -> [Pais -> Pais] -> Bool

-- 0 o 1 receta: siempre ordenadas
ordenadasDePeorAMejor _ [] = True
ordenadasDePeorAMejor _ (_ : []) = True

-- 2 o más recetas: comparar paso a paso (ACA ESTA LA RECURISVIDAD -----------> REVISAR DE NUEVO ------------->)
ordenadasDePeorAMejor pais (r1 : r2 : rs) -- obtengo dos cabeceras y el resto
                        | pbi (r1 pais) < pbi (r2 (r1 pais)) = ordenadasDePeorAMejor (r1 pais) (r2 : rs)
                        | otherwise = False