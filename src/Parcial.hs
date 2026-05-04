module Parcial where
import Text.Show.Functions()

type Raza = String
type Juguete = String
type TiempoEnMinutos = Int
type Energia = Int

data Actividad = Actividad{
    ejercicio :: (Perrito -> Perrito),
    tiempoEjercicio :: TiempoEnMinutos
} deriving(Show)

data Perrito = Perrito{
    raza :: Raza,
    juguetes :: [Juguete],
    tiempo :: TiempoEnMinutos,
    energia :: Energia
} deriving(Show)

data Guarderia = Guarderia{
    nombre :: String,
    rutina :: [Actividad]
} deriving(Show)

modificarEnergia :: Int -> Perrito -> Perrito
modificarEnergia cantidadAniadir unPerrito = unPerrito{energia = max(energia unPerrito + cantidadAniadir) 0} 

jugar :: Perrito -> Perrito
jugar unPerrito = modificarEnergia (-10) unPerrito

ladrar :: Int -> Perrito -> Perrito
ladrar cantidadLadridos unPerrito
    | cantidadLadridos <= 0 = unPerrito
    | otherwise = modificarEnergia (div cantidadLadridos 2) unPerrito

regalar :: Juguete -> Perrito -> Perrito
regalar unJuguete unPerrito = unPerrito{juguetes = juguetes unPerrito ++ [unJuguete]}

razaExtravagante :: Perrito -> Bool
razaExtravagante Perrito{raza = "dalmata"} = True
razaExtravagante Perrito{raza = "pomerania"} = True
razaExtravagante _ = False

diaDeSpa :: Perrito -> Perrito
diaDeSpa unPerrito
    | razaExtravagante unPerrito || tiempo unPerrito >= 50 = unPerrito{energia = 100, juguetes = juguetes unPerrito ++ ["peine de goma"]}
    | otherwise = unPerrito

diaDeCampo :: Perrito -> Perrito
diaDeCampo unPerrito = jugar unPerrito{juguetes = (.) (drop 1) juguetes $ unPerrito}

puedePermanecer :: Guarderia -> Perrito -> Bool
puedePermanecer unaGuarderia unPerrito = ((.) sum (map tiempoEjercicio) $ rutina unaGuarderia ) <= (tiempo unPerrito)

esResponsable :: Guarderia -> Perrito -> Bool
esResponsable unaGuarderia unPerrito = length (juguetes unPerrito) >= 3


zara :: Perrito
zara = Perrito {
    raza = "dalmata",
    juguetes = ["pelota", "manita"],
    tiempo = 90,
    energia = 80
}

pdePerritos :: Guarderia
pdePerritos = Guarderia{nombre = "PdePerritos", rutina = [Actividad{ejercicio = jugar, tiempoEjercicio = 30},Actividad{ejercicio = (ladrar 18), tiempoEjercicio = 20},Actividad{ejercicio = (regalar "pelota"), tiempoEjercicio = 0},Actividad{ejercicio = diaDeSpa, tiempoEjercicio = 120},Actividad{ejercicio = diaDeCampo, tiempoEjercicio = 720}]}