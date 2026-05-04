module Parcial where
import Text.Show.Functions()

type Raza = String
type Juguete = String
type TiempoEnMinutos = Int
type Energia = Int

data Rutina = Rutina{
    ejercicio :: (Perrito -> Perrito),
    tiempoEjercicio :: TiempoEnMinutos
} deriving(Show)

data Perrito = Perrito{
    raza :: Raza,
    juguetes :: [Juguete],
    tiempo :: TiempoEnMinutos,
    energia :: Energia
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

puedePermanecer :: [Rutina] -> Perrito -> Bool
puedePermanecer unaListaDeRutinas unPerrito = ((.) sum (map tiempoEjercicio) $ unaListaDeRutinas ) <= (tiempo unPerrito)

convertirListaDeEjerciciosEnUnoSolo :: [Rutina] -> (Perrito -> Perrito)
convertirListaDeEjerciciosEnUnoSolo unaListaDeRutinas = foldl (flip (.)) id (map ejercicio unaListaDeRutinas)

esResponsable :: [Rutina] -> Perrito -> Bool
esResponsable unaListaDeRutinas unPerrito = length (juguetes((convertirListaDeEjerciciosEnUnoSolo unaListaDeRutinas) unPerrito)) >= 3


zara :: Perrito
zara = Perrito {
    raza = "dalmata",
    juguetes = ["pelota", "manita"],
    tiempo = 90,
    energia = 80
}

guarderiaPdePerritos :: [Rutina]
guarderiaPdePerritos = [Rutina{ejercicio = jugar, tiempoEjercicio = 30},Rutina{ejercicio = (ladrar 18), tiempoEjercicio = 20},Rutina{ejercicio = (regalar "pelota"), tiempoEjercicio = 0},Rutina{ejercicio = diaDeSpa, tiempoEjercicio = 120},Rutina{ejercicio = diaDeCampo, tiempoEjercicio = 720}]