import Data.List
import Text.Show.Functions

type Ejercicios = Deportista -> Deportista

data Deportista = Gimnasta{
	
	nombreDeportista :: String,
	nivelEnergia :: Int,
	nivelEquilibrio :: Int,
	flexibilidad :: Int,
	fuerzaFisica :: Int,
	habilidades :: [Ejercicios]
}	deriving (Show)

sonia = Gimnasta{
	
	nombreDeportista = "Sonia",
	nivelEnergia = 90,
	nivelEquilibrio = 60,
	flexibilidad = 40,
	fuerzaFisica = 50,
	habilidades = [medialuna, rolAdelante 20, saltoMortal 15 40]
}

pedro = Gimnasta{
	
	nombreDeportista = "Sonia",
	nivelEnergia = 70,
	nivelEquilibrio = 50,
	flexibilidad = 50,
	fuerzaFisica = 60,
	habilidades = [saltoConSoga 150, vertical, rolAdelante 30]
}

mitad :: Int -> Int
mitad valor = div valor 2

medialuna :: Deportista -> Deportista
medialuna  deportista =  deportista {nivelEquilibrio = ((+5).nivelEquilibrio) deportista}

rolAdelante :: Int-> Deportista -> Deportista
rolAdelante velocidad  deportista =  deportista {nivelEnergia = (+ (mitad velocidad)) (nivelEnergia deportista)}

vertical :: Deportista -> Deportista
vertical deportista = deportista {fuerzaFisica = ((+ 7).fuerzaFisica) deportista}

saltoConSoga :: Int -> Deportista -> Deportista
saltoConSoga saltos deportista = deportista {nivelEnergia = (-) (mitad saltos) (nivelEnergia deportista) ,
				 							 fuerzaFisica = (((+) saltos).fuerzaFisica) deportista }

saltoMortal :: Int -> Int -> Deportista -> Deportista
saltoMortal impulso altura deportista = deportista {fuerzaFisica = (+) altura (fuerzaFisica deportista),
												flexibilidad = (+ (mitad impulso)) (flexibilidad deportista)}

 
