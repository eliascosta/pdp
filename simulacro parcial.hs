import Data.List
import Text.Show.Functions

type Enfermedades = Paciente -> Paciente

data Paciente = Paciente{
	
	nombrePaciente :: String,
	sexo :: Char,
	edad :: Int,
	peso :: Float,
	preexistencias :: [String]
} deriving (Show)

data Tratamiento = Tratamiento{
	
	nombreTratamiento :: String,
	costoTratamiento :: Float,
	sesiones :: Int,
	enfermedad :: String
} deriving (Show)

data Solicitud = Solicitud{
	
}

jose = Paciente{
	
	nombrePaciente = "Jose",
	sexo = 'H',
	edad = 34,
	peso = 78.9,
	preexistencias = ["zamorreaDistopica"]
}

analia = Paciente{
	nombrePaciente = "Analia",
	sexo = 'M',
	edad = 34,
	peso = 70,
	preexistencias = []
}

x1 = Tratamiento{
	nombreTratamiento = "x1",
	costoTratamiento = 5000,
	sesiones = 30,
	enfermedad = "zamorreaDistopica"
}

xfg23 = Tratamiento{
	nombreTratamiento = "xfg23",
	costoTratamiento = 10000,
	sesiones = 2,
	enfermedad = "zamorreaDistopica"
}

diagnosticarPreexistencia :: Paciente -> String-> Paciente

diagnosticarPreexistencia socio pre = socio { preexistencias = pre : (preexistencias socio)}

obesidad  =  (> 150)

edadAvanzada  = (> 75)

muyEnfermo = (>8).length

estaEnRiesgo :: Paciente -> Bool
estaEnRiesgo socio  = (obesidad.peso) socio || (edadAvanzada.edad) socio || (muyEnfermo.preexistencias) socio

prestacionTotal solicitud | any ((==.enfermedad) (tratamiento solicitud)) ((socio solicitud) preexistencias) = 0
						  | otherwise = (costoTratamiento.tratamiento) solicitud

prestacionSinPreexistencias solicitud | all ((/=.enfermedad) (tratamiento solicitud)) ((socio solicitud) preexistencias) = 0
						 			  | otherwise = (costoTratamiento.tratamiento) solicitud
						 			  

f:: (a->Bool)->([a]->Bool)->((a->Bool) -> Int)->[a]-> Int
f a y z h | (y . filter a) h = length h
		  | otherwise = z a