module TP where
-- TP funcional 2018 

-- Ejercicio 1.1:
import Data.List
import Text.Show.Functions

data Microprocesador = Microprocesador {
    nombre :: String,
    acumuladorA :: Int,
    acumuladorB :: Int,
    memoria :: [Int],
    memoriaPrograma :: [Instruccion],
    programCounter :: Int,
    mensajeError :: String
} deriving (Show)

type Instruccion = Microprocesador -> Microprocesador

-- Ejercicio 1.2: y otros microprocesadores

xt8088 = Microprocesador{
    nombre = "xt8088",
    acumuladorA = 0,
    acumuladorB = 0,
    memoria = replicate 1024 0,
    memoriaPrograma = [add,nop,lodv 22,swap,lodv 10],
    programCounter = 0,
    mensajeError = ""
}

at8086 = Microprocesador{
    nombre = "at8086",
    acumuladorA = 0,
    acumuladorB = 0,
    memoria = [1..20],
    memoriaPrograma = [],
    programCounter = 0,
    mensajeError = ""
}

fp20 = Microprocesador{
    nombre = "fp20",
    acumuladorA = 7,
    acumuladorB = 24,
    memoria = replicate 1024 0,
    memoriaPrograma = [add,lodv 22,swap,lodv 10],
    programCounter = 0,
    mensajeError = ""
}

tp02 = Microprocesador{
    nombre = "fp20",
    acumuladorA = 7,
    acumuladorB = 24,
    memoria = [0..],
    memoriaPrograma = [add,lodv 22,swap,lodv 10],
    programCounter = 0,
    mensajeError = ""
}

-- Ejerecicio 2.1:

-- Funcion NOP. No operation, el programa sigue en la próxima instrucción.

--Ejercicio 2.2

-- > nop.nop.nop $ xt8088

-- Ejercicio 3.1:
--LODV val. Carga en el acumulador A el valor val

lodv::Int -> Microprocesador -> Microprocesador
lodv x micro = micro {acumuladorA = x}

--SWAP
swap::Microprocesador -> Microprocesador
swap micro = micro {acumuladorB = (acumuladorA) micro, acumuladorA = (acumuladorB) micro}

--ADD
add::Microprocesador -> Microprocesador
add micro = setBToZero (lodv ((acumuladorA micro) + (acumuladorB micro)) micro)


-- Ejercicio 3.2:
--sumarValores micro valor1 valor2 = micro {acumuladorA = ((+valor1).(+valor2).acumuladorA) micro, programCounter = ((+1).(+1).(+1).(+1).programCounter) micro}

sumarValores:: Microprocesador -> Int -> Int -> Microprocesador
sumarValores micro valor1 valor2 = add (lodv valor1 (swap (lodv valor2 micro)))

-- Ejercicio 4.1:

{- DIVM Divide el valor del acumulador A por el valor del acumulador B,
 el resultado queda en el acumulador A, el acumulador B debe quedar en 0 
 
 CAMBIAR
 -} 

divm::Microprocesador -> Microprocesador 
divm micro 
    | acumuladorB micro == 0 = micro {mensajeError = "DIVISION BY ZERO" ++ mensajeError micro}
divm micro = setBToZero (lodv (div (acumuladorA micro) (acumuladorB micro)) micro)

setBToZero::Microprocesador -> Microprocesador
setBToZero micro = micro {acumuladorB = 0 }


--STR addr val Guarda el valor val en la posición addr de la memoria de datos
-- CAMBIAR

str::Microprocesador -> Int -> Int -> Microprocesador
str micro addr val = micro {memoria = ((take (addr - 1) (memoria micro)) ++ [val] ++ (drop addr (memoria micro)))}

--str::Microprocesador -> Int -> Int -> Microprocesador
--str micro addr val = 
--    let leftList = take (addr - 1) (memoria micro)
--        rightList = drop addr (memoria micro)
--    in nop (micro {memoria = (leftList ++ [val] ++ rightList)})

--LOD addr Carga el acumulador A con el contenido de la memoria de datos en la posición addr

lod::Microprocesador -> Int -> Microprocesador
lod micro addr = micro {acumuladorA = (memoria micro) !! (addr - 1)}

-- O EN ghci> divm (lod (swap (lod (str (str micro 1 2) 2 0) 2)) 1)
nopAdicional:: Microprocesador -> Microprocesador
realizarFuncion funcion = nopAdicional.funcion
nop = id
nopAdicional micro = micro {programCounter = ((+ 1).programCounter) micro}

--PARTE 2

--3.1
cargarPrograma::Microprocesador -> [Instruccion] -> Microprocesador
cargarPrograma micro programa = micro {memoriaPrograma = programa}

--3.2
ejecutarPrograma::Microprocesador -> Microprocesador
ejecutarPrograma micro = foldr ($) micro (memoriaPrograma micro)

ejecutarPrograma'::Microprocesador -> [Instruccion] -> Microprocesador
ejecutarPrograma' micro programa = foldr (realizarFuncion) micro programa

--3.3
ifnz micro
    | acumuladorA micro == 0 = micro
    | acumuladorA micro /=0 = ejecutarPrograma micro

--3.4
depurarMicro :: Microprocesador -> Microprocesador
depurarMicro micro = micro{ memoriaPrograma = (filter (\x -> (acumuladorA.x) micro + (acumuladorB.x) micro + ((sum.memoria.x) micro) >0) (memoriaPrograma micro))}

--3.5
estaOrdenado :: [Int] -> Bool
estaOrdenado (x:y:xs) = (x < y ) && estaOrdenado xs


