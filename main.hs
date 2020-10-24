import Data.IORef --para mutar 
import Control.Monad 

----------------------- Global constants  -----------------------
listaOpciones1 :: [String]
listaOpciones1 = ["1.Muy Malo", "2.Malo",  "3.Regular", "4.Bueno", "5.Muy Bueno"]

listaOpciones2 :: [String]
listaOpciones2 = ["Si", "No"]

-- preguntas: Pares, respuestas: impares
-- [["Pregunta1"], ["Respuesta1", "Respuesta2"], ["Pregunta2"], ["Respuesta1", "Respuesta2"]]
preguntas :: [[String]]
preguntas = [["Del 1 al 5, ¿cómo han sido las medidas tomadas por el ministerio de salud?"], ["1.Muy Malo", "2.Malo",  "3.Regular", "4.Bueno", "5.Muy Bueno"], ["¿Usted cumple la cuarentena?"], ["Si", "No"], ["¿Ha sido perjudicado por la pandemia?"], ["Si", "No"]]

respuestas :: [[Integer]]
respuestas = [[5, 0, 0], [3, 0, 1]]
----------------------- Functions -----------------------

-- Agregar un elemento a una lista 
-- @param xs: lista
-- @param new_element: nuevo elemento
-- @return xs: lista
append :: String ->  [String] -> [String]
append new_element xs = xs ++ [new_element]

-- Crear respuestas
agregarRespuestas :: [String] -> IO [String]
agregarRespuestas xs = do
  b <- getLine
  let x = xs ++ [b]
  print "Desea agregar otra respuesta? (y / n)"
  r <- getLine
  if (r == "y" || r == "Y" || r == "s" || r == "S") then do
    agregarRespuestas x
  else
    return x

-- Crear encuestas (Agregar encuestras)
agregarPregunta :: [[String]] -> IO [[String]]
agregarPregunta xs = do
  b <- getLine
  let x = xs ++ [[b]]
  y <- agregarRespuestas []
  print "Desea agregar otra pregunta? (y / n)"
  r <- getLine
  let z = x ++ [y]
  if (r == "y" || r == "Y" || r == "s" || r == "S") then do
    agregarPregunta z
  else
    return z

-- Responder encuestras
-- @param n: cantidad de cuestionarios a responder
-- responderEncuestas:: Int 

-- Responder respuestas automáticas
-- responderEncuestasAutomatico

-- Estadística
-- 3 variables de interes

-- Primer estadística
-- @param xs: lista de preguntas
-- @param numQuestion: numero de la pregunta
-- @return 

--calc1 :: [Integer] -> [Integer]
--calc1 xs = map 

estadistica1 :: [[Integer]] -> Int -> [Integer]
estadistica1 xs numQuestion = map (\x -> x !! numQuestion) xs
  --(filter (==numQuestion) xs)

-- Menús
opciones1 :: IO ()
opciones1 = do
  print "1. Agregar preguntas al cuestionario"
  print "2. Responder de forma automatica"
  print "3. Responder de forma manual"

menu :: [String] -> IO [String]
menu a = do
  opciones1
  r <- getLine
  if (r == "1") then do
    print "Agregue una pregunta al cuestionario"
    b <- getLine
    print ("Escogio " ++ b)
    print "Agregue una respuesta al cuestionario"
    let x = agregarRespuestas []
    menu (a)
  else if (r == "2") then do
    b <- getLine
    print ("Escogio2 " ++ b)
    menu (a)
  else if (r == "3") then do
    b <- getLine
    print ("Escogio3 " ++ b)
    menu (a)
    --menu (append b a)
  else do 
  -- b <- input
    menu (a)
  

----------------------- Main -----------------------
main :: IO ()
main = do
  --ref <- newIORef ([] :: [String])
  --modifyIORef ref (append "XD")

  -- name <- input
  -- estadistica1 [1,5,3,1,6]  
  --modifyIORef ref (append name)
  --readIORef ref >>= print
  --menu 1
  print ""
-- Datos de prueba
-- estadistica1 respuestas 0 -- usa map y lambda
