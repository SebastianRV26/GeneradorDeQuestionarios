import Data.IORef --para mutar 
import Control.Monad 

----------------------- Global constants  -----------------------
listaOpciones1 :: [String]
listaOpciones1 = ["1.Muy Malo", "2.Malo",  "3.Regular", "4.Bueno", "5.Muy Bueno"]

listaOpciones2 :: [String]
listaOpciones2 = ["Si", "No"]

preguntas :: [String]
preguntas = ["Del 1 al 5, ¿cómo han sido las medidas tomadas por el ministerio de salud?", "¿Usted cumple la cuarentena?", "¿Ha sido perjudicado por la pandemia?"]

respuestas :: [[Integer]]
respuestas = [[5, 0, 0], [3, 0, 1]]
----------------------- Functions -----------------------

-- Agregar un elemento a una lista 
-- @param xs: lista
-- @param new_element: nuevo elemento
-- @return xs: lista
append :: String ->  [String] -> [String]
append new_element xs = xs ++ [new_element]

-- Crear encuestas (Agregar encuestras)
-- crearEncuestas

-- Crear respuestas


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
    b <- getLine
    print ("Escogio " ++ b)
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
  

prompt :: IO ()
prompt = do
    -- get input from user
    l <- getLine
    -- unless will execute its block if the condition is False
    unless (l == "q") $ do
        -- echo back to the user
        putStrLn $ "You entered: " ++ l
        prompt
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
