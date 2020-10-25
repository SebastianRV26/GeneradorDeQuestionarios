
import Control.Monad 

------------- Formato de las preguntas  -----------------
-- [[[preguntas: Pares], [respuestas: impares]]]
-- [[["Pregunta1"], ["Respuesta1", "Respuesta2"], ["Pregunta2"], ["Respuesta1", "Respuesta2"]]]

----------------------- Functions -----------------------

-- Función recuersiva para agregar respuestas
-- @param xs: lista 
agregarRespuestas :: [String] -> IO [String]
agregarRespuestas xs = do
  putStr "\t Ingresanndo respuesta\n"
  b <- getLine
  let x = xs ++ [b]
  print "Desea agregar otra respuesta? (y / n)"
  r <- getLine
  if (r == "y" || r == "Y" || r == "s" || r == "S") then do
    agregarRespuestas x
  else
    return x

-- Función recuersiva para agregar preguntas
-- @param xs: lista 
agregarPregunta :: [[[String]]] -> IO [[[String]]]
agregarPregunta xs = do
  putStr "\t Ingresanndo pregunta\n"
  b <- getLine
  let x = xs ++ [[[b]]]
  y <- agregarRespuestas []
  print "Desea agregar otra pregunta? (y / n)"
  r <- getLine
  let z = x ++ [[y]]
  if (r == "y" || r == "Y" || r == "s" || r == "S") then do
    agregarPregunta z
  else
    return z

-- Función recuersiva para agregar la cantidad de encuestas solicitadas por el usuario
-- @param num: cantidad de encuestas solicitadas por el usuario.
agregarEncuesta :: Int -> [[[String]]] -> IO [[[String]]]
agregarEncuesta num xs = do
  --if (num == 0) then do
    -- responderEncuestaManual xs
  --else do
    putStr "\t Creando cuestionario\n"
    b <- getLine
    let x = xs ++ [[[b]]]
    y <- agregarPregunta []
    print "Desea agregar otro cuestionario? (y / n)"
    r <- getLine
    let z = x ++ y
    if (r == "y" || r == "Y" || r == "s" || r == "S") then do
      agregarEncuesta (num-1) z
    else
      return z

-- Responder encuestras manual
-- @param n: cantidad de cuestionarios a responder
--responderEncuestaManual:: [[[String]]] -> [[Int]]
--responderEncuestaManual xs = do

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
-- menuEstadisticas :: [[Int]] -> IO


opciones2 :: IO ()
opciones2 = do
  print "1. Responder de forma automatica"
  print "2. Responder de forma manual"
  print "3. Atras (Menu inicio)"

menu2 :: [[[String]]] -> IO [[[String]]]
menu2 xs = do
  putStr "\t Menu\n"
  opciones2
  r <- getLine
  if (r == "1") then do
    print "Respondiendo de forma automatica"
    menu2(xs)
  else if (r == "2") then do
    print ("Escogio2 Responder de forma manual")
    --let x = responderEncuestaManual xs 
    -- menuEstadisticas x
    menu2 (xs) -- del
  else if (r == "3") then do
    menu0 (xs)
  else do 
    menu2 (xs)

opciones0 :: IO ()
opciones0 = do
  print "1. Crear un cuestionario"
  print "2. Estadisticas"
  print "3. Salir"
  print "4. Imprimir cuestionarios"

menu0 :: [[[String]]] -> IO [[[String]]]
menu0 a = do
  putStr "\t Menu de inicio\n"
  opciones0
  r <- getLine
  if (r == "1") then do
    print "Agregue una pregunta al cuestionario"
    b <- agregarPregunta []
    menu0 (a ++ b)
  else if (r == "2") then do
    print ("Escogio2 Estadisticas")
    -- Estadisticas()----------------------
    fin
  else if (r == "3") then do
    print ("Escogio3 Salir")
    fin
  else if (r == "4") then do
    print (a)
    menu0 (a)
  else do 
    menu0 (a)

----------------------- Main -----------------------
main :: IO ()
main = do
  menu0 []
-- Datos quemados
-- estadistica1 respuestas 0 -- usa map y lambda
  print ""

fin :: IO [[[String]]]
fin = do
  print ("Fin del programa")
  return []