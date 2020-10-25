
import Control.Monad 

------------- Formato de las preguntas  -----------------
-- [[[preguntas: Pares], [respuestas: impares]]]
-- [[["Pregunta1"], ["Respuesta1", "Respuesta2"], ["Pregunta2"], ["Respuesta1", "Respuesta2"]]]
e :: [[[String]]]
e = [[["p1"],["r1","r2"],["p2"],["r1","r2"]],[["q2p1"],["r1","r2"],["q2p2"],["r1","r2"]]]
----------------------- Functions -----------------------

-- Función recuersiva para agregar respuestas
-- @param xs: lista 
agregarRespuestas1 :: [String] -> IO [String]
agregarRespuestas1 xs = do
  putStr "\t Ingresanndo respuesta\n"
  b <- getLine
  let x = xs ++ [b]
  print "Desea agregar otra respuesta? (y / n)"
  r <- getLine
  if (r == "y" || r == "Y" || r == "s" || r == "S") then do
    agregarRespuestas1 x
  else
    return x

-- Función recuersiva para agregar opciones de escala.
-- @param num: opciones restantes por ingresar.
-- @param xs: lista.
agregarRespuestas2 :: Int -> [String] -> IO [String]
agregarRespuestas2 num xs = do
  if (num == 0) then do
    return xs
  else do
    putStr "\t Ingresanndo respuesta\n"
    b <- getLine
    let x = xs ++ [b]
    agregarRespuestas2 (num - 1) x

-- Función recuersiva para agregar preguntas.
-- @param xs: lista 
agregarPregunta :: [[String]] -> IO [[String]]
agregarPregunta xs = do
  putStr "\t Ingresanndo pregunta\n"
  b <- getLine
  let x = xs ++ [[b]]
  print ("Ingrese el tipo de respuesta: 1. Escala del 1 al 5, 2. Respuestas con cantidad variable")
  r1 <- getLine
  if (r1 == "1") then do
    y <- agregarRespuestas2 5 []
    print "Desea agregar otra pregunta? (y / n)"
    r <- getLine
    let z = x ++ [y]
    if (r == "y" || r == "Y" || r == "s" || r == "S") then do
      agregarPregunta z
    else
      return z
  else do
    y <- agregarRespuestas1 []
    print "Desea agregar otra pregunta? (y / n)"
    r <- getLine
    let z = x ++ [y]
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
    let z = x ++ [y]
    if (r == "y" || r == "Y" || r == "s" || r == "S") then do
      agregarEncuesta (num-1) z
    else
      return z

-- Función que retorna una lista de índices
positions :: Int -> [Int]
positions len = [0..len]

-- Auxiliar de Responder encuestras manual.
-- @param pos: par o impar.
-- @param xs: lista.
responderEncuestaManualAux :: Int -> [String] -> IO String
responderEncuestaManualAux pos list = do
  if (even pos) then do
    let x = (list !! 0)
    print x
    return ("")
  else do
    -- imprimir opciones
    x <- getLine
    --let y = read x::Int
    return (x)

--mostrarMap :: [IO String] -> Int-> IO [String]
--mostrarMap xs num = do
--  if (num == (length xs)) then 
--    return []
--  else do
--    d <- (mostrarMap xs (num + 1))
--    --[xs !! num] ++ d


-- Responder encuestras manual
-- @param n: cantidad de cuestionarios a responder
--responderEncuestaManual:: [[[String]]] -> [[Int]] -> [Int] -> IO [[Int]]
--responderEncuestaManual xs respuestas encuestas = do
--  print ("Ingrese el numero de la encuesta que desea responder ")
--  r1 <- getLine
--  let elem = read r1 :: Int
--  let len = (length (xs !! elem) - 1)
--  let pos = positions len
--  let pares = filter (\y -> even y) pos
--  let impares = filter (\y -> odd y) pos
--  let z = zip [0..len] (e !! elem)
--  let f = map (\ (x,y) -> responderEncuestaManualAux x y) z
--  ff <- f
--  return (respuestas ++ [f])

  

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

-- Mostrar opciones a realizar en el segundo menú.
opciones2 :: IO ()
opciones2 = do
  print "1. Responder de forma automatica"
  print "2. Responder de forma manual"
  print "3. Atras (Menu inicio)"

-- Menú secundario donde se muestran las opciones a realizar.
-- @param xs: lista de encuestas.
-- @param respuestas: lista de opciones a respondidas.
-- @param encuestas: encuestas respondidas.
menu2 :: [[[String]]] -> [[Int]] -> [Int] -> IO [[[String]]]
menu2 xs respuestas encuestas = do
  putStr "\t Menu\n"
  opciones2
  r <- getLine
  if (r == "1") then do
    print "Respondiendo de forma automatica"
    menu2 xs respuestas encuestas
  else if (r == "2") then do
    print ("Escogio2 Responder de forma manual")
    --let x = responderEncuestaManual xs 
    menu2 xs respuestas encuestas -- del
  else if (r == "3") then do
    menu0 xs respuestas encuestas
  else do 
    menu2 xs respuestas encuestas

-- Mostrar opciones a realizar en el primer menú.
opciones0 :: IO ()
opciones0 = do
  print "1. Crear un cuestionario"
  print "2. Responder cuestionario"
  print "3. Estadisticas"
  print "4. Salir"
  print "5. Imprimir cuestionarios"

-- Menú principal donde se muestran las opciones iniciales.
-- @param xs: lista de encuestas.
-- @param respuestas: lista de opciones a respondidas.
-- @param encuestas: encuestas respondidas.
menu0 :: [[[String]]] -> [[Int]] -> [Int] -> IO [[[String]]]
menu0 a respuestas encuestas = do
  putStr "\t Menu de inicio\n"
  opciones0
  r <- getLine
  if (r == "1") then do
    print "Agregue una pregunta al cuestionario"
    b <- agregarPregunta []
    menu0 (a ++ [b]) respuestas encuestas
  else if (r == "2") then do
    menu2 a respuestas encuestas
  else if (r == "3") then do
    print ("Escogio3 Estadisticas")
    -- Estadisticas()----------------------
    fin
  else if (r == "4") then do
    print ("Escogio3 Salir")
    fin
  else if (r == "5") then do
    print (a)
    menu0 a respuestas encuestas
  else do 
    menu0 a respuestas encuestas

----------------------- Main -----------------------
-- Función principal, para ejecutar el proyecto con y sin datos iniciales
main :: IO ()
main = do
  menu0 [] [] []
-- Datos quemados
-- estadistica1 respuestas 0 -- usa map y lambda
  print ""

-- Cuando se finaliza el programa.
fin :: IO [[[String]]]
fin = do
  print ("Fin del programa")
  return []