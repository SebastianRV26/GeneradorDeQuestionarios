
------------- Formato de las preguntas  -----------------
-- [[[preguntas: Pares], [respuestas: impares]]]
-- [[["Pregunta1"], ["Respuesta1", "Respuesta2"], ["Pregunta2"], ["Respuesta1", "Respuesta2"]]]
e :: [[[String]]]
e = [[["p1"],["r1","r2", "r3", "r4", "r5"],["p2"],["r1","r2"]],[["q2p1"],["r1","r2"],["q2p2"],["r1","r2"]]]

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
    -- menu2 xs
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

-- Auxiliar de Responder encuestras manual.
-- @param pos: par o impar.
-- @param xs: lista.
responderEncuestaManualAux :: [[String]] -> [[String]] -> IO [[String]]
responderEncuestaManualAux listAux xs = do
  if ((null listAux) == False) then do
    if (length (head listAux) /= 1) then do
      print ("Ingrese la posicion de su respuesta")
      print (head listAux)
      pos <- getLine
      let index = read pos ::Int
      let lr = head listAux !! index
      let lra = (xs ++ [[lr]])
      responderEncuestaManualAux (tail listAux) lra
    else do
      print (head listAux !! 0)
      responderEncuestaManualAux (tail listAux) xs
  else 
    return (xs)

-- Responder encuestras manual.
-- @param xs: lista de encuestas.
-- @param respuestas: lista de respuestas.
responderEncuestaManual:: [[[String]]] -> [[[String]]] -> IO [[[String]]]
responderEncuestaManual xs respuestas = do
  print ("Ingrese el numero de la encuesta que desea responder ")
  r1 <- getLine
  let index = read r1 :: Int
  x <- responderEncuestaManualAux (xs !! index) [] 
  let encuesta = show index
  return (respuestas ++ [([[encuesta]] ++ x)])

--generarRandom :: Int -> Int
--generarRandom len = 

-- Auxiliar de Responder encuestras automáticas.
-- @param pos: par o impar.
-- @param xs: lista.
responderEncuestaAutomaticaAux :: [[String]] -> [[String]] -> IO [[String]]
responderEncuestaAutomaticaAux listAux xs = do
  if ((null listAux) == False) then do
    if (length (head listAux) /= 1) then do
      print ("Ingrese la posicion de su respuesta")
      print (head listAux)
      pos <- getLine
      let index = read pos ::Int
      let lr = head listAux !! index
      let lra = (xs ++ [[lr]])
      responderEncuestaAutomaticaAux (tail listAux) lra
    else do
      print (head listAux !! 0)
      responderEncuestaAutomaticaAux (tail listAux) xs
  else 
    return (xs)

-- Responder encuestras de forma automática.
-- @param xs: lista de encuestas.
-- @param respuestas: lista de respuestas.
responderEncuestaAutomatica:: [[[String]]] -> [[[String]]] -> IO [[[String]]]
responderEncuestaAutomatica xs respuestas = do
  print ("Ingrese el numero de la encuesta que desea responder ")
  r1 <- getLine
  let index = read r1 :: Int
  x <- responderEncuestaAutomaticaAux (xs !! index) [] 
  let encuesta = show index
  return (respuestas ++ [([[encuesta]] ++ x)])


-- Estadística
-- 3 variables de interes

-- Primer estadística
-- @param xs: lista de preguntas
-- @param numQuestion: numero de la pregunta
-- @return 

--calc1 :: [Integer] -> [Integer]
--calc1 xs = map 

-- Sacar la media de todas las preguntas 
--estadistica1 :: [[[String]]] -> [[Int]] -> Int -> IO ()
--estadistica1 xs respuestas numQuestion = map (\x -> x !! numQuestion) respuestas
  --(filter (==numQuestion) xs)

-- Menús
opciones3 :: IO ()
opciones3 = do
  print "1. Estadística 1 el promedio de las preguntas"
  print "2. Estadística 2"
  print "3. Estadística 3"
  print "4. Atras (Menu principal)"

menuEstadisticas :: [[[String]]] -> [[[String]]] -> IO ()
menuEstadisticas xs respuestas = do
  putStr "\t Menu encuestas\n"
  opciones3
  r <- getLine
  if (r == "1") then do
    print "Estadística 1"
--    estadistica1 respuestas
    menuEstadisticas xs respuestas
  else if (r == "2") then do
    print "Estadística 2"
--    estadistica1 respuestas
    menuEstadisticas xs respuestas
  else if (r == "3") then do
    print "Estadística 3"
--    estadistica1 respuestas
    menuEstadisticas xs respuestas
  else if (r == "4") then do
    print "Atras"
    menu0 xs respuestas
    print ""
  else
    menuEstadisticas xs respuestas

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
menu2 :: [[[String]]] -> [[[String]]] -> IO [[[String]]]
menu2 xs respuestas = do
  putStr "\t Menu\n"
  opciones2
  r <- getLine
  if (r == "1") then do
    print "Respondiendo de forma automatica"
    x <- responderEncuestaAutomatica xs respuestas
    menu2 xs x
  else if (r == "2") then do
    print ("Escogio2 Responder de forma manual")
    y <- responderEncuestaManual xs respuestas
    menu2 xs y -- del
  else if (r == "3") then do
    menu0 xs respuestas
  else do 
    menu2 xs respuestas

-- Mostrar opciones a realizar en el primer menú.
opciones0 :: IO ()
opciones0 = do
  print "1. Crear un cuestionario"
  print "2. Responder cuestionario"
  print "3. Estadisticas"
  print "4. Salir"
  print "5. Imprimir cuestionarios"
  print "6. Imprimir respuestas"

-- Menú principal donde se muestran las opciones iniciales.
-- @param xs: lista de encuestas.
-- @param respuestas: lista de opciones a respondidas.
-- @param encuestas: encuestas respondidas.
menu0 :: [[[String]]] -> [[[String]]] -> IO [[[String]]]
menu0 a respuestas = do
  putStr "\t Menu de inicio\n"
  opciones0
  r <- getLine
  if (r == "1") then do
    print "Agregue una pregunta al cuestionario"
    b <- agregarPregunta []
    menu0 (a ++ [b]) respuestas
  else if (r == "2") then do
    menu2 a respuestas
  else if (r == "3") then do
    print ("Escogio3 Estadisticas")
    -- Estadisticas()----------------------
    menuEstadisticas a respuestas
    menu0 a respuestas
  else if (r == "4") then do
    print ("Escogio3 Salir")
    fin
  else if (r == "5") then do
    print (a)
    menu0 a respuestas
  else if (r == "6") then do
    print (respuestas)
    menu0 a respuestas
  else do 
    menu0 a respuestas

----------------------- Main -----------------------
-- Función principal, para ejecutar el proyecto con y sin datos iniciales
main :: IO ()
main = do
-- Datos quemados
-- estadistica1 respuestas 0 -- usa map y lambda
  --let resp = [ [ [0, 0], [0,0]], [] ]
  menu0 e []
  print ""
  
  -- [[]]
-- Cuando se finaliza el programa.
fin :: IO [[[String]]]
fin = do
  print ("Fin del programa")
  return []