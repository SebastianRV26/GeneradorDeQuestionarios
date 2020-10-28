
------------- Formato de las preguntas  -----------------
-- [[[preguntas: Pares], [respuestas: impares]]]
-- [[["Pregunta1"], ["Respuesta1", "Respuesta2"], ["Pregunta2"], ["Respuesta1", "Respuesta2"]]]
e :: [[[String]]]
e = [[["Cuanto tiempo dedicas todos los dias a la tarea?"],["Menos de 2 horas","2-3 horas","3-4 horas","4-5 horas","Mas de 5 horas"],["Prefiere las clases virtuales o presenciales?"],["Virtuales","Presenciales"],["Tu maestro te anima a desempenarte mejor?"],["Si, todo el tiempo","Solo a veces","No, en absoluto"]],[["Se vio afectado economicamente por la pandemia?"],["Si","No"],["Cumple la cuarentena?"],["Si","No"],["Hace ejercicio en su casa?"],["Si","No"],["Desea que termine la pandemia?"],["Si","No"]]]
r :: [[[String]]]
r = [[["0","Mas de 5 horas","Presenciales","Si, todo el tiempo"]],[["0","4-5 horas","Presenciales","Si, todo el tiempo"]],[["1","Si","Si","Si","Si"]],[["1","Si","Si","No","Si"]],[["1","Si","No","No","Si"]]]
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
responderEncuestaManualAux :: [[String]] -> [String] -> IO [String]
responderEncuestaManualAux listAux xs = do
  if ((null listAux) == False) then do
    if (length (head listAux) /= 1) then do
      print ("Ingrese la posicion de su respuesta")
      print (head listAux)
      pos <- getLine
      let index = read pos ::Int
      let lr = head listAux !! index
      let lra = (xs ++ [lr])
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

  print ("Desea responder otra encuesta? (y / n)")
  r2 <- getLine
  if (r2 == "y" || r2 == "Y" || r2 == "s" || r2 == "S") then do
    responderEncuestaManual xs (respuestas ++ [[([encuesta] ++ x)]])
  else
    return (respuestas ++ [[([encuesta] ++ x)]])

--generarRandom :: Int -> Int
--generarRandom len = 

-- Auxiliar de Responder encuestras automáticas.
-- @param pos: par o impar.
-- @param xs: lista.
responderEncuestaAutomaticaAux :: [[String]] -> [String] -> IO [String]
responderEncuestaAutomaticaAux listAux xs = do
  if ((null listAux) == False) then do
    if (length (head listAux) /= 1) then do
      let pos = "0"
      let index = read pos ::Int
      let lr = head listAux !! index
      let lra = (xs ++ [lr])
      responderEncuestaAutomaticaAux (tail listAux) lra
    else do
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
  print ("Desea responder otra encuesta? (y / n)")
  r2 <- getLine
  if (r2 == "y" || r2 == "Y" || r2 == "s" || r2 == "S") then do
    responderEncuestaAutomatica xs (respuestas ++ [[([encuesta] ++ x)]])
  else
    return (respuestas ++ [[([encuesta] ++ x)]])

-- Estadística
-- 3 variables de interes
validar :: [[String]] -> String -> [String]
validar xs encuesta = do
  if ((head (xs !! 0)) == encuesta) then
    tail (xs !! 0)
  else
    []

-- [[["0","r4","r1"]],[["0","r1","r1"]], [["0","r4","r1"]], [["0","r3","r1"]]] -> [["r4","r1"],["r1","r1"],["r4","r1"],["r3","r1"]]
listaRespuestasDeEncuesta :: [[[String]]] -> String -> [[String]]
listaRespuestasDeEncuesta xs encuesta = do
  let y = map (\x -> validar x encuesta) xs
  filter (\x -> x/=[]) y

-- [["r5","r2"],["r4","r1"]] -> [["r5","r4"],["r2","r1"]] 
unirLista :: [[String]] -> [[String]]
unirLista xs = do 
  let z = zip [0..((length (head xs)) -1)] xs
  map (\(x, y) -> respuestasDePregunta xs x) z
   
-- [["r5","r2"],["r4","r1"]] -> 0 -> ["r5","r4"]
respuestasDePregunta :: [[String]] -> Int -> [String]
respuestasDePregunta xs index = map (\x -> (x !! index)) xs

-- Primer estadística
-- @param xs: lista de preguntas
-- @param numQuestion: numero de la pregunta
-- @return 

-- cuantas veces aparece un elemento en una lista
apariciones :: [String] -> String -> Int
apariciones xs element = do
  if (xs == []) then do
    0
  else do
    if ((head xs) ==  element) then do
      1 + (apariciones (tail xs) element)
    else do
      0 + (apariciones (tail xs) element)

-- Si aparece un elemento en la lista
aparece :: [String] -> String -> Bool
aparece xs element = do
  if (xs == []) then do
    False
  else do
    if ((head xs) ==  element) then do
      True
    else do
      (aparece (tail xs) element)

-- Calcular el promedio
percent :: Int -> Int -> Float
percent x y =   100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

-- Auxiliar de estadistica1
estadistica1Aux :: [String] -> [String] -> [String]-> IO ()
estadistica1Aux xs pos yaesta = do
  if (pos == []) then do
    putStr ""
  else do
    let ap = apariciones xs (head pos)
    let len = (length xs)
    let media = percent ap len
    let m = show media
    if (aparece yaesta (head pos) == False) then do
      print ("El promedio de la respuesta " ++ (head pos) ++ " es " ++ m ++ "%")
      estadistica1Aux xs (tail pos) (yaesta ++ [(head pos)])
    else
      estadistica1Aux xs (tail pos) (yaesta ++ [(head pos)])

-- Sacar la media de todas las preguntas 
estadistica1 :: [[String]] -> Int -> IO ()
estadistica1 respuestas index =  do
  if (respuestas == []) then do
    putStr ""
  else do
    let p = show (index + 1)
    print ("Pregunta " ++ p)
    estadistica1Aux (respuestas !! index) (respuestas !! index) []
    if ((index + 1) == (length respuestas)) then
      putStr ""
    else
      -- if (aparece )
      estadistica1 respuestas (index + 1)

-- Cantidad de veces respondida una encuesta
estadistica2 :: [[[String]]] -> String -> IO ()
estadistica2 xs encuest = do
  let y = length (listaRespuestasDeEncuesta xs encuest)
  let m = show y
  print ("La encuesta " ++ encuest ++ " ha sido respondida " ++ m ++ " veces")

-- Cantidad de encuestas creadas
estadistica3 :: [[[String]]] -> IO ()
estadistica3 xs = do
  let y = show (length xs)
  print ("Actualmente existen " ++ y ++ " encuestas creadas")

-- Cantidad de preguntas que contiene una encuesta
estadistica4 :: [[[String]]] -> String -> IO ()
estadistica4 xs encuest = do
  let encuesta = read encuest
  let leni = div (length (xs !! encuesta)) 2
  let lens = show leni
  print ("La encuesta " ++ encuest ++ " posee " ++ lens ++ " preguntas")

-- Menús
opciones3 :: IO ()
opciones3 = do
  print "1. Estadistica 1 el promedio de respuesta de las preguntas"
  print "2. Estadistica 2 cantidad de veces respondida una encuesta"
  print "3. Estadistica 3 cantidad de encuestas creadas"
  print "4. Estadistica 4 cantidad de preguntas que contiene una encuesta"
  print "5. Atras (Menu principal)"

menuEstadisticas :: [[[String]]] -> [[[String]]] -> IO ()
menuEstadisticas xs respuestas = do
  putStr "\t Menu estadisticas\n"
  opciones3
  r <- getLine
  if (r == "1") then do
    print "Estadistica 1 el promedio de respuesta de las preguntas"
    print ("Ingrese la encuesta a utilizar")
    re <- getLine
    let lis = unirLista (listaRespuestasDeEncuesta respuestas re)
    estadistica1 lis 0
    menuEstadisticas xs respuestas
  else if (r == "2") then do
    print "Estadistica 2 cantidad de veces respondida una encuesta"
    print ("Ingrese la encuesta a utilizar")
    re <- getLine
    estadistica2 respuestas re
    menuEstadisticas xs respuestas
  else if (r == "3") then do
    print "Estadistica 3 cantidad de encuestas creadas"
    estadistica3 xs
    menuEstadisticas xs respuestas
  else if (r == "4") then do
    print "Estadistica 3 cantidad de preguntas que contiene una encuesta"
    print ("Ingrese la encuesta a utilizar")
    re <- getLine
    estadistica4 xs re
    menuEstadisticas xs respuestas
  else if (r == "5") then do
    print "Atras"
    menu0 xs respuestas
    putStr ""
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
  --menu0 [] []
  menu0 e r
  putStr ""

-- Cuando se finaliza el programa.
fin :: IO [[[String]]]
fin = do
  print ("Fin del programa")
  return []