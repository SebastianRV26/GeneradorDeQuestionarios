----------------------- Utils -----------------------
input :: String -> IO String
input prompt = do
    putStr prompt
    getLine


----------------------- Functions -----------------------
test :: IO ()
test = do
  number <- input "Escoja un numero"
  print("Ud escogio " ++ number)

----------------------- Main -----------------------
main :: IO ()
main = do
  name <- input "Ingrese su nombre"
  print("Hello, " ++ name)
  test
    
