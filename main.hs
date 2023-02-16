raisingAsterisks, decreasingAsterisks, triangle, invertedTriangle :: Int -> [String]

raisingAsterisks n = do
  take n $ iterate ('*' :) "*"
  
decreasingAsterisks = do
  reverse . raisingAsterisks
  
triangle n = do
  raisingAsterisks n
  
invertedTriangle n = do
  decreasingAsterisks n

inputError =
  "[Error]\nThe command number entered does not exist."

errorPrint err = do
  putStrLn err

titlePrint = do
  putStrLn "[별찍기]"

commandListPrint = do
  putStrLn "---------------"
  putStrLn "| 1. 삼각형   |"
  putStrLn "| 2. 역삼각형 |"
  putStrLn "---------------"

commandInput = do
  putStr "Enter a command number.\n> "
  command <- fmap read getLine :: IO Int
  return command

valueInput = do
  putStr "Please enter a floor.\n> "
  value <- fmap read getLine :: IO Int
  return value

resultPrint shape value = do
  putStrLn "[Result]"
  if shape == "triangle"
    then do
      mapM_ putStrLn $ triangle value
  else
    mapM_ putStrLn $ invertedTriangle value

commandController command value = do
  if command == 1
    then do
      resultPrint "triangle" value
    else
      resultPrint "invertedTriangle" value
       

main :: IO ()
main = do
  titlePrint
  commandListPrint
  command <- commandInput
  if not(command == 1) && not(command == 2) then do
    errorPrint inputError
  else do
    value <- valueInput
    commandController command value
  putStrLn ""
  main