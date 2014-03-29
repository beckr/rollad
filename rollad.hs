-- @File       : rollad.hs
-- @Author     : Raphaël BECK
-- @Date       : 6/10/11
-- @Description: A simple dice roller for D&D players
-- Compile & launch: ghc --make rollad.hs && ./rollad

import System.Random

-- Constant of valid user choices
validChoices = ["1", "2", "3", "4", "5", "6", "7", "e"]

-- Function displayChoices
-- @params     : None
-- @returns    : Nothing
-- @description: Print the welcome presentation and available choices
displayChoices = do 
  let disclaimer = "Hello D&D's player!\n"
		   ++"You can roll 7 different dices:\n"
		   ++ "Type:\n"
		   ++ "\t- 1: For four sided dice\n"
		   ++ "\t- 2: For six sided dice\n"
		   ++ "\t- 3: For eight sided dice\n"
		   ++ "\t- 4: For ten sided dice\n"
		   ++ "\t- 5: For hundred sided dice\n"
		   ++ "\t- 6: For twelve sided dice\n"
		   ++ "\t- 7: For twenty sided dice\n"
		   ++ "\t- e: To exit\n"
  putStrLn disclaimer

-- Function isValidChoice
-- @params     : A String
-- @returns    : A Bool
-- @description: Validate if the string is in valid choices
isValidChoice :: String -> Bool
isValidChoice s =
  if s `elem` validChoices
     then True
     else False
     
-- Function getChoice
-- @params     : None
-- @returns    : A non null string with the user choice validated
-- @description: Get the user choice, validate it and return it
getChoice = do
  choice <- getLine
  
  if not $ isValidChoice choice
    then do
      displayChoices
      putStrLn ("Your have to choose something valid\n")
      choice <- getChoice
      return choice
    else return choice

-- Function rollDice
-- @params     : Number of faces
-- @returns    : An (IO) Integer which is the result of the roll
-- @description: Get the result of rolling a dice
rollDice :: Integer -> IO Integer
rollDice faces = do
  --TODO: 100 faces
  gen <- getStdGen
  let (result, newGen) = randomR (1, faces) gen
  setStdGen newGen

-- In d&d rules, 100 dices have only faces of multiple of 10
  if (faces == 100)
     then do
      if ((result `mod` 10) /= 0)
         then do
           result <- rollDice faces
           return result
         else
           return result
    else
      return result


router :: [Char] -> IO Integer
router choice = case choice of "1" -> rollDice(4)
                               "2" -> rollDice(6)
                               "3" -> rollDice(8)
                               "4" -> rollDice(10)
                               "5" -> rollDice(100)
                               "6" -> rollDice(12)
                               "7" -> rollDice(20)
                               _ -> return 0
--                                "e" -> return 0

-- Function submain
-- @params     : r the result of the dice roll
-- @returns    : Nothing
-- @description: Print the result and re-call the main
submain r = do
  putStrLn ("Your result is: ")
  print r
  main

-- Function main
-- @params     : None
-- @returns    : Implicitly IO()
-- @description: The main handler
main = do
  displayChoices
  let choice = getChoice
--   putStrLn <- choice
  strChoice <- choice
  r <- (router strChoice)
  if r == 0
     then do
       putStrLn ("Bye")
     else
      submain r

