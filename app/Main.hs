module Main where

import Ai (pickAction)
import Ansi (clearScreen, makeBlue, makeBold, makeGray, makeRed, makeUnderline)
import Cards (Card (Card), CardColor (Blue, Green, Red, White, Yellow), CardNumber, cardColor, cardNumber, colorName, colored, deckSize, longCardName, startingDeck)
import Control.Monad (forM_)
import Control.Monad.State.Strict (State, StateT (runStateT))
import Data.Char (chr, ord, toUpper)
import Data.Foldable (find)
import Data.Functor (($>))
import GHC.Conc (par)
import Game (Action (Discard, Hint, Play), ActionResult (Discarded, Hinted, Played), CardState (actual), GameState (deck, fuseTokens, hands, infoTokens, piles), Hint (ColorHint, NumberHint), Player (Computer, Human), discardPile, genStartingState, hasGameEnded, maxFuseTokens, maxInfoTokens, otherPlayer, pileToInt, play)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.Random (StdGen, initStdGen)
import System.Random.Stateful (StateGenM, runStateGen_)
import Utils (enumerate)
import Vec (Vec (toList, toListWithKey), (!))

printGameState :: GameState -> IO ()
printGameState state = do
  let putElement = putStr . (' ' :)

  let printLine title contents = do
        putStr title
        forM_ contents putElement
        putChar '\n'

  let token color True = color "*"
      token _ False = makeGray "*"

  let pile color card = colored color (show $ pileToInt card)

  putStr clearScreen

  printLine "Played:  " (map (uncurry pile) (toListWithKey (piles state)))

  printLine "Computer:" (map (show . actual) (hands state ! Computer))
  printLine "You:     " (map (const (makeGray "?")) (hands state ! Human))

  printLine "Infos:   " [token makeBlue (i <= infoTokens state) | i <- [1 .. maxInfoTokens]]
  printLine "Fuses:   " [token makeRed (i <= fuseTokens state) | i <- [1 .. maxFuseTokens]]

  let size = deckSize $ deck state
  printLine "Deck:    " [show size ++ if size == 1 then " card" else " cards"]

data OptionResult a = Selected a | Retry deriving (Functor)

promptRetry :: GameState -> IO () -> (Char -> IO (OptionResult a)) -> IO a
promptRetry state printPrompt handleAnswer = do
  printGameState state
  putChar '\n'
  printPrompt
  key <- getChar
  answer <- handleAnswer key
  case answer of
    Selected value -> return value
    Retry -> promptRetry state printPrompt handleAnswer

data Option a = Option {key :: Char, label :: String, action :: IO (OptionResult a), visible :: Bool}

prompt :: GameState -> String -> [Option a] -> IO a
prompt state heading allOptions =
  let options = filter visible allOptions
      printOption option = do
        putStr (makeBold [key option])
        putStr ": "
        putStrLn (label option)
      printPrompt = do
        putStrLn (makeUnderline heading)
        forM_ options printOption
      handleAnswer k = case find (\o -> key o == k) options of
        Just option -> action option
        Nothing -> return Retry
   in promptRetry state printPrompt handleAnswer

capitalize :: String -> String
capitalize "" = ""
capitalize (first : rest) = toUpper first : rest

promptTurn :: GameState -> IO (Maybe Action)
promptTurn state =
  let handle wrapper = fmap (fmap (Just . wrapper))
   in prompt
        state
        "What do you want to do?"
        [ Option {key = 'p', label = "Play a card", action = handle Play (promptCard state "play"), visible = True},
          Option {key = 'd', label = "Discard a card", action = handle Discard (promptCard state "discard"), visible = True},
          Option {key = 'h', label = "Give a hint", action = handle Hint (promptHint state), visible = infoTokens state > 0},
          Option {key = 'l', label = "Look through discard pile", action = showDiscardPile state $> Retry, visible = True},
          Option {key = 'Q', label = "Quit", action = return (Selected Nothing), visible = True}
        ]

backOption :: Option (OptionResult a)
backOption = Option {key = 'q', label = "Back", action = return (Selected Retry), visible = True}

numberToOrdinal :: Int -> String
numberToOrdinal 1 = "first"
numberToOrdinal 2 = "second"
numberToOrdinal 3 = "third"
numberToOrdinal 4 = "fourth"
numberToOrdinal n = show n ++ "th"

promptCard :: GameState -> String -> IO (OptionResult Int)
promptCard state action =
  let cardOptions =
        [ Option
            { key = chr (ord '0' + i),
              label = capitalize $ numberToOrdinal i ++ " card",
              action = return (Selected (Selected (i - 1))),
              visible = True
            }
        | i <- [1 .. length (hands state ! Human)]
        ]
   in prompt
        state
        ("Which card do you want to " ++ action ++ "?")
        (cardOptions ++ [backOption])

colorKey :: CardColor -> Char
colorKey Red = 'r'
colorKey Yellow = 'y'
colorKey Green = 'g'
colorKey Blue = 'b'
colorKey White = 'w'

numberKey :: CardNumber -> Char
numberKey number = chr (ord '1' + fromEnum number)

promptHint :: GameState -> IO (OptionResult Hint)
promptHint state =
  let handle wrapper = fmap (fmap (Selected . wrapper))
   in prompt
        state
        "Which attribute do you want to hint?"
        [ Option
            { key = 'c',
              label = "Color",
              action = handle ColorHint (promptHintColor state),
              visible = True
            },
          Option
            { key = 'n',
              label = "Number",
              action = handle NumberHint (promptHintNumber state),
              visible = True
            },
          backOption
        ]

promptHintColor :: GameState -> IO (OptionResult CardColor)
promptHintColor state =
  let options =
        [ Option
            { key = colorKey color,
              label = colored color $ capitalize $ colorName color,
              action = return (Selected (Selected color)),
              visible = any (\c -> cardColor (actual c) == color) (hands state ! Computer)
            }
        | color <- [minBound .. maxBound]
        ]
   in prompt
        state
        "Which color do you want to hint?"
        (options ++ [backOption])

promptHintNumber :: GameState -> IO (OptionResult CardNumber)
promptHintNumber state =
  let options =
        [ Option
            { key = numberKey number,
              label = show number,
              action = return (Selected (Selected number)),
              visible = any (\c -> cardNumber (actual c) == number) (hands state ! Computer)
            }
        | number <- [minBound .. maxBound]
        ]
   in prompt
        state
        "Which name do you want to hint?"
        (options ++ [backOption])

showDiscardPile :: GameState -> IO ()
showDiscardPile state = confirmMessage state $ do
  putStrLn (makeUnderline "Discard pile:")

  let pile = discardPile state
      printPrefix i = case i of
        0 -> pure ()
        _ -> putChar ' '
      printList list = forM_ (enumerate list) (\(i, e) -> printPrefix i *> e)
      printCard card present = case present of
        True -> putStr $ show card
        False -> putStr $ makeGray $ show $ cardNumber card
      printCardState card = printList [printCard card (i <= pile ! card) | i <- [1 .. startingDeck ! card]]
      printColorState color = do
        printList [printCardState (Card color number) | number <- [minBound .. maxBound]]
        putChar '\n'

  sequence_ [printColorState color | color <- [minBound .. maxBound]]

confirmMessage :: GameState -> IO () -> IO ()
confirmMessage state message = do
  printGameState state
  putChar '\n'

  message

  putChar '\n'
  putStrLn "Press any key to continue."

  _ <- getChar
  return ()

printAction :: Player -> ActionResult -> IO ()
printAction player action = do
  case player of
    Computer -> putStr "The computer "
    Human -> putStr "You "

  case action of
    Played card success -> do
      putStr ("played a " ++ longCardName card)
      if success
        then putStrLn "."
        else putStrLn ", which was a misfire."
    Discarded card -> putStrLn ("discarded a " ++ longCardName card ++ ".")
    Hinted hint cards -> do
      case player of
        Computer -> putStr "told you your "
        Human -> putStr "told the computer his "

      let len = length cards
          listElementPrefix i
            | i == 0 = ""
            | i == (len - 1) = " and "
            | otherwise = ", "
      forM_ (enumerate cards) (\(i, c) -> putStr (listElementPrefix i ++ numberToOrdinal (c + 1)))

      putStr (if len == 1 then " card is " else " cards are ")

      case hint of
        ColorHint color -> putStr $ show color
        NumberHint number -> putStr $ "a " ++ show number

      putStrLn "."

showAction :: GameState -> Player -> ActionResult -> IO ()
showAction state player action = confirmMessage state (printAction player action)

showGameEnd :: GameState -> IO ()
showGameEnd state =
  confirmMessage
    state
    ( do
        if fuseTokens state <= 0
          then putStrLn "You ran out of fuse tokens. You lose."
          else
            let points = sum $ map pileToInt $ toList $ piles state
             in putStrLn ("The game is over. You got " ++ show points ++ " points.")
    )

showComputerThinking :: GameState -> IO ()
showComputerThinking state = do
  printGameState state
  putChar '\n'

  putStrLn "The computer is picking a move…"

runStateGenIO :: (StateGenM StdGen -> State StdGen a) -> IO a
runStateGenIO f = do
  rng <- initStdGen
  return (runStateGen_ rng f)

runTurn :: Player -> GameState -> IO () -> IO (Maybe (GameState, IO ()))
runTurn player state showPrevAction = do
  maybeAction <- case player of
    Human -> do
      showPrevAction
      promptTurn state
    Computer -> do
      let action = pickAction state
      par action showPrevAction
      showComputerThinking state
      return $ Just action

  case maybeAction of
    Just action -> do
      (result, newState) <- runStateGenIO (\g -> runStateT (play player action g) state)
      let showThisAction = showAction newState player result

      if hasGameEnded newState
        then do
          showThisAction
          showGameEnd newState
          return Nothing
        else return $ Just (newState, showThisAction)
    Nothing -> return Nothing

runGame :: Player -> GameState -> IO () -> IO ()
runGame player state showPrevAction = do
  maybeState <- runTurn player state showPrevAction
  forM_ maybeState (uncurry (runGame (otherPlayer player)))

main :: IO ()
main = do
  state <- runStateGenIO genStartingState
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  runGame Human state (pure ())
