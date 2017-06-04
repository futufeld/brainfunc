import Text.Read (readMaybe)

data Zipper a = Zip | Zipper [a] a [a]
    deriving (Show)

data Instruction = Incr | Decr | Next | Prev | Open | Loop | Read | Wrte
    deriving (Show)

type Code = Zipper Instruction
type Tape = Zipper Integer

--
-- Zipper functions
--

zipper :: a -> Zipper a
zipper x = Zipper [] x []

zipperFromList :: [a] -> Zipper a
zipperFromList (x:xs) = Zipper [] x xs
zipperFromList []     = Zip

getCursor :: Zipper a -> Maybe a
getCursor (Zipper _ c _) = Just c
getCursor Zip            = Nothing

setCursor :: a -> Zipper a -> Zipper a
setCursor x (Zipper ls _ rs) = Zipper ls x rs
setCursor x Zip              = zipper x

next :: Zipper a -> Maybe (Zipper a)
next (Zipper ls c (r:rs)) = Just $ Zipper (c:ls) r rs
next _                    = Nothing

prev :: Zipper a -> Maybe (Zipper a)
prev (Zipper (l:ls) c rs) = Just $ Zipper ls l (c:rs)
prev _                    = Nothing

insertNext :: a -> Zipper a -> Zipper a
insertNext x (Zipper ls c rs) = Zipper (c:ls) x rs
insertNext x Zip              = zipper x

insertPrev :: a -> Zipper a -> Zipper a
insertPrev x (Zipper ls c rs) = Zipper ls x (c:rs)
insertPrev x Zip              = zipper x

shiftCursor :: (Zipper a -> Maybe (Zipper a))
            -> Zipper a
            -> Maybe (a, Zipper a)
shiftCursor f z = do
    z' <- f z
    c <- getCursor z'
    pure (c, z')

nextCursor :: Zipper a -> Maybe (a, Zipper a)
nextCursor = shiftCursor next

prevCursor :: Zipper a -> Maybe (a, Zipper a)
prevCursor = shiftCursor prev

--
-- Code
--

nextInstruction :: Zipper Instruction
                -> Maybe (Instruction, Zipper Instruction)
nextInstruction = nextCursor

prevInstruction :: Zipper Instruction
                -> Maybe (Instruction, Zipper Instruction)
prevInstruction = prevCursor

findMatchingLoop :: Code -> Maybe Code
findMatchingLoop code = case getCursor code of
    Just Open -> findMatchingLoop' code
    _         -> Nothing
    where
        findMatchingLoop' code = do
            (nextIns, nextCode) <- nextInstruction code
            case nextIns of
                Open -> do
                    loopedCode <- findMatchingLoop' nextCode
                    findMatchingLoop' loopedCode
                Loop -> Just nextCode
                _    -> findMatchingLoop' nextCode

findMatchingOpen :: Code -> Maybe Code
findMatchingOpen code = case getCursor code of
    Just Loop -> findMatchingOpen' code
    _         -> Nothing
    where
        findMatchingOpen' code = do
            (prevIns, prevCode) <- prevInstruction code
            case prevIns of
                Loop -> do
                    loopedCode <- findMatchingOpen' prevCode
                    findMatchingOpen' loopedCode
                Open -> Just prevCode
                _    -> findMatchingOpen' prevCode

--
-- Tape
--

applyCell :: (Integer -> Integer) -> Tape -> Tape
applyCell f z = wrteCell (f $ readCell z) z

incrCell :: Tape -> Tape
incrCell = applyCell (+1)

decrCell :: Tape -> Tape
decrCell = applyCell (subtract 1)

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe x Nothing  = x

nextCell :: Tape -> Tape
nextCell z = fromMaybe (insertNext 0 z) (next z)

prevCell :: Tape -> Tape
prevCell z = fromMaybe (insertPrev 0 z) (prev z)

readCell :: Tape -> Integer
readCell = fromMaybe 0 . getCursor

wrteCell :: Integer -> Tape -> Tape
wrteCell = setCursor

--
-- P'' interpreter
--

executeIncr :: (Code, Tape) -> (Code, Tape)
executeIncr (code, tape) = (code, incrCell tape)

executeDecr :: (Code, Tape) -> (Code, Tape)
executeDecr (code, tape) = (code, decrCell tape)

executeNext :: (Code, Tape) -> (Code, Tape)
executeNext (code, tape) = (code, nextCell tape)

executePrev :: (Code, Tape) -> (Code, Tape)
executePrev (code, tape) = (code, prevCell tape)

executeOpen :: (Code, Tape) -> (Code, Tape)
executeOpen (code, tape) = case readCell tape of
    0 -> case findMatchingLoop code of
        Just code' -> (code', tape)
        Nothing    -> error "Error: No matching ']' instruction"
    _ -> (code, tape)

executeLoop :: (Code, Tape) -> (Code, Tape)
executeLoop (code, tape) = case readCell tape of
    0 -> (code, tape)
    _ -> case findMatchingOpen code of
        Just code' -> (code', tape)
        Nothing    -> error "Error: No matching '[' instruction"

executeInstruction :: Instruction -> (Code, Tape) -> (Code, Tape)
executeInstruction Incr = executeIncr
executeInstruction Decr = executeDecr
executeInstruction Next = executeNext
executeInstruction Prev = executePrev
executeInstruction Open = executeOpen
executeInstruction Loop = executeLoop
executeInstruction _    = error "Error: Instruction not supported!"

executeCode :: Code -> Tape -> Tape
executeCode code tape = case getCursor code of
    Just ins -> executeCode' ins (code, tape)
        where
            executeCode' :: Instruction -> (Code, Tape) -> Tape
            executeCode' ins state = case nextInstruction updatedCode of
                Just (nextIns, nextCode) -> executeCode'
                    nextIns (nextCode, updatedTape)
                Nothing                  -> updatedTape
                where
                    (updatedCode, updatedTape) = executeInstruction ins state
    Nothing  -> tape

--
-- Brainfuck interpreter
--

executeRead :: (Code, Tape) -> IO (Code, Tape)
executeRead (code, tape) = do
    putStrLn . show $ readCell tape
    pure (code, tape)

executeWrte :: (Code, Tape) -> IO (Code, Tape)
executeWrte (code, tape) = do
    x <- getLine
    case readMaybe x :: Maybe Integer of
        Just x' -> pure (code, wrteCell x' tape)
        Nothing -> do
            putStrLn "Expected integer value"
            executeWrte (code, tape)

executeInstructionIO :: Instruction -> (Code, Tape) -> IO (Code, Tape)
executeInstructionIO Incr = pure . executeIncr
executeInstructionIO Decr = pure . executeDecr
executeInstructionIO Next = pure . executeNext
executeInstructionIO Prev = pure . executePrev
executeInstructionIO Open = pure . executeOpen
executeInstructionIO Loop = pure . executeLoop
executeInstructionIO Read = executeRead
executeInstructionIO Wrte = executeWrte

executeCodeIO :: Code -> Tape -> IO ()
executeCodeIO code tape = case getCursor code of
    Just ins -> executeCodeIO' ins (code, tape)
        where
            executeCodeIO' :: Instruction -> (Code, Tape) -> IO ()
            executeCodeIO' ins state = do
                (updatedCode, updatedTape) <- executeInstructionIO ins state
                case nextInstruction updatedCode of
                    Just (nextIns, nextCode) -> executeCodeIO'
                        nextIns (nextCode, updatedTape)
                    Nothing                  -> pure ()
    Nothing  -> pure ()

--
-- Completion
--

charToInstruction :: Char -> Maybe Instruction
charToInstruction '+' = Just Incr
charToInstruction '-' = Just Decr
charToInstruction '>' = Just Next
charToInstruction '<' = Just Prev
charToInstruction '[' = Just Open
charToInstruction ']' = Just Loop
charToInstruction '.' = Just Read
charToInstruction ',' = Just Wrte
charToInstruction _   = Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f (x:xs) = case f x of
    Just x' -> x' : mapMaybe f xs
    Nothing -> mapMaybe f xs
mapMaybe _ []     = []

stringToCode :: String -> [Instruction]
stringToCode = mapMaybe charToInstruction

process :: String -> IO ()
process source = executeCodeIO code Zip
    where code = zipperFromList $ stringToCode source

main :: IO ()
main = do
    putStrLn "Enter Brainfuck code:"
    input <- getLine
    process input
