-- {-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC #-}
module LogAnalysis where 
import Log
import Text.Read

-- Exercise 1

validateNumber :: String -> Bool
validateNumber n = (readMaybe n :: Maybe Int) /= Nothing

parseString :: String -> [String]
parseString [] = []
parseString (x:[]) = case x of
                        ' ' -> []
                        otherwise -> [[x]] 

parseString (x:xs) = case x of
                        ' ' -> [""] ++ parseString xs
                        otherwise -> (x : (head $ parseString xs)) : (tail $ parseString xs)

unifyString :: [String] -> String
unifyString [] = []
unifyString [x] = x
unifyString (x:xs) = x ++ " " ++ unifyString(xs)

parseSplitMessage :: [String] -> LogMessage
parseSplitMessage whole_message@(['E']:severity_number:time_stamp:message)
                        | ((validateNumber severity_number) && (validateNumber time_stamp)) 
                            = LogMessage (Error (read severity_number :: Int)) (read time_stamp :: Int) (unifyString message)
                        | otherwise = Unknown (unifyString whole_message)
parseSplitMessage whole_message@(['I']:time_stamp:message)
                        | ((validateNumber time_stamp))
                            = LogMessage Info (read time_stamp :: Int) (unifyString message)
                        | otherwise = Unknown (unifyString whole_message)

parseSplitMessage whole_message@(['W']:time_stamp:message)
                        | ((validateNumber time_stamp))
                            = LogMessage Warning (read time_stamp :: Int) (unifyString message)
                        | otherwise = Unknown (unifyString whole_message)

parseSplitMessage whole_message = Unknown (unifyString whole_message)

parseMessage :: String -> LogMessage
parseMessage s = parseSplitMessage $ parseString s


parse :: String -> [LogMessage]
-- parse filename = map parseMessage . lines $ (readFile filename)
parse file = map parseMessage (lines $ file)

printList [] = pure ()
printList (x:xs) = do
    print(x)
    printList xs

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown message) tree = tree  
insert (LogMessage message_type timestamp message) Leaf = Node Leaf (LogMessage message_type timestamp message) Leaf
insert mm@(LogMessage message_type timestamp message) (Node m_tree_left nn@(LogMessage message_type2 timestamp2 message2) m_tree_right)
    | timestamp >= timestamp2 = (Node m_tree_left nn (insert mm m_tree_right))
    | otherwise = (Node (insert mm m_tree_left) nn m_tree_right)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node m_tree_left message m_tree_right) = inOrder m_tree_left ++ [message] ++ inOrder m_tree_right

isImp :: LogMessage -> Bool
isImp (LogMessage (Error severity) timestamp message) = severity>=50
isImp _ = False

showLog :: LogMessage -> String
showLog (LogMessage mt ts st) = st
showLog (Unknown st) = st

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ls = ((map (showLog)) $ inOrder $ build $ ((filter isImp ls)))

main = do
    -- Exercise 1
    -- print $ parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
    -- print $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
    -- print $ parseMessage "This is not in the right format" == Unknown "This is not in the right format"
    -- print $ parseString "E 2 562 help help"
    -- print $ parseString "This is not in the right format"
    -- print $ unifyString $ parseString "This is not in the right format"
    -- -- print $ parse "sample.log"
    let filename = "error.log"
    contents <- readFile filename
    -- print $ lines $ contents
    -- print . map parseMessage . lines $ contents
    -- print $ parse $ contents
    printList $ parse $ contents

    -- -- print . map parseMessage . words $ contents
    -- testParse parse 10 "error.log"

    -- Exercise 5
    print("***Important Errors***")
    printList $ whatWentWrong $ parse $ contents
