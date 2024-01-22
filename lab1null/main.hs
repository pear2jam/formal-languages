import Control.Monad (forM_)
import Data.List
import System.IO

-- import Data.List.Split (splitOn)

mySplit :: Char -> String -> [String]
mySplit _ [] = []
mySplit delimiter str = case break (== delimiter) str of
  (token, rest) -> token : mySplit delimiter (drop 1 rest)

main :: IO ()
main = do
  content <- readFile "input.txt"
  -- let a = map (splitOn ",") [line | line <- lines content]
  let a = [mySplit ',' (init (tail line)) | line <- lines content]

  withFile "out.smt2" WriteMode $ \outFile -> return ()

  let res' s = withFile "out.smt2" AppendMode $ \outFile -> hPutStrLn outFile s

  let cnt' s p
        | length p > length s = 0
        | otherwise = length [i | i <- [0 .. length s - length p], isPrefixOf p (drop i s)]
  print a
  let alphabet = nub (concatMap (\[x, y] -> x ++ y) a)

  -- кол-во вхождений доминошек
  forM_ [0 .. length a - 1] $ \i -> do
    res' $ "(declare-const Md" ++ show i ++ " Int)"
    res' $ "(assert (>= Md" ++ show i ++ " 0))"

  res' ""

  -- кол-во вхождений пар доминошек
  forM_ [0 .. length a - 1] $ \i ->
    forM_ [0 .. length a - 1] $ \j -> do
      res' $ "(declare-const Md" ++ show i ++ "d" ++ show j ++ " Int)"
      res' $ "(assert (>= Md" ++ show i ++ "d" ++ show j ++ " 0))"

  res' ""

  -- определим последнюю доминошку
  forM_ [0 .. length a - 1] $ \i -> do
    res' $ "(declare-const IsLast_d" ++ show i ++ " Int)"
    res' $ "(assert (>= IsLast_d" ++ show i ++ " 0))"

  let sum_lasts = unwords ["IsLast_d" ++ show i | i <- [0 .. length a - 1]]

  res' $ "(assert (= (+ " ++ sum_lasts ++ ") 1))"

  res' ""

  -- связываем количество доминошек и пар доминошек
  forM_ [0 .. length a - 1] $ \i -> do
    let sum_d = unwords ["Md" ++ show i ++ "d" ++ show j | j <- [0 .. length a - 1]]
    res' $ "(assert(=(+ " ++ sum_d ++ ")(- Md" ++ show i ++ " IsLast_d" ++ show i ++ ")))"

  res' ""

  -- количество букв
  forM_ [0 .. length a - 1] $ \i ->
    forM_ alphabet $ \letter -> do
      res' $ "(declare-const Lu_" ++ [letter] ++ "d" ++ show i ++ " Int)"
      res' $ "(assert (= Lu_" ++ [letter] ++ "d" ++ show i ++ " " ++ show (cnt' (a !! i !! 0) [letter]) ++ "))"

      res' $ "(declare-const Ld_" ++ [letter] ++ "d" ++ show i ++ " Int)"
      res' $ "(assert (= Ld_" ++ [letter] ++ "d" ++ show i ++ " " ++ show (cnt' (a !! i !! 1) [letter]) ++ "))"

  res' ""

  -- количество пар букв внутри доминошки
  forM_ [0 .. length a - 1] $ \i ->
    forM_ alphabet $ \letter1 ->
      forM_ alphabet $ \letter2 -> do
        res' $ "(declare-const Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ " Int)"
        res' $ "(assert (= Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ " " ++ show (cnt' (a !! i !! 0) [letter1, letter2]) ++ "))"

        res' $ "(declare-const Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ " Int)"
        res' $ "(assert (= Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ " " ++ show (cnt' (a !! i !! 1) [letter1, letter2]) ++ "))"

  res' ""

  -- пары букв на стыках доминошек
  forM_ [0 .. length a - 1] $ \i ->
    forM_ [0 .. length a - 1] $ \j ->
      forM_ alphabet $ \letter1 ->
        forM_ alphabet $ \letter2 -> do
          res' $ "(declare-const Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " Int)"
          res' $ "(assert (= Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " " ++ show (cnt' (a !! i !! 0 ++ a !! j !! 0) [letter1, letter2]) ++ "))"

          res' $ "(declare-const Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " Int)"
          res' $ "(assert (= Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " " ++ show (cnt' (a !! i !! 1 ++ a !! j !! 1) [letter1, letter2]) ++ "))"

  res' ""

  -- сравним количество букв
  forM_ alphabet $ \letter -> do
    let sum_u = unwords ["(* Md" ++ show i ++ " Lu_" ++ [letter] ++ "d" ++ show i ++ ")" | i <- [0 .. length a - 1]]
    let sum_d = unwords ["(* Md" ++ show i ++ " Ld_" ++ [letter] ++ "d" ++ show i ++ ")" | i <- [0 .. length a - 1]]
    res' $ "(assert (= (+ " ++ sum_u ++ ") (+ " ++ sum_d ++ ")  ))"

  res' ""

  -- сравним количество пар букв
  let sum_u =
        concat
          [ ["(* Md" ++ show i ++ " Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ ")" | i <- [0 .. length a - 1]]
            | letter1 <- alphabet,
              letter2 <- alphabet
          ]
          ++ concat
            [ ["(* Md" ++ show i ++ "d" ++ show j ++ " Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ ")" | i <- [0 .. length a - 1], j <- [0 .. length a - 1]]
              | letter1 <- alphabet,
                letter2 <- alphabet
            ]
  let sum_d =
        concat
          [ ["(* Md" ++ show i ++ " Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ ")" | i <- [0 .. length a - 1]]
            | letter1 <- alphabet,
              letter2 <- alphabet
          ]
          ++ concat
            [ ["(* Md" ++ show i ++ "d" ++ show j ++ " Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ ")" | i <- [0 .. length a - 1], j <- [0 .. length a - 1]]
              | letter1 <- alphabet,
                letter2 <- alphabet
            ]
  res' $ "(assert (= (+ " ++ unwords sum_u ++ ") (+ " ++ unwords sum_d ++ ") ))"

  res' "(check-sat)"
