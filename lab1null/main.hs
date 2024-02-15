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

  forM_ [0 .. length a - 1] $ \i ->
    forM_ [0 .. length a - 1] $ \j -> do
      res' $ "(declare-const MMd" ++ show i ++ "d" ++ show j ++ " Int)"
      res' $ "(assert (>= MMd" ++ show i ++ "d" ++ show j ++ " 0))"

  res' ""

  -- синхронизация

  forM_ [0 .. length a - 1] $ \i ->
    forM_ [0 .. length a - 1] $ \j -> do
      res' $ "(assert (= Md" ++ show i ++ "d" ++ show j ++ " MMd" ++ show i ++ "d" ++ show j ++ "))"


  res' ""

  -- определим последнюю доминошку
  forM_ [0 .. length a - 1] $ \i -> do
    res' $ "(declare-const IsLast_d" ++ show i ++ " Int)"
    res' $ "(assert (>= IsLast_d" ++ show i ++ " 0))"

  let sum_lasts = unwords ["IsLast_d" ++ show i | i <- [0 .. length a - 1]]

  res' $ "(assert (= (+ " ++ sum_lasts ++ ") 1))"

  res' ""

  -- определим первую доминошку (!)
  forM_ [0 .. length a - 1] $ \i -> do
    res' $ "(declare-const IsFirst_d" ++ show i ++ " Int)"
    res' $ "(assert (>= IsFirst_d" ++ show i ++ " 0))"

  let sum_firsts = unwords ["IsFirst_d" ++ show i | i <- [0 .. length a - 1]]

  res' $ "(assert (= (+ " ++ sum_firsts ++ ") 1))"

  res' ""

  -- связываем количество доминошек и пар доминошек
  -- последние
  forM_ [0 .. length a - 1] $ \i -> do
    let sum_d = unwords ["Md" ++ show i ++ "d" ++ show j | j <- [0 .. length a - 1]]
    res' $ "(assert(=(+ " ++ sum_d ++ ")(- Md" ++ show i ++ " IsLast_d" ++ show i ++ ")))"

  res' ""

  -- первые (!)

  forM_ [0 .. length a - 1] $ \i -> do
    let sum_d = unwords ["MMd" ++ show i ++ "d" ++ show j | j <- [0 .. length a - 1]]
    res' $ "(assert(=(+ " ++ sum_d ++ ")(- Md" ++ show i ++ " IsFirst_d" ++ show i ++ ")))"

  res' ""

  -- исправление бага о единственном конечном и начальном домино

  -- условие в переменной situation для i-ой доминошки соответсвует что тому что где то выполнилось   -- условие бага, в таком случае мы
  -- зануляем количество доминошек

  -- из за того что такое условие может быть выполнено только для определенной доминошки, в случае    -- его выполения, занулены будут все кроме
  -- самой доминошки

  forM_ [0..length a - 1] $ \i -> do
    let situation = "(or " ++ unwords [ "(and (= (* IsFirst_d" ++ show j ++ " IsLast_d" ++ show j ++ ") 1) (= Md" ++ show j ++ " 1))" | j <- [0..length a - 1], i /= j] ++ ")"
    res' $ "(assert (= Md" ++ show i ++ " (ite " ++ situation ++ " 0 Md" ++ show i ++ ") ))"

  res' ""

  -- ограничение количества само-соседств

  forM_ [0..length a - 1] $ \i -> do
    res' $ "(assert (< Md" ++ show i ++ "d" ++ show i ++ " Md" ++ show i ++ "))"

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
  -- исправлен баг с подсчетом на стыках
  forM_ [0 .. length a - 1] $ \i ->
    forM_ [0 .. length a - 1] $ \j ->
      forM_ alphabet $ \letter1 ->
        forM_ alphabet $ \letter2 -> do
          res' $ "(declare-const Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " Int)"
          res' $ "(assert (= Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " " ++ show (cnt' ((last (a !! i !! 0) : []) ++ (head (a !! j !! 0) : [])) [letter1, letter2]) ++ "))"

          res' $ "(declare-const Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " Int)"
          res' $ "(assert (= Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " " ++ show (cnt' ((last (a !! i !! 1) : []) ++ (head (a !! j !! 1) : [])) [letter1, letter2]) ++ "))"

  res' ""

  -- !!!

  forM_ [0 .. length a - 1] $ \i ->
    forM_ [0 .. length a - 1] $ \j ->
      forM_ alphabet $ \letter1 ->
        forM_ alphabet $ \letter2 -> do
          res' $ "(declare-const PPu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " Int)"
          res' $ "(assert (= PPu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " " ++ show (cnt' ((last (a !! j !! 0) : []) ++ (head (a !! i !! 0) : [])) [letter1, letter2]) ++ "))"

          res' $ "(declare-const PPd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " Int)"
          res' $ "(assert (= PPd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ " " ++ show (cnt' ((last (a !! j !! 1) : []) ++ (head (a !! i !! 1) : [])) [letter1, letter2]) ++ "))"

  res' ""

  -- сравним количество букв
  forM_ alphabet $ \letter -> do
    let sum_u = unwords ["(* Md" ++ show i ++ " Lu_" ++ [letter] ++ "d" ++ show i ++ ")" | i <- [0 .. length a - 1]]
    let sum_d = unwords ["(* Md" ++ show i ++ " Ld_" ++ [letter] ++ "d" ++ show i ++ ")" | i <- [0 .. length a - 1]]
    res' $ "(assert (= (+ " ++ sum_u ++ ") (+ " ++ sum_d ++ ")  ))"

  res' ""

  -- сравним количество пар букв

  forM_ alphabet $ \letter1 ->
    forM_ alphabet $ \letter2 -> do
      let r =
            "(assert (= (+"
              ++ concatMap (\i -> "(* Md" ++ show i ++ " Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ ")") [0 .. length a - 1]
              ++ concatMap (\i -> concatMap (\j -> "(* Md" ++ show i ++ "d" ++ show j ++ " Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ ")") [0 .. length a - 1]) [0 .. length a - 1]
              ++ ") (+ "
              ++ concatMap (\i -> "(* Md" ++ show i ++ " Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ ")") [0 .. length a - 1]
              ++ concatMap (\i -> concatMap (\j -> "(* Md" ++ show i ++ "d" ++ show j ++ " Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ ")") [0 .. length a - 1]) [0 .. length a - 1]
              ++ ")))"
      res' r

  forM_ alphabet $ \letter1 ->
    forM_ alphabet $ \letter2 -> do
      let r =
            "(assert (= (+"
              ++ concatMap (\i -> "(* Md" ++ show i ++ " Pu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ ")") [0 .. length a - 1]
              ++ concatMap (\i -> concatMap (\j -> "(* MMd" ++ show i ++ "d" ++ show j ++ " PPu_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ ")") [0 .. length a - 1]) [0 .. length a - 1]
              ++ ") (+ "
              ++ concatMap (\i -> "(* Md" ++ show i ++ " Pd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ ")") [0 .. length a - 1]
              ++ concatMap (\i -> concatMap (\j -> "(* MMd" ++ show i ++ "d" ++ show j ++ " PPd_" ++ [letter1] ++ [letter2] ++ "d" ++ show i ++ "d" ++ show j ++ ")") [0 .. length a - 1]) [0 .. length a - 1]
              ++ ")))"
      res' r

  res' "(check-sat)"
