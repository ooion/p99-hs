import Data.Array
import Data.List

-- 95
fullWords :: Int -> String
fullWords num =
  let num2word =
        listArray
          (0, 9)
          [ "zero",
            "one",
            "two",
            "three",
            "four",
            "five",
            "six",
            "seven",
            "eight",
            "nine"
          ]
   in intercalate "-" $
        map ((num2word !) . snd) $
          reverse $
            takeWhile ((> 0) . fst) $
              iterate
                ( \(n, _) ->
                    let n' = div n 10 in (n', rem n' 10)
                )
                (num, rem num 10)

-- 96
identifier :: String -> Bool
identifier s = check s True
  where
    check [] b = b
    check ('-':xs) b = b && check xs False
    check (_:xs) _ = check xs True
    