-- 1
myLast :: [a] -> a
myLast [] = error "empty"
myLast [x] = x
myLast (x:xs) = myLast xs