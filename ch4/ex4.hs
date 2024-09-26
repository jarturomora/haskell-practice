-- conditional
safetailif :: [a] -> [a]
safetailif xs = if null xs then [] else tail xs

-- guarded equations
safetailge :: [a] -> [a]
safetailge xs  | null xs   = []
               | otherwise = tail xs

-- pattern matching
safetailpm :: [a] -> [a]
safetailpm [] = []
safetailpm (_:xs) = xs

-- or operator patterns matching
(||>) :: Bool -> Bool -> Bool
False ||> False = False
_     ||> _     = True

(||<) :: Bool -> Bool -> Bool
True  ||< _ = True
False ||< b = b

{-- Redefine this with conditionals
True && True = True
_ && _ = False
--}
(&&>) :: Bool -> Bool -> Bool
(&&>) x y = if x == False then False else
            if y == False then False else True

{-- Redefine this with conditionals
Do the (4) same for the following version:
True && b = b
False && _ = False
--}
(&&<) :: Bool -> Bool -> Bool
(&&<) x b = if x == True then b else False