module AssignmentHelp where
 {- Helper functions and data for COM2108 2019, provided by Emma Norling, with credit to Phil Green for several
    functions -}

 import Data.Char -- needed for Char ordering etc
 import Data.List -- needed for delete etc

 ----------------------------------------------------------- 
 -- define a type for Cipher to make functions more clear
 -- (the cipher is assumed to be a substitution cipher for letters)
 type Cipher = String

 ----------------------------------------------------------- 
 -- alphabetic posiiton for an uppercase letter
 -- starting @ 0 for 'A'
 -- ord in Data.Char gives ordering for Chars - 'A' is 65
 
 alphaPos :: Char-> Int
 alphaPos c = (ord c) - ord 'A'
 
 ------------------------------------------------
 
 -- percent x of y, both Ints, rounded to an Int
 
 percent :: Int->Int->Int
 
 percent x y = round (100*(intToFloat x)/(intToFloat y))
 
 intToFloat :: Int -> Float
 intToFloat n = fromInteger (toInteger n)
 
 ------------------------------------------------
 -- Letter Frequencies for English
 
 engFreq :: [(Char,Float)]

 engFreq = [('a',0.08167),('n',0.06749),
            ('b',0.01492),('o',0.07507),
            ('c',0.02782),('p',0.01929),
            ('d',0.04253),('q',0.00095),
            ('e',0.12702),('r',0.05987),
            ('f',0.02228),('s',0.06327),
            ('g',0.02015),('t',0.09056),
            ('h',0.06094),('u',0.02758),
            ('i',0.06966),('v',0.00978),
            ('j',0.00153),('w',0.02360),
            ('k',0.00772),('x',0.00150),
            ('l',0.04025),('y',0.01974),
            ('m',0.02406),('z',0.00074)]
 ---------------------------------------------------------------------------------
 -- the message to be decoded in assignment 1
 
 mystery = "QJAWXARJFBEWXZXADBAJQJDJQFYLQVCWEVEFKQHWHRFDCXKWXNFYMWYFDMCPWAAXMWAJFVNWJAPXZWJCQAFYWXNQJJNWBQJNFYMWEAJFVFZQJACFDNHBWJCWEQMCJAFEJFTAQUWYFSAJFVPXRBWYFJNWJAQYLEWXAWJCWPWAAXMWNWYMJCXBQJPFEWAJFVZWWVJCQAPWAAXMWAWLEWJFEACXEWQTRFDSXYJJCWSCFNWLNXAAJFMWJJCWBFYDAPXEZAAJFV"
 
 ------------------------------------------------
 
 -- Maybe helpers (needed in assignment 3 - will be discussed in lectures before then)

 
 nothingP :: Maybe a -> Bool
 nothingP Nothing = True
 nothingP (Just _) = False
 
 fromMaybe :: Maybe a -> a
 fromMaybe (Just x)=x
 ----------------------------------------------------------------------------------
 
 -- substitution cyphers for the Enigma rotors
 
 rotor1="EKMFLGDQVZNTOWYHXUSPAIBRCJ"
 rotor2="AJDKSIRUXBLHWTMCQGZNPYFVOE"
 rotor3="BDFHJLCPRTXVZNYEIWGAKMUSQO"
 rotor4="ESOVPZJAYQUIRHXLNFTGKDCMWB"
 rotor5="VZBRGITYUPSDNHLXAWMJQOFECK"

 {- the standard Enigma reflector (ReflectorB)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
 -}
 reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]
 
 ----------------------------------------------
 
 

 ------------------------------------------------
 -- Merge and MergeSort
 
 mergesort :: (a->a->Bool) -> [a] -> [a]
 mergesort cmp [] = []
 mergesort cmp [x] = [x]
 mergesort cmp xs = merge cmp (mergesort cmp ys) (mergesort cmp zs)
    where (ys, zs) = halves xs

 halves :: [a] -> ([a],[a])
 halves [] = ([], [])
 halves [x] = ([x], [])
 halves (x:y:zs) = (x:xs, y:ys)
    where (xs, ys) = halves zs

 merge :: (a->a->Bool) -> [a] -> [a] -> [a]
 merge cmp [] ys = ys
 merge cmp xs [] = xs
 merge cmp (x:xs) (y:ys)
    | cmp x y = x : merge cmp xs (y:ys)
    | otherwise = y : merge cmp (x:xs) ys
 
 ------------------------------------------------
 
