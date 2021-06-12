{--
    Assessment 1, written by Boxuan Shan.
--}

module Ciphers where

    import AssignmentHelp
    import Data.Char

    -- Validate a cipher which must contain each letter once and once only.
    validateCipher :: Cipher -> Bool
    validateCipher cipher = mergesort (\a b -> a < b) cipher == "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    {--
        Test : Correct cipher
        Input : validateCipher "CBADEFGHIJKLMNOPQRSTUVWXYZ"
        Output : True

        Test : Incorrect cipher (missing character)
        Input : validateCipher "CBADEFGHIJKLMNOPQRSTUVWXY"
        Output : False

        Test : Incorrect cipher (duplicate character)
        Input : validateCipher "CBADEFGHIJKLMNOPQRSTUVWXYZZ"
        Output : False
    --}

    -- Convert alphabetic posiiton to an uppercase letter. (Start with 0)
    alphaPosToChar :: Int -> Char
    alphaPosToChar alphaOrd = chr (ord 'A' + alphaOrd)
    {--
        Input : alphaPosToChar 0
        Output : 'A'

        Input : alphaPosToChar 1
        Output : 'B'

        Input : alphaPosToChar 25
        Output : 'Z'
    --}

    -- Add an offset to the alphabetic posiiton of a character, warp at Z.
    charShift :: Int -> Char -> Char
    charShift offset char = alphaPosToChar (mod (alphaPos char + offset) 26)
    {--
        Input : charShift 1 'A'
        Output : 'B'

        Input : charShift 2 'A'
        Output : 'C'

        Input : charShift 1 'Z'
        Output : 'A'
    --}

    -- Encode a character.
    encode :: Cipher -> Int -> Char -> Char
    encode cipher offset char = cipher !! (alphaPos (charShift offset char))
    {--
        Input : encode rotor1 1 'A'
        Output : 'F'

        Input : encode rotor1 2 'B'
        Output : 'M'

        Input : encode rotor2 1 'S'
        Output : 'A'
    --}

    -- Encode a message.
    encodeMessage :: Cipher -> Int -> String -> String
    encodeMessage cipher offset string = map (\c -> encode cipher offset c) string
    {--
        Input : encodeMessage rotor1 1 "ABC"
        Output : "FLN"

        Input : encodeMessage rotor1 2 "DEF"
        Output : "HNI"

        Input : encodeMessage rotor2 1 "RST"
        Output : "HAO"
    --}

    -- Reverse encoding a character.
    reverseEncode :: Cipher -> Int -> Char -> Char
    reverseEncode cipher offset char = charShift (-offset) (alphaPosToChar(findPos cipher char))
        where
            findPos :: Cipher -> Char -> Int
            findPos list char = [index | (index, c) <- zip [0..] list, c == char] !! 0
    {--
        Input : reverseEncode rotor1 1 'F'
        Output : 'A'

        Input : reverseEncode rotor1 2 'M'
        Output : 'B'

        Input : reverseEncode rotor2 1 'A'
        Output : 'S'
    --}

    -- Reverse encoding a message.
    reverseEncodeMessage :: Cipher -> Int -> String -> String
    reverseEncodeMessage cipher offset string = map (\c -> reverseEncode cipher offset c) string
    {--
        Input : reverseEncodeMessage rotor1 1 "FLN"
        Output : "ABC"

        Input : reverseEncodeMessage rotor1 2 "HNI"
        Output : "DEF"

        Input : reverseEncodeMessage rotor2 1 "HAO"
        Output : "RST"
    --}

    -- Count the number of times a character appears in a string.
    countLetter :: Char -> String -> Float
    countLetter char [] = 0
    countLetter char (x:xs)
        | x == char = 1 + countLetter char xs
        | otherwise = countLetter char xs
    {--
        Input : countLetter 'A' "ABBCCC"
        Output : 1.0

        Input : countLetter 'B' "ABBCCC"
        Output : 2.0

        Input : countLetter 'D' "ABBCCC"
        Output : 0.0
    --}

    -- Calculate the percentage of each letter occurring in this message as a list of (Char, Int) tuples.
    letterStats :: String -> [(Char, Int)]
    letterStats message = removeZero (sortAll (zipAll (getPercs (getFreqs message))))
        where
            getFreqs message = (map (\c -> countLetter c message) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            getPercs freqs = (map (\f -> round(f * 100 / sum(freqs))) freqs)
            zipAll percs = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" percs
            sortAll zips = mergesort (\(a, b) (c, d) -> b > d) zips
            removeZero sortedZips = [(a, b) | (a, b) <- sortedZips, not (b == 0)]
    {--
        Input : letterStats "ABBCCC"
        Output : [('C',50),('B',33),('A',17)]

        Input : letterStats "AAABBCCC"
        Output : [('C',38),('A',38),('B',25)]

        Input : letterStats "STDDWSD" 
        Output : [('D',43),('S',29),('T',14),('W',14)]
    --}

    -- Decode a character via a list of guesses for letters and return the alphabetic posiiton of the decoded character (return -1 if cannot decoding).
    decodeFromGuesses :: [(Char, Char)] -> Char -> Int
    decodeFromGuesses [] char = -1
    decodeFromGuesses (x:xs) char
        | (snd x) == char = alphaPos(fst x)
        | otherwise = decodeFromGuesses xs char
    {--
        Input : decodeFromGuesses [('E','X'),('S','W')] 'X'
        Output : 4

        Input : decodeFromGuesses [('E','X'),('S','W')] 'W'
        Output : 18

        Input : decodeFromGuesses [('E','X'),('S','W')] 'A'
        Output : -1
    --}

    -- Decode a message via a list of guesses for letters in the message.
    partialDecode :: [(Char, Char)] -> String -> String
    partialDecode guesses message = map (\c -> 
            if decodeFromGuesses guesses c == -1 then
                c
            else
                toLower (alphaPosToChar (decodeFromGuesses guesses c))
        ) message
    {--
        Input : partialDecode [('E','X'),('S','W')] "DXPWXW"
        Output : "DePses"
    --}

{--
    Guesses for mystery : [('S','A'),('T','J'),('O','F'),('P','V'),('I','Q'),('E','W'),('A','X'),('Y','R'),('U','D'),('M','P'),('G','M'),('K','Z'),('H','C'),('R','E'),('B','B'),('N','Y'),('C','L'),('V','K'),('D','H'),('L','N'),('F','T'),('W','S'),('Z','U')]
    Decoded mystery : "itseasytobreakasubstitutioncipherprovidedyouhavealongenoughmessagestopletsmakethisonealittlebitlongerstopokitshouldbetherightsortofsizenowstopmaybenotletsincreasethemessagelengthabitmorestopkeepthismessagesecretorshareifyouwantthewholeclasstogetthebonusmarksstop"
    Formated answer : "It's easy to break a substitution cipher provided you have a long enough message. Let's make this one a little bit longer. Ok, it should be the right sort of size now. Maybe not lets increase the message length a bit more. Keep this message secret or share if you want the whole class to get the bonus marks."
--}