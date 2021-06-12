{--
    COM2108 Assessment 2, written by Boxuan Shan (ace18bs).
--}

module Enigma where

    import AssignmentHelp

    -- Rotor is a series of characters corresponding to A-Z in order.
    type Rotor = [Char]
    -- Reflector is a series of character sets. Each group of characters corresponds to each other.
    type Reflector = [(Char, Char)]
    -- Offsets specifies the offset of each rotor as an integer. (LR, MR, RR)
    type Offsets = (Int, Int, Int)
    -- Steckerboard is an uncomplete version of Reflector.
    type SteckerPair = (Char, Char)
    type Steckerboard = [SteckerPair]
    -- An formal struct of a crib item. (Order, (Plain, Cipher))
    type FormatedCribItem = (Int, (Char, Char))
    -- An formal struct of a Crib, which is a list of formal crib item.
    type FormatedCrib = [FormatedCribItem]
    -- A crib is the text that you expect to find in the message and the section of the message that you expect it to align to. (Plain text, Cipher text)
    type Crib = (String, String)
    -- Menu is a list of integer which is an letter order chain.
    type Menu = [Int]

    -- Enigma is a SimpleEnigma or a SteckeredEnigma
    -- SimpleEnigma takes 5 parameters (LR, MR, LR, Reflector, Offsets)
    -- SteckeredEnigma takes 6 parameters (LR, MR, LR, Reflector, Offsets, Steckerboard)
    data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Steckerboard

    -- Pass a signal through a rotor form the right-side to the left-side, with a specific offset.
    -- param 1 : Cipher of the rotor
    -- param 2 : Offset of the rotor
    -- param 3 : Signal position using character representation
    -- return : Encoded signal position using character representation
    r2l :: Cipher -> Int -> Char -> Char
    r2l cipher offset char = ['A'..'Z'] !! (mod (alphaPos (cipher !! (mod ((alphaPos char) + offset) 26)) - offset) 26)
    {--
        No-offset test
        Test 1 : r2l rotor1 0 'A'
        Return 1 : 'E'

        Offset test
        Test 2 : r2l rotor1 1 'Z'
        Return 2 : 'D'
    --}

    -- Pass a signal through a rotor form the left-side to the right-side, with a specific offset.
    -- param 1 : Cipher of the rotor
    -- param 2 : Offset of the rotor
    -- param 3 : Signal position using character representation
    -- return : Decoded signal position using character representation
    l2r :: Cipher -> Int -> Char -> Char
    l2r cipher offset char = ['A'..'Z'] !! (mod ((findPos cipher (['A'..'Z'] !! (mod ((alphaPos char) + offset) 26))) - offset) 26)
        where
            findPos :: Cipher -> Char -> Int
            findPos list char = [index | (index, c) <- zip [0..] list, c == char] !! 0
    {--
        No-offset test
        Test 1 : l2r rotor1 0 'E'
        Return 1 : 'A'

        Offset test
        Test 2 : l2r rotor1 1 'D'
        Return 2 : 'Z'
    --}

    -- Advance the offsets by 1 (26-ary, return to (0,0,0) when (25,25,25))
    advance :: Offsets -> Offsets
    advance (ol, om, or) = (mod (div (index + 1) (26*26)) 26, mod (div (index + 1) 26) 26, mod (index + 1) 26)
        where
            index =  ol*26*26 + om*26 + or
    {--
        Advance test
        Test 1 : advance (0,0,0)
        Return 1 : (0,0,1)

        Carry test
        Test 2 : advance (0,0,25)
        Return 2 : (0,1,0)

        Return test
        Test 3 : advance (25,25,25)
        Return 3 : (0,0,0)
    --}

    -- Reflect a char (Assuming that the reflect complete)
    reflect :: Reflector -> Char -> Char
    reflect [] char = char
    reflect ((a,b):xs) char 
        | a == char = b
        | b == char = a
        | otherwise = reflect xs char
    {--
        Reflect test (a->b)
        Test 1 : reflect reflectorB 'G'
        Return 1 : 'L'

        Reflect test (b->a)
        Test 2 : reflect reflectorB 'L'
        Return 2 : 'G'
    --}

    -- Stecker a char
    stecker :: Steckerboard -> Char -> Char
    stecker board char = reflect board char
    {--
        Stecker test (a->b)
        Test 1 : stecker [('A','Y'),('B','R'),('C','U')] 'A'
        Return 1 : 'Y'

        Stecker test (b->a)
        Test 2 : stecker [('A','Y'),('B','R'),('C','U')] 'Y'
        Return 2 : 'A'

        Stecker test (No retated)
        Test 3 : stecker [('A','Y'),('B','R'),('C','U')] 'D'
        Return 3 : 'D'
    --}

    -- Encode a character with enigma
    enigmaEncode :: Char -> Enigma -> Char
    enigmaEncode char (SimpleEnigma lr mr rr reflector (ol, om, or)) = l2r rr aor (l2r mr aom (l2r lr aol (reflect reflector (r2l lr aol (r2l mr aom (r2l rr aor char))))))
        where
            (aol, aom, aor) = advance (ol, om, or)
    enigmaEncode char (SteckeredEnigma lr mr rr reflector (ol, om, or) steckerboard) = stecker steckerboard (enigmaEncode (stecker steckerboard char) (SimpleEnigma lr mr rr reflector (ol, om, or)))
    {--
        Encode with no offset
        Test 1 : enigmaEncode 'A' (SimpleEnigma rotor3 rotor2 rotor1 reflectorB (25,25,25))
        Return 1 : 'N'

        Encode with no offset (reverse)
        Test 2 : enigmaEncode 'N' (SimpleEnigma rotor3 rotor2 rotor1 reflectorB (25,25,25))
        Return 2 : 'A'

        Encode with offset
        Test 3 : enigmaEncode 'I' (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0))
        Return 3 : 'H'

        Encode with offset (reverse)
        Test 4 : enigmaEncode 'H' (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0))
        Return 4 : 'I'

        Encode with stecker (not applied)
        Test 5 : enigmaEncode 'H' (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0) [('A','Z'),('B','Y')])
        Return 5 : 'I'

        Encode with stecker (applied)
        Test 6 : enigmaEncode 'B' (SteckeredEnigma rotor3 rotor2 rotor1 reflectorB (25,25,25) [('A','B'),('N','O')])
        Return 6 : 'O'
    --}

    -- Encode a string with enigma
    enigmaEncodeMessage :: String -> Enigma -> String
    enigmaEncodeMessage [] _ = []
    enigmaEncodeMessage (x:xs) (SimpleEnigma lr mr rr reflector (ol, om, or)) = enigmaEncode x (SimpleEnigma lr mr rr reflector (ol, om, or)) : enigmaEncodeMessage xs (SimpleEnigma lr mr rr reflector (advance (ol, om, or)))
    enigmaEncodeMessage (x:xs) (SteckeredEnigma lr mr rr reflector (ol, om, or) steckerboard) = enigmaEncode x (SteckeredEnigma lr mr rr reflector (ol, om, or) steckerboard) : enigmaEncodeMessage xs (SteckeredEnigma lr mr rr reflector (advance (ol, om, or)) steckerboard)
    {--
        Encode test
        Test 1 : enigmaEncodeMessage "INXTHEXENIGMAXMACHINEXEACHXROTORXHADXAXNOTCHSTOPXINXTHEXSPECIFICATIONCOMMAXIXHAVEXASSUMEDXTHATXTHATXNOTCHXISXALWAYSXATXPOSITTIONXTWENTYFIVEXZXXWHEREASXINXREALITYXITXWASXATXAXDIFFERENTXPOSITIONXFORXEACHXROTORSTOPXWHENXAXKEYXWASXPRESSEDCOMMAXTHEXVERYXFIRSTXTHINGXTHATXHAPPENEDXWASXTHATXTHEXROTORSXWEREXADVANCEDSTOPXTHEXRIGHTXROTORXISXROTATEDXBYXONESTOPXIFXITXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMESXTHATXISXWASXATXPOSITIONXTWENTYFIVECOMMAXTHEXNOTCHXWOULDXCAUSEXTHEXMIDDLEXROTORXTOXALSOXROTATESTOPXIFXTHATXROTORXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMECOMMAXITXINXTURNXWOULDXCAUSEXTHEXLEFTXROTORXTOXROTATESTOPXINXOTHERXWORDSCOMMAXFORXTHEXMIDDLEXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXKEYXPRESSESSTOPXFORXTHEXLEFTXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSESSTOPXTOXGETXALLXTHEXWAYXBACKXTOXZEROCOMMAZEROCOMMAZEROCOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSEESSTOPTHEXDOWNSIDEXOFXTHEXSIMPLIFICATIONXTHATXIXHAVEXGIVENXISXTHATXTHEXONLINEXSIMULATORSXWILLXNOTXPROGRESSXTHEXSUBSEQUENTXROTORSXATXTHEXSAMEXTIMEXTHATXYOURXSIMULATIONXDOESSTOPXINXACTUALXFACTXROTORXONEXHADXAXNOTCHXATXPOSITIONXQCOMMAXROTORTWOXATXPOSITIONXECOMMAXROTORTHREEXATXPOSITIONXVCOMMAXROTORFOURXATXPOSITIONXJCOMMAXANDXROTORFIVEXATXPOSITIONXZSTOP" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 0))
        Return 1 : "HQRFMNYYVUUNBHACFQDZYSABBUEXJJGJPSFQGNTAJNLNZEIEPUSAXSYEKUBAHXLJZEUCGRFLYHUCDKDMKLZRPCQFMAHTGVYSSEYKUTWRXFHFMZRUWNNKCRTBNHSIOUWODBTAZXPRSJALISVOTAFSFXETWMZRVFRLJNYCWYNMKVBGJTUJKDQBZTNBRSXUGDJRRBUWJBKVCAAWMSSFELVIPOHZTDGOXIZDQGHNLADFAXHVFKGQYASKCZEFAWFABPIITZQPUWXJRHDFLLSMKIMVCIWEJCYSULAAWVQLOVHGJOKYFHIWVFBATADWVYARQBFEAWHLCKGDRXDRMSMNNBSKHFYIRSYHLQGCEQKIDQEXGIMHTUGHISMWQBWERWLGLEATJIJPRWZJISCGDIVXJCRWJTCJNOFDEBXBGSRRICMQXUZHDVYQVFTNXQVCLOBCNZGKSQUFTAOZUHXURSKLKZFHBBYPQTDILBLXCOSAMFHNEGJPXXBCGAXVSRIVSWSRSQOWUAGZSYVOAEMQHUOFJYKOGRFAXUQLYCPGCFMCOPIBIYGJJJZAFSJVSLRBAJZVWITZKJMFWSBGKTLVOCSWHTDSYVWYNHYZMNISJHSPLXTJGIQVNJHGYWLOCTXGCGKHAURIKBNSAMKLPJWQVAVZOHYNUBEPNAXILRQWDIQDYYPZVXBHLTLSSFXBJGJVVNHZHMWKLCWENMLOYDITLQCERPYNODYZLAPLYPLCEWOMJCEKSKRSAQKCLUMNBYWWWJAHHVEOYKXHOYYNUREFGGTVMJYMJLYUNQKMMWYJQMZXDFVFSIEKYVFTMMFAJSLBQBCKWBDUGKCJSJLRYHGADWCWMTSTKRGGPYRBOLPGZUVVKPRKCFAEJWWVVPWAHEGHKDAVPMXVHBLPWIVYILHKDSKWCSDWLVHRLOSUHCSKUDTAVIIFRXUFBWFYZLAQWBQJADGJOFDEFWGVXSKEYQCKCFTZWMBIQNWLRAXJOONXTNJQMZCREOIQZYYPIVIQEXFSHAZIOKYXJHJCHIWGWWZSIAYJPJVBKWDFKZUOUBYIGVMLCIZWIFKDELOULELFBUBUEJMUTMGTQUDIGIKZLZKNDGYQUAHODPSHEBEEOSNHUBTNPNAUQKJIZFYXHDQOQBXSCRWICRMGBETZKZBURJCHITCUBFJJHSXOLXUQRGKWGJBPKNNODIBHFOCKYDEVRVZITAMPVZPZLEKFLZKHBVLYTWBFCCUWMXGLSRQALPJQPTISHPWDBQAMBMKSKIZCQCHLGPDUVRWYWW"
        
        Encode test (reverse)
        Test 1 : enigmaEncodeMessage "HQRFMNYYVUUNBHACFQDZYSABBUEXJJGJPSFQGNTAJNLNZEIEPUSAXSYEKUBAHXLJZEUCGRFLYHUCDKDMKLZRPCQFMAHTGVYSSEYKUTWRXFHFMZRUWNNKCRTBNHSIOUWODBTAZXPRSJALISVOTAFSFXETWMZRVFRLJNYCWYNMKVBGJTUJKDQBZTNBRSXUGDJRRBUWJBKVCAAWMSSFELVIPOHZTDGOXIZDQGHNLADFAXHVFKGQYASKCZEFAWFABPIITZQPUWXJRHDFLLSMKIMVCIWEJCYSULAAWVQLOVHGJOKYFHIWVFBATADWVYARQBFEAWHLCKGDRXDRMSMNNBSKHFYIRSYHLQGCEQKIDQEXGIMHTUGHISMWQBWERWLGLEATJIJPRWZJISCGDIVXJCRWJTCJNOFDEBXBGSRRICMQXUZHDVYQVFTNXQVCLOBCNZGKSQUFTAOZUHXURSKLKZFHBBYPQTDILBLXCOSAMFHNEGJPXXBCGAXVSRIVSWSRSQOWUAGZSYVOAEMQHUOFJYKOGRFAXUQLYCPGCFMCOPIBIYGJJJZAFSJVSLRBAJZVWITZKJMFWSBGKTLVOCSWHTDSYVWYNHYZMNISJHSPLXTJGIQVNJHGYWLOCTXGCGKHAURIKBNSAMKLPJWQVAVZOHYNUBEPNAXILRQWDIQDYYPZVXBHLTLSSFXBJGJVVNHZHMWKLCWENMLOYDITLQCERPYNODYZLAPLYPLCEWOMJCEKSKRSAQKCLUMNBYWWWJAHHVEOYKXHOYYNUREFGGTVMJYMJLYUNQKMMWYJQMZXDFVFSIEKYVFTMMFAJSLBQBCKWBDUGKCJSJLRYHGADWCWMTSTKRGGPYRBOLPGZUVVKPRKCFAEJWWVVPWAHEGHKDAVPMXVHBLPWIVYILHKDSKWCSDWLVHRLOSUHCSKUDTAVIIFRXUFBWFYZLAQWBQJADGJOFDEFWGVXSKEYQCKCFTZWMBIQNWLRAXJOONXTNJQMZCREOIQZYYPIVIQEXFSHAZIOKYXJHJCHIWGWWZSIAYJPJVBKWDFKZUOUBYIGVMLCIZWIFKDELOULELFBUBUEJMUTMGTQUDIGIKZLZKNDGYQUAHODPSHEBEEOSNHUBTNPNAUQKJIZFYXHDQOQBXSCRWICRMGBETZKZBURJCHITCUBFJJHSXOLXUQRGKWGJBPKNNODIBHFOCKYDEVRVZITAMPVZPZLEKFLZKHBVLYTWBFCCUWMXGLSRQALPJQPTISHPWDBQAMBMKSKIZCQCHLGPDUVRWYWW" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 0))
        Return 1 : "INXTHEXENIGMAXMACHINEXEACHXROTORXHADXAXNOTCHSTOPXINXTHEXSPECIFICATIONCOMMAXIXHAVEXASSUMEDXTHATXTHATXNOTCHXISXALWAYSXATXPOSITTIONXTWENTYFIVEXZXXWHEREASXINXREALITYXITXWASXATXAXDIFFERENTXPOSITIONXFORXEACHXROTORSTOPXWHENXAXKEYXWASXPRESSEDCOMMAXTHEXVERYXFIRSTXTHINGXTHATXHAPPENEDXWASXTHATXTHEXROTORSXWEREXADVANCEDSTOPXTHEXRIGHTXROTORXISXROTATEDXBYXONESTOPXIFXITXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMESXTHATXISXWASXATXPOSITIONXTWENTYFIVECOMMAXTHEXNOTCHXWOULDXCAUSEXTHEXMIDDLEXROTORXTOXALSOXROTATESTOPXIFXTHATXROTORXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMECOMMAXITXINXTURNXWOULDXCAUSEXTHEXLEFTXROTORXTOXROTATESTOPXINXOTHERXWORDSCOMMAXFORXTHEXMIDDLEXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXKEYXPRESSESSTOPXFORXTHEXLEFTXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSESSTOPXTOXGETXALLXTHEXWAYXBACKXTOXZEROCOMMAZEROCOMMAZEROCOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSEESSTOPTHEXDOWNSIDEXOFXTHEXSIMPLIFICATIONXTHATXIXHAVEXGIVENXISXTHATXTHEXONLINEXSIMULATORSXWILLXNOTXPROGRESSXTHEXSUBSEQUENTXROTORSXATXTHEXSAMEXTIMEXTHATXYOURXSIMULATIONXDOESSTOPXINXACTUALXFACTXROTORXONEXHADXAXNOTCHXATXPOSITIONXQCOMMAXROTORTWOXATXPOSITIONXECOMMAXROTORTHREEXATXPOSITIONXVCOMMAXROTORFOURXATXPOSITIONXJCOMMAXANDXROTORFIVEXATXPOSITIONXZSTOP"
    --}

    -- A formated crib used for test
    fcrib1 :: [FormatedCribItem]
    fcrib1 = [
        (0, ('W','R')),
        (1, ('E','W')),
        (2, ('T','I')),
        (3, ('T','V')),
        (4, ('E','T')),
        (5, ('R','Y')),
        (6, ('V','R')),
        (7, ('O','E')),
        (8, ('R','S')),
        (9, ('H','X')),
        (10, ('E','B')),
        (11, ('R','F')),
        (12, ('S','O')),
        (13, ('A','G')),
        (14, ('G','K')),
        (15, ('E','U')),
        (16, ('B','H')),
        (17, ('I','Q')),
        (18, ('S','B')),
        (19, ('K','A')),
        (20, ('A','I')),
        (21, ('Y','S')),
        (22, ('A','E'))]
    
    -- A source (unformated) crib used for test
    crib1 :: Crib
    crib1 = ("WETTERVORHERSAGEBISKAYA", "RWIVTYRESXBFOGKUHQBAISE")

    -- Find a series of items containing a specific plain.
    -- param 1 : Source crib
    -- param 2 : The specific plain.
    -- return : A series of items containing the specific plain.
    cribFindPlain :: FormatedCrib -> Char -> [FormatedCribItem]
    cribFindPlain [] char = []
    cribFindPlain ((i,(p,c)):xs) char
        | p == char = (i,(p,c)) : cribFindPlain xs char
        | otherwise = cribFindPlain xs char
    {--
        Find plain test ('T')
        Test 1 : cribFindPlain fcrib1 'T'
        Return 1 : [(2,('T','I')),(3,('T','V'))]

        Find plain test ('A')
        Test 2 : cribFindPlain fcrib1 'A'
        Return 3 : [(13,('A','G')),(20,('A','I')),(22,('A','E'))]
    --}

    -- Find all menus starting with a specific item.
    -- param 1 : Source crib
    -- param 2 : The specific start item.
    -- return : The menu start with the specific item.
    cribItemMenus :: FormatedCrib -> FormatedCribItem -> [Menu]
    cribItemMenus fcrib (i,(p,c))
        | plains == [] = [[i]]
        | otherwise = (map (\m -> i : m) (cribItemsMenus fcrib plains))
        where
            plains = cribFindPlain fcrib c
    {--
        Find Menus for (0, ('W','R'))
        Test 1 : cribItemMenus fcrib1 (0, ('W','R'))
        Return 1 : [[0,5,21,12,7,1,0,8,18,16,9],[0,5,21,12,7,1,0,11],[0,5,21,12,7,4,2,17],[0,5,21,12,7,4,3,6,8,18,16,9],[0,5,21,12,7,4,3,6,11],[0,5,21,12,7,10,16,9],[0,5,21,12,7,15],[0,5,21,18,16,9],[0,8,12,7,1,0,5,21,18,16,9],[0,8,12,7,1,0,11],[0,8,12,7,4,2,17],[0,8,12,7,4,3,6,5,21,18,16,9],[0,8,12,7,4,3,6,11],[0,8,12,7,10,16,9],[0,8,12,7,15],[0,8,18,16,9],[0,11]]

        Find Menus for (1, ('E','W'))
        Test 2 : cribItemMenus fcrib1 (1, ('E','W'))
        Return 2 : [[1,0,5,21,12,7,1],[1,0,5,21,12,7,4,2,17],[1,0,5,21,12,7,4,3,6,8,18,16,9],[1,0,5,21,12,7,4,3,6,11],[1,0,5,21,12,7,10,16,9],[1,0,5,21,12,7,15],[1,0,5,21,18,16,9],[1,0,8,12,7,1],[1,0,8,12,7,4,2,17],[1,0,8,12,7,4,3,6,5,21,18,16,9],[1,0,8,12,7,4,3,6,11],[1,0,8,12,7,10,16,9],[1,0,8,12,7,15],[1,0,8,18,16,9],[1,0,11]]
    --}

    -- Find the menu that starts with each item in the list.
    -- param 1 : Source crib
    -- param 2 : A list of start item.
    -- return : The menu.
    cribItemsMenus :: FormatedCrib -> [FormatedCribItem] -> [Menu]
    cribItemsMenus fcrib [] = []
    cribItemsMenus fcrib ((i,(p,c)):xs) = (cribItemMenus (filter (\(ti,(tp,tc)) -> ti /= i) fcrib) (i,(p,c))) ++ (cribItemsMenus fcrib xs)
    {--
        Find Menus for both (0, ('W','R')) and (1, ('E','W'))
        Test 1 : cribItemsMenus fcrib1 [(0, ('W','R')), (1, ('E','W'))]
        Return 1 : [[0,5,21,12,7,1],[0,5,21,12,7,4,2,17],[0,5,21,12,7,4,3,6,8,18,16,9],[0,5,21,12,7,4,3,6,11],[0,5,21,12,7,10,16,9],[0,5,21,12,7,15],[0,5,21,18,16,9],[0,8,12,7,1],[0,8,12,7,4,2,17],[0,8,12,7,4,3,6,5,21,18,16,9],[0,8,12,7,4,3,6,11],[0,8,12,7,10,16,9],[0,8,12,7,15],[0,8,18,16,9],[0,11],[1,0,5,21,12,7,4,2,17],[1,0,5,21,12,7,4,3,6,8,18,16,9],[1,0,5,21,12,7,4,3,6,11],[1,0,5,21,12,7,10,16,9],[1,0,5,21,12,7,15],[1,0,5,21,18,16,9],[1,0,8,12,7,4,2,17],[1,0,8,12,7,4,3,6,5,21,18,16,9],[1,0,8,12,7,4,3,6,11],[1,0,8,12,7,10,16,9],[1,0,8,12,7,15],[1,0,8,18,16,9],[1,0,11]]
    
        Find Menus for all
        Test 2 : cribItemsMenus fcrib1 fcrib1
        Return 2 : [[0,5,21,12,7,1],[0,5,21,12,7,4,2,17],[0,5,21,12,7,4,3,6,8,18,16,9],[0,5,21,12,7,4,3,6,11],[0,5,21,12,7,10,16,9],[0,5,21,12,7,15],[0,5,21,18,16,9],[0,8,12,7,1],[0,8,12,7,4,2,17],[0,8,12,7,4,3,6,5,21,18,16,9],[0,8,12,7,4,3,6,11],[0,8,12,7,10,16,9],[0,8,12,7,15],[0,8,18,16,9],[0,11],[1,0,5,21,12,7,4,2,17],[1,0,5,21,12,7,4,3,6,8,18,16,9],[1,0,5,21,12,7,4,3,6,11],[1,0,5,21,12,7,10,16,9],[1,0,5,21,12,7,15],[1,0,5,21,18,16,9],[1,0,8,12,7,4,2,17],[1,0,8,12,7,4,3,6,5,21,18,16,9],[1,0,8,12,7,4,3,6,11],[1,0,8,12,7,10,16,9],[1,0,8,12,7,15],[1,0,8,18,16,9],[1,0,11],[2,17],[3,6,5,21,12,7,1,0,8,18,16,9],[3,6,5,21,12,7,1,0,11],[3,6,5,21,12,7,4,2,17],[3,6,5,21,12,7,10,16,9],[3,6,5,21,12,7,15],[3,6,5,21,18,16,9],[3,6,8,12,7,1,0,5,21,18,16,9],[3,6,8,12,7,1,0,11],[3,6,8,12,7,4,2,17],[3,6,8,12,7,10,16,9],[3,6,8,12,7,15],[3,6,8,18,16,9],[3,6,11],[4,2,17],[4,3,6,5,21,12,7,1,0,8,18,16,9],[4,3,6,5,21,12,7,1,0,11],[4,3,6,5,21,12,7,10,16,9],[4,3,6,5,21,12,7,15],[4,3,6,5,21,18,16,9],[4,3,6,8,12,7,1,0,5,21,18,16,9],[4,3,6,8,12,7,1,0,11],[4,3,6,8,12,7,10,16,9],[4,3,6,8,12,7,15],[4,3,6,8,18,16,9],[4,3,6,11],[5,21,12,7,1,0,8,18,16,9],[5,21,12,7,1,0,11],[5,21,12,7,4,2,17],[5,21,12,7,4,3,6,8,18,16,9],[5,21,12,7,4,3,6,11],[5,21,12,7,10,16,9],[5,21,12,7,15],[5,21,18,16,9],[6,5,21,12,7,1,0,8,18,16,9],[6,5,21,12,7,1,0,11],[6,5,21,12,7,4,2,17],[6,5,21,12,7,4,3],[6,5,21,12,7,10,16,9],[6,5,21,12,7,15],[6,5,21,18,16,9],[6,8,12,7,1,0,5,21,18,16,9],[6,8,12,7,1,0,11],[6,8,12,7,4,2,17],[6,8,12,7,4,3],[6,8,12,7,10,16,9],[6,8,12,7,15],[6,8,18,16,9],[6,11],[7,1,0,5,21,12],[7,1,0,5,21,18,16,9],[7,1,0,8,12],[7,1,0,8,18,16,9],[7,1,0,11],[7,4,2,17],[7,4,3,6,5,21,12],[7,4,3,6,5,21,18,16,9],[7,4,3,6,8,12],[7,4,3,6,8,18,16,9],[7,4,3,6,11],[7,10,16,9],[7,15],[8,12,7,1,0,5,21,18,16,9],[8,12,7,1,0,11],[8,12,7,4,2,17],[8,12,7,4,3,6,5,21,18,16,9],[8,12,7,4,3,6,11],[8,12,7,10,16,9],[8,12,7,15],[8,18,16,9],[9],[10,16,9],[11],[12,7,1,0,5,21,18,16,9],[12,7,1,0,8,18,16,9],[12,7,1,0,11],[12,7,4,2,17],[12,7,4,3,6,5,21,18,16,9],[12,7,4,3,6,8,18,16,9],[12,7,4,3,6,11],[12,7,10,16,9],[12,7,15],[13,14,19,20,17],[13,14,19,22,1,0,5,21,12,7,4,2,17],[13,14,19,22,1,0,5,21,12,7,4,3,6,8,18,16,9],[13,14,19,22,1,0,5,21,12,7,4,3,6,11],[13,14,19,22,1,0,5,21,12,7,10,16,9],[13,14,19,22,1,0,5,21,12,7,15],[13,14,19,22,1,0,5,21,18,16,9],[13,14,19,22,1,0,8,12,7,4,2,17],[13,14,19,22,1,0,8,12,7,4,3,6,5,21,18,16,9],[13,14,19,22,1,0,8,12,7,4,3,6,11],[13,14,19,22,1,0,8,12,7,10,16,9],[13,14,19,22,1,0,8,12,7,15],[13,14,19,22,1,0,8,18,16,9],[13,14,19,22,1,0,11],[13,14,19,22,4,2,17],[13,14,19,22,4,3,6,5,21,12,7,1,0,8,18,16,9],[13,14,19,22,4,3,6,5,21,12,7,1,0,11],[13,14,19,22,4,3,6,5,21,12,7,10,16,9],[13,14,19,22,4,3,6,5,21,12,7,15],[13,14,19,22,4,3,6,5,21,18,16,9],[13,14,19,22,4,3,6,8,12,7,1,0,5,21,18,16,9],[13,14,19,22,4,3,6,8,12,7,1,0,11],[13,14,19,22,4,3,6,8,12,7,10,16,9],[13,14,19,22,4,3,6,8,12,7,15],[13,14,19,22,4,3,6,8,18,16,9],[13,14,19,22,4,3,6,11],[13,14,19,22,10,16,9],[13,14,19,22,15],[14,19,13],[14,19,20,17],[14,19,22,1,0,5,21,12,7,4,2,17],[14,19,22,1,0,5,21,12,7,4,3,6,8,18,16,9],[14,19,22,1,0,5,21,12,7,4,3,6,11],[14,19,22,1,0,5,21,12,7,10,16,9],[14,19,22,1,0,5,21,12,7,15],[14,19,22,1,0,5,21,18,16,9],[14,19,22,1,0,8,12,7,4,2,17],[14,19,22,1,0,8,12,7,4,3,6,5,21,18,16,9],[14,19,22,1,0,8,12,7,4,3,6,11],[14,19,22,1,0,8,12,7,10,16,9],[14,19,22,1,0,8,12,7,15],[14,19,22,1,0,8,18,16,9],[14,19,22,1,0,11],[14,19,22,4,2,17],[14,19,22,4,3,6,5,21,12,7,1,0,8,18,16,9],[14,19,22,4,3,6,5,21,12,7,1,0,11],[14,19,22,4,3,6,5,21,12,7,10,16,9],[14,19,22,4,3,6,5,21,12,7,15],[14,19,22,4,3,6,5,21,18,16,9],[14,19,22,4,3,6,8,12,7,1,0,5,21,18,16,9],[14,19,22,4,3,6,8,12,7,1,0,11],[14,19,22,4,3,6,8,12,7,10,16,9],[14,19,22,4,3,6,8,12,7,15],[14,19,22,4,3,6,8,18,16,9],[14,19,22,4,3,6,11],[14,19,22,10,16,9],[14,19,22,15],[15],[16,9],[17],[18,16,9],[19,13,14],[19,20,17],[19,22,1,0,5,21,12,7,4,2,17],[19,22,1,0,5,21,12,7,4,3,6,8,18,16,9],[19,22,1,0,5,21,12,7,4,3,6,11],[19,22,1,0,5,21,12,7,10,16,9],[19,22,1,0,5,21,12,7,15],[19,22,1,0,5,21,18,16,9],[19,22,1,0,8,12,7,4,2,17],[19,22,1,0,8,12,7,4,3,6,5,21,18,16,9],[19,22,1,0,8,12,7,4,3,6,11],[19,22,1,0,8,12,7,10,16,9],[19,22,1,0,8,12,7,15],[19,22,1,0,8,18,16,9],[19,22,1,0,11],[19,22,4,2,17],[19,22,4,3,6,5,21,12,7,1,0,8,18,16,9],[19,22,4,3,6,5,21,12,7,1,0,11],[19,22,4,3,6,5,21,12,7,10,16,9],[19,22,4,3,6,5,21,12,7,15],[19,22,4,3,6,5,21,18,16,9],[19,22,4,3,6,8,12,7,1,0,5,21,18,16,9],[19,22,4,3,6,8,12,7,1,0,11],[19,22,4,3,6,8,12,7,10,16,9],[19,22,4,3,6,8,12,7,15],[19,22,4,3,6,8,18,16,9],[19,22,4,3,6,11],[19,22,10,16,9],[19,22,15],[20,17],[21,12,7,1,0,5],[21,12,7,1,0,8,18,16,9],[21,12,7,1,0,11],[21,12,7,4,2,17],[21,12,7,4,3,6,5],[21,12,7,4,3,6,8,18,16,9],[21,12,7,4,3,6,11],[21,12,7,10,16,9],[21,12,7,15],[21,18,16,9],[22,1,0,5,21,12,7,4,2,17],[22,1,0,5,21,12,7,4,3,6,8,18,16,9],[22,1,0,5,21,12,7,4,3,6,11],[22,1,0,5,21,12,7,10,16,9],[22,1,0,5,21,12,7,15],[22,1,0,5,21,18,16,9],[22,1,0,8,12,7,4,2,17],[22,1,0,8,12,7,4,3,6,5,21,18,16,9],[22,1,0,8,12,7,4,3,6,11],[22,1,0,8,12,7,10,16,9],[22,1,0,8,12,7,15],[22,1,0,8,18,16,9],[22,1,0,11],[22,4,2,17],[22,4,3,6,5,21,12,7,1,0,8,18,16,9],[22,4,3,6,5,21,12,7,1,0,11],[22,4,3,6,5,21,12,7,10,16,9],[22,4,3,6,5,21,12,7,15],[22,4,3,6,5,21,18,16,9],[22,4,3,6,8,12,7,1,0,5,21,18,16,9],[22,4,3,6,8,12,7,1,0,11],[22,4,3,6,8,12,7,10,16,9],[22,4,3,6,8,12,7,15],[22,4,3,6,8,18,16,9],[22,4,3,6,11],[22,10,16,9],[22,15]]
        --}

    -- Input a crib. Find a list of longest menus of a crib.
    longestMenu :: Crib -> [Menu]
    longestMenu (p,c) = filter (\i -> (length i) == maxlength) all
        where 
            fcrib = zip [0..] (zip p c)
            all = cribItemsMenus fcrib fcrib
            maxlength = length(mergesort (\a b -> length a > length b) all !! 0)
    {--
        Find longest Menus
        Test 1 : longestMenu crib1
        Return 1 : [[13,14,19,22,1,0,5,21,12,7,4,3,6,8,18,16,9],[13,14,19,22,1,0,8,12,7,4,3,6,5,21,18,16,9],[13,14,19,22,4,3,6,5,21,12,7,1,0,8,18,16,9],[13,14,19,22,4,3,6,8,12,7,1,0,5,21,18,16,9]]
    --}
