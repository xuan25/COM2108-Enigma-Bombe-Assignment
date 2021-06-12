{--
    COM2108 Assessment 3, written by Boxuan Shan (ace18bs).

    Notes on extensions: 

    In this Assignment, I removed the simplifications about rotors and the reflector. Thus, here I assume that rotors and the reflector can be any combination.
    Because I made extensions, the parameters of the method described in the document have changed, but the corresponding functions are basically the same.

    About using the extended version :
    If you want to break a Enigma without the specific of rotors and the reflector, please use <breakUncertainEnigma>.
    If you want to test a specific combination, please use <breakEnigma> where you need provides rotors and the reflector.
    If you only want to use the longest menu to break the Enigma which is same with the simplifications in the document, please use <breakEA Crib (longestMenu Crib) Rotor Rotor Rotor Reflector>.
    
    About the algorithm and the principle :
    In the extended algorithm (breakUncertainEnigma), it will generate a suitable menu set which contains a serise of menus (filtered from all menus) not only the longest one, and test all the combinations with rotors and reflectors until find a perfect solution.
    The algorithm will not just test the longest menu. Therefore, it will find a perfect solution which suitable for each position in the crib, otherwise it will return "Nothing". If there is a solution just suitable for a part of the crib, it will not return this solution.
    The <breakUncertainEnigma> will test combinations from (rotor1 rotor2 rotor3 reflectorB) to (rotor5 rotor4 rotor3 reflectorB). Therefore, if there is a solution with combination (rotor5 rotor4 rotor3 reflectorB), it will take a very long time to break it.

    About the length of the menu :
    If only using the longest menu to break the Enigma. The plains and ciphers associated to the menu need to cover all 26 english letters (At least 25 for menu length) to get the perfect solution, otherwise the steckerboard its found may missing some pairs or missing some conflict check under current combination and lead to incorrect rotors and reflector as well.
    If the longest menu cannot cover all 26 english letters, it may got a correct answer if it covered all the stecker pairs (At least n-1 for menu length, where n is the number of stecker pairs). However, it cannot guarantee to find a correct one, because we do not know how many stecker pairs the enigma has before we know the perfect solution.

    Furthermore :
    You can change the rotors and reflectors the Boome system considered by change the rotors list and reflectors list below (line 37 & line 41).
--}

module Bombe where

    -- import Debug.Trace
    -- import Bombe_Testing
    import Enigma
    import AssignmentHelp
    import Data.List

    -- The List of reflectors ready to test. The format of each item is (displayName, reflector).
    reflectors :: [(String, Reflector)]
    reflectors = [("reflectorB", reflectorB)]

    -- The List of rotors ready to test. The format of each item is (displayName, rotor).
    rotors :: [(String, Rotor)]
    rotors = [("rotor1", rotor1), ("rotor2", rotor2), ("rotor3", rotor3), ("rotor4", rotor4), ("rotor5", rotor5)]

    

    -- Break an Enigma without the knowledge of both rotors and the reflector.
    -- crib     : The crib which need to be cracked.
    -- Return   : Maybe (Left rotor name, Middle rotor name, Right rotor name, Reflector name, Offsets, Steckerboard)
    breakUncertainEnigma :: Crib -> Maybe (String, String, String, String, Offsets, Steckerboard)
    breakUncertainEnigma crib = breakUncertainReflactorRecurly crib 0
    {--
        Tests for breakUncertainEnigma

        1.
        *Bombe> breakUncertainEnigma ("TURINGBOMBEHASKELLSIMULATIONSTOP", "FDLQIYHKFXSYEEXAYTWJBNNMFCHUACVMERSLXIXVWCCOBSVUESKCQGKSCXSQUMCWLWXCWNDEKHCGRKAUWLSCNUUROQVOTZCWUICNEXDCQPKQKHSTFTCQJXEFKDLKOTH")
        Just ("rotor1","rotor2","rotor3","reflectorB",(1,2,14),[('V','Y'),('H','C'),('A','N'),('M','J'),('S','X'),('E','B')])
        *Bombe> enigmaEncodeMessage "FDLQIYHKFXSYEEXAYTWJBNNMFCHUACVMERSLXIXVWCCOBSVUESKCQGKSCXSQUMCWLWXCWNDEKHCGRKAUWLSCNUUROQVOTZCWUICNEXDCQPKQKHSTFTCQJXEFKDLKOTH" (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (1,2,14) [('V','Y'),('H','C'),('A','N'),('M','J'),('S','X'),('E','B')])
        "TURINGBOMBEHASKELLSIMULATIONSTOPYOUSHOULDHAVEAMENUFORTHISTESTTHATISSUFFICIENTTODECODETHISMESSAGESTOPIFNOTITISALLEMMASFAULTAGAIN"
        Formated:
        TURING BOMBE HASKELL SIMULATION STOP YOU SHOULD HAVE A MENU FOR THIS TEST THAT IS SUFFICIENT TO DECODE THIS MESSAGE STOP IF NOT IT IS ALL EMMAS FAULT AGAIN
        Turing bombe haskell simulation. You Should have a menu for this test that is sufficient to decode this message. If not, it is all Emma's fault again.

        2.
        *Bombe> breakUncertainEnigma ("COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP", "YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUSSXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOULMTQNUFBVHFUSXYCYPWFKBYW")
        Just ("rotor1","rotor2","rotor3","reflectorB",(4,3,7),[('O','M'),('B','Z'),('K','C'),('N','E'),('J','U'),('L','D'),('X','F'),('G','R')])
        *Bombe> enigmaEncodeMessage "YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUSSXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOULMTQNUFBVHFUSXYCYPWFKBYW" (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (4,3,7) [('O','M'),('B','Z'),('K','C'),('N','E'),('J','U'),('L','D'),('X','F'),('G','R')])
        "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPENIGMAMACHINESAREINTERESTINGBUTTHOSECODEBREAKERSWHOCRACKEDTHECODEMUSTHAVEBEENGENIUSESTODOSOWITHOUTCOMPUTERSSTOP"
        Formated:
        COMPUTER SCIENCE SHEFFIELD UNIVERSITY STOP ENIGMA MACHINES ARE INTERESTING BUT THOSE CODE BREAKERS WHO CRACKED THE CODE MUST HAVE BEEN GENIUSES TO DO SO WITHOUT COMPUTERS STOP
        Computer science sheffield university. Enigma machines are interesting, but those code breakers who cracked the code must have been geniuses to do so without computers.

        3. An example with no perfect solution
        *Bombe> breakUncertainEnigma ("COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP", "NAWLGAFTKDBKDLIKEOSZVAOKXKXFMQQUXBJDRCXRZFDKHLWCNBDJSMJBMJQCBBGJV")
        Nothing
    --}

    -- Break an Enigma using a specific reflector without the knowledge of rotors.
    -- crib         : The crib which need to be cracked.
    -- reflIndex    : The index of the reflector.
    -- Return       : Maybe (Left rotor name, Middle rotor name, Right rotor name, Reflector name, Offsets, Steckerboard)
    breakUncertainReflactorRecurly :: Crib -> Int -> Maybe (String, String, String, String, Offsets, Steckerboard)
    breakUncertainReflactorRecurly crib reflIndex
        | reflIndex == (length reflectors) = Nothing                                            -- All reflectors are tested but no solution, return Nothing.
        | result == Nothing = breakUncertainReflactorRecurly crib (reflIndex + 1)               -- Current reflector has no solution, try next.
        | otherwise = Just (rl, rm ,rr, (fst (reflectors !! reflIndex)), offsets, stecker)      -- Solution found.
        where
            result = breakUncertainRotorRecurly crib reflIndex 0
            Just (rl, rm ,rr, offsets, stecker) = result

    -- Break an Enigma using a specific reflector without the knowledge of rotors.
    -- crib         : The crib which need to be cracked.
    -- reflIndex    : The index of the reflector.
    -- rotorIndex   : The index of the rotor set. rotorIndex is a 3 digits n-ary number (using 10-ary to express in haskell), where n is the number of total rotors. Each digit represent a index of rotor on the machine (lr, mr, rr).
    -- Return       : Maybe (Left rotor name, Middle rotor name, Right rotor name, Offsets, Steckerboard)
    breakUncertainRotorRecurly :: Crib -> Int -> Int -> Maybe (String, String, String, Offsets, Steckerboard)
    breakUncertainRotorRecurly crib reflIndex rotorIndex
        | rotorIndex == rotorCount*rotorCount*rotorCount = Nothing                                                  -- All rotor sets are tested but no solution, return Nothing.
        | rl == rm || rl == rr || rm == rr = breakUncertainRotorRecurly crib reflIndex (rotorIndex + 1)             -- Current rotor set is not valid (duplicate), skip.
        | result == Nothing = breakUncertainRotorRecurly crib reflIndex (rotorIndex + 1)                            -- Current rotot set has no solution, try next.
        | otherwise = Just ((fst (rotors !! rl)), (fst (rotors !! rm)), (fst (rotors !! rr)), offsets, stecker)     -- Solution found.
        where
            rotorCount = length rotors
            (rl, rm ,rr) = (mod (div rotorIndex (rotorCount*rotorCount)) rotorCount, mod (div rotorIndex rotorCount) rotorCount, mod rotorIndex rotorCount)
            -- result = trace (" refl=" ++ show reflIndex ++ " rl=" ++ show rl ++ " rm="  ++ show rm ++ " rl="  ++ show rr) breakEnigma crib (snd (rotors !! rl)) (snd (rotors !! rm)) (snd (rotors !! rr)) (snd(reflectors !! reflIndex))      -- Used to trace the progress
            result = breakEnigma crib (snd (rotors !! rl)) (snd (rotors !! rm)) (snd (rotors !! rr)) (snd(reflectors !! reflIndex))
            Just (offsets, stecker) = result



    -- Get a series of suitable menus for breaking the Enigma. (remove infix menus form all menus)
    -- crib     : The crib which need to be generated menu.
    -- Return   : A series of suitable menus.
    getMenu :: Crib -> [Menu]
    getMenu crib = removeInfixMenu (allMenu crib)
    {--
        Tests for getMenu
        1. 
        getMenu ("TURINGBOMBEHASKELLSIMULATIONSTOP", "FDLQIYHKFXSYEEXAYTWJBNNMFCHUACVMERSLXIXVWCCOBSVUESKCQGKSCXSQUMCWLWXCWNDEKHCGRKAUWLSCNUUROQVOTZCWUICNEXDCQPKQKHSTFTCQJXEFKDLKOTH")
        [[2,16],[2,17,0],[2,17,24],[2,17,29],[2,22,4,3],[2,22,4,19],[2,22,4,25],[2,22,27,1],[2,22,27,21,4,3],[2,22,27,21,4,19],[2,22,27,21,4,25],[5],[7,14],[10,13,15,12],[13,10,18],[13,10,28,12,15,23,8],[13,10,28,12,15,23,20,6,11],[13,10,28,12,15,23,20,9],[13,10,28,23,8],[13,10,28,23,20,6,11],[13,10,28,23,20,9],[13,15,12,10,18],[13,15,12,10,28,23,8],[13,15,12,10,28,23,20,6,11],[13,15,12,10,28,23,20,9],[15,12,10,13],[21,27,1],[26,11],[28,12,10,13,15,23,8],[28,12,10,13,15,23,20,6,11],[28,12,10,13,15,23,20,9],[28,12,10,18],[30],[31,8],[31,20,6,11],[31,20,9]]
    --}


    -- Get all the menus of a crib
    -- crib     : The crib which need to be generated menu.
    -- Return   : A series of menus.
    allMenu :: Crib -> [Menu]
    allMenu crib = cribItemsMenus fcrib fcrib
        where 
            (p, c) = crib
            fcrib = zip [0..] (zip p (take (length p) c))
    {--
        Tests for allMenu
        1. 
        *Bombe> allMenu ("TURINGBOMBEHASKELLSIMULATIONSTOP", "FDLQIYHKFXSYEEXAYTWJBNNMFCHUACVMERSLXIXVWCCOBSVUESKCQGKSCXSQUMCWLWXCWNDEKHCGRKAUWLSCNUUROQVOTZCWUICNEXDCQPKQKHSTFTCQJXEFKDLKOTH")
        [[0],[1],[2,16],[2,17,0],[2,17,24],[2,17,29],[2,22,4,3],[2,22,4,19],[2,22,4,25],[2,22,27,1],[2,22,27,21,4,3],[2,22,27,21,4,19],[2,22,27,21,4,25],[3],[4,3],[4,19],[4,25],[5],[6,11],[7,14],[8],[9],[10,13,15,12],[10,13,15,23,8],[10,13,15,23,20,6,11],[10,13,15,23,20,9],[10,18],[10,28,12,15,23,8],[10,28,12,15,23,20,6,11],[10,28,12,15,23,20,9],[10,28,23,8],[10,28,23,20,6,11],[10,28,23,20,9],[11],[12,10,13,15,23,8],[12,10,13,15,23,20,6,11],[12,10,13,15,23,20,9],[12,10,18],[12,10,28,23,8],[12,10,28,23,20,6,11],[12,10,28,23,20,9],[12,15,23,8],[12,15,23,20,6,11],[12,15,23,20,9],[13,10,18],[13,10,28,12,15,23,8],[13,10,28,12,15,23,20,6,11],[13,10,28,12,15,23,20,9],[13,10,28,23,8],[13,10,28,23,20,6,11],[13,10,28,23,20,9],[13,15,12,10,18],[13,15,12,10,28,23,8],[13,15,12,10,28,23,20,6,11],[13,15,12,10,28,23,20,9],[13,15,23,8],[13,15,23,20,6,11],[13,15,23,20,9],[14],[15,12,10,13],[15,12,10,18],[15,12,10,28,23,8],[15,12,10,28,23,20,6,11],[15,12,10,28,23,20,9],[15,23,8],[15,23,20,6,11],[15,23,20,9],[16],[17,0],[17,24],[17,29],[18],[19],[20,6,11],[20,9],[21,4,3],[21,4,19],[21,4,25],[21,27,1],[22,4,3],[22,4,19],[22,4,25],[22,27,1],[22,27,21,4,3],[22,27,21,4,19],[22,27,21,4,25],[23,8],[23,20,6,11],[23,20,9],[24],[25],[26,11],[27,1],[27,21,4,3],[27,21,4,19],[27,21,4,25],[28,12,10,13,15,23,8],[28,12,10,13,15,23,20,6,11],[28,12,10,13,15,23,20,9],[28,12,10,18],[28,12,15,23,8],[28,12,15,23,20,6,11],[28,12,15,23,20,9],[28,23,8],[28,23,20,6,11],[28,23,20,9],[29],[30],[31,8],[31,20,6,11],[31,20,9]]
    --}

    -- Remove infix menus form a list of menu
    -- menus    : The menu list which need to be processed.
    -- Return   : A menu list without infix menus.
    removeInfixMenu :: [Menu] -> [Menu]
    removeInfixMenu menus = removeInfixMenuRecurly menus menus []
    {--
        Tests for isInfixOfMenus
        1. 
        *Bombe> removeInfixMenu [[0,1,2],[0,1,2,3],[1,2,3],[1,2,3,4]]
        [[0,1,2,3],[1,2,3,4]]
    --}
    
    -- Remove infix menus form a list of menu (Recurly)
    -- (m:ms)       : The menus which has not been processed.
    -- fullList     : The full list of menus.
    -- newList      : The acceptable menus which has been processed.
    -- Return       : A menu list without infix menus.
    removeInfixMenuRecurly :: [Menu] -> [Menu] -> [Menu] -> [Menu]
    removeInfixMenuRecurly [] fullList newList = newList
    removeInfixMenuRecurly (m:ms) fullList newList
        | isInfixOfMenus m fullList = removeInfixMenuRecurly ms fullList newList    -- Infix menu, not add.
        | otherwise = removeInfixMenuRecurly ms fullList (newList ++ [m])           -- Not an infix menu, add to newList.
    {--
        Tests for isInfixOfMenus
        1. not infix
        *Bombe> isInfixOfMenus [1,2,3] [[0,1,2], [2,3,4]]
        False
        2. is infix
        *Bombe> isInfixOfMenus [1,2,3] [[0,1,2,3], [2,3,4]]
        True
    --}

    -- Determine if a menu is an infix menu
    -- menu     : The menu which need to be checked.
    -- (m:ms)   : The list of menus which has not been compared.
    -- Return   : Whether the target menu is a infix menu of a series of menus
    isInfixOfMenus :: Menu -> [Menu] -> Bool
    isInfixOfMenus menu [] = False
    isInfixOfMenus menu (m:ms)
        | menu == m = isInfixOfMenus menu ms    -- Self
        | isInfixOf menu m = True               -- Infix
        | otherwise = isInfixOfMenus menu ms    -- Not infix
    {--
        Tests for isInfixOfMenus
        1. not infix
        *Bombe> isInfixOfMenus [1,2,3] [[0,1,2], [2,3,4]]
        False
        2. is infix
        *Bombe> isInfixOfMenus [1,2,3] [[0,1,2,3], [2,3,4]]
        True
    --}



    -- Break a Enigma with specific rotors and the reflector
    -- crib     : The crib which need to be cracked.
    -- rl       : Right rotor
    -- rm       : Middle rotor
    -- rr       : Right rotor
    -- refl     : Reflector
    -- Return   ：Maybe (acceptable Offsets, acceptable Steckerboard)
    breakEnigma :: Crib -> Rotor -> Rotor -> Rotor -> Reflector -> Maybe (Offsets, Steckerboard)
    breakEnigma crib rl rm rr refl = breakEA crib menus rl rm rr refl
        where
            menus = getMenu crib

    -- Break a Enigma with specific rotors and the reflector (menus has been got)
    -- crib     : The crib which need to be cracked.
    -- menus    : The menus of the current crib.
    -- rl       : Right rotor
    -- rm       : Middle rotor
    -- rr       : Right rotor
    -- refl     : Reflector
    -- Return   ：Maybe (acceptable Offsets, acceptable Steckerboard)
    breakEA :: Crib -> [Menu] -> Rotor -> Rotor -> Rotor -> Reflector -> Maybe (Offsets, Steckerboard)
    breakEA crib menus rl rm rr refl 
        | maybeStecker == Nothing = breakEARecurly crib menus (advanceBy offsets 1) offsets rl rm rr refl   -- No sulution for current rotor set and reflector.
        | otherwise = Just (offsets, fullStecker)                                                           -- Solution founc.
        where 
            maybeStecker = findStecker crib menus [] offsets rl rm rr refl
            Just fullStecker = maybeStecker
            offsets = (0,0,0)
    {--
        Tests for breakEA
        1. 
        *Bombe> dcs_header = "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP"
        *Bombe> x2 = "NAWLGAFTKDBKDLIKEOSZVAOKXKXFMQQUXBJDRCXRZFDKHLWCNBDJSMJBMJQCBBGJV"
        *Bombe> breakEA (dcs_header, x2) (longestMenu (dcs_header, x2)) rotor1 rotor2 rotor3 reflectorB
        Just ((0,0,0),[('P','A'),('N','C'),('S','L'),('V','R'),('H','T'),('G','D'),('Q','K')])
        *Bombe> enigmaEncodeMessage dcs_header (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0) [('P','A'),('N','C'),('S','L'),('V','R'),('H','T'),('G','D'),('Q','K')]) 
        "YIQDSJQTPTBHKLLXEZZSDSOKFMFXXFMXCZVDRC"
    --}

    -- Break a Enigma with specific rotors and the reflector (Recurly test different offsets)
    -- crib         : The crib which need to be cracked.
    -- menus        : The menus of the current crib.
    -- offsets      : The offsets of current recursion
    -- stopOffsets  : The offsets to stop recursion
    -- rl           : Right rotor
    -- rm           : Middle rotor
    -- rr           : Right rotor
    -- refl         : Reflector
    -- Return       ：Maybe (acceptable Offsets, acceptable Steckerboard)
    breakEARecurly :: Crib -> [Menu] -> Offsets -> Offsets -> Rotor -> Rotor -> Rotor -> Reflector -> Maybe (Offsets, Steckerboard)
    breakEARecurly crib menus offsets stopOffsets rl rm rr refl
        | offsets == stopOffsets = Nothing                                                                          -- All offsets has tried, no solution
        | maybeStecker == Nothing = breakEARecurly crib menus (advanceBy offsets 1) stopOffsets rl rm rr refl       -- No solution for current ofsets, try next.
        | otherwise = Just (offsets, fullStecker)                                                                   -- Solution found.
        where 
            maybeStecker = findStecker crib menus [] offsets rl rm rr refl
            Just fullStecker = maybeStecker

    -- Break a Enigma with specific rotors, reflector and offsets
    -- crib         : The crib which need to be cracked.
    -- menus        : The menus which has not been processed.
    -- initStecker  : The initial steckerboard.
    -- offsets      : The offsets of rotors.
    -- rl           : Right rotor
    -- rm           : Middle rotor
    -- rr           : Right rotor
    -- refl         : Reflector
    -- Return       ：Maybe acceptable Steckerboard
    findStecker :: Crib -> [Menu] -> Steckerboard -> Offsets -> Rotor -> Rotor -> Rotor -> Reflector -> Maybe Steckerboard
    findStecker crib [] initStecker offsets rl rm rr refl = Just initStecker
    findStecker crib menus initStecker offsets rl rm rr refl
        | maybeStecker == Nothing = nextAttempt     -- No sulution for current menu, try next.
        | fullStecker == Nothing = nextAttempt      -- Unresolved conflicts in subsequent menus, try next.
        | otherwise = fullStecker                   -- Solution found.
        where
            (menu:ms) = menus
            firstIndex = menu !! 0
            (plain, cipher) = crib
            (fp, fc) = (zip plain cipher) !! firstIndex

            steckerPair = (fp, fp)
            maybeStecker = steckerAdd steckerPair initStecker
            Just newStecker = maybeStecker

            nextAttempt = findSteckerRecurly crib menus initStecker (nextSteckerPair steckerPair) offsets steckerPair rl rm rr refl

            fullStecker = followMutiMenus crib menus newStecker offsets rl rm rr refl

    -- Break a Enigma with specific rotors, reflector and offsets (Recurly test different initial SteckerPair)
    -- crib             : The crib which need to be cracked.
    -- menus            : The menus which has not been processed.
    -- initStecker      : The initial steckerboard.
    -- steckerPair      : The stecker pair to be test of current recursion
    -- offsets          : The offsets of rotors.
    -- stopSteckerPair  : The stecker pair to stop recursion
    -- rl               : Right rotor
    -- rm               : Middle rotor
    -- rr               : Right rotor
    -- refl             : Reflector
    -- Return           ：Maybe acceptable Steckerboard
    findSteckerRecurly :: Crib -> [Menu] -> Steckerboard -> SteckerPair -> Offsets -> SteckerPair -> Rotor -> Rotor -> Rotor -> Reflector -> Maybe Steckerboard
    findSteckerRecurly crib menus initStecker steckerPair offsets stopSteckerPair rl rm rr refl
        | steckerPair == stopSteckerPair = Nothing      -- All initial stecker pairs has tried, no solution.
        | maybeStecker == Nothing = nextAttempt         -- No sulution for current menu, try next.
        | fullStecker == Nothing = nextAttempt          -- Unresolved conflicts in subsequent menus, try next.
        | otherwise = fullStecker                       -- Solution found.
        where
            maybeStecker = steckerAdd steckerPair initStecker
            Just stecker = maybeStecker

            nextAttempt = findSteckerRecurly crib menus initStecker (nextSteckerPair steckerPair) offsets stopSteckerPair rl rm rr refl

            fullStecker = followMutiMenus crib menus stecker offsets rl rm rr refl

    -- Get the next Initial SteckerPair
    -- (a,b)    : Current SteckerPair
    -- Return   : Next SteckerPair
    nextSteckerPair :: SteckerPair -> SteckerPair
    nextSteckerPair (a,b) = (a, (['A'..'Z'] !! (mod ((alphaPos b) + 1) 26)))
    {--
        Tests for nextSteckerPair
        1. usual
        *Bombe> nextSteckerPair ('A','B')
        ('A','C')
        2. last
        *Bombe> nextSteckerPair ('A','Z')
        ('A','A')
    --}
    
    -- Follow a series of menus. Return the Steckerboaed if all menus are satisfied.
    -- crib         : The crib which need to be cracked.
    -- (menu:ms)    : The menus which has not been processed.
    -- stecker      : The steckerboard.
    -- offsets      : The offsets of rotors.
    -- rl           : Right rotor
    -- rm           : Middle rotor
    -- rr           : Right rotor
    -- refl         : Reflector
    -- Return       ：Maybe acceptable Steckerboard
    followMutiMenus :: Crib -> [Menu] -> Steckerboard -> Offsets -> Rotor -> Rotor -> Rotor -> Reflector -> Maybe Steckerboard
    followMutiMenus crib [] stecker offset rl rm rr refl = Just stecker
    followMutiMenus crib (menu:ms) stecker offset rl rm rr refl
        | maybeStecker == Nothing = Nothing                                     -- No suitable stecker for current menu which compatible with previous menus as well.
        | otherwise = findStecker crib ms newStecker offset rl rm rr refl       -- Found a compatible stecker, process the next menu.
        where
            maybeStecker = followMenu crib menu stecker offset rl rm rr refl
            Just newStecker = maybeStecker

    -- Follow a menu. Return the Steckerboaed which satisfy the menu.
    -- (plain,cipher)   : The crib which need to be cracked.
    -- (i:m)            : The menu item which has not been processed.
    -- stecker          : The steckerboard.
    -- offsets          : The offsets of rotors.
    -- rl               : Right rotor
    -- rm               : Middle rotor
    -- rr               : Right rotor
    -- refl             : Reflector
    -- Return           ：Maybe acceptable Steckerboard
    followMenu :: Crib -> Menu -> Steckerboard -> Offsets -> Rotor -> Rotor -> Rotor -> Reflector -> Maybe Steckerboard
    followMenu (plain,cipher) [] stecker offset rl rm rr refl = Just stecker
    followMenu (plain,cipher) (i:m) stecker offset rl rm rr refl
        | maybeStecker == Nothing = Nothing                                             -- No suitable stecker has found.
        | otherwise = followMenu (plain,cipher) m newStecker offset rl rm rr refl       -- Stecker found.
        where
            formatedCrib = zip plain cipher
            (p, c) = formatedCrib !! i
            q = doStecker stecker p
            r = enigmaEncode q (SimpleEnigma rl rm rr refl (advanceBy offset i))
            maybeStecker = steckerAdd (r, c) stecker
            Just newStecker = maybeStecker



    -- Add a SteckerPair to a Steckerboard, and check for conflicts as well.
    -- (a, b)   : The setcker pair need to be added.
    -- stecker  : The setckerboard need to be added to.
    -- Return   ：Maybe acceptable Steckerboard
    steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
    steckerAdd (a, b) stecker
        | doStecker stecker a == b && doStecker stecker b == a = Just stecker                   -- Already existed.
        | doStecker stecker a == a && doStecker stecker b == b = Just (stecker ++ [(a, b)])     -- Compatible new stecker pair.
        | otherwise = Nothing                                                                   -- Conflict.
    {--
        Tests for steckerAdd
        1. usual
        steckerAdd ('A', 'B') [('C', 'D')]
        Just [('C','D'),('A','B')]
        2. conflict
        *Bombe> steckerAdd ('A', 'B') [('A', 'D')]
        Nothing
        3. meaningless
        steckerAdd ('A', 'A') [('C', 'D')]
        Just [('C','D')]
        4. meaningless but conflict
        *Bombe> steckerAdd ('A', 'A') [('A', 'D')]
        Nothing 
    --}

    -- Advance the Offsets to specific position
    -- (ol, om, or) : The initial offset.
    -- amount       : The amount of offset.
    -- Return       ：The offsets after advanced.
    advanceBy :: Offsets -> Int -> Offsets
    advanceBy (ol, om, or) amount = (mod (div (index + amount) (26*26)) 26, mod (div (index + amount) 26) 26, mod (index + amount) 26)
        where
            index = ol*26*26 + om*26 + or
    {--
        Tests for advanceBy
        1.
        *Bombe> advanceBy (0,0,0) 5
        (0,0,5)
        2.
        *Bombe> advanceBy (0,0,0) 100
        (0,3,22)
    --}