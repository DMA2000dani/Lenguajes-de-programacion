{-# LANGUAGE RecordWildCards #-}    -- per utilitzar fields

import Data.Char (isUpper)
import Data.List (nub, isInfixOf)
import Data.List.Split (splitOn)    -- cal instal·lar: cabal install split
import Data.String.Utils (strip)    -- cal instal·lar: cabal install MissingH
import Data.Maybe (mapMaybe, fromMaybe)

type Programa = [ Regla ]
type Sustitucio = [(Term, Term)]     -- [(variable, constant), (variable, constant), ...]
type BaseConeixement = [ Atom ]

data Regla = Regla { _cap::Atom, _cos::[ Atom ] }
    deriving (Eq, Show)
data Atom = Atom { _nomPredicat::String, _termes::[ Term ] }
    deriving (Eq, Show)
    
data Term = Var String | Sym String
   deriving (Eq, Show)

   
-- Això hauria de ser el main, però no hem compila perque li retorno [IO()] i vol un tipus IO(IO()) segons el compilador, i no se com aonseguir-ho
main = do
    contingut <- getContents
    let programa = parsejar (head (splitOn "end." contingut))
    let preguntes = parsejar (head (tail(splitOn "end." contingut)))
    let b_c = (consequencia (parsejar (head (splitOn "end." contingut))) [])
    print (fer_preguntes programa preguntes b_c)
-- 
-- Parsejar el programa
parsejar:: String -> Programa
parsejar text = if(last (splitOn "." text) == "") then afegir_regles (init (splitOn "." text)) else afegir_regles (splitOn "." text)

-- Crea totes les regles del Programa
afegir_regles sregles
    | sregles == [] = []
    | otherwise = afegir_regla (splitOn " => " (head sregles)) : afegir_regles (tail sregles) -- Llegeix totes les regles i les fica a Programa 
--     
afegir_regla::[String] -> Regla
afegir_regla sregla  -- Crea una Regla
    | length sregla == 1 = Regla { _cap = (afegir_atom $ splitOn " " (head sregla)), _cos = []}
    | otherwise = Regla {_cap = (afegir_atom $ splitOn " " (last sregla)), _cos = (afegir_atoms (splitOn " & "  (head sregla)))}
-- 

afegir_atoms::[String] -> [Atom]
afegir_atoms satoms -- Crea tots els atoms d'una Regla
    | length satoms == 1 = (afegir_atom $ (splitOn " " (head satoms))) : []
    | otherwise = ((afegir_atom $ (splitOn " " (head satoms))) : (afegir_atoms $ tail satoms))
--     

afegir_atom::[String] -> Atom
afegir_atom sparaules = Atom {_nomPredicat = (head sparaules), _termes = (afegir_termes $ tail sparaules)} -- Crea un Atom

afegir_termes sparaules -- Crea tots els termes d'un Atom
    | sparaules == [] = []
    | otherwise = ((afegir_terme $ head sparaules) : (afegir_termes $ tail sparaules))
-- 
afegir_terme paraula  -- Crea un terme
    | isUpper $ head paraula = Var paraula   -- Si la primera lletra és majúscula es una Var
    | otherwise = Sym paraula          -- Si la primera letra és minúscula és un Sym
--  -- fi parser 
--   
--     
fer_preguntes::[Regla] -> [Regla] -> BaseConeixement-> [String]
fer_preguntes programa preguntes b_c = map (respon_pregunta b_c) (preguntes)

respon_pregunta::BaseConeixement -> Regla -> String
respon_pregunta b_c (Regla (Atom nomP termes) cosP)
    | length (termes) == 0 = if busca_atom b_c (head cosP) then "true" else "false" --busco si el fet és a programa
    | length (termes) == 1 && length cosP == 1 = (lsustitucions_a_String (avaluaAtom b_c (head cosP) []) [] ) -- query amb 1 variable
--    | length (termes) == 1 && length cosP > 1 = print --query amb 1 variable i molts atoms al _cos no implementat
    | length (termes) == 2 && length cosP == 1 = (lsustitucions_a_String (avaluaAtom b_c (head cosP) []) []) -- query amb 2 variables i un atom al _cos
    | otherwise = "false"

-- per a poder imprimir, transformo la resposta a String en respon_pregunta i ho imprimeixo en el main
lsustitucions_a_String:: [Sustitucio] -> String -> String
lsustitucions_a_String lsus s
    | length lsus == 1 = "[" ++ s ++ "[" ++ (sustitucio_a_String (head lsus) "") ++ "]]"
    | length lsus > 0 = lsustitucions_a_String (tail lsus) (s ++ "[" ++ sustitucio_a_String(head lsus) "" ++ "],")
    
sustitucio_a_String::Sustitucio -> String -> String
sustitucio_a_String sus string --string és la string fins al moment
    | length sus == 1 = "[" ++ string ++ "]"
    | length sus > 0 = sustitucio_a_String (tail sus) (string ++ "(" ++ termes_a_String (head sus) ++ "),")
    | otherwise = ""
    
termes_a_String::(Term, Term) -> String
termes_a_String termes = terme_a_String (fst termes) ++ "," ++ terme_a_String (snd termes)

terme_a_String:: Term -> String
terme_a_String(Var s) = ("Var " ++ s)
terme_a_String(Sym s) = ("Sym " ++ s)
    
--     
busca_atom::BaseConeixement -> Atom -> Bool
busca_atom b_c (Atom nomP termes) -- mira si l'atom esta a la base de coneixement
    | length b_c == 0 = False
    | length b_c > 0 && (((_nomPredicat (head b_c))) == nomP) && (length (_termes (head b_c)) == length termes) && igual_termes (_termes (head b_c)) termes = True
    |otherwise = busca_atom (tail b_c) (Atom{_nomPredicat = nomP, _termes = termes})

igual_termes lt1 lt2
    | length lt1 == 0 = True
    | length lt1 == length lt2 && length lt1 > 0 && esIgual (head lt1) (head lt2) = igual_termes (tail lt1) ( tail lt2)
    | otherwise = False
    
-- avaluar_capcos:: BaseConeixement -> Regla -> [Sustitucio] -> [Sustitucio]
-- avaluar_capcos11 b_c lsus termesQ-- genera les respostes a la regla -- no ho he fet



-- -- --Creo una baseConeixement amb les regles que ja hi havia i vaig cridant a consequencia fins que la baseConeixement no canvii

-- avaluació de les regles i creació de noves bases de coneixement
consequencia :: Programa -> BaseConeixement -> BaseConeixement
consequencia programa kbx -- retorna la base de coneixement amb tots els atoms_ground possibles(els fets inicials del programa i els nous atoms ground resultat d'avaluar les regles)
        | kbx == avaluaRegles kbx programa = kbx
        | kbx /= avaluaRegles kbx programa = consequencia programa (avaluaRegles kbx programa)

avaluaRegles b_c programa --retorna la base de coneixement b_c amb els nous atoms ground resultants de avaluar les regles
    | length programa == 0 = b_c  
    | otherwise = b_c ++ (avaluaRegles (avaluaRegla b_c (head programa)) (tail programa)) -- afegeix els nous atoms ground obtinguts en evaluar la 1a regla del programa i evalua les altres regles del programa
--     
-- Pre: | _cos r /= [] = obtenir_mes_atoms_ground
avaluaRegla :: BaseConeixement -> Regla -> BaseConeixement  
avaluaRegla b_c (Regla capR cosR) --retorna una base de coneixement a partir de la regla
        | cosR == [] && _nomPredicat capR /= "query" = [Atom{_nomPredicat = _nomPredicat capR, _termes =  _termes capR}]  --retorna la base de coneixement sense canvis
        | cosR /= [] && length cosR == 1 = nub (b_c ++ (avaluaRegla1 b_c (Regla{_cap = capR, _cos = cosR}))) -- afegir tots els atoms resultants de la unificació i substitució. Sense repeticions
        | cosR /= [] && length cosR > 1 = afegeix_atoms_B_C (escull_sustitucions (avaluaRegla2 b_c (Regla{_cap = capR, _cos = cosR}))) (capR) b_c
--      
avaluaRegla1:: BaseConeixement -> Regla -> BaseConeixement
avaluaRegla1 b_c (Regla capR cosR) = -- retorna la base de coneixement resultant d'avaluar la regla amb una variable en els termes
    let substitucions1 = head  (avaluaAtom b_c (head $ cosR) [])
        atom1 = head $ cosR
        atom2 = capR
        in
            afegeix_atoms_B_C substitucions1 atom2 b_c
--     
afegeix_atoms_B_C:: Sustitucio -> Atom -> BaseConeixement -> BaseConeixement
afegeix_atoms_B_C subs atom b_c-- genera nous atoms ground per a la base de coneixement a partir de la llista de substitucions possibles de variables i l'atom que en el que es vol fer les substitucions i els retorna juntament amb la base de coneixement inicial
    | subs == [] = []
    | sustitueix atom subs /= atom && elem (sustitueix atom subs) b_c == False = [sustitueix atom subs] ++ afegeix_atoms_B_C  (tail subs) atom b_c
    | otherwise = afegeix_atoms_B_C  (tail subs) atom b_c
-- 
avaluaRegla2::BaseConeixement -> Regla -> [Sustitucio]
avaluaRegla2 b_c (Regla capR cosR) -- retorna la llista de substitucions que es poden fer per a tots els atoms del _cos amb la b_c actual
    | cosR == [] = []
    | cosR /= [] = avaluaRegla2' b_c cosR ++ avaluaRegla2 b_c (Regla{_cap = capR, _cos = cosR})
    
avaluaRegla2'::BaseConeixement -> [Atom] -> [Sustitucio]
avaluaRegla2' b_c atoms -- retorna la llista de substitucions que es poden fer per a tots els atoms amb la b_c actual
    | atoms == [] = []
    | atoms /= [] = (avaluaAtom b_c (head $ atoms) []) ++ avaluaRegla2' b_c (tail atoms)
    


--     
escull_sustitucions:: [Sustitucio] -> Sustitucio
escull_sustitucions lsus --retorna la llista de sustitucions resultant de fusionar lsus comú en ambes parts
    | length lsus == 1 = head lsus 
    | length lsus == 2  = fusiona (head $ lsus) (head $ tail lsus)
    | length lsus > 2 = escull_sustitucions ((fusiona (head $ lsus) (head $ tail lsus)) : (tail $ tail $ lsus))



fusiona:: Sustitucio-> Sustitucio -> Sustitucio
fusiona sus1 sus2 -- retorna les sustitucions que estan a sus1 i sus2
    | sus2 == [] = []
    | sus2 /= [] && (fusiona' sus1 (head sus2) /= termtermBuit)= fusiona' sus1 (head sus2) : fusiona sus1 (tail sus2)
    | otherwise = fusiona sus1 (tail sus2)
    
fusiona'::Sustitucio -> (Term, Term) -> (Term,Term) -- mira si termes pertany a sus1. Si està el retorna, sinó, retorna buit 
fusiona' sus1 termes
    | sus1 == [] = termtermBuit
    | sus1 /= [] && esIgual(fst termes) (fst $ head sus1) && esIgual(snd termes) (snd $ head sus1) = termes-- si son la mateixa variable, afegeix-la
    | otherwise = fusiona' (tail sus1) termes

-- fusiona sus1 sus2 = sus1

sustitucioBuida :: Sustitucio
sustitucioBuida = []

termtermBuit :: (Term, Term)
termtermBuit = ((Var "invalid"), (Sym "Invalid")) -- poso com a buit això, ja que mai pasarà i [] o () no m'ho compila
-- 


-- -- unificacio

avaluaAtom :: BaseConeixement -> Atom -> [ Sustitucio ] -> [Sustitucio] -- retorna la llista de substitucions que es poden fer per a l'atom amb la BaseConeixement donats
avaluaAtom b_c (Atom nomP termes) sustitucions = sustitucions ++ mapMaybe id (avaluaAtom' b_c (Atom{_nomPredicat = nomP, _termes = termes}) [])

avaluaAtom':: BaseConeixement -> Atom -> [Maybe Sustitucio] -> [Maybe Sustitucio]
avaluaAtom' b_c atom sustitucions
    | b_c == [] = sustitucions
    | b_c /= [] = avaluaAtom' (tail b_c) atom (sustitucions ++ [unifica atom (head b_c)])
    
crear_sustitucio::Term -> Term -> Sustitucio
crear_sustitucio t1 t2 = [((t1),(t2))]
-- escriu atom1 atom2 =
--     print (unifica atom1 atom2)
    

-- Pre: Atom2 és atom ground
unifica::Atom -> Atom -> Maybe Sustitucio
unifica atom1 atom2 -- retorna, si es pot fer, la substitució(amb Just[(variable, constant), (variable, constant), ...]) resultant de la unificació de l'atom1 amb l'atom2. Si no, retorna Nothing 
    | atom1 == atom2 = Just [] -- son dos fets
    | _nomPredicat atom1 == _nomPredicat atom2  = unifica' (_termes atom1) (_termes atom2)-- Si tenen el mateix nom de predicat, mira d'unificar
    | otherwise = Nothing
 
unifica' :: [Term] -> [Term] -> Maybe Sustitucio
unifica' termes1 termes2 -- terme2 han de ser ground
    | (esVar $ head termes1) && (esVar $ head $ tail termes1) && (not(esIgual (head termes1) (head $ tail termes1))) && (not(esIgual (head termes2) (head $ tail termes2))) = Just [(head termes1, head termes2), (head $ tail termes1, head $ tail termes2)] -- X Y, a,b => [(X, a), (Y,b)] 
    | (esVar $ head termes1) && (esVar $ head $ tail termes1) && (esIgual (head termes1) (head $ tail termes1)) && (esIgual (head termes2) (head $ tail termes2)) = Just [(head termes1, head termes2)] -- X X, a,a => [(X, a)]
    | (esVar $ head termes1) && (not(esVar (head $ tail termes1))) && (esIgual (head $ tail termes1) (head $ tail termes2)) = Just[(head termes1, head termes2)] -- X, b    a,b=> [(X, a)] 
    | (not(esVar $ head termes1) ) && ( esVar $ head $ tail termes1 ) && (esIgual (head termes1) (head termes2)) = Just[(head $ tail termes1, head $ tail termes2)] -- a, X    a,b=> [(X, b)]
    | otherwise = Nothing
    
esVar:: Term -> Bool
esVar (Var s) = True
esVar (Sym s) = False

esIgual:: Term -> Term -> Bool
esIgual (Var s1) (Var s2) = if s1 == s2 then True else False
esIgual (Sym s1) (Sym s2) = if s1 == s2 then True else False
esIgual t1 t2 = False 

getString::Term -> String
getString (Var s) = s
getString (Sym s) = s

-- Se suposa que la substitució només te les variables per a l'atom
-- sustitueix:: Atom -> Sustitucio -> Atom
sustitueix::Atom-> Sustitucio -> Atom
sustitueix (Atom n termes) sus
    | length sus == 2 && (esIgual (head termes) (fst $ head sus)) && (esIgual(head $ tail termes) (head termes)) = Atom {_nomPredicat = n, _termes = [(snd $ head sus),(snd $ head sus)]}-- [Var "X", Var "X"] i [(Var "X", Sym elquesigui)]
    | length sus == 2 && (esIgual (head termes) (fst $ head $ tail sus)) && (esIgual (head $ tail termes) (fst $ head sus)) = Atom {_nomPredicat = n, _termes = [(snd $ head $ tail sus), (snd $ head sus)]} -- Y X
    | length sus == 2 &&  (esIgual (head termes) (fst $ head sus)) && (esIgual (head $ tail termes) (fst $ head $ tail sus)) = Atom {_nomPredicat = n, _termes = [(snd $ head sus), (snd $ head $ tail sus)]} -- var var X Y
    | length sus == 1 && (esIgual (head termes) (fst $ head sus)) && not(esVar (head $ tail termes)) = Atom {_nomPredicat = n, _termes = [(snd $ head sus), (last termes)]} -- Variable Constant
    | length sus == 1 && not(esVar $ head termes) && (esIgual (head $ tail $ termes) (fst $ head sus)) = Atom {_nomPredicat = n, _termes = [(head termes), (snd $ head sus)]} -- constant Variable
    | otherwise = Atom {_nomPredicat = n, _termes = termes} -- constant constant

    
    
    
