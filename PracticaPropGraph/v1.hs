import Control.Monad ( filterM )

data Date = Date {_dia::Int,  _mes::Int, _any::Int}
    deriving(Eq,Show)

data Val = VB Bool| VI Int | VD Double |VS String | VDate Date
  deriving(Eq,Show)
  
data Edge = Edge{_nomEdge::String, _nomNodeInici::String, _nomNodeFi::String}
  deriving (Eq, Show)
  
data Prop = Prop{_nomP::String, _val::Val}
  deriving (Eq,Show)

data Etiqueta = Etiqueta{_nomNode::String, _nomEtiqueta::String} 
  deriving (Eq,Show)
  
type Label = Either String Etiqueta
type Vertex = String
  
data Sigma = Sigma{_nomVertex::String, _propietat::Prop}
  deriving (Eq,Show)
  
data PG = PG{_vertexs::[String], _edges::[Edge], _labels::[Label], _sigma::[Sigma], _prop::[Prop]}
  deriving (Eq, Show) --redefinir el show


main = do
  rhoString <- readFile $ "rhoFile.pg"
  lambdaString <- readFile $ "lambdaFile.pg"
  sigmaString <- readFile $ "sigmaFile.pg"
  propString <- readFile $ "propFile.pg"
 
  let graf = populate rhoString lambdaString sigmaString propString
  putStrLn "S'escriu el graf:"
  putStrLn $ showGraph graf
  putStrLn "Resultats de la consulta σ':"
  putStrLn $ getStringProps $ treuRepetits $ rho' graf
  putStrLn "Resultats de la consulta propV amb els parametres del codi:"
  putStrLn $ getStringPropVoE (_labels graf) $ propV graf 5 (Prop{_nomP = "firstName", _val = VS "Joan"})
  putStrLn "Resultats de la consulta propE amb els parametres del codi:"
  putStrLn $ getStringPropVoE (_labels graf) $ propE graf 5 (Prop{_nomP = "since", _val = VDate Date{_dia = 14, _mes = 02, _any = 2019}})
  putStrLn "Resultats de la consulta KHops amb els parametres del codi:"
  putStrLn $ getStringkHops  (kHops graf 8 "n5" "firstName") graf
  putStrLn "Resultats de la consulta reachable amb els parametres del codi:"
  putStrLn $ isreachable $ reachable graf "n1" "p1" "has"


--Funcion per obtenir les string de les consultores

getStringProps:: [Prop] -> String
getStringProps props
    | props == [] = ""
    | otherwise = stringProp2 (head props) ++ "\n" ++ getStringProps (tail props)

getStringPropVoE::[Label] -> [Sigma] -> String
getStringPropVoE ls (sigma@(Sigma nomV prop):ss)
    | ss == [] = "(" ++ findEtiq nomV ls ++  "," ++ stringS sigma ++ ")" ++ "\n"
    | otherwise = "(" ++ findEtiq nomV ls ++  "," ++ stringS sigma ++ ")"++ "\n" ++ getStringPropVoE ls ss
    
isreachable:: Bool -> String
isreachable True = "True" ++ "\n"
isreachable False = "False" ++ "\n"

stringS::Sigma -> String
stringS (Sigma nomN prop) = nomN ++ "," ++ (stringProp2 prop)
    
stringEt::Etiqueta -> String
stringEt et@(Etiqueta nomN nomEt) = nomN ++ " " ++ nomEt

--(v', λ(v'), σ(v', p))
getStringkHops:: [Sigma] -> PG -> String
getStringkHops [] _ = ""
getStringkHops (sigma@(Sigma nomV prop@(Prop nomP val)):ss) g@(PG vertexs es ls sigmes ps)
    | ss == [] = "(" ++ nomV ++ "," ++ findEtiq nomV ls ++ ",(" ++ stringS sigma ++ ")\n"
    | otherwise = "(" ++ nomV ++ "," ++ findEtiq nomV ls ++ ",(" ++ stringS sigma ++ ")\n" ++ getStringkHops ss g

    
treuRepetits::[Prop] -> [Prop]
treuRepetits (prop:props)
    | props == [] = [prop]
    | elem prop props == True = treuRepetits props
    | otherwise = [prop] ++ treuRepetits props

--Creadores de tipus a partir dels fitxers

createS:: Prop -> [String] -> Sigma
createS (Prop nomP (VB _)) (x1:x2:x3:xs) = Sigma{ _nomVertex = x1, _propietat = Prop{_nomP = x2, _val = VB (string2Bool x3)}}
createS (Prop nomP (VI _)) (x1:x2:x3:xs) = Sigma{ _nomVertex = x1, _propietat = Prop{_nomP = x2, _val = VI ((read x3)::Int)}}
createS (Prop nomP (VD _)) (x1:x2:x3:xs) = Sigma{ _nomVertex = x1, _propietat = Prop{_nomP = x2, _val = VD ((read x3)::Double)}}
createS (Prop nomP (VS _)) (x1:x2:x3:xs) = Sigma{ _nomVertex = x1, _propietat = Prop{_nomP = x2, _val = (VS x3)}}
createS (Prop nomP (VDate _)) (x1:x2:x3:xs) = Sigma{ _nomVertex = x1, _propietat = Prop{_nomP = x2, _val = VDate (string2Date x3)}}

createE:: [String] -> Edge
createE (x1:x2:x3:xs) = Edge{_nomEdge = x1, _nomNodeInici = x2, _nomNodeFi = x3}

createEtiqueta::[String] -> Etiqueta
createEtiqueta (nomN:nomEt:lbuida) = Etiqueta{_nomNode = nomN, _nomEtiqueta = nomEt}

createLabel::PG -> [String] -> Label
createLabel (PG v e labels s p) et@(nomN:nomEt:lbuida)
  | any (\x -> jaExisteixEtiquetaPelNode nomN x) labels == True = Left ("Ja existia l'etiqueta amb nom: " ++ nomEt ++ " pel node " ++ nomN ++ " en el graf\n")
  | otherwise = Right $ createEtiqueta et
  
jaExisteixEtiquetaPelNode:: String -> Label -> Bool
jaExisteixEtiquetaPelNode nomN (Right (Etiqueta nomNode nomEt)) = (nomNode == nomN)
jaExisteixEtiquetaPelNode nomN (Left errors) = False

createP:: [String] -> Prop
createP (x1:x2:xs)
    | x2 == "Bool" = Prop{_nomP = x1, _val = VB True}
    | x2 == "Int" = Prop{_nomP = x1, _val = VI 0}
    | x2 == "Double" = Prop{_nomP = x1, _val =VD 0.0}
    | x2 == "String" = Prop{_nomP = x1, _val = VS "buit"}
    | x2 == "Date" = Prop{_nomP = x1, _val = VDate (createDbuit)}

--creadores de tipus complexes buits
create:: PG
create = PG{_vertexs = [], _edges = [], _labels= [], _sigma = [], _prop = []}

createDbuit::Date
createDbuit = Date{_dia = 0, _mes = 0, _any = 0}
  
--1a Part

--Crear Graf a partir dels fitxers
populate::String -> String -> String -> String -> PG
populate rhoString lambdaString sigmaString propString = 
    let rhowords = map (words) (lines rhoString)
        lambdawords = map (words) (lines lambdaString)
        sigmawords = map (words) (lines sigmaString)
        propwords = map (words) (lines propString)
        graf = create
    in
        omplegraf graf rhowords lambdawords sigmawords propwords
    
omplegraf::PG -> [[String]] -> [[String]] -> [[String]] -> [[String]] -> PG
omplegraf graf rw lw sw pw =  defVEProp (ompleLabels (ompleProps  (ompleEdges graf rw) pw) lw) sw

--Aquesta funció és la fusió de defVProp i defEProp
defVEProp::PG -> [[String]]-> PG
defVEProp graf@(PG v e et s props) stringSigmes 
    | stringSigmes == [] = graf
    | otherwise = defVEProp (afegirSigma graf (createS (findProp props (head stringSigmes)) (head  stringSigmes))) (tail stringSigmes)
  
afegirSigma::PG -> Sigma -> PG
afegirSigma graf@(PG vertexs edges labels s ps) sigma
    | elem sigma s = graf
    | otherwise = PG{_vertexs = vertexs, _edges = edges, _labels = labels, _sigma = s ++ [sigma], _prop = ps}

-- Com que per passar String a Val he de saber de quin tipus és, aquesta funció retornara un valor del mateix tipus que ValP
findProp:: [Prop] -> [String] -> Prop
findProp (prop@(Prop nomProp nomV):props) string@(node:nomP:val:xs)
    | props == [] && nomP /= nomProp = Prop{_nomP = "ERROR", _val = VS "ERROR"}
    | nomP == nomProp = prop
    | otherwise = findProp props string

-- diferentsNomsP nomP (Prop nom val)
--   | nomP == nom = False
--   | otherwise = True
  
ompleProps:: PG -> [[String]] -> PG
ompleProps graf stringProps
    | stringProps == [] = graf
    | otherwise = ompleProps (afegirProp graf (createP (head stringProps))) (tail stringProps)
  
afegirProp:: PG -> Prop -> PG
afegirProp graf@(PG vertexs edges etiquetes s ps) prop
    | elem prop ps = graf
    | otherwise = PG{_vertexs = vertexs, _edges = edges, _labels = etiquetes, _sigma = s, _prop = ps ++ [prop]}

ompleLabels::PG -> [[String]]-> PG
ompleLabels graf stringLabels 
    | stringLabels == [] = graf
    | otherwise = ompleLabels (afegirEtiq graf (createLabel graf (head stringLabels))) (tail stringLabels)

-- Aquesta funció és la fusió de defVlabel i defElabel de l'enunciat. 
-- Les he afegit en una mateixa estructura ja que era el mateix per ambdues
afegirEtiq::PG -> Label -> PG
afegirEtiq graf@(PG vertexs edges etiquetes s ps) etiq
    | elem etiq etiquetes = graf
    | otherwise = PG{_vertexs = vertexs, _edges = edges, _labels = etiquetes ++ [etiq],_sigma = s, _prop = ps}

ompleEdges::PG -> [[String]] -> PG
ompleEdges graf@(PG vertexs edges labels sigmes props) stringEdges
    | stringEdges == [] = graf
    | otherwise = ompleEdges (addEdge graf (createE (head stringEdges))) (tail stringEdges)
    
addEdge:: PG -> Edge -> PG
addEdge graf@(PG vertex edges etiquetes s p) edge@(Edge nE ni nf)
    | elem edge (edges) = graf
    | not(elem ni vertex) = addEdge PG{_vertexs = vertex ++ [ni], _edges = edges, _labels = etiquetes, _sigma = s, _prop = p} edge -- Mira si ni pertany a vertexs, si no és així, l'afegeix
    | not(elem nf vertex) = addEdge PG{_vertexs = vertex ++ [nf], _edges = edges, _labels = etiquetes, _sigma = s, _prop = p} edge-- Mira si nf pertany a vertexs, si no és així, l'afegeix
    | otherwise = PG{_vertexs = vertex, _edges = edges ++ [edge], _labels = etiquetes, _sigma = s, _prop = p}

 
showGraph::PG -> String
showGraph graf =  (printNodes graf)  ++  (printEdges graf)

printNodes::PG -> String
printNodes graf@(PG v e labels propsViE props)
    | v == [] = "\n"
    | otherwise = (printNode (head v) labels propsViE) ++ (printNodes (PG (tail v) e labels propsViE props))
  
printNode:: String -> [Label] -> [Sigma] -> String
printNode vertex labels propsViE = vertex ++ "[" ++ findEtiq vertex labels ++ "]{" ++ (printSigmes vertex (filter (igualsNomsS vertex) propsViE))++ "}\n"

findEtiq::String -> [Label] -> String
findEtiq v labels
    | (dropWhile (diferentsL v) labels) == []  = "Error: no existeix vertex amb nom " ++ v
    | otherwise = printNomEt (head (dropWhile (diferentsL v) labels))
  
printNomEt::Label-> String
printNomEt (Right (Etiqueta nomN nomEt)) = nomEt

diferentsL::String -> Label -> Bool
diferentsL v (Right (Etiqueta nomN nomEt))
    | v == nomN = False
    | otherwise = True
diferentsL v (Left _) = True

--Pre: Estan tots els Sigmes del vertex vertex
--Post: retorna totes les línies que es volen imprimir de les Propietats que hi ha a cada Sigma
printSigmes:: String -> [Sigma] -> String
printSigmes vertex propsV
    | propsV == [] = ""
    | otherwise = printSigma (head propsV) ++ printSigmes vertex (tail propsV)

printSigma::Sigma -> String
printSigma (Sigma nomV prop) = stringProp2 prop
      
igualsNomsS:: String -> Sigma -> Bool
igualsNomsS vertex (Sigma nomV (Prop prop val)) = vertex == nomV
  

stringProp2:: Prop -> String
stringProp2 (Prop prop (VB b)) = "(" ++ prop ++ "," ++ (show b) ++ ")"
stringProp2 (Prop prop (VI int)) = "(" ++ prop ++ "," ++ (show int) ++ ")"
stringProp2 (Prop prop (VS str)) = "(" ++ prop ++ "," ++ str ++ ")"
stringProp2 (Prop prop (VDate (Date dia mes anys))) = "(" ++ prop ++ "," ++ show dia ++ "-" ++ show mes ++ "-" ++ show anys ++ ")"
stringProp2 (Prop prop (VD double)) = "(" ++ prop ++ "," ++ show double ++ ")"

printEdges:: PG -> String
printEdges graf@(PG v edges labels propsViE props)
    | edges == [] = "\n"
    | otherwise = (printEdge (head edges) labels propsViE) ++ (printEdges (PG v (tail edges) labels propsViE props))
  
printEdge:: Edge -> [Label] -> [Sigma] -> String
printEdge (Edge edge nI nF) labels sigmes =  "(" ++ nI ++ ")-" ++ edge ++ "[" ++ findEtiq edge labels ++ "]-> (" ++ nF ++ ") {" ++  printSigmes edge (filter (igualsNomsS edge) sigmes) ++ "}\n"

--conversors String2tipus

string2Date::String->Date
string2Date stringDate = Date{_dia = read(takeWhile (\x -> x /= '-') stringDate)::Int, 
                              _mes = read(takeWhile (\x -> x /= '-') (tail (dropWhile (\x -> x /= '-') stringDate)))::Int,
                              _any = read(tail $ dropWhile (\x -> x /= '-') (tail (dropWhile (\x -> x /= '-') stringDate)))::Int}

string2Bool:: String -> Bool
string2Bool sbool
    | sbool == "True" = True
    | otherwise = False

-- Segona part: consultes

rho'::PG -> [Prop]-- Busca les propietats identiques en nom i valor entre els vèrtexs i els edges
rho' graf@(PG vertexs edges labels (sigma@(Sigma nomN prop):sigmes) props)
    | sigmes == [] = []
    | otherwise = afegirSigmes vertexs edges sigma sigmes ++ rho'(PG vertexs edges labels sigmes props)
  
afegirSigmes:: [Vertex] -> [Edge] -> Sigma -> [Sigma] -> [Prop]
afegirSigmes vertexs edges sigma@(Sigma nomV prop) sigmes 
    | sigmes == [] = []
    | cumpleixRequisits vertexs edges sigma (head sigmes) = [prop] -- al poner sigmes habrá repetidos
    | otherwise = afegirSigmes vertexs edges sigma (tail sigmes)
  
cumpleixRequisits::[String] -> [Edge] -> Sigma -> Sigma -> Bool
cumpleixRequisits vertexs edges sigma@(Sigma nomN1 prop1) (Sigma nomN2 prop2)
    | areViE nomN1 nomN2 vertexs edges && prop1 == prop2 = True
    | otherwise = False

areViE:: String -> String -> [String] -> [Edge] -> Bool
areViE nomN1 nomN2 vertexs edges
    | elem nomN1 vertexs && esEdge nomN2 edges == True = True
    | elem nomN2 vertexs && esEdge nomN1 edges == True = True
    | otherwise = False
    
esEdge:: String -> [Edge] -> Bool
esEdge nomE ((Edge nomEdge _ _):edges)
    | edges == [] && nomE == nomEdge = True
    | edges == [] && nomE /= nomEdge = False
    | nomE == nomEdge = True
    | nomE /= nomEdge = esEdge nomE edges
    
-- Pre l'enter nat ha de ser un Natural incloent-hi el 0
propV:: PG -> Int -> Prop -> [Sigma]
propV graf@(PG vs es ls (sigma@(Sigma nomV prop1):ss) ps) nat prop
    | nat == 0 || ss == [] = []
    | esCandidat vs sigma prop = [sigma] ++ propV (PG vs es ls ss ps) (nat-1) prop
    | otherwise = propV (PG vs es ls ss ps) nat prop

esCandidat::[Vertex] -> Sigma ->  Prop -> Bool
esCandidat vs (Sigma nomV prop1) prop2
    | (elem nomV vs) && (prop1 == prop2) = True
    | otherwise = False
    
-- Pre l'enter nat ha de ser un Natural incloent-hi el 0
propE:: PG -> Int -> Prop -> [Sigma]
propE graf@(PG vs es ls (sigma@(Sigma nomE prop1):ss) ps) nat prop
    | nat == 0 || ss == [] = []
    | esCandidat' es sigma prop = [sigma] ++ propE (PG vs es ls ss ps) (nat-1) prop
    | otherwise = propE (PG vs es ls ss ps) nat prop
    
esCandidat'::[Edge] -> Sigma ->  Prop -> Bool
esCandidat' es (Sigma nomE prop1) prop2
    | esEdge nomE es && (prop1 == prop2) = True
    | otherwise = False

-- retorna true si existeix un camí entre vI i vF en el qual, els arcs pels que es passa tenen la mateixa etiqueta l
reachable::PG -> Vertex -> Vertex -> String -> Bool
reachable (PG vs es ls ss ps) vI vF nomEt = 
    let edges = obteEdges (obteNomEdgesAmbEtiq es ls nomEt) es
        dests = obtePosiblesDest vI edges
    in
        buscaCami vI vF edges dests
    
buscaCami::Vertex -> Vertex -> [Edge] -> [Vertex] -> Bool
buscaCami vI vF edges dest
    | dest == [] = False
    | elem vF dest = True
    | otherwise = buscaCami (head dest) vF edges (obtePosiblesDest vI edges) || buscaCami vI vF edges (tail dest)
    
obtePosiblesDest:: Vertex -> [Edge] -> [Vertex]
obtePosiblesDest _ [] = []
obtePosiblesDest vI ((Edge nomE nomVI nomVF):edges)
    | edges == [] && vI == nomVI = [nomVF]
    | edges == [] && vI /= nomVI = []
    | vI == nomVI = [nomVF] ++ obtePosiblesDest vI edges
    | otherwise = obtePosiblesDest vI edges

--retorna tots els Edges del graf que tenen el mateix nom que un dels noms dels edges de la llista pasada com a paràmetre
obteEdges:: [String] -> [Edge] -> [Edge]
obteEdges [] edges = []
obteEdges (nomE:nomEdges) edges
    | nomEdges == [] = findEdge nomE edges
    | nomEdges /= [] = findEdge nomE edges ++ obteEdges nomEdges edges
    
--Retorna un únic edge en una llista d'un element, ja que el nom de l'edge només estarà una vegada
findEdge::String -> [Edge] -> [Edge]
findEdge nomE1 (edge@(Edge nomE2 nomV1 nomV2):edges)
    | edges == [] && nomE1 == nomE2 = [edge]
    | edges == [] && nomE1 /= nomE2 = []
    | nomE1 == nomE2 = [edge]
    | otherwise = findEdge nomE1 edges

--retorna la llista dels noms de les arestes que tenen la mateixa etiqueta
obteNomEdgesAmbEtiq:: [Edge] -> [Label] -> String -> [String]
obteNomEdgesAmbEtiq es ((Right (Etiqueta nomE1 nomEt1)):ls) nomEt2
    | ls == [] && edgeSameEtiq nomE1 nomEt1 es nomEt2 = [nomE1]
    | ls == [] && not(edgeSameEtiq nomE1 nomEt1 es nomEt2) = []
    | edgeSameEtiq nomE1 nomEt1 es nomEt2 = [nomE1] ++ obteNomEdgesAmbEtiq es ls nomEt2
    | otherwise = obteNomEdgesAmbEtiq es ls nomEt2
     
--retorna True si tenen el mateix nom d'etiqueta i nomE1 és un Edge. Altrament, False
edgeSameEtiq::String -> String -> [Edge] -> String -> Bool
edgeSameEtiq nomEdge1 nomEt1 es nomEt2
    | nomEt2 == nomEt1 && esEdge nomEdge1 es = True
    | otherwise = False

--kHops(G, k, v, p, f, x) (grafo, nat, vertice, nomPropiedad, funcion, valor)
kHops:: PG -> Int -> Vertex -> String -> [Sigma]
kHops (PG vs es ls ss ps) nat vI nomP = {-getSigmesCumpleixFunc-} getSigmesSameProp nomP ss (treuVRepetits $ reachK vI nat es)

getSigmesSameProp::String -> [Sigma] -> [Vertex] -> [Sigma]
getSigmesSameProp _ _ [] = []
getSigmesSameProp nomP sigmesG (nomV1:nomVs)
    | nomVs == [] = findSigmaSamenomProp nomV1 sigmesG nomP
    | otherwise = findSigmaSamenomProp nomV1 sigmesG nomP ++ getSigmesSameProp nomP sigmesG nomVs

findSigmaSamenomProp:: Vertex -> [Sigma] -> String -> [Sigma]
findSigmaSamenomProp nomNode (sigma1@(Sigma nomV (Prop nomP2 val)):sigmes) nomP1
    | sigmes == [] && nomNode == nomV && nomP2 == nomP1 = [sigma1]
    | sigmes == [] && (nomNode /= nomV || nomP2 /= nomP1) = []
    | nomNode == nomV && nomP2 == nomP1 = [sigma1] ++ findSigmaSamenomProp nomNode sigmes nomP1
    | otherwise = findSigmaSamenomProp nomNode sigmes nomP1

-- Pre: nat > 0
reacheableK::Vertex -> Vertex -> Int -> [Edge] -> Bool
reacheableK vI vF nat edges = elem vF (buscaCamiK vI (nat) (obtePosiblesDest vI edges) edges)

reachK::Vertex -> Int -> [Edge] -> [Vertex]
reachK vI nat edges = treuVRepetits $ buscaCamiK vI (nat) (obtePosiblesDest vI edges) edges

-- Pre: nat > 0
buscaCamiK:: Vertex -> Int -> [Vertex] -> [Edge] -> [Vertex]
buscaCamiK vI nat dests edges
    | dests == [] = []
    | nat == 0 = dests
    | otherwise = dests ++ buscaCamiK (head dests) (nat-1) (obtePosiblesDest vI edges) edges ++ buscaCamiK vI nat (tail dests) edges

treuVRepetits:: [Vertex] -> [Vertex]
treuVRepetits [] = []
treuVRepetits (v:vs)
    | vs == [] = [v]
    | elem v vs == True = treuVRepetits vs
    | otherwise = [v] ++ treuVRepetits vs

-------------------------------------------------------------------------------------------------------------
--A PARTIR D'AQUI NO SERVEIX RES
    
-- El populate deixar-ho pel final, de mentre, per veure si funciona, anar escrivint per pantalla 1 a 1
--     mapM (putStrLn) (populate rhoString {-lambdaString sigmaString-})

{-
populate::String -> {-String -> String-} -> PG
populate rhoString {-lambdaString sigmaString-} = 
    let rhowords = map (words) (lines rhoString)
--         lambdawords = map (words) (lines lambdaString)
--         sigmawords = map (words) (lines sigmaString)       
    in
        rhowords
--         [rhowords] ++ [lambdawords] ++ [sigmawords]
-} 
--Veure Propietats

--getProp (populate rhoString lambdaString sigmaString propString) ++ "\n" ++

-- getProp:: PG -> String
-- getProp (PG _ _ _ _ []) = ""
-- getProp (PG v e et s p) = stringProp (head p) ++  getProp(PG v e et s (tail p))
-- 
-- --Propietats sense valors
-- stringProp:: Prop -> String
-- stringProp (Prop prop (VI _)) = prop ++ " " ++ "Int" ++ "\n"
-- stringProp (Prop prop (VS _)) = prop ++ " " ++ "String" ++ "\n"
-- stringProp (Prop prop (VDate _)) = prop ++ " " ++ "Date" ++ "\n"
-- stringProp (Prop prop (VD _)) = prop ++ " " ++ "Double" ++ "\n"


-- getVertexs (populate rhoString lambdaString sigmaString propString) ++ "\n"
--   putStrLn $ getEdges (populate rhoString lambdaString sigmaString propString) ++ "\n"
--   putStrLn $ getProp (populate rhoString lambdaString sigmaString propString) ++ "\n"
--   putStrLn $ getLabels (populate rhoString lambdaString sigmaString propString) ++ "\n"
--   putStrLn $ 

--ver grafo
-- getVertexs::PG-> String
-- getVertexs (PG [] _ _ _ _) = ""
-- getVertexs (PG vertex edges etiquetes sigma prop) = head vertex ++ " " ++ getVertexs (PG (tail vertex) edges etiquetes sigma prop)
-- 
-- getEdges:: PG -> String
-- getEdges (PG _ [] _ _ _) = ""
-- getEdges (PG v e et sigma p) = stringEdge (head e) ++ getEdges (PG v (tail e) et sigma p)
-- 
-- stringEdge:: Edge -> String
-- stringEdge (Edge nome ni nf) = nome ++ " " ++ ni ++ " " ++ nf ++ "\n"
-- 
-- getProp:: PG -> String
-- getProp (PG _ _ _ _ []) = ""
-- getProp (PG v e et s p) = stringProp (head p) ++  getProp(PG v e et s (tail p))

--Propietats sense valors
-- stringProp:: Prop -> String
-- stringProp (Prop prop (VI _)) = prop ++ " " ++ "Int" ++ "\n"
-- stringProp (Prop prop (VS _)) = prop ++ " " ++ "String" ++ "\n"
-- stringProp (Prop prop (VDate _)) = prop ++ " " ++ "Date" ++ "\n"
-- stringProp (Prop prop (VD _)) = prop ++ " " ++ "Double" ++ "\n"

--Propietats de les SIgmes amb valors

-- getLabels:: PG -> String
-- getLabels (PG _ _ [] _ _) = ""
-- getLabels (PG v e labels s p) = getLString (head labels) ++ getLabels (PG v e (tail labels) s p)
-- 
-- getLString::Label -> String
-- getLString (Right etiq) = stringEt etiq
-- getLString (Left errors) = errors
-- 
-- stringEt::Etiqueta -> String
-- stringEt et@(Etiqueta nomN nomEt) = nomN ++ " " ++ nomEt ++ "\n"
--  
-- getSigmes::PG -> String
-- getSigmes (PG _ _ _ [] _) = ""
-- getSigmes (PG v e et s p) = stringS (head s) ++ getSigmes(PG v e et (tail s) p)
-- 
-- stringS::Sigma -> String
-- stringS (Sigma nomN prop) = nomN ++ " " ++ (stringProp2 prop)

