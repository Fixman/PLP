module Routes where

import Data.List
import Data.Maybe

data PathPattern = Literal String | Capture String deriving (Eq, Show)
data Routes f = Route [PathPattern] f | Scope [PathPattern] (Routes f) | Many [Routes f] deriving Show

rutasFacultad = many [ route "" "ver inicio", route "ayuda" "ver ayuda", scope "materia/:nombre/alu/:lu" $ many [ route "inscribir" "inscribe alumno", route "aprobar" "aprueba alumno" ] , route "alu/:lu/aprobadas" "ver materias aprobadas por alumno" ]
rutasStringOps = route "concat/:a/:b" (\ ctx -> (get "a" ctx) ++ (get "b" ctx) )

-- Ejercicio 1: Dado un elemento separador y una lista, se deber a partir la lista en sublistas de acuerdo a la aparicíon del separador (sin incluirlo).

-- Split reduce un separador y un string a una lista de string.
-- Dado un separador y un elemento de un string, la funcion lambda agrega este
-- elemento al principio de la primer lista a menos que el elemento sea el separador.
-- En ese caso, crea una lista nueva (string vacio) al principio de la lista de listas.
split :: Eq a => a -> [a] -> [[a]]
split d = foldr
        (\ c l ->
                if c == d then
                        [] : l
                else
                        (c : (head l)) : (tail l)
        )
        [[]]

-- Ejercicio 2: A partir de una cadena que denota un patrón de URL se deberá construir la secuencia de literales y capturas correspondiente.

-- Separa un string en directorios (haciendo split por '/'), y borra los
-- componentes vacios.
splitDirs :: String -> [String]
splitDirs s = filter (not . null) $ split '/' s

-- parseEntity usa pattern matching para determinar si un argumento es una
-- captura o un literal.
-- pattern usa split para separar un string en una lista de entities y luego
-- parsea cada una de ellas.
parseEntity :: String -> PathPattern
parseEntity (':':xs) = Capture xs
parseEntity x = Literal x

pattern :: String -> [PathPattern]
pattern = map parseEntity . splitDirs

-- Ejercicio 3: Obtiene el valor registrado en una captura determinada. Se puede suponer que la captura está definida en el contexto.
type PathContext = [(String, String)]

-- Busca el segundo elemento de la tupla del primer elemento de la lista (que
-- deberia ser el unico) que tenga como primer elemento de la tupla cierto argumento.
get :: String -> PathContext -> String
get s = snd.head.filter ((==s).fst)

-- Ejercicio 4: Dadas una ruta particionada y un patrón de URL, trata de aplicar el patrón a la ruta y devuelve, en caso de que
--              la ruta sea un prefijo válido para el patrón, el resto de la ruta que no se haya llegado a consumir y el contexto capturado hasta el punto alcanzado.
-- Se puede usar recursión explícita.

captures :: [String] -> [PathPattern] -> [(String, String)]
captures h [] = []
captures (h:hs) (Literal l:ls) = captures hs ls
captures (h:hs) (Capture l:ls) = (l, h) : captures hs ls

literals :: [String] -> [PathPattern] -> Maybe([String])
literals h [] = Just h
literals [] l = Nothing
literals (h:hs) (Capture l:ls) = literals hs ls
literals (h:hs) (Literal l:ls)
        | h == l = literals hs ls
        | otherwise = Nothing

matches :: [String] -> [PathPattern] -> Maybe ([String], PathContext)
matches hs ls = literals hs ls >>= Just . flip (,) (captures hs ls)

-- DSL para rutas
route :: String -> a -> Routes a
route s f = Route (pattern s) f

scope :: String -> Routes a -> Routes a
scope s r = Scope (pattern s) r

many :: [Routes a] -> Routes a
many l = Many l

-- Ejercicio 5: Definir el fold para el tipo Routes f y su tipo. Se puede usar recursión explícita.

-- foldrRoutes reduce un elemento (Routes f) a cierto tipo s.
-- Para esto toma tres funciones: una que le aplica a los nodos de tipo Route, uno a Scope, y
-- uno a Many.
-- La implementacion es similar a la de foldr.
foldrRoutes :: ([PathPattern] -> f -> s) -> ([PathPattern] -> s -> s) -> ([s] -> s) -> Routes f -> s
foldrRoutes fr fs fm (Route pp ff) = fr pp ff 
foldrRoutes fr fs fm (Scope pp r) = fs pp (foldrRoutes fr fs fm r)
foldrRoutes fr fs fm (Many r) = fm (map (foldrRoutes fr fs fm) r)

-- Auxiliar para mostrar patrones. Es la inversa de pattern.
patternShow :: [PathPattern] -> String
patternShow ps = concat $ intersperse "/" ((map (\p -> case p of
  Literal s -> s
  Capture s -> (':':s)
  )) ps)

-- Ejercicio 6: Genera todos los posibles paths para una ruta definida.

-- Funcion auxiliar que, dado un Routes, devuelve una lista de tuplas con las
-- direcciones y descripciones de todos sus elementos.
sites :: Routes f -> [([PathPattern], f)]
sites = foldrRoutes (\ x y -> [(x, y)] ) (\ x y -> map (\ z -> (x ++ fst z, snd z) ) y ) concat

-- Saca el primer elemento de sites (que contiene la direccion, en forma
-- de [PathPattern]), y le aplica patternShow.
paths :: Routes a -> [String]
paths f = map (patternShow . fst) $ sites f

-- Ejercicio 7: Evalúa un path con una definición de ruta y, en caso de haber coincidencia, obtiene el handler correspondiente 
--              y el contexto de capturas obtenido.

-- Funcion auxiliar (que no puedo creer que no exista en Data.Maybe pero no
-- encontre nada parecido): dado un elemento y una funcion booleana, devuelve
-- Nothing o Just elemento dependiendo del resultado.
filterSingle :: (a -> Bool) -> a -> Maybe a
filterSingle f a
        | f a = Just a
        | otherwise = Nothing

-- Esta funcion aprovecha funciones ya existentes para simplificar el problema.
-- Primero, busca todos los pares (direccion, descripcion) usando sites.
-- Luego, a cada elemento le mapea una funcion lambda que, usando monadas,
-- devuelve una tupla con el segundo elemento del par de sites (descripcion) y el
-- segundo elemento que devuelve matches (capturas) cuando se le aplica el
-- primer elemento de sites (direccion), siempre y cuando matches no devuelva
-- Nothing, en cuyo caso tambien devuelve Nothing.
-- La composicion listToMaybe.mapMaybe devuelve el primer elemento que no sea
-- Nothing de aplicarle una funcion a una lista, si existe, y Nothing sino.
eval :: Routes a -> String -> Maybe (a, PathContext)
eval f s = listToMaybe . mapMaybe
        (\ x ->
                matches (splitDirs s) (fst x) >>=
                filterSingle (null . fst) >>=
                Just . ((,) $ snd x) . snd
        )
        $ sites f

-- Ejercicio 8: Similar a eval, pero aquí se espera que el handler sea una función que recibe como entrada el contexto 
--              con las capturas, por lo que se devolverá el resultado de su aplicación, en caso de haber coincidencia.

-- Simplemente ejecuta eval, y si no devuelve Nothing entonces le aplica el
-- primer elemento del resultado al segundo.
-- Hacerlo con uncurry ($) es rock.
exec :: Routes (PathContext -> a) -> String -> Maybe a
exec routes path = eval routes path >>= Just . uncurry ($)

-- Ejercicio 9: Permite aplicar una funci ́on sobre el handler de una ruta. Esto, por ejemplo, podría permitir la ejecución 
--              concatenada de dos o más handlers.


-- Hace un foldr de un Routes a otro Routes con otro tipo, donde todos los
-- Scopes y Manys quedan igual, pero a los Route se les aplica f.
wrap :: (a -> b) -> Routes a -> Routes b
wrap f = foldrRoutes
        (\ x y -> Route x (f y) )
        Scope
        Many

-- Ejercicio 10: Genera un Routes que captura todas las rutas, de cualquier longitud. A todos los patrones devuelven el mismo valor. 
--               Las capturas usadas en los patrones se deberán llamar p0, p1, etc. 
--               En este punto se permite recursión explícita.
catch_all :: a -> Routes a
catch_all h = undefined
