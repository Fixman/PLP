import Routes
import Test.HUnit
import Data.List (sort)
import Data.Maybe (fromJust, isNothing	)

rutasFacultad = many [
  route ""             "ver inicio",
  route "ayuda"        "ver ayuda",
  scope "materia/:nombre/alu/:lu" $ many [
    route "inscribir"   "inscribe alumno",
    route "aprobar"     "aprueba alumno"
  ],
  route "alu/:lu/aprobadas"  "ver materias aprobadas por alumno"
  ]

rutasStringOps = route "concat/:a/:b" (\ctx -> (get "a" ctx) ++ (get "b" ctx))

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
	"patterns" ~: testsPattern,
	"matches" ~: testsMatches,
	"paths" ~: testsPaths,
	"eval" ~: testsEval,
	"evalWrap" ~: testsEvalWrap,
	"evalCtxt"~: testsEvalCtxt,
	"execEntity" ~: testsExecEntity
	]

splitSlash = split '/'

testsPattern = test [
  splitSlash "" ~=? [""],
	splitSlash "/" ~=? ["",""],
	splitSlash "/foo" ~=? ["", "foo"],
	pattern "" ~=? [],
	pattern "/" ~=? [],
    pattern "/base/" ~=? [Literal "base"],
    pattern "/base//:capture/" ~=? [Literal "base",Capture "capture"],
    pattern ":capture" ~=? [Capture "capture"],
	pattern "lit1/:cap1/:cap2/lit2/:cap3" ~=? [Literal "lit1", Capture "cap1", Capture "cap2", Literal "lit2", Capture "cap3"]
	]


testsMatches = test [
	Just (["tpf"],[("nombreMateria","plp")]) ~=? matches (splitSlash "materias/plp/tpf") (pattern "materias/:nombreMateria"),
    Just (["lit2"],[("cap1","materias"),("cap2","plp")]) ~=? matches (splitSlash "materias/plp/lit/lit2") (pattern "/:cap1/:cap2/lit/"),
    Just (["lit3"],[]) ~=? matches (splitSlash "lit/li2/lit3") (pattern "/lit/li2/"),
    Nothing ~=? matches (splitSlash "lit") (pattern "/lit/li2/"),
    Just (["materias","plp","tpf"],[]) ~=? matches (splitSlash "materias/plp/tpf") (pattern "")
	]

path0 = route "foo" 1
path1 = scope "foo" (route "bar" 2)
path2 = scope "foo" (route ":bar" 3)
path3 = scope "" $ scope "" $ many [ scope "" $ route "foo" 1]

testsEvalCtxt = test [
	Just (1, []) ~=? eval path0 "foo",
	Just (2, []) ~=? eval path1 "foo/bar",
	isNothing (eval path1 "foo/bla") ~? "",
	Just (3, [("bar", "bla")]) ~=? eval path2 "foo/bla",
	Just (1, []) ~=? eval path3 "foo"
	]

path4 = many [
  (route "" 1),
  (route "lo/rem" 2),
  (route "ipsum" 3),
  (scope "folder" (many [
    (route "lorem" 4),
    (route "ipsum" 5)
    ]))
  ]

pathEvalCapt = many[ 
    (scope "base"  (many [  
        (scope "libreta" (many [ route ":lu/nombre/:nombre" "por lu"])),
        (scope "dni" (many [ route ":lu/:nombre" "por dni"]))
    ]))]


testsEval = test [
		1 ~=? justEvalP4 "",
		4 ~=? justEvalP4 "folder/lorem",
        Just ("por lu",[("lu","777"),("nombre","juan carlos")]) ~=? eval pathEvalCapt  "/base/libreta/777/nombre/juan carlos" ,
        Just ( "por dni", [ ("lu","333") ,("nombre","juan carlos") ])  ~=? eval pathEvalCapt  "/base/dni/333/juan carlos"  ,
        Nothing  ~=? eval pathEvalCapt  "/base/dni/333/juan carlos/literal"  
	]
	where justEvalP4 s = fst (fromJust (eval path4 s))

path410 = wrap (+10) path4

testsEvalWrap = test [
		14 ~=? justEvalP410 "folder/lorem"
	]
	where justEvalP410 s = fst (fromJust (eval path410 s))


-- ejempo donde el valor de cada ruta es una función que toma context como entrada.
-- para estos se puede usar en lugar además de eval, la función exec para devolver
-- la applicación de la función con sobre el contexto determinado por la ruta
rest entity = many [
  (route entity (const (entity++"#index"))),
  (scope (entity++"/:id") (many [
    (route "" (const (entity++"#show"))),
    (route "create" (\ctx -> entity ++ "#create of " ++ (get "id" ctx))),
    (route "update" (\ctx -> entity ++ "#update of " ++ (get "id" ctx))),
    (route "delete" (\ctx -> entity ++ "#delete of " ++ (get "id" ctx)))
    ]))
  ]

path5 = many [
  (route "" (const "root_url")),
  (rest "post"),
  (rest "category")
  ]

path6 = many [ ( route "/home/index" "Site Home" ),
    (scope ("/home") 
        (many [
            route "/index" "Scope Site Home",
            route "/search" "Scope Site Search"
        ])),
    (many [
        (route "/home/index" "Many Site Home"),
        (route "/home/search" "Many Site Search")
        ])]


testsPaths = test [
 	sort ["","post","post/:id","post/:id/create","post/:id/update","post/:id/delete","category","category/:id","category/:id/create","category/:id/update","category/:id/delete"] ~=?
	 	sort (paths path5),
    sort ["home/index","home/index","home/search","home/index","home/search"] ~=?
        sort (paths path6)
	]


testsExecEntity = test [
	Just "root_url" ~=? exec path5 "",
	Just "post#index" ~=? exec path5 "post",
	Just "post#show" ~=? exec path5 "post/35",
	Just "category#create of 7" ~=? exec path5 "category/7/create"
	]
