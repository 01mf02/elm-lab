-- Based on: <http://dev.stephendiehl.com/fun/005_evaluation.html>

import Browser
import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)

type alias VarId = String
type alias ConstrId = String
type alias TypeId = String

type Expr
  = EAbs VarId Expr
  | EApp Expr Expr
  | EVar VarId
  | EPrim Prim
  | EFix Expr
  | EInt Int

type alias ThunkId = Int

type Prim
  = PConstr ConstrId
  | PCase TypeId

{-
Reading material:

- <http://www.scs.stanford.edu/16wi-cs240h/slides/ghc-compiler.html>
- <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects>
- <https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf>
-}
type Value
  = VAbs Env VarId Expr
  | VApp Prim (Array ThunkId)
  | VInt Int

printList f l =
  if List.isEmpty l
  then ""
  else "(" ++ String.join ", " (List.map f l) ++ ")"

printThunk : Cache -> Thunk -> String
printThunk cache thunk =
  case thunk of
    TValue v -> valueToString cache v
    TPending _ _ -> "<<thunk>>"

printThunkId : Cache -> ThunkId -> String
printThunkId cache thunkId =
  Array.get thunkId cache
    |> Maybe.map (printThunk cache)
    |> Maybe.withDefault "<<thunk not in cache>>"

printPrim : Prim -> String
printPrim prim =
  case prim of
    PConstr constr -> constr
    PCase typ -> "case[" ++ typ ++ "]"

valueToString : Cache -> Value -> String
valueToString cache value =
  case value of
    VInt n -> String.fromInt n
    VApp prim thunks -> printPrim prim ++ printList (printThunkId cache) (Array.toList thunks)
    VAbs _ _ _ -> "<<closure>>"

type alias Env = Dict VarId ThunkId
type alias Cache = Array Thunk

type alias State =
  { cache : Cache
  , types : Dict TypeId (List ConstrId)
  , constructors : Dict ConstrId (Int, TypeId)
  }

type Thunk
  = TValue Value
  | TPending Env Expr

updateThunk : ThunkId -> ( Value, Cache ) -> ( Value, Cache )
updateThunk ref ( v, cache ) =
  ( v, Array.set ref (TValue v) cache )

force : Cache -> ThunkId -> Maybe ( Value, Cache )
force cache ref =
  Array.get ref cache
    |> Maybe.andThen
      (\thunk ->
        case thunk of
          TValue v -> Just ( v, cache )
          TPending env expr ->
            eval env expr cache
              |> Maybe.map (updateThunk ref)
      )

forceRec : ( Value, Cache ) -> Maybe Cache
forceRec ( value, cache ) =
  case value of
    VApp (PConstr constr) args ->
      Array.foldl
        (\ thunkId ->
          Maybe.andThen (\tempCache -> force tempCache thunkId)
            >> Maybe.andThen forceRec
        )
        (Just cache)
        args

    _ -> Just cache

envInsert x a env =
  Dict.insert x a env

constrOffset constrId =
  case constrId of
    "Nil" -> 0
    "Cons" -> 1
    "Zero" -> 0
    "Succ" -> 1
    _ -> -1

primArity prim =
  case prim of
    PConstr constrId ->
      case constrId of
        "Nil" -> 0
        "Cons" -> 2
        "Zero" -> 0
        "Succ" -> 1
        _ -> -1

    PCase typeId ->
      let typeArity = 2
      in
      1 + typeArity

evalPrim : Prim -> Array ThunkId -> Cache -> Maybe ( Value, Cache )
evalPrim prim thunks cache =
  case prim of
    PCase typeId ->
      let
        firstValue =
          Array.get 0 thunks
            |> Maybe.andThen (force cache)
      in
      case firstValue of
        Just ( VApp (PConstr constrId as constr) constrThunks, cache1 ) ->
          if Array.length constrThunks == primArity constr
          then
            Array.get (1 + constrOffset constrId) thunks
              |> Maybe.andThen (force cache1)
              |> Maybe.andThen (evalApps constrThunks)
          else
            Nothing

        _ -> Nothing

    PConstr constrId ->
      Just ( VApp (PConstr constrId) thunks, cache )


evalApp : ThunkId -> ( Value, Cache ) -> Maybe ( Value, Cache )
evalApp thunkId ( value, cache ) =
  case value of
    VAbs closEnv closVar closExpr ->
      eval (envInsert closVar thunkId closEnv) closExpr cache

    VApp prim thunks ->
      let thunks1 = Array.push thunkId thunks
      in
      case compare (Array.length thunks1) (primArity prim) of
        LT -> Just ( VApp prim thunks1, cache )
        EQ -> evalPrim prim thunks1 cache
        GT -> Nothing

    VInt _ -> Nothing

evalApps : Array ThunkId -> ( Value, Cache ) -> Maybe ( Value, Cache )
evalApps thunks valueCache =
  Array.foldl
    (\ thunk -> Maybe.andThen (evalApp thunk))
    (Just valueCache) thunks


eval : Env -> Expr -> Cache -> Maybe ( Value, Cache )
eval env ex cache =
  case ex of
    EVar n ->
      Dict.get n env
        |> Maybe.andThen (force cache)
  
    EAbs x e -> Just (VAbs env x e, cache)
  
    EApp a b ->
      eval env a cache
        |> Maybe.andThen
          (\ ( value, cache1 ) ->
            let
              thunk = TPending env b
              thunkId = Array.length cache1
              cache2 = Array.push thunk cache1
            in
            evalApp thunkId ( value, cache2 )
          )

    EFix e -> eval env (EApp e (EFix e)) cache
    EInt n -> Just ( VInt n, cache )
    EPrim prim -> Just ( VApp prim Array.empty, cache )


const = EAbs "x" (EAbs "y" (EVar "x"))

diverge = EFix (EAbs "x" (EApp (EVar "x") (EVar "x")))

-- omega = (\x -> x x) (\x -> x x)
omega = EApp (EAbs "x" (EApp (EVar "x") (EVar "x")))
             (EAbs "x" (EApp (EVar "x") (EVar "x")))

-- test1 = (\y -> 42) omega
test1 = EApp (EAbs "y" (EInt 42)) omega

test2 = EApp (EApp const (EInt 42)) omega

repeat x = EFix (EAbs "zeroes" (EApp (EApp cons x) (EVar "zeroes")))

abs vars term =
  List.foldr EAbs term vars

app term args =
  List.foldl (\ arg acc -> EApp acc arg) term args

eCase = PCase >> EPrim
eConstr = PConstr >> EPrim

take =
  EFix <| EAbs "take" <|
    abs ["n", "l"] <|
      app (eCase "Nat")
        [ EVar "n"
        , nil -- Zero case
        , EAbs "n'" <|  -- Succ(n') case
            app (eCase "List")
            [ EVar "l"
            , nil -- Nil case
            , abs ["x", "xs"] <| -- Cons(x, xs) case
                app cons [EVar "x", app (EVar "take") [EVar "n'", EVar "xs"]]
            ]
        ]

identityExpr = EAbs "x" (EVar "x")

zero = eConstr "Zero"
succ = eConstr "Succ"
one = EApp succ zero

nil = eConstr "Nil"
cons = eConstr "Cons"

iterate f x n =
  if n <= 0
  then x
  else iterate f (f x) (n-1)

natOfInt n =
  iterate (EApp succ) zero n

init =
  ()

view model =
  eval Dict.empty (app take [ natOfInt 2, repeat zero ]) Array.empty
    |> Maybe.andThen (\ (value, cache) -> forceRec (value, cache) |> Maybe.map (\cache_ -> (value, cache_)))
    |> Maybe.map (\ (value, cache) -> valueToString cache value)
    |> Maybe.withDefault "<<nothing>>"
    |> Html.text

update msg model =
  model

main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
