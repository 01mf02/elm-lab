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
  | PEq

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

printThunk : State -> Thunk -> String
printThunk state thunk =
  case thunk of
    TValue v -> valueToString state v
    TPending _ _ -> "<<thunk>>"

printThunkId : State -> ThunkId -> String
printThunkId state thunkId =
  Array.get thunkId state.cache
    |> Maybe.map (printThunk state)
    |> Maybe.withDefault "<<thunk not in cache>>"

printPrim : Prim -> String
printPrim prim =
  case prim of
    PConstr constr -> constr
    PCase typ -> "case[" ++ typ ++ "]"
    PEq -> "=="

valueToString : State -> Value -> String
valueToString state value =
  case value of
    VInt n -> String.fromInt n
    VApp prim thunks -> printPrim prim ++ printList (printThunkId state) (Array.toList thunks)
    VAbs _ _ _ -> "<<closure>>"

type alias Env = Dict VarId ThunkId
type alias Cache = Array Thunk

type alias State =
  { cache : Cache
  , types : Dict TypeId (List ConstrId)
  , constructors : Dict ConstrId { arity : Int, offset : Int, typ : TypeId }
  }

defaultTypes =
  [ ( "List" , [ "Nil", "Cons" ] )
  , ( "Nat", [ "Zero", "Succ" ] )
  ]

defaultConstrs =
  [ ( "Nil"  , { arity = 0, offset = 0, typ = "List" } )
  , ( "Cons" , { arity = 2, offset = 1, typ = "List" } )
  , ( "Zero" , { arity = 0, offset = 0, typ = "Nat" } )
  , ( "Succ" , { arity = 1, offset = 1, typ = "Nat" } )
  ]

initState =
  { cache = Array.empty
  , types = Dict.fromList defaultTypes
  , constructors = Dict.fromList defaultConstrs
  }

type Thunk
  = TValue Value
  | TPending Env Expr

updateThunk : ThunkId -> ( Value, State ) -> ( Value, State )
updateThunk thunkId ( v, state ) =
  ( v, { state | cache = Array.set thunkId (TValue v) state.cache } )

force : State -> ThunkId -> Maybe ( Value, State )
force state thunkId =
  Array.get thunkId state.cache
    |> Maybe.andThen
      (\thunk ->
        case thunk of
          TValue v -> Just ( v, state )
          TPending env expr ->
            eval env expr state
              |> Maybe.map (updateThunk thunkId)
      )

forceRec : ( Value, State ) -> Maybe State
forceRec ( value, state ) =
  case value of
    VApp (PConstr constr) args ->
      Array.foldl
        (\ thunkId ->
          Maybe.andThen (\tempState -> force tempState thunkId)
            >> Maybe.andThen forceRec
        )
        (Just state)
        args

    _ -> Just state

envInsert x a env =
  Dict.insert x a env

constrOffset : State -> ConstrId -> Int
constrOffset state constrId =
  Dict.get constrId state.constructors
    |> Maybe.map .offset
    |> Maybe.withDefault (-1)

primArity : State -> Prim -> Int
primArity state prim =
  case prim of
    PConstr constrId ->
      Dict.get constrId state.constructors
        |> Maybe.map .arity
        |> Maybe.withDefault (-1)

    PCase typeId ->
      Dict.get typeId state.types
        |> Maybe.map (List.length >> (+) 1)
        |> Maybe.withDefault -1

    PEq -> 2

valuesEqual : State -> Value -> Value -> Maybe ( Bool, State )
valuesEqual state v1 v2 =
  case ( v1, v2 ) of
    ( VInt i1, VInt i2 ) -> Just ( i1 == i2, state )
    ( VApp (PConstr constrId1) thunks1, VApp (PConstr constrId2) thunks2 ) ->
      if constrId1 == constrId2
      then thunkArraysEqual state thunks1 thunks2
      else
        Maybe.map2
          (\ constr1 constr2 -> constr1.typ == constr2.typ)
          (Dict.get constrId1 state.constructors)
          (Dict.get constrId2 state.constructors)
          |> Maybe.andThen
            (\ typesEqual -> if typesEqual then Just ( False, state ) else Nothing)
    _ -> Nothing

thunkArraysEqual : State -> Array ThunkId -> Array ThunkId -> Maybe ( Bool, State )
thunkArraysEqual state thunks1 thunks2 =
  if Array.length thunks1 == Array.length thunks2
  then
    List.foldl
      (\ ( thunk1, thunk2 ) ->
        Maybe.andThen
          (\ ( equalSoFar, stateSoFar ) ->
            if equalSoFar
            then thunksEqual stateSoFar thunk1 thunk2
            else Just ( False, stateSoFar )
          )
      )
      (Just ( True, state ))
      (List.map2 Tuple.pair (Array.toList thunks1) (Array.toList thunks2))
  else Nothing

thunksEqual : State -> ThunkId -> ThunkId -> Maybe ( Bool, State )
thunksEqual state t1 t2 =
  if t1 == t2
  then Just ( True, state )
  else
    force state t1
      |> Maybe.andThen
        (\ ( v1, state1 ) ->
          force state1 t2
            |> Maybe.andThen
              (\ ( v2, state2 ) ->
                valuesEqual state2 v1 v2
              )
        )

valueOfBool : Bool -> Value
valueOfBool bool =
  case bool of
    True -> VApp (PConstr "True") Array.empty
    False -> VApp (PConstr "False") Array.empty

evalPrim : Prim -> Array ThunkId -> State -> Maybe ( Value, State )
evalPrim prim thunks state =
  case prim of
    PCase typeId ->
      let
        firstValue =
          Array.get 0 thunks
            |> Maybe.andThen (force state)
      in
      case firstValue of
        Just ( VApp (PConstr constrId as constr) constrThunks, state1 ) ->
          if Array.length constrThunks == primArity state1 constr
          then
            Array.get (1 + constrOffset state1 constrId) thunks
              |> Maybe.andThen (force state1)
              |> Maybe.andThen (evalApps constrThunks)
          else
            Nothing

        _ -> Nothing

    PConstr constrId ->
      Just ( VApp (PConstr constrId) thunks, state )

    PEq ->
      Maybe.map2
        (thunksEqual state)
        (Array.get 0 thunks)
        (Array.get 1 thunks)
        |> Maybe.andThen identity
        |> Maybe.map (Tuple.mapFirst valueOfBool)


evalApp : ThunkId -> ( Value, State ) -> Maybe ( Value, State )
evalApp thunkId ( value, state ) =
  case value of
    VAbs closEnv closVar closExpr ->
      eval (envInsert closVar thunkId closEnv) closExpr state

    VApp prim thunks ->
      let thunks1 = Array.push thunkId thunks
      in
      case compare (Array.length thunks1) (primArity state prim) of
        LT -> Just ( VApp prim thunks1, state )
        EQ -> evalPrim prim thunks1 state
        GT -> Nothing

    VInt _ -> Nothing

evalApps : Array ThunkId -> ( Value, State ) -> Maybe ( Value, State )
evalApps thunks valueState =
  Array.foldl
    (\ thunk -> Maybe.andThen (evalApp thunk))
    (Just valueState) thunks


eval : Env -> Expr -> State -> Maybe ( Value, State )
eval env ex state =
  case ex of
    EVar n ->
      Dict.get n env
        |> Maybe.andThen (force state)
  
    EAbs x e -> Just (VAbs env x e, state)
  
    EApp a b ->
      eval env a state
        |> Maybe.andThen
          (\ ( value, state1 ) ->
            let
              thunk = TPending env b
              thunkId = Array.length state1.cache
              state2 = { state1 | cache = Array.push thunk state1.cache }
            in
            evalApp thunkId ( value, state2 )
          )

    EFix e -> eval env (EApp e (EFix e)) state
    EInt n -> Just ( VInt n, state )
    EPrim prim -> Just ( VApp prim Array.empty, state )


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
eEq = PEq |> EPrim

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

eqTest =
  app eEq [ EInt 42, test1 ]

eqTest2 = app eEq [ takeTest, app take [ natOfInt 2, repeat zero ] ]

takeTest =
  app take [ natOfInt 2, repeat zero ]

identityExpr = EAbs "x" (EVar "x")

zero = eConstr "Zero"
succ = eConstr "Succ"
one = EApp succ zero

nil = eConstr "Nil"
cons = eConstr "Cons"

true = eConstr "True"
false = eConstr "False"

iterate f x n =
  if n <= 0
  then x
  else iterate f (f x) (n-1)

natOfInt n =
  iterate (EApp succ) zero n

init =
  ()

view model =
  eval Dict.empty takeTest initState
    |> Maybe.andThen (\ (value, state) -> forceRec (value, state) |> Maybe.map (\state_ -> (value, state_)))
    |> Maybe.map (\ (value, state) -> valueToString state value)
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
