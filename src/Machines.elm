module Machines exposing (..)

import Browser
import Array exposing (Array)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import Html exposing (Html)
import Test
import Graph exposing (Graph)


type Type
  = TyAbs Type Type
  | TyConst String (List Type)
  | TyVar Int

-- all-quantified variables, followed by the type
type alias TypeScheme = (List Int, Type)


-- for every machine variable, map to the corresponding type
type alias VarMap = Array Type
type alias ConstMap = Dict String TypeScheme
-- for every type variable, map to corresponding type
type alias Substitution = IntDict Type
type FreshGen = FreshGen Int

emptySubstitution = IntDict.empty

instantiateTypeScheme : TypeScheme -> FreshGen -> (Type, FreshGen)
instantiateTypeScheme (quants, typ) fg =
  Debug.todo ""

freshTyVar : FreshGen -> (Type, FreshGen)
freshTyVar (FreshGen i) =
  (TyVar i, FreshGen (i+1))

substitute : Substitution -> VarMap -> VarMap
substitute = Debug.todo ""

type TypecheckError
  = Error

algW : ConstMap -> VarMap -> FreshGen -> Machine -> Result TypecheckError (Substitution, FreshGen, Type)
algW cm vm fg machine =
  case machine of
    Var v ->
      Array.get v vm
        |> Result.fromMaybe Error
        |> Result.map (\ typ -> (emptySubstitution, fg, typ))

    Reference r ->
      Dict.get r cm
        |> Result.fromMaybe Error
        |> Result.map (\ scheme ->
          let (typ, fg_) = instantiateTypeScheme scheme fg
          in (emptySubstitution, fg_, typ))

{-
    App args machine ->
      let (sm, fgm, tm) = algW cm vm fg machine
          args_ = List.Extra.mapAccuml (\ (fga, sa) arg -> algW cm (substitute sm vm) fgm 
          (sa, fga, ta) = 
-}

    _ -> Debug.todo ""



type Arity = Arity Int

type Value
  = IntValue Int

valueEquals : Value -> Value -> Bool
valueEquals x y =
  case (x, y) of
    (IntValue ix, IntValue iy) -> ix == iy

stringFromValue v =
  case v of
    IntValue i -> String.fromInt i

intfun2 f =
  ( Arity 2
  , \ args ->
      case args of
        [IntValue x, IntValue y] -> Just (IntValue (f x y))
        _ -> Nothing
  )

builtinFunctions =
  List.foldl (\ (name, fun) -> Dict.insert name fun) Dict.empty
  [ ("add", intfun2 (+))
  , ("mul", intfun2 (*))
  ]

type Machine
  = Const Value
  | App (List Machine) Machine
  | Abs Arity Machine
  | Reference String
  | Ghost (Maybe Arity) Machine
  | Var Int
  | Case
  | Constr String

machineEquals : Machine -> Machine -> Bool
machineEquals m1 m2 =
  case (m1, m2) of
    (Const v1, Const v2) -> valueEquals v1 v2
    (App a1 (Constr c1), App a2 (Constr c2)) ->
      c1 == c2 && List.all identity (List.map2 machineEquals a1 a2)
    _ -> Debug.todo "machineEquals"

machineReferences : Machine -> List String
machineReferences machine =
  case machine of
    App args m -> List.concatMap machineReferences args ++ machineReferences m
    Abs _ m -> machineReferences m
    Reference s -> [s]
    Ghost _ m -> machineReferences m
    _ -> []

graphFromDependencies : List (comparable, List comparable) -> Graph comparable ()
graphFromDependencies l =
  let
    nodes = List.map Tuple.first l
    dict  = List.indexedMap (\ i x -> (x, i)) nodes |> Dict.fromList
    edges = List.concatMap
      (\ (node, refs) ->
        List.filterMap
          (\ ref ->
            let n = Dict.get node dict
                r = Dict.get ref  dict
            in Maybe.map2 Tuple.pair n r
          ) refs
      ) l
  in Graph.fromNodeLabelsAndEdgePairs nodes edges

machineGraph : Dict String Machine -> List (String, List String)
machineGraph machines =
  Dict.foldl (\ k v acc -> (k, machineReferences v) :: acc) [] machines

graphSCCs : Graph n e -> List (List n)
graphSCCs graph =
  case Graph.stronglyConnectedComponents graph of
    Ok acyclic ->
      [Graph.nodes graph |> List.map .label]
    Err components ->
      List.map (Graph.nodes >> List.map .label) components

machineSCCs : Dict String Machine -> List (List String)
machineSCCs machines =
  machineGraph machines
  |> graphFromDependencies
  |> graphSCCs



getConst machine =
  case machine of
    Const v -> Just v
    _ -> Nothing

stringFromMachine : Machine -> Maybe String
stringFromMachine m =
  case m of
    Const v -> Just (stringFromValue v)
    App args (Constr c) ->
      let
        stringArgs =
          List.map (stringFromMachine >> Maybe.withDefault "<unhandled>") args
      in Just (c ++ "(" ++ String.join ", " stringArgs ++ ")")
    Constr c -> Just c
    _ -> Debug.todo "stringFromMachine"


allJust : List (Maybe a) -> Maybe (List a)
allJust = List.foldr (Maybe.map2 (::)) (Just [])


substituteMachine : Machine -> Machine -> Machine
substituteMachine arg machine =
  case machine of
    Var v ->
      if v == 0
      then arg
      else Var (v - 1)
    App args m ->
      substituteMachine arg m |> App (List.map (substituteMachine arg) args)
    Ghost maybeArity m -> substituteMachine arg m
    Const v -> Const v
    Reference s -> Reference s
    Abs arity m -> Abs arity m
    Case -> Case
    Constr c -> Constr c

type alias Context =
  { machines : Dict String Machine
  }

idMachine =
  Abs (Arity 1) (Var 0)

notMachine =
  Abs (Arity 1)
    <| App [Var 0, Constr "false", Constr "true"]
      <| Case

andMachine =
  Abs (Arity 2)
    <| App [Var 0, App [Var 1, true, false] Case, false]
      <| Case

yCombinator =
  Abs (Arity 1)
    <| App
         [App [Var 0] (Reference "Y")]
         (Ghost Nothing (Var 0))

defaultCtx =
  { machines = defaultMachines
  }

defaultMachines =
  List.foldl (\ (name, machine) -> Dict.insert name machine) Dict.empty
  [ ("not", notMachine)
  , ("and", andMachine)
  , ("id", idMachine)
  , ("Y", yCombinator)
  ]


evalBuiltin : String -> List Value -> Maybe Value
evalBuiltin name args =
  Dict.get name builtinFunctions
    |> Maybe.andThen
      (\ (Arity arity, fun) ->
        if List.length args == arity
        then fun args
        else Nothing
      )

machineOfBool : Bool -> Machine
machineOfBool b =
  if b then Constr "true" else Constr "false"

evalMachine : Context -> Machine -> Maybe Machine
evalMachine ctx machine =
  case machine of
    Const v -> Just (Const v)
    App [m1, m2] (Reference "=") ->
      let
        n1 = evaluateNF ctx m1
        n2 = evaluateNF ctx m2
      in
        Maybe.map2 (\ x y -> machineEquals x y |> machineOfBool) n1 n2
    App args (Reference n) ->
      case Dict.get n ctx.machines of
        Just m -> evalMachine ctx (App args m)
        Nothing ->
          args
            |> List.map (evalMachine ctx >> Maybe.andThen getConst)
            |> allJust
            |> Maybe.andThen (evalBuiltin n)
            |> Maybe.map Const
    App args (Ghost maybeArity m) ->
      evalMachine ctx (App args m)
    App argsNewer (App argsOlder m) ->
      evalMachine ctx (App (argsOlder ++ argsNewer) m)
    App (arg :: args) (Abs (Arity arity) m) ->
      if arity == 0 then
        evalMachine ctx (App (arg :: args) m)
      else
        substituteMachine arg m |> Abs (Arity (arity - 1)) |> App args |> evalMachine ctx
    App [] m -> evalMachine ctx m
    Abs (Arity 0) m -> evalMachine ctx m
    App args Case ->
      case args of
        [bool, trueCase, falseCase] ->
          evalMachine ctx bool |> Maybe.andThen (\ v -> case v of
            Constr "true" -> evalMachine ctx trueCase
            Constr "false" -> evalMachine ctx falseCase
            _ -> Nothing
            )
        _ -> Debug.todo ("case: " ++ Debug.toString args)
    Case -> Nothing
    Constr c -> Just (Constr c)
    App args (Constr c) -> Just (App args (Constr c))

    _ -> Debug.todo "evalM"

      {-
      case evalBuiltin n of
        Nothing -> Nothing
        Just (Arity arity, fun) ->
          case allJust args of
            Nothing -> Nothing
            Just args_ ->
              if List.length args_ == arity
              then
                case allJust (List.map evalMachine args_) of
                  Nothing -> Nothing
                  Just argsE -> fun argsE
              else Nothing
      -}

zero  = Const (IntValue 0)
one   = Const (IntValue 1)
two   = Const (IntValue 2)
true  = Constr "true"
false = Constr "false"

testMachine1 =
  App
    [
        (App
          [one, one]
          (Reference "add")
        )
    , two
    ]
    (Reference "mul")

testMachine2 =
  App [two]
    <| Ghost Nothing
      <| App [one]
        <| Reference "add"

testMachine3 =
  App [two] idMachine

testMachine4 =
  App [true, false] (Reference "and")

testMachine5 =
  App [testMachine4, testMachine1, testMachine2] Case

testMachine6 = App [one, (Constr "nil")] (Constr "cons")

testMachine7 = App [testMachine4] (Reference "not")

alwaysTrueMachineY =
  Abs (Arity 2)
    <| App
      [ (Var 1)
      , (Constr "true")
      , (App [(Constr "true")] (Var 0))
      ]
      Case

testMachine8 =
  App [alwaysTrueMachineY, Constr "false"] (Reference "Y")


  

type alias Model = ()

type alias Msg = ()

init = ()

evaluateNF ctx machine =
  case evalMachine ctx machine of
    Just (App args (Constr c)) ->
      args
        |> List.map (evaluateNF ctx)
        |> allJust
        |> Maybe.map (\ evalArgs -> App evalArgs (Constr c))
    m -> m

view model =
  testMachine8
    |> evaluateNF defaultCtx
    |> Maybe.andThen stringFromMachine
    |> Maybe.withDefault "<error>"
    |> Html.text

update msg model = model

main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
