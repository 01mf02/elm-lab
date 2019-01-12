module Machines exposing (..)

import Browser
import Array exposing (Array)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import Html exposing (Html)
import Test
import Graph exposing (Graph)
import List.Extra as ListE
import Result.Extra as ResultE


type Type
  = TyAbs Type Type
  | TyConst String (List Type)
  | TyVar Int

-- all-quantified variables, followed by the type
type alias TypeConstructor = (String, List Type)
type alias TypeName = String
type alias ConstructorName = String
type alias MachineName = String
type alias VariableIndex = Int

-- for every type, save number of polymorphic type variables and its constructors
type alias TypeDict = Dict TypeName (Arity, List TypeConstructor)
type alias ConstructorDict = Dict ConstructorName Type

boolType =
  ("bool", (Arity 0, [( "true", []),
                      ("false", [])]))
listType =
  ("list", (Arity 1, [( "nil", []),
                      ("cons", [TyVar 0, TyConst "list" [TyVar 0]])]))

typeOfCase (Arity arity) constructors =
  List.foldr TyAbs (TyVar arity) (List.map (\ (name, args) -> List.foldr TyAbs (TyVar arity) args) constructors)

defaultTypes =
  [ boolType
  , listType
  ]

typeOfNameAndArity name (Arity arity) =
  TyConst name (List.range 0 (arity-1) |> List.map TyVar)

typeConstructors : (TypeName, (Arity, List TypeConstructor)) -> List (String, Type)
typeConstructors (typeName, (arity, constructors)) =
  List.map (\ (name, args) -> (name, List.foldr TyAbs (typeOfNameAndArity typeName arity) args)) constructors

typeToString : Type -> String
typeToString typ =
  case typ of
    TyAbs t1 t2 -> "(" ++ typeToString t1 ++ " → " ++ typeToString t2 ++ ")"
    TyConst s args -> String.join " " (s :: List.map typeToString args)
    TyVar v -> "t" ++ String.fromInt v

typeArity typ =
  case typ of
    TyAbs _ t -> 1 + typeArity t
    _ -> 0


-- for every machine variable, map to the corresponding type
type alias VarMap = Array Type
type alias ConstMap = Dict String Type
-- for every type variable, map to corresponding type
type alias Substitution = IntDict Type

type FreshGen = FreshGen Int

initFreshGen = FreshGen 0

emptySubstitution = IntDict.empty
emptyVarMap = Array.empty
emptyConstMap = Dict.empty

freshTyVar : FreshGen -> (Type, FreshGen)
freshTyVar (FreshGen i) =
  (TyVar i, FreshGen (i+1))

maxTyVar : Type -> VariableIndex
maxTyVar typ =
  case typ of
    TyAbs t1 t2 -> max (maxTyVar t1) (maxTyVar t2)
    TyConst s args -> List.map maxTyVar args |> List.maximum |> Maybe.withDefault (-1)
    TyVar v -> v

offsetTyVars : Int -> Type -> Type
offsetTyVars off typ =
  case typ of
    TyAbs t1 t2 -> TyAbs (offsetTyVars off t1) (offsetTyVars off t2)
    TyConst s args -> TyConst s (List.map (offsetTyVars off) args)
    TyVar v -> TyVar (v + off)

typeVars : Type -> List VariableIndex
typeVars typ =
  case typ of
    TyAbs t1 t2 -> typeVars t1 ++ typeVars t2
    TyConst s args -> List.concatMap typeVars args
    TyVar v -> [v]

refreshType : Type -> FreshGen -> (Type, FreshGen)
refreshType typ (FreshGen i) =
  (offsetTyVars i typ, FreshGen (i + maxTyVar typ + 1))

typeContainsTyVar : VariableIndex -> Type -> Bool
typeContainsTyVar var typ =
  case typ of
    TyVar v -> v == var
    TyConst c args -> List.any (typeContainsTyVar var) args
    TyAbs t1 t2 -> typeContainsTyVar var t1 || typeContainsTyVar var t2



freshTyVars : FreshGen -> Int -> (List Type, FreshGen)
freshTyVars (FreshGen i) n =
  let i_ = i + n
      tyvars = List.range i (i_ - 1) |> List.map TyVar
  in (tyvars, FreshGen i_)

freshTyVarsList : FreshGen -> List a -> (FreshGen, List (a, Type))
freshTyVarsList =
  ListE.mapAccuml
    (\ fg x ->
      let (typ, fg_) = freshTyVar fg
      in (fg_, (x, typ))
    )



substituteVarMap : Substitution -> VarMap -> VarMap
substituteVarMap subst vm =
  Array.map (substituteType subst) vm

mapTyVars : (VariableIndex -> Type) -> Type -> Type
mapTyVars f typ =
  case typ of
    TyAbs t1 t2 -> TyAbs (mapTyVars f t1) (mapTyVars f t2)
    TyConst s args -> TyConst s (List.map (mapTyVars f) args)
    TyVar v -> f v

substituteType : Substitution -> Type -> Type
substituteType subst typ =
  let
    substitute v =
      case IntDict.get v subst of
        Nothing -> TyVar v
        Just typ_ -> substituteType subst typ_
  in mapTyVars substitute typ

-- TODO: handle case when substitutions clash on some variable?
composeSubstitutions : Substitution -> Substitution -> Substitution
composeSubstitutions s1 s2 =
  IntDict.map (\ k v -> substituteType s1 v) s2
    |> IntDict.union s1



type UnifyError
  = UnifyError
  | ConstrArgsCountsDiffer
  | ConstructorsDiffer
  | OccursCheckFailed

-- precondition: substitution must be non-circular!
-- ({x -> x} is not allowed, neither is {x -> list x})
unifyTypes : Type -> Type -> Substitution -> Result UnifyError Substitution
unifyTypes t1 t2 subst =
  case (substituteType subst t1, substituteType subst t2) of
    (TyVar v as t1s, t2s) ->
      if t1s == t2s
      then Ok subst
      else
        if typeContainsTyVar v t2s
        then Err OccursCheckFailed
        else Ok (IntDict.insert v t2 subst)
    (_, TyVar _) -> unifyTypes t2 t1 subst
    (TyAbs t11 t12, TyAbs t21 t22) ->
      subst |> unifyTypes t11 t21 |> Result.andThen (unifyTypes t12 t22)
    (TyConst c1 args1, TyConst c2 args2) ->
      if c1 == c2
      then
        if List.length args1 == List.length args2
        then
          List.map2 Tuple.pair args1 args2
            |> List.foldl
              (\ (arg1, arg2) -> Result.andThen (unifyTypes arg1 arg2))
              (Ok subst)
        else
          Err ConstrArgsCountsDiffer
      else
        Err ConstructorsDiffer
    (TyAbs _ _, TyConst _ _) -> Err UnifyError
    (TyConst _ _, TyAbs _ _) -> Err UnifyError

type TypecheckError
  = Error
  | UnboundVariable
  | UnknownReference String
  | TypecheckUnifyError UnifyError
  | CaseTypeUnknown
  | ConstructorUnknown

{-
mapAccumlResult : (a -> b -> Result e ( a, c )) -> a -> List b -> Result e ( a, List c )
mapAccumlResult f acc0 list0 =
  let
    aux acc listAcc listIn =
      case listIn of
        [] -> Ok (acc, List.reverse listAcc)
        x::xs ->
          case f acc x of
            Ok (acc_, y) -> aux acc_ (y::listAcc) xs
            Err e -> Err e

  in aux acc0 [] list0

mapAccumrResult : (a -> b -> Result e ( a, c )) -> a -> List b -> Result e ( a, List c )
mapAccumrResult f acc0 =
  List.foldr
    (\ x ->
      Result.andThen
        (\ (acc, l) ->
          f acc x
            |> Result.map (\ (acc_, y) -> (acc_, y::l))
        )
    ) (Ok (acc0, []))
-}

type alias TypecheckContext =
  { constMap : ConstMap
  , definedMap : Dict String Type
  , types : TypeDict
  , constructors : ConstructorDict
  }

defaultTypecheckContext : TypecheckContext
defaultTypecheckContext =
  { constMap = Dict.map (\ _ (typ, f) -> typ) builtinFunctions
  , definedMap = Dict.empty
  , types = Dict.fromList defaultTypes
  , constructors = Dict.fromList <| List.concatMap typeConstructors defaultTypes
  }


algWApp ctx vm m2 ((subst1, fg1), typ1) =
  algW ctx (substituteVarMap subst1 vm) fg1 m2
    |> Result.andThen
      (\ ((subst2, fg2), typ2) ->
        let (beta, fgbeta) = freshTyVar fg2
        in
          unifyTypes (substituteType subst2 typ1) (TyAbs typ2 beta) emptySubstitution
            |> Result.mapError TypecheckUnifyError
            |> Result.map
              (\ v ->
                let
                  subst = composeSubstitutions v (composeSubstitutions subst2 subst1)
                  typ = substituteType v beta
                in ((subst, fgbeta), typ)
              )
      )

algW : TypecheckContext -> VarMap -> FreshGen -> Machine -> Result TypecheckError ((Substitution, FreshGen), Type)
algW ctx vm fg machine =
  case machine of
    Const v -> Ok ((emptySubstitution, fg), valueType v)

    Var v ->
      Array.get v vm
        |> Result.fromMaybe UnboundVariable
        |> Result.map (\ typ -> ((emptySubstitution, fg), typ))

    Reference "=" ->
      let
        equalityType = TyAbs (TyVar 0) (TyAbs (TyVar 0) boolTy)
        (typ, fg_) = refreshType equalityType fg
      in Ok ((emptySubstitution, fg_), typ)

    Reference r ->
      case Dict.get r ctx.constMap of
        Just typ ->
          let (typ_, fg_) = refreshType typ fg
          in Ok ((emptySubstitution, fg_), typ_)
        Nothing ->
          Dict.get r ctx.definedMap
            |> Result.fromMaybe (UnknownReference r)
            |> Result.map (\ typ -> ((emptySubstitution, fg), typ))

    App args m ->
      List.foldl (\ arg -> Result.andThen (algWApp ctx vm arg)) (algW ctx vm fg m) args

    Abs (Arity arity) m ->
      let (betan, fgb) = freshTyVars fg arity
          vm_ = Array.fromList betan
      in
        algW ctx vm_ fgb m
        |> Result.map (\ ((sub, fgc), typ) -> ((sub, fgc), substituteType sub (List.foldr TyAbs typ betan)))
        --|> Debug.log "Abs"

    Case typeName ->
      Dict.get typeName ctx.types
        |> Result.fromMaybe CaseTypeUnknown
        |> Result.map
          (\ (arity, constructors) ->
            let
              ty = TyAbs (typeOfNameAndArity typeName arity) (typeOfCase arity constructors)
              (ty_, fg_) = refreshType ty fg
            in ((emptySubstitution, fg_), ty_)
          )
        --|> Debug.log "Case"

    Constr constructorName ->
      Dict.get constructorName ctx.constructors
        |> Result.fromMaybe ConstructorUnknown
        |> Result.map
          (\ typ ->
            let (typ_, fg_) = refreshType typ fg
            in  ((emptySubstitution, fg_), typ_)
          )


normaliseType : Type -> Type
normaliseType typ =
  let
    subst =
      typeVars typ
        |> ListE.unique
        |> List.indexedMap (\ index var -> (var, TyVar index))
        |> IntDict.fromList

    substitute v = IntDict.get v subst |> Maybe.withDefault (TyVar v)

  in mapTyVars substitute typ

substituteDefinedMap subst ctx =
  {ctx | definedMap = Dict.map (\ _ -> substituteType subst) ctx.definedMap}

algWRecursiveStep machineName machine (ctx, fg) =
  algW ctx emptyVarMap fg machine
    |> Result.andThen
      (\ ((subst, fg_), inferredTyp) ->
        Dict.get machineName ctx.definedMap
          |> Result.fromMaybe Error
          |> Result.andThen
            (\ initTyp ->
              unifyTypes initTyp inferredTyp subst
                |> Result.mapError TypecheckUnifyError
            )
          |> Result.map (\ subst_ -> (substituteDefinedMap subst_ ctx, fg_))
      )

algWRecursive : List (MachineName, Machine) -> TypecheckContext -> Result TypecheckError TypecheckContext
algWRecursive machines initCtx =
  let
    (initFg, machinesTypes) = freshTyVarsList initFreshGen machines
    namesTypes = List.map (\ ((name, m), typ) -> (name, typ)) machinesTypes
    namesMachines = List.map Tuple.first machinesTypes
    initAcc = Ok ({initCtx | definedMap = Dict.fromList namesTypes}, initFg)
  in
    List.foldl
      (\ (machineName, machine) ->
        Result.andThen (algWRecursiveStep machineName machine)
      ) initAcc namesMachines
      |> Result.map
        (\ (ctx, fg) ->
          let normalised = Dict.map (\ _ -> normaliseType) ctx.definedMap
          in {initCtx | constMap = Dict.union normalised ctx.constMap}
        )

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

valueType v =
  case v of
    IntValue _ -> TyConst "int" []

intfun2 f =
  ( TyAbs intTy (TyAbs intTy intTy)
  , \ args ->
      case args of
        [IntValue x, IntValue y] -> Just (IntValue (f x y))
        _ -> Nothing
  )

builtinFunctions =
  Dict.fromList
  [ ("add", intfun2 (+))
  , ("sub", intfun2 (-))
  , ("mul", intfun2 (*))
  ]

type Machine
  = Const Value
  | App (List Machine) Machine
  | Abs Arity Machine
  | Reference String
  | Var Int
  | Case TypeName
  | Constr ConstructorName

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
      Graph.nodes graph |> List.map (.label >> List.singleton)
    Err components ->
      List.map (Graph.nodes >> List.map .label) components

machineSCCs : Dict MachineName Machine -> List (List (MachineName, Machine))
machineSCCs machines =
  machineGraph machines
  |> graphFromDependencies
  |> graphSCCs
  |> List.map (List.filterMap (\ name -> Dict.get name machines |> Maybe.map (Tuple.pair name)))


getConst machine =
  case machine of
    Const v -> Just v
    _ -> Nothing

stringFromMachine : Machine -> String
stringFromMachine m =
  case m of
    Const v -> stringFromValue v
    App args (Constr c) ->
      let
        stringArgs = List.map stringFromMachine args
      in c ++ "(" ++ String.join ", " stringArgs ++ ")"
    Constr c -> c
    _ -> Debug.log "sfm" m |> Debug.todo "stringFromMachine"


substituteMachine : Machine -> Machine -> Machine
substituteMachine arg machine =
  case machine of
    Var v ->
      if v == 0
      then arg
      else Var (v - 1)
    App args m ->
      substituteMachine arg m |> App (List.map (substituteMachine arg) args)
    Const v -> Const v
    Reference s -> Reference s
    Abs arity m -> Abs arity m
    Case typeName -> Case typeName
    Constr c -> Constr c

type alias Context =
  { machines : Dict String Machine
  }

idMachine =
  Abs (Arity 1) (Var 0)

constMachine =
  Abs (Arity 2) (Var 0)

notMachine =
  Abs (Arity 1)
    <| App [Var 0, Constr "false", Constr "true"]
      <| Case "bool"

andMachine =
  Abs (Arity 2)
    <| App [Var 0, App [Var 1, true, false] (Case "bool"), false]
      <| Case "bool"

yCombinator =
  Abs (Arity 1)
    <| App
         [App [Var 0] (Reference "Y")]
         (Var 0)

repeatMachine =
  Abs (Arity 1)
    <| App [Var 0, App [Var 0] (Reference "repeat")]
     <| Constr "cons"

takeMachine =
  Abs (Arity 2)
    <| App
      [ App [zero, Var 0] <| Reference "="
      , Constr "nil"
      , App
          [ Var 1
          , Constr "nil"
          , App [Var 0]
              <| Abs (Arity 3)
                <| App
                  [ Var 1
                  , App
                      [ App
                          [ Var 0
                          , one
                          ]
                          <| Reference "sub"
                      , Var 2
                      ]
                      <| Reference "take"
                  ]
                  <| Constr "cons"
          ]
          <| Case "list"
      ]
      <| Case "bool"

defaultCtx =
  { machines = Dict.fromList defaultMachines
  }

defaultMachines =
  [ ("not", notMachine)
  , ("and", andMachine)
  , ("id", idMachine)
  , ("Y", yCombinator)
  , ("repeat", repeatMachine)
  , ("take", takeMachine)
  ]


evalBuiltin : String -> List Value -> Result EvalError Value
evalBuiltin name args =
  Dict.get name builtinFunctions
    |> Result.fromMaybe EvalBuiltin
    |> Result.andThen
      (\ (typ, fun) ->
        if List.length args == typeArity typ
        then fun args |> Result.fromMaybe EvalBuiltin
        else Err EvalBuiltin
      )

machineOfBool : Bool -> Machine
machineOfBool b =
  if b then Constr "true" else Constr "false"

type EvalError
  = EvalError
  | EvalBuiltin
  | EvalCase

-- evaluate machine to WHNF
evalMachine : Context -> Machine -> Result EvalError Machine
evalMachine ctx machine =
  case machine of
    Const v -> Ok (Const v)
    App [m1, m2] (Reference "=") ->
      let
        n1 = evaluateNF ctx m1
        n2 = evaluateNF ctx m2
      in
        Result.map2 (\ x y -> machineEquals x y |> machineOfBool) n1 n2
    App args (Reference n) ->
      case Dict.get n ctx.machines of
        Just m -> evalMachine ctx (App args m)
        Nothing ->
          args
            |> List.map (evalMachine ctx >> Result.andThen (getConst >> Result.fromMaybe EvalError))
            |> ResultE.combine
            |> Result.andThen (evalBuiltin n)
            |> Result.map Const
    App argsNewer (App argsOlder m) ->
      evalMachine ctx (App (argsOlder ++ argsNewer) m)
    App (arg :: args) (Abs (Arity arity) m) ->
      if arity == 0 then
        evalMachine ctx (App (arg :: args) m)
      else
        substituteMachine arg m |> Abs (Arity (arity - 1)) |> App args |> evalMachine ctx
    App [] m -> evalMachine ctx m
    Abs (Arity 0) m -> evalMachine ctx m
    App (arg::args) (Case typeName) ->
      Dict.get typeName defaultTypecheckContext.types
        |> Result.fromMaybe EvalCase
        |> Result.andThen
          (\ (arity, constructors) ->
            if List.length args < List.length constructors
            then Err EvalCase
            else
              evalMachine ctx arg
                |> Result.andThen
                  (\ v -> case v of
                    Constr c -> Ok ([], c)
                    App constrArgs (Constr c) -> Ok (constrArgs, c)
                    _ -> Err EvalCase
                  )
                |> Result.andThen
                  (\ (constrArgs, c) ->
                    List.indexedMap Tuple.pair constructors
                      |> ListE.find (\ (index, (constrName, constrArgsTypes)) -> constrName == c)
                      |> Result.fromMaybe EvalCase
                      |> Result.andThen
                        (\ (index, (constrName, constrArgsTypes)) ->
                          ListE.getAt index args
                            |> Result.fromMaybe EvalCase
                            |> Result.andThen
                              (App constrArgs >> App (List.drop (List.length constructors) args) >> evalMachine ctx)
                        )
                  )
          )
    Case typeName -> Err EvalCase
    Constr c -> Ok (Constr c)
    App args (Constr c) -> Ok (App args (Constr c))

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

boolTy = TyConst "bool" []
intTy = TyConst "int" []

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
    <| App [one]
      <| Reference "add"

testMachine3 =
  App [two] idMachine

testMachine4 =
  App [true, false] (Reference "and")

testMachine5 =
  App [testMachine4, testMachine1, testMachine2] <| Case "bool"

testMachine6 = App [one, (Constr "nil")] (Constr "cons")

testMachine7 = App [testMachine4] (Reference "not")

alwaysTrueMachineY =
  Abs (Arity 2)
    <| App
      [ (Var 1)
      , (Constr "true")
      , (App [(Constr "true")] (Var 0))
      ]
      <| Case "bool"

testMachine8 =
  App [alwaysTrueMachineY, Constr "false"] (Reference "Y")

testMachine9 =
  App [two, App [one] <| Reference "repeat"] (Reference "take")

  

type alias Model = ()

type alias Msg = ()

init = ()

evaluateNF : Context -> Machine -> Result EvalError Machine
evaluateNF ctx machine =
  case evalMachine ctx machine of
    Ok (App args (Constr c)) ->
      args
        |> List.map (evaluateNF ctx)
        |> ResultE.combine
        |> Result.map (\ evalArgs -> App evalArgs (Constr c))
    m -> m

view model =
  testMachine9
    |> evaluateNF defaultCtx
    |> Result.map stringFromMachine
    |> Debug.toString
    |> Html.text

compactAndMachine =
  Abs (Arity 2)
    <| App [Var 0, Var 1, false]
      <| Case "bool"


fullTypecheckContext =
  defaultCtx.machines
  |> machineSCCs
  |> List.foldl (\ machines -> Result.andThen (algWRecursive machines)) (Ok defaultTypecheckContext)


{-
view model =
  fullTypecheckContext
    |> Result.andThen
      (\ ctx -> algW ctx emptyVarMap initFreshGen testMachine9)
    |> Result.map (\ (acc, typ) -> typeToString typ)
    |> Debug.toString
    |> Html.text
-}

update msg model = model

main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
