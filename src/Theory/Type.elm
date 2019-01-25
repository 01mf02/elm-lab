module Type exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import List.Extra as ListE

type Type
  = Fun Type Type
  | Const String (List Type)
  | Var Int

-- all-quantified variables, followed by the type
type alias TypeConstructor = (String, List Type)
type alias TypeName = String
type alias ConstructorName = String
type alias VariableIndex = Int
type Arity = Arity Int

-- for every type, save number of polymorphic type variables and its constructors
type alias TypeDict = Dict TypeName (Arity, List TypeConstructor)
type alias ConstructorDict = Dict ConstructorName Type

boolType =
  ("bool", (Arity 0, [( "true", []),
                      ("false", [])]))
listType =
  ("list", (Arity 1, [( "nil", []),
                      ("cons", [Var 0, Const "list" [Var 0]])]))

defaultTypes =
  [ boolType
  , listType
  ]

boolTy = Const "bool" []
equalityType = Fun (Var 0) (Fun (Var 0) boolTy)

typeOfCase (Arity arity) constructors =
  List.foldr Fun (Var arity) (List.map (\ (name, args) -> List.foldr Fun (Var arity) args) constructors)

typeOfNameAndArity name (Arity arity) =
  Const name (List.range 0 (arity-1) |> List.map Var)

typeConstructors : (TypeName, (Arity, List TypeConstructor)) -> List (String, Type)
typeConstructors (typeName, (arity, constructors)) =
  List.map (\ (name, args) -> (name, List.foldr Fun (typeOfNameAndArity typeName arity) args)) constructors

toString : Type -> String
toString typ =
  case typ of
    Fun t1 t2 -> "(" ++ toString t1 ++ " → " ++ toString t2 ++ ")"
    Const s args -> String.join " " (s :: List.map toString args)
    Var v -> "t" ++ String.fromInt v

normalise : Type -> Type
normalise typ =
  let
    subst =
      variables typ
        |> ListE.unique
        |> List.indexedMap (\ index var -> (var, Var index))
        |> IntDict.fromList

    substitute v = IntDict.get v subst |> Maybe.withDefault (Var v)

  in mapTyVars substitute typ

typeArity typ =
  case typ of
    Fun _ t -> 1 + typeArity t
    _ -> 0


-- for every type variable, map to corresponding type
type alias Substitution = IntDict Type

type FreshGen = FreshGen Int

initFreshGen = FreshGen 0

emptySubstitution = IntDict.empty

freshVariable : FreshGen -> (Type, FreshGen)
freshVariable (FreshGen i) =
  (Var i, FreshGen (i+1))

freshVariables : FreshGen -> Int -> (List Type, FreshGen)
freshVariables (FreshGen i) n =
  let i_ = i + n
      tyvars = List.range i (i_ - 1) |> List.map Var
  in (tyvars, FreshGen i_)

refresh : Type -> FreshGen -> (Type, FreshGen)
refresh typ (FreshGen i) =
  (offsetVariables i typ, FreshGen (i + maxVariable typ + 1))

maxVariable : Type -> VariableIndex
maxVariable typ =
  case typ of
    Fun t1 t2 -> max (maxVariable t1) (maxVariable t2)
    Const s args -> List.map maxVariable args |> List.maximum |> Maybe.withDefault (-1)
    Var v -> v

offsetVariables : Int -> Type -> Type
offsetVariables off typ =
  case typ of
    Fun t1 t2 -> Fun (offsetVariables off t1) (offsetVariables off t2)
    Const s args -> Const s (List.map (offsetVariables off) args)
    Var v -> Var (v + off)

variables : Type -> List VariableIndex
variables typ =
  case typ of
    Fun t1 t2 -> variables t1 ++ variables t2
    Const s args -> List.concatMap variables args
    Var v -> [v]

containsVariable : VariableIndex -> Type -> Bool
containsVariable var typ =
  case typ of
    Var v -> v == var
    Const c args -> List.any (containsVariable var) args
    Fun t1 t2 -> containsVariable var t1 || containsVariable var t2



mapTyVars : (VariableIndex -> Type) -> Type -> Type
mapTyVars f typ =
  case typ of
    Fun t1 t2 -> Fun (mapTyVars f t1) (mapTyVars f t2)
    Const s args -> Const s (List.map (mapTyVars f) args)
    Var v -> f v

substituteType : Substitution -> Type -> Type
substituteType subst typ =
  let
    substitute v =
      case IntDict.get v subst of
        Nothing -> Var v
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
unify : Type -> Type -> Substitution -> Result UnifyError Substitution
unify t1 t2 subst =
  case (substituteType subst t1, substituteType subst t2) of
    (Var v as t1s, t2s) ->
      if t1s == t2s
      then Ok subst
      else
        if containsVariable v t2s
        then Err OccursCheckFailed
        else Ok (IntDict.insert v t2 subst)
    (_, Var _) -> unify t2 t1 subst
    (Fun t11 t12, Fun t21 t22) ->
      subst |> unify t11 t21 |> Result.andThen (unify t12 t22)
    (Const c1 args1, Const c2 args2) ->
      if c1 == c2
      then
        if List.length args1 == List.length args2
        then
          List.map2 Tuple.pair args1 args2
            |> List.foldl
              (\ (arg1, arg2) -> Result.andThen (unify arg1 arg2))
              (Ok subst)
        else
          Err ConstrArgsCountsDiffer
      else
        Err ConstructorsDiffer
    (Fun _ _, Const _ _) -> Err UnifyError
    (Const _ _, Fun _ _) -> Err UnifyError
