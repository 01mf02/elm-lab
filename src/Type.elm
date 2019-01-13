module Type exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import List.Extra as ListE

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
type Arity = Arity Int

-- for every type, save number of polymorphic type variables and its constructors
type alias TypeDict = Dict TypeName (Arity, List TypeConstructor)
type alias ConstructorDict = Dict ConstructorName Type

boolType =
  ("bool", (Arity 0, [( "true", []),
                      ("false", [])]))
listType =
  ("list", (Arity 1, [( "nil", []),
                      ("cons", [TyVar 0, TyConst "list" [TyVar 0]])]))

boolTy = TyConst "bool" []

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
