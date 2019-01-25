module Typecheck exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)

import Type exposing (Type, ConstructorDict, TypeDict, FreshGen, Substitution, emptySubstitution, Arity, Arity(..), typeOfNameAndArity, typeOfCase, composeSubstitutions, typeConstructors, defaultTypes)
import Machine exposing (..)
import Builtin exposing (..)


-- for every machine variable, map to the corresponding type
type alias VarMap = Array Type
type alias ConstMap = Dict String Type

emptyVarMap = Array.empty
emptyConstMap = Dict.empty


type TypecheckError
  = Error
  | UnboundVariable
  | UnknownReference String
  | TypecheckUnifyError Type.UnifyError
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
  { constMap = Dict.map (\ _ (typ, f) -> typ) (Dict.fromList builtinFunctions)
  , definedMap = Dict.empty
  , types = Dict.fromList defaultTypes
  , constructors = Dict.fromList <| List.concatMap typeConstructors defaultTypes
  }


substituteVarMap : Substitution -> VarMap -> VarMap
substituteVarMap subst vm =
  Array.map (Type.substituteType subst) vm


algWApp ctx vm m2 ((subst1, fg1), typ1) =
  algW ctx (substituteVarMap subst1 vm) fg1 m2
    |> Result.andThen
      (\ ((subst2, fg2), typ2) ->
        let (beta, fgbeta) = Type.freshVariable fg2
        in
          Type.unify (Type.substituteType subst2 typ1) (Type.Fun typ2 beta) emptySubstitution
            |> Result.mapError TypecheckUnifyError
            |> Result.map
              (\ v ->
                let
                  subst = composeSubstitutions v (composeSubstitutions subst2 subst1)
                  typ = Type.substituteType v beta
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
        (typ, fg_) = Type.refresh Type.equalityType fg
      in Ok ((emptySubstitution, fg_), typ)

    Reference r ->
      case Dict.get r ctx.constMap of
        Just typ ->
          let (typ_, fg_) = Type.refresh typ fg
          in Ok ((emptySubstitution, fg_), typ_)
        Nothing ->
          Dict.get r ctx.definedMap
            |> Result.fromMaybe (UnknownReference r)
            |> Result.map (\ typ -> ((emptySubstitution, fg), typ))

    App args m ->
      List.foldl (\ arg -> Result.andThen (algWApp ctx vm arg)) (algW ctx vm fg m) args

    Abs (Arity arity) m ->
      let (betan, fgb) = Type.freshVariables fg arity
          vm_ = Array.fromList betan
      in
        algW ctx vm_ fgb m
        |> Result.map (\ ((sub, fgc), typ) -> ((sub, fgc), Type.substituteType sub (List.foldr Type.Fun typ betan)))
        --|> Debug.log "Abs"

    Case typeName ->
      Dict.get typeName ctx.types
        |> Result.fromMaybe CaseTypeUnknown
        |> Result.map
          (\ (arity, constructors) ->
            let
              ty = Type.Fun (typeOfNameAndArity typeName arity) (typeOfCase arity constructors)
              (ty_, fg_) = Type.refresh ty fg
            in ((emptySubstitution, fg_), ty_)
          )
        --|> Debug.log "Case"

    Constr constructorName ->
      Dict.get constructorName ctx.constructors
        |> Result.fromMaybe ConstructorUnknown
        |> Result.map
          (\ typ ->
            let (typ_, fg_) = Type.refresh typ fg
            in  ((emptySubstitution, fg_), typ_)
          )


substituteDefinedMap subst ctx =
  {ctx | definedMap = Dict.map (\ _ -> Type.substituteType subst) ctx.definedMap}

algWRecursiveStep machineName machine (ctx, fg) =
  algW ctx emptyVarMap fg machine
    |> Result.andThen
      (\ ((subst, fg_), inferredTyp) ->
        Dict.get machineName ctx.definedMap
          |> Result.fromMaybe Error
          |> Result.andThen
            (\ initTyp ->
              Type.unify initTyp inferredTyp subst
                |> Result.mapError TypecheckUnifyError
            )
          |> Result.map (\ subst_ -> (substituteDefinedMap subst_ ctx, fg_))
      )

algWRecursive : List (MachineName, Machine) -> TypecheckContext -> Result TypecheckError TypecheckContext
algWRecursive machines initCtx =
  let
    (freshTypes, initFg) = Type.freshVariables Type.initFreshGen (List.length machines)
    machinesTypes = List.map2 Tuple.pair machines freshTypes
    namesTypes = List.map (\ ((name, m), typ) -> (name, typ)) machinesTypes
    initAcc = Ok ({initCtx | definedMap = Dict.fromList namesTypes}, initFg)
  in
    List.foldl
      (\ (machineName, machine) ->
        Result.andThen (algWRecursiveStep machineName machine)
      ) initAcc machines
      |> Result.map
        (\ (ctx, fg) ->
          let normalised = Dict.map (\ _ -> Type.normalise) ctx.definedMap
          in {initCtx | constMap = Dict.union normalised ctx.constMap}
        )
