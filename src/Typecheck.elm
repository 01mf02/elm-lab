module Typecheck exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)

import Type exposing (..)
import Machine exposing (..)
import Builtin exposing (..)

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
  { constMap = Dict.map (\ _ (typ, f) -> typ) (Dict.fromList builtinFunctions)
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
