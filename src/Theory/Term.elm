module Theory.Term exposing (..)

import Dict exposing (Dict)
import List.Extra as ListE
import Result.Extra as ResultE

import Theory.Builtin exposing (..)
import Theory.Type as Type exposing (..)

type alias MachineName = String

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

machineGraph : Dict String Machine -> List (String, List String)
machineGraph machines =
  Dict.foldl (\ k v acc -> (k, machineReferences v) :: acc) [] machines

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
  , types : TypeDict
  }

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
      Dict.get typeName ctx.types
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

evaluateNF : Context -> Machine -> Result EvalError Machine
evaluateNF ctx machine =
  case evalMachine ctx machine of
    Ok (App args (Constr c)) ->
      args
        |> List.map (evaluateNF ctx)
        |> ResultE.combine
        |> Result.map (\ evalArgs -> App evalArgs (Constr c))
    m -> m


-- TODO: move to Builtin
evalBuiltin : String -> List Value -> Result EvalError Value
evalBuiltin name args =
  Dict.get name (Dict.fromList builtinFunctions)
    |> Result.fromMaybe EvalBuiltin
    |> Result.andThen
      (\ (typ, fun) ->
        if List.length args == typeArity typ
        then fun args |> Result.fromMaybe EvalBuiltin
        else Err EvalBuiltin
      )
