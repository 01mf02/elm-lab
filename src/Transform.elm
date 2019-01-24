module Transform exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Coord exposing (SVGCoord)

type alias EntityId = Int

-- transformation is by default from local to global (local -> global)
type alias Transform =
  { translate : SVGCoord
  , parent : Maybe EntityId
  , children : Set EntityId
  }

type alias Transforms a =
  { a | transforms : Dict EntityId Transform }

root : Transform
root = empty { x = 0, y = 0 }

-- TODO: move this somewhere else
rootTransformId : EntityId
rootTransformId = 0

empty : SVGCoord -> Transform
empty translate =
  { translate = translate
  , parent = Nothing
  , children = Set.empty
  }

map : (Transform -> Transform) -> EntityId -> Transforms a -> Transforms a
map fn id components =
  { components
    | transforms = Dict.update id (Maybe.map fn) components.transforms
  }

translateBy : SVGCoord -> Transform -> Transform
translateBy offset transform =
  { transform | translate = Coord.add offset transform.translate }



transformFrom : Transforms a -> EntityId -> EntityId -> SVGCoord -> SVGCoord
transformFrom components from to =
  toGlobal components from >> toLocal components to


-- transformations of more top-level machines come first
transformTrail : Transforms a -> EntityId -> List Transform
transformTrail components =
  let
    aux acc id =
      Dict.get id components.transforms
        |> Maybe.map
          (\transform ->
            case transform.parent of
              Nothing -> transform :: acc
              Just parent -> aux (transform :: acc) parent
          )
        |> Maybe.withDefault []
  in
  aux []

transformCoord : Transform -> SVGCoord -> SVGCoord
transformCoord transform =
  Coord.add transform.translate

transformInverse : Transform -> SVGCoord -> SVGCoord
transformInverse transform coord =
  Coord.subtract coord transform.translate


toLocal : Transforms a -> EntityId -> SVGCoord -> SVGCoord
toLocal components id coord =
  List.foldl transformInverse coord (transformTrail components id)

toGlobal : Transforms a -> EntityId -> SVGCoord -> SVGCoord
toGlobal components id coord =
  List.foldr transformCoord coord (transformTrail components id)


reorientParentChild : EntityId -> EntityId -> Transforms a -> Transforms a
reorientParentChild newParentId childId components =
  let
    maybeChild = Dict.get childId components.transforms
    oldParentId =
      maybeChild
        |> Maybe.andThen .parent
        |> Maybe.withDefault rootTransformId
    addChild parent = { parent | children = Set.insert childId parent.children }
    removeChild parent = { parent | children = Set.remove childId parent.children }
    updateChild transform =
      { transform
        | parent = Just newParentId
        , translate = transformFrom components oldParentId newParentId transform.translate
      }
    updateTransforms =
      Dict.update oldParentId (Maybe.map removeChild)
        >> Dict.update newParentId (Maybe.map addChild)
        >> Dict.update childId (Maybe.map updateChild)
  in
  { components | transforms = updateTransforms components.transforms }


setParentChild : EntityId -> EntityId -> Transforms a -> Transforms a
setParentChild parentId childId components =
  let
    addChild parent = { parent | children = Set.insert childId parent.children }
    setParent child = { child | parent = Just parentId }
    updateTransforms =
      Dict.update parentId (Maybe.map addChild)
        >> Dict.update childId (Maybe.map setParent)
  in
  { components | transforms = updateTransforms components.transforms }

