module Transform exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Maybe.Extra as MaybeE

import Frame2d exposing (Frame2d)
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)

type alias EntityId = Int

-- transformation is by default from local to global (local -> global)
type alias Transform =
  { frame : Frame2d
  , parent : Maybe EntityId
  , children : Set EntityId
  }

type alias Transforms a =
  { a | transforms : Dict EntityId Transform }

root : Transform
root =
  { frame = Frame2d.atOrigin
  , parent = Nothing
  , children = Set.empty
  }

map : (Transform -> Transform) -> EntityId -> Transforms a -> Transforms a
map fn id components =
  { components
    | transforms = Dict.update id (Maybe.map fn) components.transforms
  }

translateBy : Vector2d -> Transform -> Transform
translateBy offset transform =
  { transform
    | frame = Frame2d.translateBy offset transform.frame
  }

placeInRoot : Transforms a -> EntityId -> Frame2d
placeInRoot components id =
  List.foldr
    (\transform frame -> Frame2d.placeIn frame transform.frame)
    Frame2d.atOrigin (transformTrail components id)

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

adoptBy : EntityId -> EntityId -> Transforms a -> Transforms a
adoptBy newParentId childId components =
  let
    maybeChild = Dict.get childId components.transforms
    maybeOldParentId = maybeChild |> Maybe.andThen .parent
    applyIfOldParent fn = MaybeE.unwrap identity fn maybeOldParentId
    addChild parent = { parent | children = Set.insert childId parent.children }
    removeChild parent = { parent | children = Set.remove childId parent.children }
    updateFrame =
      applyIfOldParent (\oldParent -> Frame2d.placeIn (placeInRoot components oldParent))
        >> Frame2d.relativeTo (placeInRoot components newParentId)
    updateChild transform =
      { transform
        | parent = Just newParentId
        , frame = updateFrame transform.frame
      }
    updateTransforms =
      applyIfOldParent (\oldParentId -> Dict.update oldParentId (Maybe.map removeChild))
        >> Dict.update newParentId (Maybe.map addChild)
        >> Dict.update childId (Maybe.map updateChild)
  in
  { components | transforms = updateTransforms components.transforms }
