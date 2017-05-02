module Lofi.Schema.Output.Julia exposing
  ( createStructCode
  )

{-|

# Functions
@docs createStructCode
-}

import Lofi.Schema exposing (Schema, Item, Kind(..))
import String.Extra exposing (underscored, camelize, decapitalize, quote)


lowerCamelize : String -> String
lowerCamelize = camelize >> decapitalize

pairsToStructFields : String -> List (String, String) -> String
pairsToStructFields name pairs =
  let
    propertyDivider =
      "\n  "

    innerCode =
      pairs
      |> List.map (\(a, b) -> "" ++ a ++ "::" ++ b)
      |> String.join propertyDivider

  in
    innerCode

createRecordPropertiesCodeFold : Item -> List (String, String) -> List (String, String)
createRecordPropertiesCodeFold item list =
  let
    nativeKind =
      case item.kind of
        Text _ ->
          "AbstractString"
        Number { real } ->
          if real then
            "Float64"
          else
            "Int64"
        Date { time } ->
          if time then
            "DateTime"
          else
            "Date"

    propertyName =
      lowerCamelize item.name

    schemaString =
      [ Just nativeKind
      ]
      |> List.filterMap identity
      |> String.join ""

    field =
      (propertyName, schemaString)

  in
    field :: list


-- 3-tuple contains propertyName, constraint predicate, error call
itemToConstraintViolations : Item -> List (String, String, String) -> List (String, String, String)
itemToConstraintViolations item list =
  let
    propertyName =
      lowerCamelize item.name

    predicate =
      case item.kind of
        Text { maximumLength } ->
          case maximumLength of
            Just ml ->
              "(length(" ++ propertyName ++ ") > " ++ toString ml ++ ")"
            Nothing ->
              ""
        Number { allowNegative } ->
          if allowNegative then
            ""
          else
            "(" ++ propertyName ++ " < 0)"
        _ -> ""

    errorMsg =
      "error(\"Constraint on " ++ propertyName ++ " violated\")"
  in
    case predicate of
      "" -> list
      _ -> (propertyName, predicate, errorMsg) :: list


createInnerCtor : String -> List Item -> List (String, String, String) -> String
createInnerCtor objectName items constraints =
  let
    argList =
      String.join ", " (List.map (\item -> lowerCamelize item.name) items)

    lhs =
      objectName ++ "(" ++ argList ++ ")"

    constraintString =
      case constraints of
        [] -> ""
        _ ->
          String.join "\n    elseif " (List.map (\(_, pred, err) -> pred ++ "\n      " ++ err) constraints)

    newCall =
      "new(" ++ argList ++ ")"

    rhs =
      case constraintString of
        "" -> newCall
        _ ->
          "if " ++ constraintString ++ "\n    else\n      " ++ newCall ++ "\n    end"
  in
    case constraintString of
      "" -> "" -- Default ctor suffices when no constraints
      _ -> "\n\n  " ++ lhs ++ " =\n    " ++ rhs


{-| Generates Julia code declaring a composite type -}
createStructCode : Schema -> String
createStructCode schema =
  let

    objectName = camelize schema.individualName

    structCode =
      List.foldr createRecordPropertiesCodeFold [] schema.items
      |> pairsToStructFields (camelize schema.individualName)

    constraints =
      List.foldr itemToConstraintViolations [] schema.items

    innerCtor =
      createInnerCtor objectName schema.items constraints
  in
    "struct " ++ objectName ++ "\n  " ++ structCode ++ innerCtor ++ "\nend"


  {- Handling optional values could be done via an outer constructor using
     keyword args, or by modeling values with a Nullable. Both strategies make
     assumptions that will be wrong in enough cases to simply omit handling
     optional values and default values here.
   -}
