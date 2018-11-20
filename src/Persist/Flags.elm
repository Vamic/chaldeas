module Persist.Flags exposing
  ( Flags, decodeFlags
  , storePreferences, storeTeam
  )

import Json.Decode as D
import Json.Encode as E
import List.Extra  as List
import Maybe.Extra as Maybe
import Set         as Set
import Date exposing (Date)
import Dict exposing (Dict)
import Time

import StandardLibrary     exposing (..)
import Database            exposing (..)
import Database.Base       exposing (..)
import Database.Servant    exposing (..)
import MyServant           exposing (..)
import Persist.Preferences exposing (..)

import Class.Show as Show

type alias Flags =
    { today       : Date
    , preferences : Preferences
    , team        : Dict OrdServant MyServant
    }

decodeFlags : D.Decoder Flags
decodeFlags =
    D.map3 Flags
    (D.field "today" decodeDate)
    (D.field "preferences" decodePreferences)
    (D.field "team" decodeTeam)

decodeDate : D.Decoder Date
decodeDate =
    D.int |> D.andThen (Time.millisToPosix >> Date.today >> D.succeed)

encodePreferences : Preferences -> Value
encodePreferences =
  let
    encodePref x = List.find (ordPreference >> (==) x) enumPreference
  in
    Set.toList
    >> List.map encodePref
    >> Maybe.values
    >> List.map Show.preference
    >> E.list E.string

decodePreferences : D.Decoder Preferences
decodePreferences =
  let
    fromList : Maybe (List String) -> D.Decoder Preferences
    fromList a = D.succeed <| case a of
      Nothing    -> noPreferences
      Just prefs ->
        let
          acc pref =
              setPreference pref <| List.member (Show.preference pref) prefs
        in
          List.foldr acc Set.empty enumPreference
  in
    D.list D.string
    |> D.nullable
    >> D.andThen fromList

encodeStat : Stat -> Value
encodeStat stat =
    E.object
    [ ("atk", E.int stat.atk)
    , ("hp",  E.int stat.hp)
    ]

decodeStat : D.Decoder Stat
decodeStat =
    D.map2 Stat
    (D.field "atk" D.int)
    (D.field "hp"  D.int)

encodeServant : Servant -> Value
encodeServant = .id >> E.int

decodeServant : D.Decoder Servant
decodeServant =
  let
    fromId id = case List.find (.id >> (==) id) servants of
      Nothing -> D.fail <| "Unknown Servant #" ++ String.fromInt id
      Just s  -> D.succeed s
    -- Backward compatibility
    fromName name = case List.find (.name >> (==) name) servants of
      Nothing -> D.fail <| "Unknown Servant " ++ name
      Just s  -> D.succeed s
  in
    D.oneOf
    [ D.int    |> D.andThen fromId
    , D.string |> D.andThen fromName
    ]

encodeMyServant : MyServant -> Value
encodeMyServant ms =
    E.object
    [ ("level",   E.int ms.level)
    , ("fou",     encodeStat ms.fou)
    , ("skills",  E.list E.int ms.skills)
    , ("npLvl",   E.int ms.npLvl)
    , ("ascent",  E.int ms.ascent)
    , ("servant", encodeServant ms.servant)
    ]

decodeMyServant : D.Decoder MyServant
decodeMyServant =
    D.map8 MyServant
    (D.field "level" D.int)
    (D.field "fou" decodeStat)
    (D.field "skills" <| D.list D.int)
    (D.field "npLvl" D.int)
    (D.field "ascent" D.int)
    (D.field "servant" decodeServant)
    (D.field "servant" decodeServant)
    (D.succeed Dict.empty)

encodeTeam : Dict OrdServant MyServant -> E.Value
encodeTeam =
    Dict.toList
    >> List.map Tuple.second
    >> E.list encodeMyServant

decodeTeam : D.Decoder (Dict OrdServant MyServant)
decodeTeam =
  let
    keyPair ms = (ordServant ms.base, recalc ms)
  in
    D.list decodeMyServant
    |> D.andThen (List.map keyPair >> Dict.fromList >> D.succeed)

storePreferences : (String -> Value -> Cmd msg) -> Preferences -> Cmd msg
storePreferences store = encodePreferences >> store "preferences"

storeTeam : (String -> Value -> Cmd msg) -> Dict OrdServant MyServant -> Cmd msg
storeTeam store = encodeTeam >> store "team"
