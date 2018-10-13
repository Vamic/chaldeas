module Database.Export exposing (craftEssence, servant)

import Json.Encode as E
import List.Extra  as List

import StandardLibrary       exposing (..)
import Database.Base         exposing (..)
import Database.Skill        exposing (..)
import Database.CraftEssence exposing (..)
import Database.Servant      exposing (..)

import Class.Show as Show

nullable : (a -> Value) -> Maybe a -> Value
nullable f a = case a of
  Nothing -> E.null
  Just x  -> f x

amount : Amount -> Value
amount a = E.object <| case a of
    Flat x -> [("from", E.float x), ("to", E.float x)]
    Range x y -> [("from", E.float x), ("to", E.float y)]
    Full -> [("from", E.float 0), ("to", E.float 0)]
    Placeholder -> [("from", E.float 0), ("to", E.float 0)]

stat : Stat -> Value
stat x = 
    E.object
    [ ("atk", E.int x.atk) 
    , ("hp",  E.int x.hp)
    ]

skillEffect : SkillEffect -> Value 
skillEffect =
  let
    modEffect f fields = case List.find (Tuple.first >> (==) "effect") fields of
      Nothing -> fields
      Just (_, effect) -> 
        let 
          f_ = 
              E.encode 0
              >> String.dropLeft 1
              >> String.dropRight 1
              >> f
              >> E.string
        in
          fields ++ 
          [("effect", f_ effect)]
    go a = case a of
      Grant target duration effect amt ->
          [ ("target",   E.string << .s <| Show.possessiveAndSubject target) 
          , ("duration", E.int duration)
          , ("effect",   E.string <| Show.buffEffect Someone Placeholder effect)
          , ("amount",   amount amt)
          , ("chance",   E.object [("from", E.int 100), ("to", E.int 100)])
          ]
      Debuff target duration effect amt ->
          [ ("target",   E.string << .s <| Show.possessiveAndSubject target) 
          , ("duration", E.int duration)
          , ("effect",   E.string <| Show.debuffEffect Someone Placeholder effect)
          , ("amount",   amount amt)
          , ("chance",   E.object [("from", E.int 100), ("to", E.int 100)])
          ]
      To target effect amt ->
          [ ("target",   E.string << .s <| Show.possessiveAndSubject target) 
          , ("duration", E.int 0)
          , ("effect",   E.string <| Show.instantEffect Someone Placeholder effect)
          , ("amount",   amount amt)
          , ("chance",   E.object [("from", E.int 100), ("to", E.int 100)])
          ]
      Bonus effect isPerc amt ->
          [ ("target",   E.string << .s <| Show.possessiveAndSubject Self) 
          , ("duration", E.int 0)
          , ("effect",   E.string <| Show.bonusEffect False Placeholder effect)
          , ("amount",   amount amt)
          , ("chance",   E.object [("from", E.int 100), ("to", E.int 100)])
          ]
      Chance x effect ->
          go effect ++
          [ ("chance",   E.object [("from", E.int x), ("to", E.int x)]) ]
      Chances x y effect ->
          go effect ++
          [ ("chance",   E.object [("from", E.int x), ("to", E.int y)]) ]
      ToMax x effect ->
          go effect 
          |> modEffect (flip (++) <| " every turn up to " ++ Show.amount x)
      When x effect ->
          go effect 
          |> modEffect ((++) <| "If " ++ x ++ ": ")
      Times 1 effect ->
          go effect 
          |> modEffect (flip (++) <| " (1 time)")
      Times x effect ->
          go effect 
          |> modEffect (flip (++) <| " (" ++ String.fromInt x ++ "time)")
  in
    go >> E.object

skill : Skill -> Value
skill x =
    E.object
    [ ("name",   E.string x.name) 
    , ("icon",   E.string <| Show.icon x.icon)
    , ("cd",     E.int x.cd)
    , ("effect", E.list skillEffect x.effect)
    ]

noblePhantasm : NoblePhantasm -> Value
noblePhantasm x =
    E.object
    [ ("name",           E.string x.name) 
    , ("desc",           E.string x.desc)
    , ("rank",           E.string <| Show.rank x.rank)
    , ("card",           E.string <| Show.card x.card)
    , ("classification", E.string x.kind)
    , ("hits",           E.int x.hits)
    , ("effect",         E.list skillEffect x.effect)
    , ("over",           E.list skillEffect x.over)
    , ("activatesFirst", E.bool x.first)
    ]

hits : Hits -> Value
hits x =
    E.object
    [ ("quick",  E.int x.quick) 
    , ("arts",   E.int x.arts)
    , ("buster", E.int x.buster)
    , ("extra",  E.int x.ex)
    ]

craftEssence : CraftEssence -> Value
craftEssence ce =
  let
    stats x =
        E.object
        [ ("base",  stat x.base) 
        , ("max",   stat x.max)
        ]
  in
    E.object
    [ ("name",    E.string ce.name) 
    , ("id",      E.int ce.id)
    , ("rarity",  E.int ce.rarity)
    , ("icon",    E.string <| Show.icon ce.icon)
    , ("stats",   stats ce.stats)
    , ("effect",  E.list skillEffect ce.effect)
    , ("limited", E.bool ce.limited)
    , ("bond",    nullable E.string ce.bond)
    ]

servant : Servant -> Value
servant s = 
  let
    stats x =
        E.object
        [ ("base",  stat x.base) 
        , ("max",   stat x.max)
        , ("grail", stat x.grail)
        ]
  in
    E.object
    [ ("name",          E.string s.name) 
    , ("id",            E.int s.id)
    , ("rarity",        E.int s.rarity)
    , ("class",         E.string <| Show.class s.class)
    , ("attribute",     E.string <| Show.attribute s.attr)
    , ("deck",          E.list (E.string << Show.card) <| getDeck s)
    , ("curve",         E.int s.curve)
    , ("stats",         stats s.stats)
    , ("skills",        E.list skill s.skills)
    , ("passives",      E.list skill s.passives)
    , ("noblePhantasm", noblePhantasm s.phantasm)
    , ("starWeight",    E.int s.gen.starWeight)
    , ("starRate",      E.float s.gen.starRate)
    , ("npAtk",         E.float s.gen.npAtk)
    , ("npDef",         E.int s.gen.npDef)
    , ("hits",          hits s.hits)
    , ("traits",        E.list (Show.trait >> E.string) s.traits)
    , ("deathRate",     E.float s.death)
    , ("alignment",     E.list (Show.alignment >> E.string) s.align)
    , ("limited",       E.bool s.limited)
    , ("free",          E.bool s.free)
    , ("bond",          nullable craftEssence <| getBond s)
    ]
