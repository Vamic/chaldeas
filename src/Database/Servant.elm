module Database.Servant exposing 
  ( Servant, eqServant, OrdServant, ordServant
  , Deck(..)
  , Ratings
  , NoblePhantasm
  , Hits
  , Gen
  , PhantasmType(..), phantasmType
  , Ascension(..)
  , Reinforcement(..)
  , getDeck
  , reduceMats, getAscensions, getReinforcements, getMaterials
  )

import List.Extra as List

import StandardLibrary exposing (..)
import Database.Base exposing (..)
import Database.Skill exposing (..)

type alias Servant = 
    { name     : String
    , id       : Int
    , rarity   : Int
    , class    : Class
    , attr     : Attribute
    , deck     : Deck
    , curve    : Int
    , stats    : { base : Stat, max : Stat, grail : Stat }
    , skills   : List Skill
    , passives : List Skill
    , phantasm : NoblePhantasm
    , gen      : Gen
    , hits     : Hits
    , traits   : List Trait
    , death    : Float
    , align    : List Alignment
    , limited  : Bool
    , free     : Bool
    , ascendUp : Ascension
    , skillUp  : Reinforcement
    }

type alias OrdServant = Int

ordServant : Servant -> OrdServant
ordServant = .id

eqServant : Servant -> Servant -> Bool
eqServant = on (==) ordServant

type Deck = Deck Card Card Card Card Card

type alias Ratings = 
    { damage     : Int 
    , np         : Int
    , critical   : Int
    , utility    : Int
    , support    : Int
    , durability : Int
    }

type alias NoblePhantasm =
    { name   : String
    , desc   : String
    , rank   : Rank
    , card   : Card
    , kind   : String
    , hits   : Int
    , effect : List SkillEffect
    , over   : List SkillEffect
    , first  : Bool
    }

type alias Hits = 
    { arts   : Int
    , buster : Int
    , quick  : Int
    , ex     : Int 
    }

type alias Gen =
    { starWeight : Int
    , starRate   : Float
    , npAtk      : Float
    , npDef      : Int
    }

type PhantasmType = SingleTarget | MultiTarget | Support

type Ascension
    = Welfare String
    | Clear String String String String
    | Ascension
      (List (Material, Int)) -- 1
      (List (Material, Int)) -- 2
      (List (Material, Int)) -- 3
      (List (Material, Int)) -- 4

type Reinforcement
    = Reinforcement
      (List (Material, Int)) -- 1
      (List (Material, Int)) -- 2
      (List (Material, Int)) -- 3
      (List (Material, Int)) -- 4
      (List (Material, Int)) -- 5
      (List (Material, Int)) -- 6
      (List (Material, Int)) -- 7
      (List (Material, Int)) -- 8
      -- 9 is always [ (CrystallizedLore: 1) ]

getDeck : Servant -> List Card
getDeck {deck} = let (Deck a b c d e) = deck in [a, b, c, d, e]

reduceMats : List (List (Material, Int)) -> List (Material, Int)
reduceMats = 
  let
    reduce : ((Material, Int), List (Material, Int)) -> (Material, Int)
    reduce ((x, y), xs) = (x, List.sum <| y :: List.map Tuple.second xs)
  in
    List.concat
    >> List.sortBy (Tuple.first >> ordMaterial)
    >> List.groupWhile (on (==) Tuple.first)
    >> List.map reduce

getAscensions : Servant -> List (List (Material, Int))
getAscensions {ascendUp} = case ascendUp of
    Ascension a b c d -> [a, b, c, d]
    _                 -> []
  
getReinforcements : Servant -> List (List (Material, Int))
getReinforcements {skillUp} = 
  let
    (Reinforcement a b c d e f g h) = skillUp
  in 
    [a, b, c, d, e, f, g, h, [ (CrystallizedLore, 1) ]]

getMaterials : Servant -> List Material
getMaterials s = 
    getAscensions s ++ getReinforcements s
    |> List.concat
    >> List.map Tuple.first
    >> List.uniqueBy ordMaterial

phantasmType : NoblePhantasm -> PhantasmType
phantasmType np =
  let
    match target a = case a of
      To x Damage _        -> x == target
      To x DamageThruDef _ -> x == target
      _                    -> False
    effects = np.effect ++ np.over
  in 
    if List.any (match Enemy) effects then 
      SingleTarget
    else if List.any (match Enemies) effects then 
      MultiTarget
    else 
      Support
