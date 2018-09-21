-- | This module defines the data structure of Servants.
-- It does not contain actual database information from the game; that can
-- be found in the [Database.Servant](./Servant) _folder_, which has a
-- separate file for each Class.
module Database.Servant
  ( Servant(..)
  , Deck(..)
  , NoblePhantasm
  , Gen
  , Hits
  , PhantasmType(..)
  , Ascension(..), Reinforcement(..)
  , getDeck, getMaterials
  ) where

import StandardLibrary
import Data.String.CodeUnits as CodeUnits
import Generic               as G

import Database.Base
import Database.Skill

newtype Servant =
    Servant { name     :: String
            , id       :: Int
            , rarity   :: Int
            , class    :: Class
            , attr     :: Attribute
            , deck     :: Deck
            , curve    :: Int
            , stats    :: { base :: Stat, max :: Stat, grail :: Stat }
            , skills   :: Array Skill
            , passives :: Array Skill
            , phantasm :: NoblePhantasm
            , gen      :: Gen
            , hits     :: Hits
            , traits   :: Array Trait
            , death    :: Number
            , align    :: Array Alignment
            , limited  :: Boolean
            , free     :: Boolean
            , ascendUp :: Ascension
            , skillUp  :: Reinforcement
            }

instance _0_ :: Show Servant where
    show (Servant s) = s.name
instance _1_ :: Eq Servant where
    eq (Servant x) (Servant y) = eq x.id y.id
instance _2_ :: Ord Servant where
    compare = comparing \(Servant x) -> x.id

data Deck = Deck Card Card Card Card Card

data Ratings = Ratings { damage     :: Int
                        , np         :: Int
                        , critical   :: Int
                        , utility    :: Int
                        , support    :: Int
                        , durability :: Int
                        }

type NoblePhantasm = { name   :: String
                     , desc   :: String
                     , rank   :: Rank
                     , card   :: Card
                     , kind   :: String
                     , hits   :: Int
                     , effect :: Array SkillEffect
                     , over   :: Array SkillEffect
                     , first  :: Boolean
                     }

type Hits = { arts :: Int, buster :: Int, quick :: Int, ex :: Int }

type Gen = { starWeight :: Int
           , starRate   :: Number
           , npAtk      :: Number
           , npDef      :: Int
           }

data PhantasmType = SingleTarget | MultiTarget | Support
instance _01_ :: Show PhantasmType where
      show SingleTarget = "Single-Target"
      show MultiTarget  = "Multi-Target"
      show Support      = "Support"

-- type Item = Array (Material : Int)

data Ascension     = Welfare String
                   | Clear String String String String
                   | Ascension
                     (Array (Material : Int)) -- 1
                     (Array (Material : Int)) -- 2
                     (Array (Material : Int)) -- 3
                     (Array (Material : Int)) -- 4
data Reinforcement = Reinforcement
                     (Array (Material : Int)) -- 1
                     (Array (Material : Int)) -- 2
                     (Array (Material : Int)) -- 3
                     (Array (Material : Int)) -- 4
                     (Array (Material : Int)) -- 5
                     (Array (Material : Int)) -- 6
                     (Array (Material : Int)) -- 7
                     (Array (Material : Int)) -- 8
                     -- 9 is always [ CrystallizedLore: 1 ]

-- | Returns all `Card`s in a `Servant`'s `Deck`. Does not include NP card.
getDeck :: Servant -> Array Card
getDeck (Servant {deck:Deck a b c d e}) = [a, b, c, d, e]

getMaterials :: Servant -> Array Material
getMaterials (Servant {ascendUp, skillUp}) = nub $ fst <$> ascendUps <> skillUps
  where
    ascendUps = case ascendUp of
        Ascension a b c d -> join [a, b, c, d]
        _                 -> []
    skillUps = case skillUp of
        Reinforcement a b c d e f g h -> join [a, b, c, d, e, f, g, h]

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _7_ :: Eq PhantasmType
derive instance _8_ :: Ord PhantasmType
derive instance _9_ :: G.Generic PhantasmType _
instance _10_ :: G.Enum PhantasmType where
    succ = G.genericSucc
    pred = G.genericPred
instance _11_ :: G.Bounded PhantasmType where
    top = G.genericTop
    bottom = G.genericBottom
instance _12_ :: G.BoundedEnum PhantasmType where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _34_ :: Eq Deck
derive instance _35_ :: Ord Deck
derive instance _36_ :: G.Generic Deck _
instance _37_ :: G.Enum Deck where
    succ = G.genericSucc
    pred = G.genericPred
instance _38_ :: G.Bounded Deck where
    top = G.genericTop
    bottom = G.genericBottom
instance _39_ :: G.BoundedEnum Deck where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum
instance _40_ :: Show Deck where
    show (Deck a b c d e) = CodeUnits.fromCharArray $
      (fromMaybe '?' <<< CodeUnits.charAt 0 <<< show) <$> [a, b, c, d, e]
