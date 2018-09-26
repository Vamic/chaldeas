module Site.Servants.Filters
  ( getFilters
  , passiveFilter
  , singleFilter
  ) where

import Database
import Site.Algebra
import Site.Common
import Site.Filtering
import Site.ToImage
import StandardLibrary

import Data.Date (Date, Month(..))
import Data.String as String

extraFilters :: Array (Filter Servant)
extraFilters = join
  [ [ namedBonus FilterAvailability "New"
      [ "Illyasviel von Einzbern"
      , "Chloe von Einzbern"
      ]
    , simpleFilter FilterAvailability "Free"
      \_ (Servant s) -> s.free
    ]
  , [ simpleFilter FilterSource "Limited"
      \_ (Servant s) -> s.limited
    , simpleFilter FilterSource "Non-Limited"
      \_ (Servant s) -> not s.limited
  ]
  , reverse (1..5) <#> \rarity
    -> simpleFilter FilterRarity (String.joinWith "" $ replicate rarity "★")
    \_ (Servant s) -> rarity == s.rarity
  ]

scheduledFilters :: Array (ScheduledFilter Servant)
scheduledFilters =
  [ ScheduledFilter (ymd 2018 September 13) (ymd 2018 September 26) $
    namedBonus FilterAvailability "Limited to Event"
    [ "Nero Claudius (Bride)" ]

  , ScheduledFilter (ymd 2018 September 13) (ymd 2018 September 26) $
    namedBonus FilterAvailability "Rate-Up"
    [ "Nero Claudius (Bride)", "Nero Claudius"
    , "Gaius Julius Caesar", "Romulus", "Boudica"
    ]

  , ScheduledFilter (ymd 2018 September 26) (ymd 2018 September 26) $
    namedBonus FilterAvailability "Rate-Up"
    [ "Altria Pendragon", "Altria Pendragon (Alter)", "EMIYA", "Heracles"
    , "Cu Chulainn", "Medusa", "Medea", "Cu Chulainn (Caster)"
    ]
  , ScheduledFilter (ymd 2018 September 27) (ymd 2018 September 27) $
    namedBonus FilterAvailability "Rate-Up"
    [ "Jeanne d'Arc", "Siegfried", "Chevalier d'Eon", "Gilles de Rais"
    , "Gilles de Rais (Caster)"
    , "Kiyohime"
    ]
  , ScheduledFilter (ymd 2018 September 28) (ymd 2018 September 28) $
    namedBonus FilterAvailability "Rate-Up"
    [ "Altera", "Nero Claudius", "Gaius Julius Caesar", "Romulus", "Boudica" ]
  , ScheduledFilter (ymd 2018 September 29) (ymd 2018 September 29) $
    namedBonus FilterAvailability "Rate-Up"
    [ "Francis Drake", "Anne Bonny & Mary Read", "Medea (Lily)", "Euryale"
    , "David", "Hektor"
    ]
  , ScheduledFilter (ymd 2018 September 30) (ymd 2018 September 30) $
    namedBonus FilterAvailability "Rate-Up"
    [ "Mordred", "Nikola Tesla", "Altria Pendragon (Lancer Alter)"
    , "Nursery Rhyme", "Frankenstein", "Henry Jekyll & Hyde"
    , "Paracelsus von Hohenheim", "Charlies Babbage"
    ]
  , ScheduledFilter (ymd 2018 October 1) (ymd 2018 October 1) $
    namedBonus FilterAvailability "Rate-Up"
    [ "Queen Medb", "Florence Nightingale", "Cu Chulainn (Alter)", "Rama"
    , "Li Shuwen", "Helena Blavatsky", "Thomas Edison", "Billy the Kid"
    , "Geronimo"
    ]
  , ScheduledFilter (ymd 2018 October 2) (ymd 2018 October 2) $
    namedBonus FilterAvailability "Rate-Up"
    [ "Altria Pendragon (Lancer)", "Ozymandias", "Gawain", "Lancelot", "Tristan"
    , "Nitocris", "Bedivere", "Tawara Touta", "Hassan of the Serenity"
    ]
  ]

passiveFilter :: Skill -> Filter Servant
passiveFilter p =
    Filter { icon: Just $ toImagePath p.icon
           , tab:  FilterPassiveSkill
           , name: p.name
           , match: \_ (Servant s) ->
                   any (eq p.name) $ _.name <$>  s.passives
           }

singleFilter :: ∀ a. Has Servant a => FilterTab -> a -> Array (Filter Servant)
singleFilter tab x
  | exclusive tab = matchFilter tab <$> delete x getAll
  | otherwise = [matchFilter tab x]

getExtraFilters :: Date -> FilterTab -> Array (Filter Servant)
getExtraFilters today tab =
    filter fromTab $ getScheduled scheduledFilters today <> extraFilters
  where
    fromTab (Filter x) = tab == x.tab

getFilters :: Date -> FilterTab -> Array (Filter Servant)
getFilters _ f@FilterAlignment    = matchFilter f
                                    <$> getAll :: Array Alignment
getFilters _ f@FilterAttribute    = matchFilter f
                                    <$> getAll :: Array Attribute
getFilters _ f@FilterCard         = imageFilter f
                                    <$> getAll :: Array Card
getFilters _ f@FilterClass        = imageFilter f
                                    <$> getAll :: Array Class
getFilters _ f@FilterDebuff       = imageFilter f
                                    <$> getAll :: Array DebuffEffect
getFilters _ f@FilterDeck         = matchFilter f
                                    <$> getAll :: Array Deck
getFilters _ f@FilterPhantasm     = matchFilter f
                                    <$> getAll :: Array PhantasmType
getFilters _ f@FilterTrait        = matchFilter f
                                    <$> getAll :: Array Trait
getFilters _ f@FilterPassiveSkill = passiveFilter
                                    <$> getPassives
getFilters _ f@(FilterBuff c)     = imageFilter f
                                    <$> filter (eq c <<< buffCategory)
                                        getAll :: Array BuffEffect
getFilters _ f@FilterAction       = matchFilter f
                                    <$> filter (not <<< isDamage)
                                        getAll :: Array InstantEffect
getFilters _ f@FilterDamage       = matchFilter f
                                    <$> filter isDamage
                                        getAll :: Array InstantEffect
getFilters _ f@FilterMaterial     = imageFilter f
                                    <$> getAll :: Array Material
getFilters today f               = getExtraFilters today f
