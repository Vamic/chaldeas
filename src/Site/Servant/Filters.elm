module Site.Servant.Filters exposing
  ( getFilters
  , singleFilter
  )

import List.Extra  as List
import Maybe.Extra as Maybe
import Date exposing (Date)
import Time exposing (Month(..))

import StandardLibrary  exposing (..)
import Database         exposing (..)
import Database.Base    exposing (..)
import Database.Servant exposing (..)
import Database.Skill   exposing (..)
import Printing         exposing (..)
import Site.Algebra     exposing (..)
import Site.Base        exposing (..)
import Site.Common      exposing (..)
import Site.Filtering   exposing (..)

import Class.Has     as Has     exposing (Has)
import Class.ToImage as ToImage

extraFilters : List (Filter Servant)
extraFilters = List.concat
  [ [ nameFilter FilterAvailability "New"
      [ "Elisabeth Bathory (Brave)"
      , "Cleopatra"
      , "Vlad III (EXTRA)"
      ]
    , Filter Nothing FilterAvailability "Free" <|
      \_ s -> s.free
    , Filter Nothing FilterSource "Limited" <|
      \_ s -> s.limited
    , Filter Nothing FilterSource "Non-Limited" <|
      \_ s -> not s.limited
    ]
  , flip List.map (List.range 1 5) <| \rarity ->
    Filter Nothing FilterRarity (stars False rarity) <|
    \_ s -> rarity == s.rarity
  ]

scheduledFilters : List (ScheduledFilter Servant)
scheduledFilters =
  [ ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    nameFilter FilterEventBonus "+100% ATK"
    [ "Elisabeth Bathory (Brave)" ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    nameFilter FilterEventBonus "+50% ATK"
    [ "Cleopatra", "Vlad III (EXTRA)", "Nitocris", "Ibaraki-Douji"
    , "Robin Hood"
    ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    nameFilter FilterAvailability "Limited to Event"
    [ "Elisabeth Bathory (Brave)", "Cleopatra", "Vlad III (EXTRA)" ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    nameFilter FilterAvailability "Rate-Up"
    [ "Cleopatra", "Vlad III (EXTRA)" ]
  , ScheduledFilter (Date 2018 Nov 8) (Date 2018 Nov 10) <|
    nameFilter FilterAvailability "Rate-Up"
    [ "Altria Pendragon", "Mordred", "Altria Pendragon (Lancer)"
    , "Altria Pendragon (Lancer Alter)", "Lancelot (Saber)", "Gawain", "Tristan"
    , "Altria Pendragon (Lancer Alter)", "Lancelot" ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    Filter Nothing FilterEventBonus "Female or Nonbinary" <|
    \_ s -> s.gender /= Male
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    Filter Nothing FilterEventBonus "Male or Nonbinary" <|
    \_ s -> s.gender /= Female
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    Filter Nothing FilterEventBonus "Not King" <|
    \_ s -> not <| List.member King s.traits
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    nameFilter FilterEventBonus "Westerner"
    [ "Altria Pendragon", "Altria Pendragon (Alter)", "Altria Pendragon (Lily)"
    , "Nero Claudius", "Siegfried", "Gaius Julius Caesar", "Gilles de Rais"
    , "Chevalier d'Eon", "Fergus mac Roich", "Mordred", "Nero Claudius (Bride)"
    , "Lancelot (Saber)", "Gawain", "Bedivere", "Elisabeth Bathory (Brave)"

    , "Robin Hood", "Atalante", "Euryale", "Orion", "Nikola Tesla"
    , "Billy the Kid", "Tristan", "Altria Pendragon (Archer)"
    , "Anne Bonny & Mary Read (Archer)"

    , "Cu Chulainn", "Elisabeth Bathory", "Cu Chulainn (Prototype)"
    , "Leonidas I", "Romulus", "Hektor", "Scathach", "Diarmuid Ua Duibhne"
    , "Altria Pendragon (Lancer Alter)", "Fionn mac Cumhaill", "Brynhild"
    , "Altria Pendragon (Lancer)", "Vlad III (EXTRA)"

    , "Medusa", "Georgios", "Edward Teach", "Boudica", "Alexander"
    , "Marie Antoinette", "Martha", "Francis Drake", "Anne Bonny & Mary Read"
    , "Altria Pendragon (Santa Alter)", "Astolfo", "Queen Medb", "Iskandar"
    , "Ozymandias", "Mordred (Rider)"

    , "Medea", "Gilles de Rais (Caster)", "Hans Christian Andersen"
    , "William Shakespeare", "Mephistopheles", "Wolfgang Amadeus Mozart"
    , "Cu Chulainn (Caster)", "Elisabeth Bathory (Halloween)", "Medea (Lily)"
    , "Nursery Rhyme", "Paracelsus von Hohenheim", "Charles Babbage"
    , "Helena Blavatsky", "Thomas Edison", "Geronimo"
    , "Irisviel (Dress of Heaven)", "Nitocris", "Leonardo da Vinci"
    , "Marie Antoinette (Caster)"

    , "Stheno", "Charles-Henri Sanson", "Phantom of the Opera", "Mata Hari"
    , "Carmilla", "Jack the Ripper", "Henry Jekyll & Hyde"
    , "Mysterious Heroine X", "Scathach (Assassin)", "Cleopatra"

    , "Heracles", "Lancelot", "Spartacus", "Vlad III", "Asterios", "Caligula"
    , "Eric Bloodaxe", "Frankenstein", "Beowulf", "Florence Nightingale"
    , "Cu Chulainn (Alter)"

    , "Jeanne d'Arc", "Martha (Ruler)"
    , "Edmond Dantes", "Jeanne d'Arc (Alter)"
    , "Mash Kyrielight"
    ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    nameFilter FilterEventBonus "Da Vinci's Selection"
    [ "Siegfried", "Gaius Julius Caesar", "Gilles de Rais", "Chevalier d'Eon"
    , "Fergus mac Roich", "Rama", "Lancelot (Saber)", "Gawain", "Bedivere"

    , "EMIYA", "Gilgamesh", "Robin Hood", "Arash", "David", "Nikola Tesla"
    , "Arjuna", "Gilgamesh (Child)", "Billy the Kid", "Tristan", "Tawara Touta"

    , "Cu Chulainn", "Musashibou Benkei", "Cu Chulainn (Prototype)"
    , "Leonidas I", "Romulus", "Hektor", "Diarmuid Ua Duibhne", "Karna"
    , "Fionn mac Cumhaill", "Li Shuwen", "Vlad III (EXTRA)"

    , "Georgios", "Ushiwakamaru", "Alexander", "Astolfo", "Iskandar"
    , "Sakata Kintoki (Rider)", "Ozymandias"

    , "Hans Christian Andersen", "William Shakespeare"
    , "Wolfgang Amadeus Mozart", "Zhuge Liang (Lord El-Melloi II)"
    , "Cu Chulainn (Caster)", "Paracelsus von Hohenheim", "Thomas Edison"
    , "Geronimo", "Sasaki Kojirou", "Henry Jekyll & Hyde", "Emiya (Assassin)"
    , "Fuuma \"Evil-wind\" Kotarou"

    , "Lancelot", "Sakata Kintoki", "Vlad III", "Beowulf", "Cu Chulainn (Alter)"

    , "Amakusa Shirou"
    , "Edmond Dantes"
    ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    nameFilter FilterEventBonus "165 cm or under"
    [ "Altria Pendragon", "Altria Pendragon (Alter)", "Altria Pendragon (Lily)"
    , "Nero Claudius", "Altera", "Chevalier d'Eon", "Okita Souji", "Mordred"
    , "Nero Claudius (Bride)", "Ryougi Shiki (Saber)"
    , "Elisabeth Bathory (Brave)"

    , "Euryale", "Orion", "Oda Nobunaga", "Gilgamesh (Child)", "Billy the Kid"
    , "Altria Pendragon (Archer)", "Chloe von Einzbern"

    , "Elisabeth Bathory", "Tamamo-no-Mae (Lancer)", "Kiyohime (Lancer)"

    , "Alexander", "Marie Antoinette", "Martha", "Francis Drake"
    , "Anne Bonny & Mary Read", "Altria Pendragon (Santa Alter)", "Astolfo"
    , "Queen Medb", "Mordred (Rider)"

    , "Medea", "Hans Christian Andersen", "Zhuge Liang (Lord El-Melloi II)"
    , "Elisabeth Bathory (Halloween)", "Tamamo-no-Mae", "Medea (Lily)"
    , "Nursery Rhyme", "Helena Blavatsky", "Irisviel (Dress of Heaven)"
    , "Xuanzang Sanzang", "Nitocris", "Leonardo da Vinci"
    , "Marie Antoinette (Caster)", "Illyasviel von Einzbern"

    , "Stheno", "Jing Ke", "Mata Hari", "Jack the Ripper"
    , "Mysterious Heroine X", "Ryougi Shiki (Assassin)", "Shuten-Douji"
    , "Fuuma \"Evil-wind\" Kotarou", "Hassan of the Serenity"

    , "Kiyohime", "Tamamo Cat", "Florence Nightingale", "Ibaraki-Douji"

    , "Jeanne d'Arc", "Martha (Ruler)"
    , "Jeanne d'Arc (Alter)"
    , "Mash Kyrielight"
    ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    nameFilter FilterEventBonus "Newcomer"
    [ "Gaius Julius Caesar", "Altera", "Gilles de Rais", "Chevalier d'Eon"
    , "Okita Souji", "Fergus mac Roich", "Ryougi Shiki (Saber)", "Rama"
    , "Lancelot (Saber)", "Bedivere", "Elisabeth Bathory (Brave)"

    , "Orion", "David", "Oda Nobunaga", "Nikola Tesla", "Arjuna"
    , "Billy the Kid", "Tristan", "Tawara Touta", "Altria Pendragon (Archer)"
    , "Anne Bonny & Mary Read (Archer)", "Chloe von Einzbern"

    , "Musashibou Benkei", "Leonidas I", "Romulus", "Hektor", "Scathach"
    , "Altria Pendragon (Lancer Alter)", "Fionn mac Cumhaill"
    , "Altria Pendragon (Lancer)", "Tamamo-no-Mae (Lancer)", "Kiyohime (Lancer)"

    , "Georgios", "Edward Teach", "Boudica", "Ushiwakamaru", "Alexander"
    , "Marie Antoinette", "Martha", "Anne Bonny & Mary Read"
    , "Altria Pendragon (Santa Alter)", "Queen Medb", "Sakata Kintoki (Rider)"
    , "Mordred (Rider)"

    , "Mephistopheles", "Wolfgang Amadeus Mozart", "Cu Chulainn (Caster)"
    , "Elisabeth Bathory (Halloween)", "Medea (Lily)", "Charles Babbage"
    , "Helena Blavatsky", "Thomas Edison", "Geronimo", "Xuanzang Sanzang"
    , "Nitocris", "Leonardo da Vinci", "Marie Antoinette (Caster)"
    , "Illyasviel von Einzbern"

    , "Jing Ke", "Charles-Henri Sanson", "Phantom of the Opera", "Mata Hari"
    , "Carmilla", "Ryougi Shiki (Assassin)", "Emiya (Assassin)", "Shuten-Douji"
    , "Fuuma \"Evil-wind\" Kotarou", "Scathach (Assassin)", "Cleopatra"

    , "Sakata Kintoki", "Asterios", "Caligula", "Darius III", "Kiyohime"
    , "Eric Bloodaxe", "Beowulf", "Florence Nightingale", "Cu Chulainn (Alter)"
    , "Minamoto-no-Raikou", "Ibaraki-Douji"

    , "Martha (Ruler)"
    , "Edmond Dantes", "Jeanne d'Arc (Alter)"
    , "Mash Kyrielight"
    ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    nameFilter FilterEventBonus "Ally of Justice"
    [ "Altria Pendragon", "Altria Pendragon (Lily)", "Mordred", "Siegfried"
    , "Altera", "Gilles de Rais", "Rama", "Lancelot (Saber)", "Gawain"
    , "Bedivere", "Elisabeth Bathory (Brave)"

    , "EMIYA", "Gilgamesh", "Robin Hood", "Euryale", "Nikola Tesla"
    , "Gilgamesh (Child)", "Tristan", "Tawara Touta"
    , "Altria Pendragon (Archer)", "Chloe von Einzbern"

    , "Musashibou Benkei", "Scathach", "Altria Pendragon (Lancer Alter)"
    , "Karna", "Brynhild", "Altria Pendragon (Lancer)", "Vlad III (EXTRA)"

    , "Medusa", "Georgios", "Boudica", "Alexander", "Marie Antoinette"
    , "Martha", "Altria Pendragon (Santa Alter)", "Astolfo", "Iskandar"
    , "Sakata Kintoki (Rider)", "Mordred (Rider)"

    , "Wolfgang Amadeus Mozart", "Zhuge Liang (Lord El-Melloi II)"
    , "Medea (Lily)", "Paracelsus von Hohenheim", "Helena Blavatsky", "Geronimo"
    , "Irisviel (Dress of Heaven)", "Xuanzang Sanzang", "Nitocris"
    , "Leonardo da Vinci", "Marie Antoinette (Caster)"
    , "Illyasviel von Einzbern"

    , "Stheno", "Jing Ke", "Henry Jekyll & Hyde", "Mysterious Heroine X"
    , "Ryougi Shiki (Assassin)", "Emiya (Assassin)", "Scathach (Assassin)"

    , "Heracles", "Sakata Kintoki", "Astolfo", "Beowulf", "Florence Nightingale"
    , "Ibaraki-Douji", "Minamoto-no-Raikou", "Tamamo Cat"

    , "Jeanne d'Arc", "Amakusa Shirou", "Martha (Ruler)"
    , "Mash Kyrielight"
    ]
  ]

singleFilter : Has Servant a -> FilterTab -> a -> List (Filter Servant)
singleFilter has tab x =
  if exclusive tab then
    getAll has |> List.remove x >> List.map (matchFilter Nothing has tab)
  else
    [matchFilter Nothing has tab x]

getExtraFilters : Date -> FilterTab -> List (Filter Servant)
getExtraFilters today tab =
    getScheduled scheduledFilters today ++ extraFilters
    |> List.filter (.tab >> (==) tab)

getFilters : Date -> FilterTab -> List (Filter Servant)
getFilters today tab =
  let
    allEffects has toImage pred =
        getAll (has Has.servant)
        |> List.filter pred
        >> List.map (matchFilter toImage (has Has.servant) tab)
    all has toImage =
        getAll has
        |> List.map (matchFilter toImage has tab)
  in case tab of
    FilterAlignment    -> all Has.alignment Nothing
    FilterAttribute    -> all Has.attribute Nothing
    FilterCard         -> all Has.card <| Just ToImage.card
    FilterClass        -> all Has.class <| Just ToImage.class
    FilterDeck         -> all Has.deck Nothing
    FilterPhantasm     -> all Has.phantasmType Nothing
    FilterTrait        -> all Has.trait Nothing
    FilterPassiveSkill -> all Has.passive << Just <| ToImage.icon << .icon
    FilterMaterial     -> all Has.material <| Just ToImage.material

    FilterDebuff -> allEffects Has.debuffEffect (Just ToImage.debuffEffect) <|
                    always True
    FilterBuff c -> allEffects Has.buffEffect (Just ToImage.buffEffect) <|
                    buffCategory >> (==) c
    FilterAction -> allEffects Has.instantEffect Nothing <|
                    not << isDamage
    FilterDamage -> allEffects Has.instantEffect Nothing <|
                    isDamage
    _            -> getExtraFilters today tab
