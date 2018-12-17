module Database.Calculator exposing (npPer, starsPer, npDamage)

{-| Calculates information for sorting based on datamined formulas. -}

import StandardLibrary  exposing (..)
import Database.Base    exposing (..)
import Database.Skill   exposing (..)
import Database.Servant exposing (..)

{-| Formula source: [Beast's Lair: Mining for Bits, by Kyte](http://blogs.nrvnqsr.com/entry.php/3306-How-much-NP-do-I-get-in-combat) -}
npPer : Servant -> Card -> Float
npPer s card =
  let
    offensiveNPRate  = s.gen.npAtk
    firstCardBonus   = 1
    cardNpValue      = 1.5 * case card of
                               Arts   -> 3
                               Quick  -> 1
                               Buster -> 0
    cardMod          = matchSum buffs <| Performance card
    enemyServerMod   = 1
    npChargeRateMod  = matchSum buffs NPGen
    criticalModifier = 1
    overkillModifier = 1
    buffs            = passiveBuffs s
  in
    toFloat s.hits.arts
    * offensiveNPRate
    * (firstCardBonus + (cardNpValue * (1 + cardMod)))
    * enemyServerMod
    * (1 + npChargeRateMod)
    * criticalModifier
    * overkillModifier

{-| Formula source: [Beast's Lair: Mining for Bits, by Kyte](http://blogs.nrvnqsr.com/entry.php/3307-How-many-crit-stars-do-I-get-in-combat) -}
starsPer : Servant -> Card -> Float
starsPer s card =
  let
    baseStarRate     = s.gen.starRate / 100
    firstCardBonus   = 0.2
    cardStarValue    = case card of
                         Quick  -> 1.3
                         Arts   -> 0.15
                         Buster -> 0
    cardMod          = matchSum buffs <| Performance card
    serverRate       = 0
    starDropMod      = matchSum buffs StarUp
    enemyStarDropMod = 0
    criticalModifier = 0
    overkillModifier = 1
    overkillAdd      = 0
    buffs            = passiveBuffs s
  in
    toFloat s.hits.quick
    * min 3 ( baseStarRate
            + firstCardBonus
            + (cardStarValue * (1 + cardMod))
            + serverRate
            + starDropMod
            - enemyStarDropMod
            + criticalModifier
            )
    * overkillModifier
    + overkillAdd

{-| Formula source: [Beast's Lair: Mining for Bits, by Kyte](http://blogs.nrvnqsr.com/entry.php/3309-How-is-damage-calculated) -}
npDamage : Bool -> Bool -> Bool -> Servant -> Float
npDamage addSkills special maxOver s =
  let
    --------------------
    -- FROM YOUR SERVANT
    --------------------
    servantAtk = toFloat s.stats.max.atk
    classAtkBonus = case s.class of
                      Berserker -> 1.1
                      Ruler     -> 1.1
                      Avenger   -> 1.1
                      Lancer    -> 1.05
                      Archer    -> 0.95
                      Caster    -> 0.9
                      Assassin  -> 0.9
                      _         -> 1
    triangleModifier =
        ifSpecial enumClass
        |> List.map (VsClass >> Special AttackUp >> matchSum buffs)
        >> List.sum
        >> (+) 1
    attributeModifier = 1
    -------------
    -- FROM CARDS
    -------------
    firstCardBonus = 0
    cardDamageValue = case card of
                          Quick  -> 0.8
                          Arts   -> 1
                          Buster -> 1.5
    busterChainMod = 0
    extraCardModifier = 1
    -----------
    -- FROM RNG
    -----------
    randomModifier   = 1
    criticalModifier = 1
    ---------------------
    -- FROM NP PROPERTIES
    ---------------------
    npDamageMultiplier =
        [Damage, DamageThruDef]
        |> (if not special then identity else (::) LastStand)
        >> List.map (matchSum instants)
        >> List.sum
    superEffectiveModifier =
        ifSpecial enumSpecial
        |> List.map (SpecialDamage >> matchSum instants)
        >> List.maximum
        >> Maybe.withDefault 0
        >> (+) (matchSum instants DamagePoison)
        >> (+) 1
    isSuperEffective = 1.0
    directDamage =
        toFloat (max s.stats.base.hp s.stats.max.hp) * matchSum instants Avenge
    -------------
    -- FROM BUFFS
    -------------
    cardMod = matchSum buffs <| Performance card
    atkMod =
        ifSpecial enumSpecial
        |> List.map (Special AttackUp >> matchSum buffs)
        >> List.maximum
        >> Maybe.withDefault 0
        >> (+) (matchSum buffs AttackUp)
    defMod        = -1 * matchSum debuffs DefenseDown
    specialDefMod = -0
    powerMod      = 0
    selfDamageMod = 0
    isCrit        = 0
    critDamageMod = 0
    isNP          = 1
    npDamageMod   = matchSum buffs NPUp
    dmgPlusAdd    =
        100 * (matchSum buffs DamageUp + matchSum debuffs DamageVuln)
    selfDmgCutAdd = 0
    -----------
    -- INTERNAL
    -----------
    {card, effect, over, first} = s.phantasm

    costsCharge ef = case ef of
      To _ GaugeSpend _ -> True
      _                 -> False
    unCost efs   = if List.any costsCharge efs then [] else efs
    ifSpecial    = if special then identity else always []
    maxIf x      = if x then toMax else toMin
    npStrength   = maxIf <| s.free || (s.rarity <= 3 && s.rarity > 0)
    overStrength = maxIf maxOver
    skillFs      =
        s.passives
        |> List.concatMap .effect
        >> ( if not addSkills then identity else 
             (++) <| List.concatMap (.effect >> unCost) s.skills
           )
        >> List.map simplify
    npFs        = List.map simplify effect
    overFs      = List.map simplify over
    firstFs     = if first then List.take 1 overFs else []
    buffs       =
      let
        go f a = case a of
          Grant t _ buff n -> if selfable t then [(buff, f n / 100)] else []
          _ -> []
      in
        List.concatMap (go toMax) skillFs
        ++ List.concatMap (go npStrength) npFs
        ++ List.concatMap (go overStrength) firstFs
    debuffs     =
      let
        go f a = case a of
          Debuff t _ debuff n ->
              if not (allied t) then [(debuff, f n / 100)] else []
          _ -> []
      in
        List.concatMap (go toMax) skillFs
        ++ List.concatMap (go npStrength) npFs
        ++ List.concatMap (go overStrength) firstFs
    instants    =
      let
        go f a = case a of
          To t instant n ->
              if not (allied t) then [(instant, f n / 100)] else []
          _ -> []
      in
        List.concatMap (go toMax) skillFs
        ++ List.concatMap (go npStrength) npFs
        ++ List.concatMap (go overStrength) overFs
  in
    if npDamageMultiplier + directDamage == 0 then
      0
    else
      servantAtk
      * npDamageMultiplier
      * ( firstCardBonus + (cardDamageValue * (1.0 + cardMod)) )
      * classAtkBonus
      * triangleModifier
      * attributeModifier
      * randomModifier
      * 0.23
      * ( 1.0 + atkMod - defMod )
      * criticalModifier
      * extraCardModifier
      * ( 1.0 - specialDefMod )
      * ( 1.0
        + powerMod
        + selfDamageMod
        + (critDamageMod * isCrit)
        + (npDamageMod * isNP)
        )
      * ( 1.0 + ((superEffectiveModifier - 1.0) * isSuperEffective) )
      + dmgPlusAdd
      + selfDmgCutAdd
      + ( servantAtk * busterChainMod )
      + directDamage

{-| Obtains all self-granted always-active buff effects from passive skills.
Returns an array of (Buff, Strength%) pairs. -}
passiveBuffs : Servant -> List (BuffEffect, Float)
passiveBuffs s =
  let
    go a = case a of
      Grant t _ buff n -> if selfable t then [(buff, toMax n / 100)] else []
      _ -> []
  in
    s.passives |> List.concatMap .effect |> List.concatMap (simplify >> go)

{-| Attacker vs. Defender. Currently not in use. -}
-- TODO figure out how to use this
attributeBonus : Attribute -> Attribute -> Float
attributeBonus a b = case (a, b) of
    (Mankind, Heaven)  -> 1.1
    (Heaven,  Earth)   -> 1.1
    (Earth,   Mankind) -> 1.1
    (Mankind, Earth)   -> 0.9
    (Earth,   Heaven)  -> 0.9
    (Heaven,  Mankind) -> 0.9
    {-
    (Star,    Beast)   -> 1.1
    (Beast,   Star)    -> 1.1
    (Beast,   Beast)   -> 0
    -}
    _                  -> 1

{-| If a skill's target is not `Self`, `Ally`, or `Party`,
it cannot be self-applied and therefore should not be used in calculations. -}
selfable : Target -> Bool
selfable a = case a of
  Self  -> True
  Ally  -> True
  Party -> True
  _     -> False

{-| Sums up all effects of a certain type from a Servant's skills
in (Effect, Strength%) format. -}
matchSum : List (a, Float) -> a -> Float
matchSum xs k = List.sum <| List.map (\(k1, v) -> if k == k1 then v else 0) xs
