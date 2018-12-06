import Database.Base exposing (..)
import Database.Passive exposing (..)
import Database.Servant exposing (..)
import Database.Skill exposing (..)

ø : a
ø = Debug.todo "Missing Servant information!"

todo : List Servant
todo =
  [ 
  -- Template
    { name      = ø
    , id        = ø
    , rarity    = ø
    , class     = ø
    , stats     = { base  = { atk = ø,  hp = ø }
                  , max   = { atk = ø,  hp = ø }
                  , grail = { atk = ø, hp = ø }
                  }
    , gen       = { starWeight = ø, starRate = ø, npAtk = ø, npDef = ø }
    , death     = ø
    , curve     = ø
    , attr      = ø
    , align     = ø
    , gender    = ø
    , traits    = ø
    , deck      = Deck ø ø ø ø ø
    , hits      = { quick = ø, arts = ø, buster = ø, ex = ø }
    , skills    = [ { name   = ø
                    , rank   = ø
                    , icon   = ø
                    , cd     = ø
                    , effect = ø
                    }
                  , { name   = ø
                    , rank   = ø
                    , icon   = ø
                    , cd     = ø
                    , effect = ø
                    }
                  , { name   = ø
                    , rank   = ø
                    , icon   = ø
                    , cd     = ø
                    , effect = ø
                    }
                  ]
    , passives  = ø
    , phantasm  = { name   = ø
                  , desc   = ø
                  , rank   = ø
                  , card   = ø
                  , kind   = ø
                  , hits   = ø
                  , effect = ø
                  , over   = ø
                  , first  = ø
                  }
    , limited   = ø
    , free      = ø
    , ascendUp  = ø
    , skillUp   = ø
    }
  ]
