module Test.Sample where

import Prelude

import Data.Array (find)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

import Database (Servant, servants)

import Test.Base (MaybeRank(..))
import Test.Wiki (Wiki, toWiki)

sasaki :: Servant
sasaki = unsafePartial fromJust $ find (eq "Sasaki Kojirou" <<< show) servants

testWiki :: Wiki
testWiki = toWiki rawArticle Unranked

rawArticle :: String
rawArticle = """
{{ServantPageTabs|{{FULLPAGENAME}}|General|S}}
''This article is for the 4{{Star}} {{Caster}} Medea, for the 3{{Star}} {{CasterS}} variant, see [[Medea]].''
{{ServantBox
|fool = yes
|info= Unlocked for summoning after completing the [[Quests/Okeanos|Sealed Ends of the Four Seas: Okeanos]] Story Chapter
|status=locked
|jpname = メディア［リリィ］
|voice = Nonaka Ai
|artist = Hirokazu Koyama
|class = Caster
|name = Medea (Lily)
|minatk = 1,294
|maxatk = 7,766
|minhp = 2,091
|maxhp = 13,070
|grailatk = 9,403
|grailhp = 15,847
|rarity = 4
|cost = 12
|commandcard=QAAAB
|mlevel = 80
|id = 067
|attribute = Earth
|QuickHit = 4
|ArtsHit = 4
|BusterHit = 3
|ExtraHit = 5
|deathresist = 36%
|starabsorption = 51
|stargeneration = 10.7%
|npchargeattack = 0.4%
|npchargedefense = 3%
|growth = Reverse S
|traits = Servant, Humanoid, Weak to Enuma Elish, FGO Newcomer, Ally of Justice, Under 165cm, Western, European, Greek
|active1 = Rapid Words of Divine;A
|active2 = Poison Resistance;A
|active2s = Poison Resistance;A++;jponly;Upgrades after Interlude 2
|active3 = Ephemeral Love;B
|passive1 = Territory Creation;B
|passive2 = Item Construction;B
}}

== Noble Phantasm ==
<tabber>
Rank C-=
{{NoblePhantasmTable
|skillname = Pain Breaker<br/>All Flaws Must Be Repaired
|skilltier = C-
|type = Anti-Magic
|icon = Arts
|hitcount = －
|effect0 = Removes debuffs from all allies.
|effect1 = Greatly restore all allies' HP.
|oceffect1 = Greatly increase all allies' Debuff Resist (3 turns)
|leveleffectname = Heal
|leveleffect-lvl1 = 3000 HP
|leveleffect-lvl2 = 4000 HP
|leveleffect-lvl3 = 4500 HP
|leveleffect-lvl4 = 4750 HP
|leveleffect-lvl5 = 5000 HP
|overchargename = Debuff Res +
|overcharge-lvl1 = 20%
|overcharge-lvl2 = 40%
|overcharge-lvl3 = 60%
|overcharge-lvl4 = 80%
|overcharge-lvl5 = 100%
}}
|-|
Rank C=
{{NoblePhantasmTable
|skillname = Pain Breaker<br/>All Flaws Must Be Repaired
|skilltier = C
|type = Anti-Magic
|icon = Arts
|hitcount = －
|effect1 = Removes debuffs from all allies & greatly restore all allies' HP.
|oceffect1 = Greatly increase all allies' Debuff Resist (3 turns)
|leveleffectname = Heal
|leveleffect-lvl1 = 4000 HP
|leveleffect-lvl2 = 5000 HP
|leveleffect-lvl3 = 5500 HP
|leveleffect-lvl4 = 5750 HP
|leveleffect-lvl5 = 6000 HP
|overchargename = Debuff Res +
|overcharge-lvl1 = 40%
|overcharge-lvl2 = 60%
|overcharge-lvl3 = 80%
|overcharge-lvl4 = 100%
|overcharge-lvl5 = 120%
}}
|-|
Video=
<youtube>https://www.youtube.com/watch?v=MC5HWueFPx0</youtube>
</tabber>

== Ascension ==
{{ascension
|11 = Caster Piece*4
|21 = Caster Piece*10
|22 = Void's Dust*12
|31 = Caster Monument*4
|32 = Ghost Lantern*8
|33 = Phoenix Feather*4
|41 = Caster Monument*10
|42 = Phoenix Feather*7
|43 = Forbidden Page*10
}}

==Skill Reinforcement==
{{skillup
|11 = Gem of Caster*4
|21 = Gem of Caster*10
|31 = Magic Gem of Caster*4
|41 = Magic Gem of Caster*10
|42 = Ghost Lantern*4
|51 = Secret Gem of Caster*4
|52 = Ghost Lantern*8
|61 = Secret Gem of Caster*10
|62 = Void's Dust*8
|71 = Void's Dust*16
|72 = Heart of the Foreign God*2
|81 = Heart of the Foreign God*6
|82 = Forbidden Page*20
}}

== Stats ==
{{stats
|strength = E
|stbar = 1
|endurance = E
|enbar = 1
|agility = D
|agbar = 2
|mana = A
|mabar = 5
|luck = A
|lubar = 5
|np = C
|npbar = 3
}}

== Bond Level ==
{{BondLevel
|b1 = 6,000
|b2 = 24,000
|b3 = 15,000
|b4 = 2,500
|b5 = 2,500
|b6 = 150,000
|b7 = 400,000
|b8 = 310,000
|b9 = 320,000
|b10 = 335,000
|-
|2b1 = 6,000
|2b2 = 30,000
|2b3 = 45,000
|2b4 = 47,500
|2b5 = 50,000
|2b6 = 200,000
|2b7 = 600,000
|2b8 = 910,000
|2b9 = 1,230,000
|2b10 = 1,565,000
|- 
|image = [[File:Icon CE 0318.png|75px|link=Unlimited Pancakes]]
|effect1 = '''[[Unlimited Pancakes]]'''<br/>When equipped on [[Medea (Lily)]], Increase HP Recovery Amount of all allies by 30% while on the field
}}

== Lore ==
{{Lore
|def=Medea, queen of Colchis. Here she's been summoned as a young girl, before she was known as a witch. She's innocent, pure, and beautiful, with no sign of what would later become "The Witch of Colchis."
|height=149
|weight=41
|origin=Greek Mythology
|region=Greece
|alignment=Lawful Good
|gender=Female
|b1=Medea, Age<!--sic--> 14.
|b2=Here she's been summoned as she was before Jason seduced her, when she learned magecraft from Hecate and was raised in an opulent carefree lifestyle. She knows barely any offensive spells, but excels in healing spells.
|b3=Pretty, polite, and beloved by everyone, if it weren't for Jason she would've lived a happy life.<br/>If it weren't for Jason.
|b4='''『Pain Breaker』'''<br/>'''Rank:''' C<br/>'''NP Type:''' Anti-Magecraft<br/>All Wounds Must Be Repaired.<br/>This is the Noble Phantasm in pairs with Caster Medea's "Rule Breaker."<br/>It can reduce all damage caused by curse and magical energy to 0.
|b5=Pain Breaker doesn't control time, but calculates how something "should be" in its original state then repairs it. It does appear like time control to other people. It is powerful but it cannot bring back the dead.
|int=Lily is pure and easily believes people. She approaches her Master with the maximum possible amount of kindness. However, this also indicates that she is driven by a desire to believe in others. Both Lily and the original Medea are filled with mistrust towards other people.
|intname=Close Friend of a Witch
|tldef=The princess of Colchis, Medea, summoned from a younger age, before she was called a 'witch'. Pure and innocent, as a lovely girl she has no vestiges of the 'Witch of Colchis' yet.
|tlb1=Medea, 14 years old.
|tlb2=Summoned as the Medea from before being cajoled by Jason, when she was being raised up like a princess while learning magic from Hecate. She mostly cannot use offensive magic, but excels in healing-type magic.
|tlb3=A young girl with lovely bearing that anyone can love, if it wasn't for Jason, she would have lead a happy life. If it wasn't for Jason.
|tlb4='''Pain Breaker: All Wounds Must Be Repaired'''<br/>A medical Noble Phantasm that forms a pair with the "All Spells Must Be Broken" that Medea would originally use as a Caster. It returns any and all curses, injuries by means of magic to zero.
|tlb5='''All Wounds Must Be Repaired'''<br/>performs automatic repairs not by means of time manipulation, but by calculating the proper figure one originally had. For those that do not know any better, it would just look as rewinding of time. It can abolish any and all unreasonableness other than "death", but it simply cannot bring back the dead.
|tlint=Lily is pure and trusts people easily. Even towards the Master, she comes in contact with maximum affability. However, turning another way, this is nothing but something driven by the thoughts of 'wanting to trust people'. Be it the original Medea or Lily, both are filled with distrust towards others.
|jpdef= コルキスの女王メディア、彼女が「魔女」と呼ばれる前、少女時代のメディアとして召喚された。
純粋無垢、可憐な少女であった彼女に「コルキスの魔女」の面影は未だない。
|jpb1=身長／体重：149cm・41kg<br/>
出典：ギリシャ神話<br/>
地域：ギリシャ<br/>
属性：秩序・善　　　性別：女性<br/>
メディアさん１４歳。
|jpb2=イアソンに籠絡される前、魔術をヘカテーより学び、蝶よ花よと育てられている頃のメディアとして召喚された。
攻撃的な魔術はほとんど使用できないが、治癒系の魔術に長ける。
|jpb3=可憐な立ち居振る舞いから誰にも愛される少女であり、イアソンさえいなければ幸福な一生を送っていただろう。
イアソンさえいなければ。
|jpb4=『修補すべき全ての疵』<br/>
ランク：Ｃ　種別：対魔術宝具<br/>
ペインブレイカー。<br/>
本来のキャスターであるメディアが使用する『破戒すべき全ての符』と対になる治療宝具。
あらゆる呪い、魔術による損傷を零に戻す。
|jpb5=『修補すべき全ての疵』は時間操作ではなく、本来あるべき姿を算定することにより自動修復している。
知らぬ者には時間の巻き戻しにしか見えないであろう。
“死”以外のあらゆる理不尽を打破できるが、死者だけは取り戻せない。
|jpint=リリィは純粋で人を信じやすい。マスターに対しても最大限好意的に接する。だが、それは裏を返せば「人を信じたい」という想いに駆られていることに他ならない。他者への不信に満ちているのは、本来のメディアもリリィも変わらないのだ。
}}

== Trivia==
* Medea Lily's final Ascension artwork was changed during maintenance on November 11th 2015 [http://typemoon.com/news/2015/1ae56o], for the original art go [[Servant Updates|here]]
"""
