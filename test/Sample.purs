module Test.Sample where

skillText :: String
skillText = """
<tabber>
Scathach=<onlyinclude>{{#ifeq:{{{1|(Scathach)}}}|(Scathach)|
{{ActiveSkillTable
|target= One Ally
|imagetype= Quick Up
|skillname = Primordial Rune
|jpname=原初のルーン
|skilltier=(Scathach)
|effect1= Increase one ally's Quick Card effectiveness. (1 turn)
|e1name = Quick +
|e1-lvl1 = 30%
|e1-lvl2 = 32%
|e1-lvl3 = 34%
|e1-lvl4 = 36%
|e1-lvl5 = 38%
|e1-lvl6 = 40%
|e1-lvl7 = 42%
|e1-lvl8 = 44%
|e1-lvl9 = 46%
|e1-lvl10 = 50%
|cooldown1 = 8
|cooldown2 = 7
|cooldown3 = 6
}}
}}
</onlyinclude>
|-|
Brynhild=<onlyinclude>{{#ifeq:{{{1|(Brynhild)}}}|(Brynhild)|
{{ActiveSkillTable
|target=One Enemy
|imagetype= Crit Down
|skillname = Primordial Rune
|jpname=原初のルーン
|skilltier= (Brynhild)
|effect1= Reduces one enemy's Critical Rate (3 turns)
|effect2= Reduces one enemy's NP Strength (1 turn)
|e1name = Crit Rate -
|e1-lvl1 = 30%
|e1-lvl2 = 32%
|e1-lvl3 = 34%
|e1-lvl4 = 36%
|e1-lvl5 = 38%
|e1-lvl6 = 40%
|e1-lvl7 = 42%
|e1-lvl8 = 44%
|e1-lvl9 = 46%
|e1-lvl10 = 50%
|e2name = NP Strength -
|e2-lvl1 = 15%
|e2-lvl2 = 16.5%
|e2-lvl3 = 18%
|e2-lvl4 = 19.5%
|e2-lvl5 = 21%
|e2-lvl6 = 22.5%
|e2-lvl7 = 24%
|e2-lvl8 = 25.5%
|e2-lvl9 = 27%
|e2-lvl10 = 30%
|cooldown1 = 8
|cooldown2 = 7
|cooldown3 = 6
}}
}}</onlyinclude><noinclude>[[Category:Active Skills]]</noinclude>
</tabber>
"""
