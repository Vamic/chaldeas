module Test.Sample where

skillText :: String
skillText = """
<tabber>
Rank A=
<onlyinclude>{{#ifeq:{{{1|A}}}|A|
{{ActiveSkillTable
|target= Self
|imagetype= NP Generation
|skillname = Galvanism
|jpname=ガルバニズム
|skilltier= A
|effect1= Increase your NP Gain (3 turns)
|e1name = NP Rate +
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
|cooldown1 = 7
|cooldown2 = 6
|cooldown3 = 5
}}
}}</onlyinclude>
|-|
Rank B=
<onlyinclude>{{#ifeq:{{{1|B}}}|B|
{{ActiveSkillTable
|target= Self
|imagetype= NP Generation
|skillname = Galvanism
|jpname=ガルバニズム
|skilltier= B
|effect1= Increase your NP Gain (3 turns)
|e1name = NP Rate +
|e1-lvl1 = 25%
|e1-lvl2 = 27%
|e1-lvl3 = 29%
|e1-lvl4 = 31%
|e1-lvl5 = 33%
|e1-lvl6 = 35%
|e1-lvl7 = 37%
|e1-lvl8 = 39%
|e1-lvl9 = 41%
|e1-lvl10 = 45%
|cooldown1 = 7
|cooldown2 = 6
|cooldown3 = 5
}}
}}</onlyinclude>
</tabber>
<noinclude>
[[Category:Active Skills]]
</noinclude>
"""
