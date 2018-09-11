module Database.MyServant 
  ( MyServant(..), getBase
  , recalc
  , unowned, newServant, owned
  , growthCurves
  ) where

import Prelude
import Data.Array (index, zipWith)
import Data.Int
import Data.Maybe
import Data.Map (Map, fromFoldable, lookup)
import Data.Profunctor.Strong

import Database

newtype MyServant = MyServant { base    :: Servant 
                              , level   :: Int
                              , fou     :: Stat
                              , skills  :: Array Int
                              , npLvl   :: Int
                              , ascent  :: Int
                              , servant :: Servant
                              }
instance _0_ :: Eq MyServant where
  eq a b = eq (getBase a) (getBase b)
instance _1_ :: Ord MyServant where
  compare a b = compare (getBase a) (getBase b)

getBase :: MyServant -> Servant
getBase (MyServant {base}) = base

recalc :: MyServant -> MyServant
recalc (MyServant ms@{base:s'@(Servant s)}) = MyServant ms
    { servant = Servant s 
        { stats    = s.stats{ base = calcStats, max = calcStats }
        , phantasm = s.phantasm
              { effect = mapAmount calcNP <$> s.phantasm.effect
              , over   = if ms.level == 0 then s.phantasm.over 
                          else mapAmount calcOver <$> s.phantasm.over
              }
        , actives  = zipWith calcActives ms.skills s.actives 
        } 
    }
  where
    calcStats = addStats ms.fou $ lvlStats s' ms.level
    calcNP minAmount maxAmount = Flat $ minAmount + (maxAmount - minAmount) * 
        case ms.npLvl of
            1 -> 0.0
            2 -> 0.5
            3 -> 0.75 
            4 -> 0.875
            _ -> 1.0
    calcOver minAmount maxAmount 
      | ms.npLvl == 1 = Flat minAmount
      | otherwise = Range minAmount $ minAmount + (maxAmount - minAmount) 
                                    * (toNumber ms.npLvl - 1.0) / 4.0
    calcActives lvl skill = skill { effect = mapAmount calc <$> skill.effect 
                                  , cd = skill.cd - (max 2 lvl - 2) / 4
                                  }
      where
        calc minAmount maxAmount
          | lvl == 10 = Flat maxAmount
          | otherwise = Flat $ minAmount + (maxAmount - minAmount)
                                         * (toNumber lvl - 1.0) / 10.0

makeUnowned :: Servant -> MyServant
makeUnowned servant@(Servant s) = MyServant 
    { servant
    , base:   servant
    , level:  0
    , fou:    {atk: 990, hp: 990}
    , skills: [10, 10, 10]
    , npLvl:  if s.rarity <= 3 || s.free then 5 else 1
    , ascent: 1
    }

unowneds :: Map Servant MyServant
unowneds = fromFoldable $ (identity &&& makeUnowned) <$> servants

unowned :: Servant -> MyServant
unowned s = fromMaybe' (\_ -> makeUnowned s) $ lookup s unowneds

newServant :: Servant -> MyServant
newServant servant@(Servant s) = MyServant 
    { servant
    , base:   servant
    , level:  1
    , fou:    {atk: 0, hp: 0}
    , skills: [1, 1, 1]
    , npLvl:  1
    , ascent: 1
    }
    
owned :: Map Servant MyServant -> Servant -> MyServant
owned team servant = fromMaybe (unowned servant) $
                     lookup servant team

lvlStats :: Servant -> Int -> Stat
lvlStats (Servant {stats:{max}}) 0 = max
lvlStats (Servant {curve, stats:{base, max}}) lvl = { atk: go base.atk max.atk
                                                    , hp:  go base.hp  max.hp
                                                    }
  where
    go :: Int -> Int -> Int
    go baseVal maxVal = add baseVal <<< floor $
                        toNumber (maxVal - baseVal) * modifier / 1000.0
    modifier :: Number
    modifier    = toNumber <<< fromMaybe 0 $
                  index growthCurves curve >>= flip index lvl

growthCurves :: Array (Array Int)
growthCurves = [
  [ 0, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 101, 111, 121, 131, 141, 151, 161, 171, 181, 191, 202, 212, 222, 232, 242, 252, 262, 272, 282, 292, 303, 313, 323, 333, 343, 353, 363, 373, 383, 393, 404, 414, 424, 434, 444, 454, 464, 474, 484, 494, 505, 515, 525, 535, 545, 555, 565, 575, 585, 595, 606, 616, 626, 636, 646, 656, 666, 676, 686, 696, 707, 717, 727, 737, 747, 757, 767, 777, 787, 797, 808, 818, 828, 838, 848, 858, 868, 878, 888, 898, 909, 919, 929, 939, 949, 959, 969, 979, 989, 1000, 1010, 1020, 1030, 1040, 1050, 1060, 1070, 1080, 1090, 1101, 1111, 1121, 1131, 1141, 1151, 1161, 1171, 1181, 1191, 1202, 1212, 1222, 1232, 1242, 1252, 1262, 1272, 1282, 1292, 1303, 1313, 1323, 1333, 1343, 1353, 1363, 1373, 1383, 1393, 1404, 1414, 1424, 1434, 1444, 1454, 1464, 1474, 1484, 1494, 1505, 1515, 1525, 1535, 1545, 1555, 1565, 1575, 1585, 1595, 1606, 1616, 1626, 1636, 1646, 1656, 1666, 1676, 1686, 1696, 1707, 1717, 1727, 1737, 1747, 1757, 1767, 1777, 1787, 1797, 1808, 1818, 1828, 1838, 1848, 1858, 1868, 1878, 1888, 1898, 1909, 1919, 1929, 1939, 1949, 1959, 1969, 1979, 1989, 2000, 2010 ]
, [ 0, 0, 16, 33, 50, 67, 84, 101, 118, 135, 152, 169, 186, 203, 220, 237, 254, 271, 288, 305, 322, 338, 355, 372, 389, 406, 423, 440, 457, 474, 491, 508, 525, 542, 559, 576, 593, 610, 627, 644, 661, 677, 694, 711, 728, 745, 762, 779, 796, 813, 830, 847, 864, 881, 898, 915, 932, 949, 966, 983, 1000, 1016, 1033, 1050, 1067, 1084, 1101, 1118, 1135, 1152, 1169, 1186, 1203, 1220, 1237, 1254, 1271, 1288, 1305, 1322, 1338, 1355, 1372, 1389, 1406, 1423, 1440, 1457, 1474, 1491, 1508, 1525, 1542, 1559, 1576, 1593, 1610, 1627, 1644, 1661, 1677 ]
, [ 0, 0, 15, 31, 46, 62, 78, 93, 109, 125, 140, 156, 171, 187, 203, 218, 234, 250, 265, 281, 296, 312, 328, 343, 359, 375, 390, 406, 421, 437, 453, 468, 484, 500, 515, 531, 546, 562, 578, 593, 609, 625, 640, 656, 671, 687, 703, 718, 734, 750, 765, 781, 796, 812, 828, 843, 859, 875, 890, 906, 921, 937, 953, 968, 984, 1000, 1015, 1031, 1046, 1062, 1078, 1093, 1109, 1125, 1140, 1156, 1171, 1187, 1203, 1218, 1234, 1250, 1265, 1281, 1296, 1312, 1328, 1343, 1359, 1375, 1390, 1406, 1421, 1437, 1453, 1468, 1484, 1500, 1515, 1531, 1546 ]
, [ 0, 0, 14, 28, 43, 57, 72, 86, 101, 115, 130, 144, 159, 173, 188, 202, 217, 231, 246, 260, 275, 289, 304, 318, 333, 347, 362, 376, 391, 405, 420, 434, 449, 463, 478, 492, 507, 521, 536, 550, 565, 579, 594, 608, 623, 637, 652, 666, 681, 695, 710, 724, 739, 753, 768, 782, 797, 811, 826, 840, 855, 869, 884, 898, 913, 927, 942, 956, 971, 985, 1000, 1014, 1028, 1043, 1057, 1072, 1086, 1101, 1115, 1130, 1144, 1159, 1173, 1188, 1202, 1217, 1231, 1246, 1260, 1275, 1289, 1304, 1318, 1333, 1347, 1362, 1376, 1391, 1405, 1420, 1434 ]
, [ 0, 0, 12, 25, 37, 50, 63, 75, 88, 101, 113, 126, 139, 151, 164, 177, 189, 202, 215, 227, 240, 253, 265, 278, 291, 303, 316, 329, 341, 354, 367, 379, 392, 405, 417, 430, 443, 455, 468, 481, 493, 506, 518, 531, 544, 556, 569, 582, 594, 607, 620, 632, 645, 658, 670, 683, 696, 708, 721, 734, 746, 759, 772, 784, 797, 810, 822, 835, 848, 860, 873, 886, 898, 911, 924, 936, 949, 962, 974, 987, 1000, 1012, 1025, 1037, 1050, 1063, 1075, 1088, 1101, 1113, 1126, 1139, 1151, 1164, 1177, 1189, 1202, 1215, 1227, 1240, 1253 ]
, [ 0, 0, 11, 22, 33, 44, 56, 67, 78, 89, 101, 112, 123, 134, 146, 157, 168, 179, 191, 202, 213, 224, 235, 247, 258, 269, 280, 292, 303, 314, 325, 337, 348, 359, 370, 382, 393, 404, 415, 426, 438, 449, 460, 471, 483, 494, 505, 516, 528, 539, 550, 561, 573, 584, 595, 606, 617, 629, 640, 651, 662, 674, 685, 696, 707, 719, 730, 741, 752, 764, 775, 786, 797, 808, 820, 831, 842, 853, 865, 876, 887, 898, 910, 921, 932, 943, 955, 966, 977, 988, 1000, 1011, 1022, 1033, 1044, 1056, 1067, 1078, 1089, 1101, 1112 ]
, [ 0, 0, 26, 53, 79, 105, 131, 157, 182, 206, 230, 253, 276, 298, 319, 339, 358, 376, 393, 409, 423, 437, 449, 460, 470, 478, 485, 491, 495, 498, 499, 500, 501, 504, 508, 514, 521, 529, 539, 550, 562, 576, 590, 606, 623, 641, 660, 680, 701, 723, 746, 769, 793, 817, 842, 868, 894, 920, 946, 973, 1000, 1016, 1033, 1050, 1067, 1084, 1101, 1118, 1135, 1152, 1169, 1186, 1203, 1220, 1237, 1254, 1271, 1288, 1305, 1322, 1338, 1355, 1372, 1389, 1406, 1423, 1440, 1457, 1474, 1491, 1508, 1525, 1542, 1559, 1576, 1593, 1610, 1627, 1644, 1661, 1677 ]
, [ 0, 0, 24, 49, 73, 97, 121, 145, 168, 191, 213, 235, 257, 277, 297, 317, 335, 353, 370, 386, 401, 415, 428, 440, 451, 461, 470, 478, 485, 490, 494, 497, 499, 500, 501, 502, 505, 509, 514, 521, 529, 538, 548, 559, 571, 584, 598, 613, 629, 646, 664, 682, 702, 722, 742, 764, 786, 808, 831, 854, 878, 902, 926, 950, 975, 1000, 1015, 1031, 1046, 1062, 1078, 1093, 1109, 1125, 1140, 1156, 1171, 1187, 1203, 1218, 1234, 1250, 1265, 1281, 1296, 1312, 1328, 1343, 1359, 1375, 1390, 1406, 1421, 1437, 1453, 1468, 1484, 1500, 1515, 1531, 1546 ]
, [ 0, 0, 22, 45, 68, 90, 112, 134, 156, 178, 199, 219, 240, 259, 278, 297, 315, 332, 349, 365, 380, 394, 408, 421, 433, 443, 453, 463, 471, 478, 484, 489, 493, 496, 498, 499, 500, 501, 503, 506, 510, 515, 521, 528, 536, 546, 556, 566, 578, 591, 605, 619, 634, 650, 667, 684, 702, 721, 740, 759, 780, 800, 821, 843, 865, 887, 909, 931, 954, 977, 1000, 1014, 1028, 1043, 1057, 1072, 1086, 1101, 1115, 1130, 1144, 1159, 1173, 1188, 1202, 1217, 1231, 1246, 1260, 1275, 1289, 1304, 1318, 1333, 1347, 1362, 1376, 1391, 1405, 1420, 1434 ]
, [ 0, 0, 19, 39, 59, 79, 98, 118, 137, 156, 175, 193, 211, 229, 247, 264, 280, 297, 312, 328, 342, 357, 370, 383, 396, 407, 419, 429, 439, 448, 457, 464, 471, 477, 483, 488, 492, 495, 497, 499, 500, 501, 502, 503, 504, 507, 511, 516, 522, 528, 535, 542, 551, 560, 570, 580, 592, 603, 616, 629, 642, 657, 671, 687, 702, 719, 735, 752, 770, 788, 806, 824, 843, 862, 881, 901, 920, 940, 960, 980, 1000, 1012, 1025, 1037, 1050, 1063, 1075, 1088, 1101, 1113, 1126, 1139, 1151, 1164, 1177, 1189, 1202, 1215, 1227, 1240, 1253 ]
, [ 0, 0, 17, 35, 52, 70, 87, 105, 122, 139, 156, 172, 189, 205, 221, 237, 252, 267, 282, 296, 310, 324, 337, 350, 362, 374, 386, 397, 407, 417, 427, 435, 444, 452, 459, 466, 472, 477, 482, 486, 490, 493, 496, 498, 499, 500, 501, 502, 503, 504, 506, 509, 513, 517, 522, 527, 533, 540, 547, 555, 564, 572, 582, 592, 602, 613, 625, 637, 649, 662, 675, 689, 703, 717, 732, 747, 762, 778, 794, 810, 827, 843, 860, 877, 894, 912, 929, 947, 964, 982, 1000, 1011, 1022, 1033, 1044, 1056, 1067, 1078, 1089, 1101, 1112 ]
, [ 0, 0, 2, 6, 11, 17, 25, 34, 44, 56, 69, 83, 98, 115, 132, 151, 170, 191, 212, 234, 257, 281, 305, 330, 355, 381, 407, 433, 460, 486, 513, 539, 566, 592, 618, 644, 669, 694, 718, 742, 765, 787, 808, 829, 848, 867, 884, 901, 916, 930, 943, 955, 965, 974, 982, 985, 988, 991, 994, 997, 1000, 1016, 1033, 1050, 1067, 1084, 1101, 1118, 1135, 1152, 1169, 1186, 1203, 1220, 1237, 1254, 1271, 1288, 1305, 1322, 1338, 1355, 1372, 1389, 1406, 1423, 1440, 1457, 1474, 1491, 1508, 1525, 1542, 1559, 1576, 1593, 1610, 1627, 1644, 1661, 1677 ]
, [ 0, 0, 2, 5, 9, 14, 21, 29, 38, 48, 59, 71, 84, 98, 113, 129, 146, 164, 182, 202, 222, 242, 264, 286, 308, 331, 354, 378, 402, 426, 450, 475, 500, 524, 549, 573, 597, 621, 645, 668, 691, 713, 735, 757, 777, 797, 817, 835, 853, 870, 886, 901, 915, 928, 940, 951, 961, 970, 978, 982, 985, 988, 991, 994, 997, 1000, 1015, 1031, 1046, 1062, 1078, 1093, 1109, 1125, 1140, 1156, 1171, 1187, 1203, 1218, 1234, 1250, 1265, 1281, 1296, 1312, 1328, 1343, 1359, 1375, 1390, 1406, 1421, 1437, 1453, 1468, 1484, 1500, 1515, 1531, 1546 ]
, [ 0, 0, 2, 4, 8, 12, 18, 25, 32, 41, 50, 61, 72, 85, 98, 112, 126, 142, 158, 175, 193, 211, 230, 250, 269, 290, 311, 332, 354, 376, 398, 420, 443, 465, 488, 511, 534, 556, 579, 601, 623, 645, 667, 688, 709, 730, 750, 769, 788, 806, 824, 841, 857, 873, 887, 901, 914, 927, 938, 949, 958, 967, 974, 981, 986, 990, 992, 994, 996, 998, 1000, 1014, 1028, 1043, 1057, 1072, 1086, 1101, 1115, 1130, 1144, 1159, 1173, 1188, 1202, 1217, 1231, 1246, 1260, 1275, 1289, 1304, 1318, 1333, 1347, 1362, 1376, 1391, 1405, 1420, 1434 ]
, [ 0, 0, 1, 3, 6, 9, 14, 19, 25, 31, 39, 47, 55, 65, 75, 86, 97, 109, 122, 136, 149, 164, 179, 194, 210, 227, 244, 261, 279, 297, 315, 334, 353, 372, 391, 411, 430, 450, 470, 490, 509, 529, 549, 569, 588, 608, 627, 646, 665, 684, 702, 720, 738, 755, 772, 789, 805, 820, 835, 850, 863, 877, 890, 902, 913, 924, 934, 944, 952, 960, 968, 974, 980, 984, 987, 990, 992, 994, 996, 998, 1000, 1012, 1025, 1037, 1050, 1063, 1075, 1088, 1101, 1113, 1126, 1139, 1151, 1164, 1177, 1189, 1202, 1215, 1227, 1240, 1253 ]
, [ 0, 0, 1, 2, 4, 7, 11, 15, 19, 25, 30, 37, 44, 51, 59, 68, 77, 87, 97, 108, 119, 131, 143, 155, 168, 182, 196, 210, 224, 239, 255, 270, 286, 302, 318, 335, 352, 369, 386, 403, 420, 438, 455, 473, 491, 508, 526, 544, 561, 579, 596, 613, 630, 647, 664, 681, 697, 713, 729, 744, 760, 775, 789, 803, 817, 831, 844, 856, 868, 880, 891, 902, 912, 922, 931, 940, 948, 955, 962, 969, 974, 980, 984, 986, 988, 990, 992, 994, 996, 998, 1000, 1011, 1022, 1033, 1044, 1056, 1067, 1078, 1089, 1101, 1112 ]
, [ 0, 0, 20, 40, 61, 81, 102, 122, 142, 163, 183, 204, 224, 244, 265, 285, 306, 326, 346, 367, 387, 408, 428, 448, 469, 489, 510, 530, 551, 571, 591, 612, 632, 653, 673, 693, 714, 734, 755, 775, 795, 816, 836, 857, 877, 897, 918, 938, 959, 979, 1000, 1020, 1040, 1061, 1081, 1102, 1122, 1142, 1163, 1183, 1204, 1224, 1244, 1265, 1285, 1306, 1326, 1346, 1367, 1387, 1408, 1428, 1448, 1469, 1489, 1510, 1530, 1551, 1571, 1591, 1612, 1632, 1653, 1673, 1693, 1714, 1734, 1755, 1775, 1795, 1816, 1836, 1857, 1877, 1897, 1918, 1938, 1959, 1979, 2000, 2020 ]
, [ 0, 0, 18, 37, 55, 74, 92, 111, 129, 148, 166, 185, 203, 222, 240, 259, 277, 296, 314, 333, 351, 370, 388, 407, 425, 444, 462, 481, 500, 518, 537, 555, 574, 592, 611, 629, 648, 666, 685, 703, 722, 740, 759, 777, 796, 814, 833, 851, 870, 888, 907, 925, 944, 962, 981, 1000, 1018, 1037, 1055, 1074, 1092, 1111, 1129, 1148, 1166, 1185, 1203, 1222, 1240, 1259, 1277, 1296, 1314, 1333, 1351, 1370, 1388, 1407, 1425, 1444, 1462, 1481, 1500, 1518, 1537, 1555, 1574, 1592, 1611, 1629, 1648, 1666, 1685, 1703, 1722, 1740, 1759, 1777, 1796, 1814, 1833 ]
, [ 0, 0, 16, 33, 50, 67, 84, 101, 118, 135, 152, 169, 186, 203, 220, 237, 254, 271, 288, 305, 322, 338, 355, 372, 389, 406, 423, 440, 457, 474, 491, 508, 525, 542, 559, 576, 593, 610, 627, 644, 661, 677, 694, 711, 728, 745, 762, 779, 796, 813, 830, 847, 864, 881, 898, 915, 932, 949, 966, 983, 1000, 1016, 1033, 1050, 1067, 1084, 1101, 1118, 1135, 1152, 1169, 1186, 1203, 1220, 1237, 1254, 1271, 1288, 1305, 1322, 1338, 1355, 1372, 1389, 1406, 1423, 1440, 1457, 1474, 1491, 1508, 1525, 1542, 1559, 1576, 1593, 1610, 1627, 1644, 1661, 1677 ]
, [ 0, 0, 12, 25, 37, 50, 63, 75, 88, 101, 113, 126, 139, 151, 164, 177, 189, 202, 215, 227, 240, 253, 265, 278, 291, 303, 316, 329, 341, 354, 367, 379, 392, 405, 417, 430, 443, 455, 468, 481, 493, 506, 518, 531, 544, 556, 569, 582, 594, 607, 620, 632, 645, 658, 670, 683, 696, 708, 721, 734, 746, 759, 772, 784, 797, 810, 822, 835, 848, 860, 873, 886, 898, 911, 924, 936, 949, 962, 974, 987, 1000, 1012, 1025, 1037, 1050, 1063, 1075, 1088, 1101, 1113, 1126, 1139, 1151, 1164, 1177, 1189, 1202, 1215, 1227, 1240, 1253 ]
, [ 0, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 101, 111, 121, 131, 141, 151, 161, 171, 181, 191, 202, 212, 222, 232, 242, 252, 262, 272, 282, 292, 303, 313, 323, 333, 343, 353, 363, 373, 383, 393, 404, 414, 424, 434, 444, 454, 464, 474, 484, 494, 505, 515, 525, 535, 545, 555, 565, 575, 585, 595, 606, 616, 626, 636, 646, 656, 666, 676, 686, 696, 707, 717, 727, 737, 747, 757, 767, 777, 787, 797, 808, 818, 828, 838, 848, 858, 868, 878, 888, 898, 909, 919, 929, 939, 949, 959, 969, 979, 989, 1000 ]
, [ 0, 0, 21, 43, 65, 86, 108, 129, 150, 171, 191, 211, 231, 250, 269, 288, 306, 323, 340, 357, 372, 388, 402, 416, 430, 442, 454, 466, 476, 486, 495, 504, 513, 523, 533, 545, 557, 569, 583, 597, 611, 627, 642, 659, 676, 693, 711, 730, 749, 768, 788, 808, 828, 849, 870, 891, 913, 934, 956, 978, 1000, 1016, 1033, 1050, 1067, 1084, 1101, 1118, 1135, 1152, 1169, 1186, 1203, 1220, 1237, 1254, 1271, 1288, 1305, 1322, 1338, 1355, 1372, 1389, 1406, 1423, 1440, 1457, 1474, 1491, 1508, 1525, 1542, 1559, 1576, 1593, 1610, 1627, 1644, 1661, 1677 ]
, [ 0, 0, 20, 40, 60, 80, 99, 119, 138, 158, 177, 195, 214, 232, 250, 267, 285, 301, 318, 333, 349, 364, 378, 392, 405, 418, 430, 442, 453, 463, 473, 483, 491, 500, 508, 516, 526, 536, 546, 557, 569, 581, 594, 607, 621, 635, 650, 666, 681, 698, 714, 732, 749, 767, 785, 804, 822, 841, 861, 880, 900, 919, 939, 959, 979, 1000, 1015, 1031, 1046, 1062, 1078, 1093, 1109, 1125, 1140, 1156, 1171, 1187, 1203, 1218, 1234, 1250, 1265, 1281, 1296, 1312, 1328, 1343, 1359, 1375, 1390, 1406, 1421, 1437, 1453, 1468, 1484, 1500, 1515, 1531, 1546 ]
, [ 0, 0, 18, 37, 55, 74, 92, 110, 129, 147, 164, 182, 199, 216, 233, 250, 266, 282, 297, 313, 327, 342, 356, 370, 383, 395, 408, 419, 431, 442, 452, 462, 471, 480, 488, 496, 503, 511, 519, 528, 537, 547, 557, 568, 580, 591, 604, 616, 629, 643, 657, 672, 686, 702, 717, 733, 749, 766, 783, 800, 817, 835, 852, 870, 889, 907, 925, 944, 962, 981, 1000, 1014, 1028, 1043, 1057, 1072, 1086, 1101, 1115, 1130, 1144, 1159, 1173, 1188, 1202, 1217, 1231, 1246, 1260, 1275, 1289, 1304, 1318, 1333, 1347, 1362, 1376, 1391, 1405, 1420, 1434 ]
, [ 0, 0, 16, 32, 48, 64, 81, 97, 113, 128, 144, 160, 175, 190, 205, 220, 235, 249, 264, 277, 291, 305, 318, 331, 343, 355, 367, 379, 390, 401, 412, 422, 432, 441, 450, 459, 467, 475, 482, 490, 496, 503, 509, 517, 524, 532, 540, 549, 558, 567, 577, 587, 598, 609, 620, 632, 644, 656, 668, 681, 694, 708, 722, 735, 750, 764, 779, 794, 809, 824, 839, 855, 871, 886, 902, 918, 935, 951, 967, 983, 1000, 1012, 1025, 1037, 1050, 1063, 1075, 1088, 1101, 1113, 1126, 1139, 1151, 1164, 1177, 1189, 1202, 1215, 1227, 1240, 1253 ]
, [ 0, 0, 14, 28, 43, 57, 71, 86, 100, 114, 128, 142, 156, 170, 183, 197, 210, 223, 236, 249, 262, 274, 286, 298, 310, 322, 333, 344, 355, 366, 376, 386, 396, 405, 415, 424, 432, 441, 449, 456, 464, 471, 478, 484, 491, 497, 502, 508, 515, 521, 528, 535, 543, 550, 558, 567, 575, 584, 594, 603, 613, 623, 633, 644, 655, 666, 677, 689, 701, 713, 725, 737, 750, 763, 776, 789, 802, 816, 829, 843, 857, 871, 885, 899, 913, 928, 942, 956, 971, 985, 1000, 1011, 1022, 1033, 1044, 1056, 1067, 1078, 1089, 1101, 1112 ]
, [ 0, 0, 8, 18, 28, 39, 51, 63, 76, 90, 104, 119, 134, 151, 167, 184, 202, 220, 239, 258, 278, 298, 318, 339, 360, 381, 402, 424, 445, 467, 489, 510, 532, 554, 575, 597, 618, 639, 660, 681, 701, 721, 741, 760, 779, 797, 815, 832, 848, 865, 880, 895, 909, 923, 936, 948, 960, 971, 981, 991, 1000, 1016, 1033, 1050, 1067, 1084, 1101, 1118, 1135, 1152, 1169, 1186, 1203, 1220, 1237, 1254, 1271, 1288, 1305, 1322, 1338, 1355, 1372, 1389, 1406, 1423, 1440, 1457, 1474, 1491, 1508, 1525, 1542, 1559, 1576, 1593, 1610, 1627, 1644, 1661, 1677 ]
, [ 0, 0, 8, 16, 26, 36, 46, 57, 69, 81, 94, 107, 121, 135, 150, 166, 181, 198, 214, 232, 249, 267, 285, 304, 322, 341, 361, 380, 400, 419, 439, 459, 479, 500, 520, 540, 560, 580, 599, 619, 638, 658, 677, 695, 714, 732, 750, 767, 785, 801, 818, 833, 849, 864, 878, 892, 905, 918, 930, 942, 953, 963, 973, 983, 991, 1000, 1015, 1031, 1046, 1062, 1078, 1093, 1109, 1125, 1140, 1156, 1171, 1187, 1203, 1218, 1234, 1250, 1265, 1281, 1296, 1312, 1328, 1343, 1359, 1375, 1390, 1406, 1421, 1437, 1453, 1468, 1484, 1500, 1515, 1531, 1546 ]
, [ 0, 0, 7, 15, 24, 33, 42, 52, 63, 74, 85, 97, 110, 123, 136, 150, 164, 179, 194, 209, 225, 241, 258, 274, 291, 308, 326, 344, 361, 379, 398, 416, 434, 453, 472, 490, 509, 527, 546, 565, 583, 601, 620, 638, 655, 673, 691, 708, 725, 741, 758, 774, 790, 805, 820, 835, 849, 863, 876, 889, 902, 914, 925, 936, 947, 957, 966, 975, 984, 992, 1000, 1014, 1028, 1043, 1057, 1072, 1086, 1101, 1115, 1130, 1144, 1159, 1173, 1188, 1202, 1217, 1231, 1246, 1260, 1275, 1289, 1304, 1318, 1333, 1347, 1362, 1376, 1391, 1405, 1420, 1434 ]
, [ 0, 0, 6, 13, 20, 28, 36, 45, 53, 63, 72, 82, 93, 103, 114, 126, 138, 150, 162, 175, 188, 201, 215, 228, 243, 257, 271, 286, 301, 316, 332, 347, 363, 379, 394, 410, 427, 443, 459, 475, 491, 508, 524, 540, 556, 572, 589, 605, 620, 636, 652, 667, 683, 698, 713, 728, 742, 756, 771, 784, 798, 811, 824, 837, 849, 861, 873, 885, 896, 906, 917, 927, 936, 946, 954, 963, 971, 979, 986, 993, 1000, 1012, 1025, 1037, 1050, 1063, 1075, 1088, 1101, 1113, 1126, 1139, 1151, 1164, 1177, 1189, 1202, 1215, 1227, 1240, 1253 ]
, [ 0, 0, 5, 11, 18, 24, 31, 39, 46, 54, 63, 71, 80, 89, 98, 108, 118, 128, 139, 149, 160, 172, 183, 195, 207, 219, 231, 244, 256, 269, 282, 296, 309, 323, 336, 350, 364, 378, 392, 406, 420, 435, 449, 463, 478, 492, 507, 521, 536, 550, 564, 579, 593, 607, 621, 635, 649, 663, 676, 690, 703, 717, 730, 743, 755, 768, 780, 792, 804, 816, 827, 839, 850, 860, 871, 881, 891, 901, 910, 919, 928, 936, 945, 953, 960, 968, 975, 981, 988, 994, 1000, 1011, 1022, 1033, 1044, 1056, 1067, 1078, 1089, 1101, 1112 ]
]
