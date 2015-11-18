module Sources.MatchBot.Templates.Flairs where

import Control.Monad.Trans.Writer
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Format
import WebAPI.Dota.Types.Hero
import WebAPI.Dota.Types.Player
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

replaceBlock :: Text -> Text -> Text -> Text
replaceBlock blockName oldText replacement =
  case Text.breakOn blockStart oldText of
    (_, "") -> oldText
    (_, (Text.stripPrefix blockStart -> Nothing)) -> undefined
    (before, (Text.stripPrefix blockStart -> Just postBefore)) ->
      case Text.breakOn blockEnd postBefore of
        (_, "") -> oldText
        (_, (Text.stripPrefix blockEnd -> Nothing)) -> undefined
        (_, (Text.stripPrefix blockEnd -> Just after)) ->
          mconcat [ before, blockStart, replacement, blockEnd, replaceBlock blockName after replacement ]
        (_, _) -> undefined
    (_, _) -> undefined
  where
    blockStart = Lazy.toStrict $ format "[](#start-{})" $ Only blockName
    blockEnd = Lazy.toStrict $ format "[](#end-{})" $ Only blockName

wrapBlock :: Lazy.Text -> Lazy.Text -> Lazy.Text
wrapBlock n x = Lazy.intercalate "\n\n"
  [ format "[](#start-{})" $ Only n
  , x
  , format "[](#end-{})" $ Only n ]

lastUpdateTime :: Integer -> Lazy.Text
lastUpdateTime 0 = "Pre-game"
lastUpdateTime d =
  if h > 0
    then format "{}:{}:{}" (fmt h, fmt m, fmt s)
    else format "{}:{}" (fmt m, fmt s)
  where
    (m', s) = d `divMod` 60
    (h, m) = m' `divMod` 60
    fmt x = left 2 '0' x

tellBlock :: Lazy.Text -> Writer Lazy.Text ()
tellBlock x = tell x >> tell "\n\n"

heroFlair :: HeroID -> Lazy.Text
heroFlair h@(HeroID n) =
  case Map.lookup h heroFlairs of
    Just x -> format "[](/hero-{} \"{}\")" (Lazy.toLower $ Lazy.replace "'" "" $ Lazy.replace "-" "" $ Lazy.replace " " "" x, x)
    Nothing -> format "{}" $ Only n

teamFlag :: Text -> Lazy.Text
teamFlag n =
  case Map.lookup n teamFlags of
    Just x -> format "[](/logo-{} \"{}\")" (x, n)
    Nothing -> Lazy.fromStrict n

teamFlags :: Map Text Lazy.Text
teamFlags = Map.fromList
  [ "Vega_Squadron" =: "vega"
  , "Vega Squadron" =: "vega"
  , "CDEC Gaming" =: "cdec"
  , "Team Archon" =: "archon"
  , "MVP Phoenix" =: "mvp"
  , "LGD-GAMING" =: "lgd"
  , "Cloud9 G2A" =: "c9"
  , "Team Secret" =: "secret"
  , "Fnatic" =: "fnatic"
  , "compLexity Gaming" =: "col"
  , "Invictus Gaming" =: "ig"
  , "Natus Vincere" =: "navi"
  , "Newbee" =: "newbee"
  , "Virtus.pro" =: "virtus"
  , "Evil Geniuses" =: "eg"
  , "MVP HOT6" =: "mvp"
  , "Vici Gaming" =: "vg"
  , "Team Empire" =: "empire"
  , "EHOME" =: "ehome"
  , "(monkey) Business" =: "monkey"
  , "Alliance" =: "alliance" ]
  where (=:) = (,)

playerOfficialNames :: Map AccountID Lazy.Text
playerOfficialNames = Map.fromList
  [ 113800818 =: "Fenrir"
  , 88508515 =: "Hao"
  , 90882159 =: "Super"
  , 84772440 =: "iceiceice"
  , 101695162 =: "fy"
  , 100812084 =: "JaL"
  , 21289303 =: "Black^"
  , 87177591 =: "Fear"
  , 86727555 =: "ppd"
  , 111620041 =: "SumaiL"
  , 40547474 =: "Aui_2000"
  , 87276347 =: "UNiVeRsE"
  , 86743032 =: "Jonathan"
  , 86745912 =: "RTZ"
  , 73562326 =: "zai"
  , 87278757 =: "Puppey"
  , 86745912 =: "RTZ"
  , 82262664 =: "KuroKy"
  , 41231571 =: "s4"
  , 76482434 =: "AdmiralBulldog"
  , 19672354 =: "BigDaddyN0tail"
  , 89246836 =: "Xi"
  , 82327674 =: "Faith"
  , 88553213 =: "ChuaN"
  , 90892734 =: "BurNIng"
  , 88585077 =: "Ferrari_430"
  , 90031225 =: "Ch"
  , 138885864 =: "June"
  , 106863163 =: "Maybe"
  , 98878010 =: "Yao"
  , 98887913 =: "xiao8"
  , 108382060 =: "Sylar"
  , 89407113 =: "MMY!"
  , 118678359 =: "飞"
  , 114239371 =: "ddc"
  , 43276219 =: "EternaLEnVy"
  , 88719902 =: "bOne7"
  , 19672354 =: "BigDaddyN0tail"
  , 87382579 =: "MISERY"
  , 86799300 =: "FATA-"
  , 31078647 =: "paS"
  , 94155156 =: "Fly"
  , 123051238 =: "ALOHADANCE"
  , 89269794 =: "Silent"
  , 86725175 =: "@Resolut1on_"
  , 91064780 =: "ALWAYSWANNAFLY"
  , 112250781 =: "yoky-"
  , 93552791 =: "Nofear"
  , 87201671 =: "blowyourbrain"
  , 106809101 =: "Łîł"
  , 87586992 =: "G"
  , 94338967 =: "DkPhobos"
  , 26316691 =: "Illidan Stormrage"
  , 94049589 =: "Fng"
  , 76600360 =: "u_u"
  , 53178236 =: "Sedoy"
  , 94155156 =: "Fly"
  , 49317728 =: "swindlemelonzz"
  , 87293485 =: "Zyzz"
  , 38628747 =: "MoonMeander"
  , 50828662 =: "Zefrik"
  , 86768178 =: "Enzoe"
  , 84429681 =: "Moo"
  , 100883708 =: "SanSheng"
  , 123854991 =: "Rabbit"
  , 89157606 =: "Mu"
  , 89217927 =: "Banana"
  , 138885864 =: "June"
  , 88508515 =: "Hao"
  , 98887913 =: "xiao8"
  , 92551671 =: "Kecik Imba"
  , 91644707 =: "kyx Y"
  , 102020930 =: "JoHnNy"
  , 89871557 =: "MuShi-"
  , 93616251 =: "Ohaiyo"
  , 97676580 =: "@NEQROMAN"
  , 130586940 =: "JotM"
  , 91698091 =: "rOtk"
  , 89423756 =: "LaNm"
  , 204834394 =: "PennyTang"
  , 114239371 =: "ddc"
  , 139280377 =: "YJ"
  , 108376607 =: "y!!"
  , 111189717 =: "Faint"
  , 89625472 =: "XBOCTlovesLakshmi"
  , 70388657 =: "Dendi"
  , 86723143 =: "Funn1k"
  , 117015167 =: "PABLO"
  , 90423751 =: "Bignum"
  , 120111123 =: "Ingman"
  , 176184718 =: "ArtStyle"
  , 117421467 =: "!!! SoNNeikO !!!"
  , 105599955 =: "Heen"
  , 26771994 =: "JerAx"
  , 32936165 =: "SunBhie"
  , 88933594 =: "FoREv"
  , 101450083 =: "MP"
  , 145550466 =: "DuBu"
  , 108076950 =: "데헷데헷"
  , 131237305 =: "Xz"
  , 130416036 =: "Agressif"
  , 140153524 =: "Q"
  , 142750189 =: "garder"
  , 101375717 =: "ShiKi"
  , 106863163 =: "Maybe"
  , 108484186 =: ".mikasa"
  , 87012746 =: "kpii"
  , 93119769 =: "QO"
  , 89603649 =: "NutZ"
  , 3940262 =: "March"
  , 112377459 =: "Febby"
  , 105599955 =: "Heen"
  , 88933594 =: "FoREv"
  , 177648913 =: "Ritsu"
  , 31818853 =: "Brax"
  ]
  where x =: y = (AccountID x, y)

heroFlairs :: Map HeroID Lazy.Text
heroFlairs = Map.fromList
  [ HeroID 1 =: "Anti-Mage"
  , HeroID 2 =: "Axe"
  , HeroID 3 =: "Bane"
  , HeroID 4 =: "Bloodseeker"
  , HeroID 5 =: "Crystal Maiden"
  , HeroID 6 =: "Drow Ranger"
  , HeroID 7 =: "Earthshaker"
  , HeroID 8 =: "Juggernaut"
  , HeroID 9 =: "Mirana"
  , HeroID 11 =: "Shadow Fiend"
  , HeroID 10 =: "Morphling"
  , HeroID 12 =: "Phantom Lancer"
  , HeroID 13 =: "Puck"
  , HeroID 14 =: "Pudge"
  , HeroID 15 =: "Razor"
  , HeroID 16 =: "Sand King"
  , HeroID 17 =: "Storm Spirit"
  , HeroID 18 =: "Sven"
  , HeroID 19 =: "Tiny"
  , HeroID 20 =: "Vengeful Spirit"
  , HeroID 21 =: "Windranger"
  , HeroID 22 =: "Zeus"
  , HeroID 23 =: "Kunkka"
  , HeroID 25 =: "Lina"
  , HeroID 31 =: "Lich"
  , HeroID 26 =: "Lion"
  , HeroID 27 =: "Shadow Shaman"
  , HeroID 28 =: "Slardar"
  , HeroID 29 =: "Tidehunter"
  , HeroID 30 =: "Witch Doctor"
  , HeroID 32 =: "Riki"
  , HeroID 33 =: "Enigma"
  , HeroID 34 =: "Tinker"
  , HeroID 35 =: "Sniper"
  , HeroID 36 =: "Necrophos"
  , HeroID 37 =: "Warlock"
  , HeroID 38 =: "Beastmaster"
  , HeroID 39 =: "Queen of Pain"
  , HeroID 40 =: "Venomancer"
  , HeroID 41 =: "Faceless Void"
  , HeroID 42 =: "Wraith King"
  , HeroID 43 =: "Death Prophet"
  , HeroID 44 =: "Phantom Assassin"
  , HeroID 45 =: "Pugna"
  , HeroID 46 =: "Templar Assassin"
  , HeroID 47 =: "Viper"
  , HeroID 48 =: "Luna"
  , HeroID 49 =: "Dragon Knight"
  , HeroID 50 =: "Dazzle"
  , HeroID 51 =: "Clockwerk"
  , HeroID 52 =: "Leshrac"
  , HeroID 53 =: "Nature's Prophet"
  , HeroID 54 =: "Lifestealer"
  , HeroID 55 =: "Dark Seer"
  , HeroID 56 =: "Clinkz"
  , HeroID 57 =: "Omniknight"
  , HeroID 58 =: "Enchantress"
  , HeroID 59 =: "Huskar"
  , HeroID 60 =: "Night Stalker"
  , HeroID 61 =: "Broodmother"
  , HeroID 62 =: "Bounty Hunter"
  , HeroID 63 =: "Weaver"
  , HeroID 64 =: "Jakiro"
  , HeroID 65 =: "Batrider"
  , HeroID 66 =: "Chen"
  , HeroID 67 =: "Spectre"
  , HeroID 69 =: "Doom"
  , HeroID 68 =: "Ancient Apparition"
  , HeroID 70 =: "Ursa"
  , HeroID 71 =: "Spirit Breaker"
  , HeroID 72 =: "Gyrocopter"
  , HeroID 73 =: "Alchemist"
  , HeroID 74 =: "Invoker"
  , HeroID 75 =: "Silencer"
  , HeroID 76 =: "Outworld Devourer"
  , HeroID 77 =: "Lycan"
  , HeroID 78 =: "Brewmaster"
  , HeroID 79 =: "Shadow Demon"
  , HeroID 80 =: "Lone Druid"
  , HeroID 81 =: "Chaos Knight"
  , HeroID 82 =: "Meepo"
  , HeroID 83 =: "Treant Protector"
  , HeroID 84 =: "Ogre Magi"
  , HeroID 85 =: "Undying"
  , HeroID 86 =: "Rubick"
  , HeroID 87 =: "Disruptor"
  , HeroID 88 =: "Nyx Assassin"
  , HeroID 89 =: "Naga Siren"
  , HeroID 90 =: "Keeper of the Light"
  , HeroID 91 =: "Io"
  , HeroID 92 =: "Visage"
  , HeroID 93 =: "Slark"
  , HeroID 94 =: "Medusa"
  , HeroID 95 =: "Troll Warlord"
  , HeroID 96 =: "Centaur Warrunner"
  , HeroID 97 =: "Magnus"
  , HeroID 98 =: "Timbersaw"
  , HeroID 99 =: "Bristleback"
  , HeroID 100 =: "Tusk"
  , HeroID 101 =: "Skywrath Mage"
  , HeroID 102 =: "Abaddon"
  , HeroID 103 =: "Elder Titan"
  , HeroID 104 =: "Legion Commander"
  , HeroID 106 =: "Ember Spirit"
  , HeroID 107 =: "Earth Spirit"
  , HeroID 109 =: "Terrorblade"
  , HeroID 110 =: "Phoenix"
  , HeroID 111 =: "Oracle"
  , HeroID 105 =: "Techies"
  , HeroID 112 =: "Winter Wyvern" ]
  where (=:) = (,)
