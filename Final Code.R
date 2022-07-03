library("dplyr")
library("tidyr")
library("plyr")
library("lubridate")
library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("lmtest")

#data wrangling

##average draft position combine data and remove unnecessary data
adp17 = select(adp.2017, -c(1,5,6,9,10,11,12,13), all = TRUE)
adp18 = select(adp.2018, -c(1,5,6,9,10,11,12,13), all = TRUE)
adp19 = select(adp.2019, -c(1,5,6,9,10,11,12,13), all = TRUE)
adp20 = select(adp.2020, -c(1,5,6,9,10,11,12,13), all = TRUE)
adp21 = select(adp.2021, -c(1,5,6,9,10,11,12,13), all = TRUE)

adp17 = rename(adp17,
               Team17 = Team,
               Position17 = Position,
               PositionRank17 = PositionRank,
               ADP17 = AverageDraftPosition
)
adp18 = rename(adp18,
               Team18 = Team,
               Position18 = Position,
               PositionRank18 = PositionRank,
               ADP18 = AverageDraftPosition
)
adp19 = rename(adp19,
               Team19 = Team,
               Position19 = Position,
               PositionRank19 = PositionRank,
               ADP19 = AverageDraftPosition
)
adp20 = rename(adp20,
               Team20 = Team,
               Position20 = Position,
               PositionRank20 = PositionRank,
               ADP20 = AverageDraftPosition
)
adp21 = rename(adp21,
               Team21 = Team,
               Position21 = Position,
               PositionRank21 = PositionRank,
               ADP21 = AverageDraftPosition
)

adp = merge(adp17, adp18, by = c("PlayerID","Name"), all = TRUE)
adp = merge(adp, adp19, by = c("PlayerID","Name"), all = TRUE)
adp = merge(adp, adp20, by = c("PlayerID","Name"), all = TRUE)
adp = merge(adp, adp21, by = c("PlayerID","Name"), all = TRUE)

##game stats wrangling
dst17 = `2017.DST`
stats17 = select(dst17, -c(7:16), all = TRUE)
`2017Stats` = select(`2017Stats`, -c(7:16), all = TRUE)
stats.17 = rbind(stats17, `2017Stats`)
stats.17 = select(stats.17, -c(1), all = TRUE)
stat17 = rename(stats.17,
                Team17 = Team,
                Position17 = Position,
                Week17 = Week,
                PPG17 = FantasyPointsPerGame,
                GamePoints17 = FantasyPoints
)
dstats18 = select(`2018.DSTstats`, -c(1, 7:16), all = TRUE)
stats18 = select(`2018stats`, -c(1, 7:16), all = TRUE)
stats.18 = rbind(stats18, dstats18)
stat18 = rename(stats.18,
                Team18 = Team,
                Position18 = Position,
                Week18 = Week,
                PPG18 = FantasyPointsPerGame,
                GamePoints18 = FantasyPoints
)
dstats19 = select(`2019.DSTstats`, -c(1, 7:16), all = TRUE)
stats19 = select(`2019stats`, -c(1, 7:16), all = TRUE)
stats.19 = rbind(stats19, dstats19)
stat19 = rename(stats.19,
                Team19 = Team,
                Position19 = Position,
                Week19 = Week,
                PPG19 = FantasyPointsPerGame,
                GamePoints19 = FantasyPoints
)
dstats20 = select(`2020.DSTstats`, -c(1, 7:16), all = TRUE)
stats20 = select(`2020stats`, -c(1, 7:16), all = TRUE)
stats.20 = rbind(stats20, dstats20)
stat20 = rename(stats.20,
                Team20 = Team,
                Position20 = Position,
                Week20 = Week,
                PPG20 = FantasyPointsPerGame,
                GamePoints20 = FantasyPoints
)
dstats21 = select(`2021.DSTstats`, -c(1, 7:16), all = TRUE)
stats21 = select(`2021stats`, -c(1, 7:16), all = TRUE)
stats.21 = rbind(stats21, dstats21)
stat21 = rename(stats.21,
                Team21 = Team,
                Position21 = Position,
                Week21 = Week,
                PPG21 = FantasyPointsPerGame,
                GamePoints21 = FantasyPoints
)

##wrangle season projections
DST17Proj = select(DST17Proj, -c(1, 7:15), all = TRUE)
`2017Proj` = select(`2017Proj`, -c(1, 7:18), all = TRUE)
Proj.17 = rbind(DST17Proj, `2017Proj`)
proj17 = rename(Proj.17,
                Team17 = Team,
                Position17 = Position,
                ProjGamesPlayed17 = Played,
                ProjPPG17 = FantasyPointsPerGame,
                ProjPoints17 = FantasyPoints
)
DST18Proj = select(DST18Proj, -c(1, 7:15), all = TRUE)
`2018Proj` = select(`2018Proj`, -c(1, 7:18), all = TRUE)
Proj.18 = rbind(DST18Proj, `2018Proj`)
proj18 = rename(Proj.18,
                Team18 = Team,
                Position18 = Position,
                ProjGamesPlayed18 = Played,
                ProjPPG18 = FantasyPointsPerGame,
                ProjPoints18 = FantasyPoints
)
DST19Proj = select(DST19Proj, -c(1, 7:15), all = TRUE)
`2019Proj` = select(`2019Proj`, -c(1, 7:18), all = TRUE)
Proj.19 = rbind(DST19Proj, `2019Proj`)
proj19 = rename(Proj.19,
                Team19 = Team,
                Position19 = Position,
                ProjGamesPlayed19 = Played,
                ProjPPG19 = FantasyPointsPerGame,
                ProjPoints19 = FantasyPoints
)
DST20Proj = select(DST20Proj, -c(1, 7:15), all = TRUE)
`2020Proj` = select(`2020Proj`, -c(1, 7:18), all = TRUE)
Proj.20 = rbind(DST20Proj, `2020Proj`)
proj20 = rename(Proj.20,
                Team20 = Team,
                Position20 = Position,
                ProjGamesPlayed20 = Played,
                ProjPPG20 = FantasyPointsPerGame,
                ProjPoints20 = FantasyPoints
)
DST21Proj = select(DST21Proj, -c(1, 7:15), all = TRUE)
`2021Proj` = select(`2021Proj`, -c(1, 7:18), all = TRUE)
Proj.21 = rbind(DST21Proj, `2021Proj`)
proj21 = rename(Proj.21,
                Team21 = Team,
                Position21 = Position,
                ProjGamesPlayed21 = Played,
                ProjPPG21 = FantasyPointsPerGame,
                ProjPoints21 = FantasyPoints
)

projections = merge(proj17, proj18, by = c("PlayerID","Name"), all = TRUE)
projections = merge(projections, proj19, by = c("PlayerID","Name"), all = TRUE)
projections = merge(projections, proj20, by = c("PlayerID","Name"), all = TRUE)
projections = merge(projections, proj21, by = c("PlayerID","Name"), all = TRUE)

##season stat wrangling
DST17seastat = select(`2017.DSTstats`, -c(1, 7:15), all = TRUE)
`2017seastat` = select(`2017seasstat`, -c(7:15), all = TRUE)
seastat.17 = merge(DST17seastat, `2017seastat`, by = c("PlayerID","Name", "Team","Position","Played", "FantasyPointsPerGame","FantasyPoints"), all = TRUE)
seastat17 = rename(seastat.17,
                   Rank17 = Rank,
                   Team17 = Team,
                   Position17 = Position,
                   GamesPlayed17 = Played,
                   PPG17 = FantasyPointsPerGame,
                   Points17 = FantasyPoints
)
DST18seastat = select(DST18seastats, -c(1, 7:15), all = TRUE)
`2018seastat` = select(`2018seastats`, -c(7:15), all = TRUE)
seastat.18 = merge(DST18seastat, `2018seastat`, by = c("PlayerID","Name", "Team","Position","Played", "FantasyPointsPerGame","FantasyPoints"), all = TRUE)
seastat18 = rename(seastat.18,
                   Rank18 = Rank,
                   Team18 = Team,
                   Position18 = Position,
                   GamesPlayed18 = Played,
                   PPG18 = FantasyPointsPerGame,
                   Points18 = FantasyPoints
)
DST19seastat = select(DST19seastats, -c(1, 7:15), all = TRUE)
`2019seastat` = select(`2019seastats`, -c(7:15), all = TRUE)
seastat.19 = merge(DST19seastat, `2019seastat`, by = c("PlayerID","Name", "Team","Position","Played", "FantasyPointsPerGame","FantasyPoints"), all = TRUE)
seastat19 = rename(seastat.19,
                   Rank19 = Rank,
                   Team19 = Team,
                   Position19 = Position,
                   GamesPlayed19 = Played,
                   PPG19 = FantasyPointsPerGame,
                   Points19 = FantasyPoints
)
DST20seastat = select(DST20seastats, -c(1, 7:15), all = TRUE)
`2020seastat` = select(`2020seastats`, -c(7:15), all = TRUE)
seastat.20 = merge(DST20seastat, `2020seastat`, by = c("PlayerID","Name", "Team","Position","Played", "FantasyPointsPerGame","FantasyPoints"), all = TRUE)
seastat20 = rename(seastat.20,
                   Rank20 = Rank,
                   Team20 = Team,
                   Position20 = Position,
                   GamesPlayed20 = Played,
                   PPG20 = FantasyPointsPerGame,
                   Points20 = FantasyPoints
)
DST21seastat = select(DST21seastats, -c(1, 7:15), all = TRUE)
`2021seastat` = select(`2021seastats`, -c(7:15), all = TRUE)
seastat.21 = merge(DST21seastat, `2021seastat`, by = c("PlayerID","Name", "Team","Position","Played", "FantasyPointsPerGame","FantasyPoints"), all = TRUE)
seastat21 = rename(seastat.21,
                   Rank21 = Rank,
                   Team21 = Team,
                   Position21 = Position,
                   GamesPlayed21 = Played,
                   PPG21 = FantasyPointsPerGame,
                   Points21 = FantasyPoints
)

seasonstat = merge(seastat17, seastat18, by = c("PlayerID","Name"), all = TRUE)
seasonstat1 = merge(seasonstat, seastat19, by = c("PlayerID","Name"), all = TRUE)
seasonstat2 = merge(seasonstat1, seastat20, by = c("PlayerID","Name"), all = TRUE)
seasonstat3 = merge(seasonstat2, seastat21, by = c("PlayerID","Name"), all = TRUE)

###merge adp, projections, and seasonstat together

fantasya = merge(adp, projections, by = c("PlayerID", "Name","Team17","Team18", "Team19", "Team20", "Team21", "Position17", "Position18", "Position19", "Position20", "Position21"), all = TRUE)
fantasy = merge(fantasya, seasonstat3, by = c("PlayerID", "Name", "Team17", "Team18", "Team19", "Team20", "Team21", "Position17", "Position18", "Position19", "Position20", "Position21"), all = TRUE)

###the above fantasy merge is giving me issues with duplicates, using full join to solve issues
fantasyb = full_join(fantasya, seasonstat3, by = "PlayerID", all = TRUE)

###remove duplicate columns and rename
colnames(fantasyb)
fantasyb = subset(fantasyb, select = -c(38:40, 45,46, 51,52, 57,58, 63,64), all = TRUE)
colnames(fantasyb)
colnames(fantasyb) = gsub(pattern = ".x", replacement = "", colnames(fantasy))
colnames(fantasyb)
fantasy = fantasyb

####reorder columns
colnames(fantasy)
fantasy = fantasy[, c(1,2,14,41,16,45,18,49,20,53,22,57,24,39,25,40,27,43,28,44,30,47,31,48,33,51,34,52,36,55,37,56,8,13,9,15,10,17,11,19,12,21,23,38,26,42,29,46,32,50,35,54,3,4,5,6,7)]
colnames(fantasy)

######convert data to numeric
str(fantasy)

#####change position 17,18,19,20,21

fantasy["Position17"][fantasy["Position17"] == "NA"] = "0"
fantasy["Position17"][fantasy["Position17"] == "QB"] = "1"
fantasy["Position17"][fantasy["Position17"] == "RB"] = "2"
fantasy["Position17"][fantasy["Position17"] == "WR"] = "3"
fantasy["Position17"][fantasy["Position17"] == "TE"] = "4"
fantasy["Position17"][fantasy["Position17"] == "K"] = "5"
fantasy["Position17"][fantasy["Position17"] == "DST"] = "6"
unique(fantasy$Position17)

fantasy["Position18"][fantasy["Position18"] == "NA"] = "0"
fantasy["Position18"][fantasy["Position18"] == "QB"] = "1"
fantasy["Position18"][fantasy["Position18"] == "RB"] = "2"
fantasy["Position18"][fantasy["Position18"] == "WR"] = "3"
fantasy["Position18"][fantasy["Position18"] == "TE"] = "4"
fantasy["Position18"][fantasy["Position18"] == "K"] = "5"
fantasy["Position18"][fantasy["Position18"] == "DST"] = "6"
unique(fantasy$Position18)

fantasy["Position19"][fantasy["Position19"] == "NA"] = "0"
fantasy["Position19"][fantasy["Position19"] == "QB"] = "1"
fantasy["Position19"][fantasy["Position19"] == "RB"] = "2"
fantasy["Position19"][fantasy["Position19"] == "WR"] = "3"
fantasy["Position19"][fantasy["Position19"] == "TE"] = "4"
fantasy["Position19"][fantasy["Position19"] == "K"] = "5"
fantasy["Position19"][fantasy["Position19"] == "DST"] = "6"
unique(fantasy$Position19)

fantasy["Position20"][fantasy["Position20"] == "NA"] = "0"
fantasy["Position20"][fantasy["Position20"] == "QB"] = "1"
fantasy["Position20"][fantasy["Position20"] == "RB"] = "2"
fantasy["Position20"][fantasy["Position20"] == "WR"] = "3"
fantasy["Position20"][fantasy["Position20"] == "TE"] = "4"
fantasy["Position20"][fantasy["Position20"] == "K"] = "5"
fantasy["Position20"][fantasy["Position20"] == "DST"] = "6"
unique(fantasy$Position20)

fantasy["Position21"][fantasy["Position21"] == "NA"] = "0"
fantasy["Position21"][fantasy["Position21"] == "QB"] = "1"
fantasy["Position21"][fantasy["Position21"] == "RB"] = "2"
fantasy["Position21"][fantasy["Position21"] == "WR"] = "3"
fantasy["Position21"][fantasy["Position21"] == "TE"] = "4"
fantasy["Position21"][fantasy["Position21"] == "K"] = "5"
fantasy["Position21"][fantasy["Position21"] == "DST"] = "6"
unique(fantasy$Position21)

#####change team 17,18,19,20,21

unique(fantasy$Team17)
fantasy["Team17"][fantasy["Team17"] == "NA"] = "0"
fantasy["Team17"][fantasy["Team17"] == "ARI"] = "1"
fantasy["Team17"][fantasy["Team17"] == "ATL"] = "2"
fantasy["Team17"][fantasy["Team17"] == "BAL"] = "3"
fantasy["Team17"][fantasy["Team17"] == "BUF"] = "4"
fantasy["Team17"][fantasy["Team17"] == "CAR"] = "5"
fantasy["Team17"][fantasy["Team17"] == "CHI"] = "6"
fantasy["Team17"][fantasy["Team17"] == "CIN"] = "7"
fantasy["Team17"][fantasy["Team17"] == "CLE"] = "8"
fantasy["Team17"][fantasy["Team17"] == "DAL"] = "9"
fantasy["Team17"][fantasy["Team17"] == "DEN"] = "10"
fantasy["Team17"][fantasy["Team17"] == "DET"] = "11"
fantasy["Team17"][fantasy["Team17"] == "GB"] = "12"
fantasy["Team17"][fantasy["Team17"] == "HOU"] = "13"
fantasy["Team17"][fantasy["Team17"] == "IND"] = "14"
fantasy["Team17"][fantasy["Team17"] == "JAX"] = "15"
fantasy["Team17"][fantasy["Team17"] == "KC"] = "16"
fantasy["Team17"][fantasy["Team17"] == "LAC"] = "17"
fantasy["Team17"][fantasy["Team17"] == "LAR"] = "18"
fantasy["Team17"][fantasy["Team17"] == "LV"] = "19"
fantasy["Team17"][fantasy["Team17"] == "MIA"] = "20"
fantasy["Team17"][fantasy["Team17"] == "MIN"] = "21"
fantasy["Team17"][fantasy["Team17"] == "NE"] = "22"
fantasy["Team17"][fantasy["Team17"] == "NO"] = "23"
fantasy["Team17"][fantasy["Team17"] == "NYG"] = "24"
fantasy["Team17"][fantasy["Team17"] == "NYJ"] = "25"
fantasy["Team17"][fantasy["Team17"] == "PHI"] = "26"
fantasy["Team17"][fantasy["Team17"] == "PIT"] = "27"
fantasy["Team17"][fantasy["Team17"] == "SEA"] = "28"
fantasy["Team17"][fantasy["Team17"] == "SF"] = "29"
fantasy["Team17"][fantasy["Team17"] == "TB"] = "30"
fantasy["Team17"][fantasy["Team17"] == "TEN"] = "31"
fantasy["Team17"][fantasy["Team17"] == "WAS"] = "32"
unique(fantasy$Team17)

unique(fantasy$Team18)
fantasy["Team18"][fantasy["Team18"] == "ARI"] = "1"
fantasy["Team18"][fantasy["Team18"] == "ATL"] = "2"
fantasy["Team18"][fantasy["Team18"] == "BAL"] = "3"
fantasy["Team18"][fantasy["Team18"] == "BUF"] = "4"
fantasy["Team18"][fantasy["Team18"] == "CAR"] = "5"
fantasy["Team18"][fantasy["Team18"] == "CHI"] = "6"
fantasy["Team18"][fantasy["Team18"] == "CIN"] = "7"
fantasy["Team18"][fantasy["Team18"] == "CLE"] = "8"
fantasy["Team18"][fantasy["Team18"] == "DAL"] = "9"
fantasy["Team18"][fantasy["Team18"] == "DEN"] = "10"
fantasy["Team18"][fantasy["Team18"] == "DET"] = "11"
fantasy["Team18"][fantasy["Team18"] == "GB"] = "12"
fantasy["Team18"][fantasy["Team18"] == "HOU"] = "13"
fantasy["Team18"][fantasy["Team18"] == "IND"] = "14"
fantasy["Team18"][fantasy["Team18"] == "JAX"] = "15"
fantasy["Team18"][fantasy["Team18"] == "KC"] = "16"
fantasy["Team18"][fantasy["Team18"] == "LAC"] = "17"
fantasy["Team18"][fantasy["Team18"] == "LAR"] = "18"
fantasy["Team18"][fantasy["Team18"] == "LV"] = "19"
fantasy["Team18"][fantasy["Team18"] == "MIA"] = "20"
fantasy["Team18"][fantasy["Team18"] == "MIN"] = "21"
fantasy["Team18"][fantasy["Team18"] == "NE"] = "22"
fantasy["Team18"][fantasy["Team18"] == "NO"] = "23"
fantasy["Team18"][fantasy["Team18"] == "NYG"] = "24"
fantasy["Team18"][fantasy["Team18"] == "NYJ"] = "25"
fantasy["Team18"][fantasy["Team18"] == "PHI"] = "26"
fantasy["Team18"][fantasy["Team18"] == "PIT"] = "27"
fantasy["Team18"][fantasy["Team18"] == "SEA"] = "28"
fantasy["Team18"][fantasy["Team18"] == "SF"] = "29"
fantasy["Team18"][fantasy["Team18"] == "TB"] = "30"
fantasy["Team18"][fantasy["Team18"] == "TEN"] = "31"
fantasy["Team18"][fantasy["Team18"] == "WAS"] = "32"
unique(fantasy$Team18)

fantasy["Team19"][fantasy["Team19"] == "ARI"] = "1"
fantasy["Team19"][fantasy["Team19"] == "ATL"] = "2"
fantasy["Team19"][fantasy["Team19"] == "BAL"] = "3"
fantasy["Team19"][fantasy["Team19"] == "BUF"] = "4"
fantasy["Team19"][fantasy["Team19"] == "CAR"] = "5"
fantasy["Team19"][fantasy["Team19"] == "CHI"] = "6"
fantasy["Team19"][fantasy["Team19"] == "CIN"] = "7"
fantasy["Team19"][fantasy["Team19"] == "CLE"] = "8"
fantasy["Team19"][fantasy["Team19"] == "DAL"] = "9"
fantasy["Team19"][fantasy["Team19"] == "DEN"] = "10"
fantasy["Team19"][fantasy["Team19"] == "DET"] = "11"
fantasy["Team19"][fantasy["Team19"] == "GB"] = "12"
fantasy["Team19"][fantasy["Team19"] == "HOU"] = "13"
fantasy["Team19"][fantasy["Team19"] == "IND"] = "14"
fantasy["Team19"][fantasy["Team19"] == "JAX"] = "15"
fantasy["Team19"][fantasy["Team19"] == "KC"] = "16"
fantasy["Team19"][fantasy["Team19"] == "LAC"] = "17"
fantasy["Team19"][fantasy["Team19"] == "LAR"] = "18"
fantasy["Team19"][fantasy["Team19"] == "LV"] = "19"
fantasy["Team19"][fantasy["Team19"] == "MIA"] = "20"
fantasy["Team19"][fantasy["Team19"] == "MIN"] = "21"
fantasy["Team19"][fantasy["Team19"] == "NE"] = "22"
fantasy["Team19"][fantasy["Team19"] == "NO"] = "23"
fantasy["Team19"][fantasy["Team19"] == "NYG"] = "24"
fantasy["Team19"][fantasy["Team19"] == "NYJ"] = "25"
fantasy["Team19"][fantasy["Team19"] == "PHI"] = "26"
fantasy["Team19"][fantasy["Team19"] == "PIT"] = "27"
fantasy["Team19"][fantasy["Team19"] == "SEA"] = "28"
fantasy["Team19"][fantasy["Team19"] == "SF"] = "29"
fantasy["Team19"][fantasy["Team19"] == "TB"] = "30"
fantasy["Team19"][fantasy["Team19"] == "TEN"] = "31"
fantasy["Team19"][fantasy["Team19"] == "WAS"] = "32"
unique(fantasy$Team19)

fantasy["Team20"][fantasy["Team20"] == "ARI"] = "1"
fantasy["Team20"][fantasy["Team20"] == "ATL"] = "2"
fantasy["Team20"][fantasy["Team20"] == "BAL"] = "3"
fantasy["Team20"][fantasy["Team20"] == "BUF"] = "4"
fantasy["Team20"][fantasy["Team20"] == "CAR"] = "5"
fantasy["Team20"][fantasy["Team20"] == "CHI"] = "6"
fantasy["Team20"][fantasy["Team20"] == "CIN"] = "7"
fantasy["Team20"][fantasy["Team20"] == "CLE"] = "8"
fantasy["Team20"][fantasy["Team20"] == "DAL"] = "9"
fantasy["Team20"][fantasy["Team20"] == "DEN"] = "10"
fantasy["Team20"][fantasy["Team20"] == "DET"] = "11"
fantasy["Team20"][fantasy["Team20"] == "GB"] = "12"
fantasy["Team20"][fantasy["Team20"] == "HOU"] = "13"
fantasy["Team20"][fantasy["Team20"] == "IND"] = "14"
fantasy["Team20"][fantasy["Team20"] == "JAX"] = "15"
fantasy["Team20"][fantasy["Team20"] == "KC"] = "16"
fantasy["Team20"][fantasy["Team20"] == "LAC"] = "17"
fantasy["Team20"][fantasy["Team20"] == "LAR"] = "18"
fantasy["Team20"][fantasy["Team20"] == "LV"] = "19"
fantasy["Team20"][fantasy["Team20"] == "MIA"] = "20"
fantasy["Team20"][fantasy["Team20"] == "MIN"] = "21"
fantasy["Team20"][fantasy["Team20"] == "NE"] = "22"
fantasy["Team20"][fantasy["Team20"] == "NO"] = "23"
fantasy["Team20"][fantasy["Team20"] == "NYG"] = "24"
fantasy["Team20"][fantasy["Team20"] == "NYJ"] = "25"
fantasy["Team20"][fantasy["Team20"] == "PHI"] = "26"
fantasy["Team20"][fantasy["Team20"] == "PIT"] = "27"
fantasy["Team20"][fantasy["Team20"] == "SEA"] = "28"
fantasy["Team20"][fantasy["Team20"] == "SF"] = "29"
fantasy["Team20"][fantasy["Team20"] == "TB"] = "30"
fantasy["Team20"][fantasy["Team20"] == "TEN"] = "31"
fantasy["Team20"][fantasy["Team20"] == "WAS"] = "32"
unique(fantasy$Team20)

fantasy["Team21"][fantasy["Team21"] == "ARI"] = "1"
fantasy["Team21"][fantasy["Team21"] == "ATL"] = "2"
fantasy["Team21"][fantasy["Team21"] == "BAL"] = "3"
fantasy["Team21"][fantasy["Team21"] == "BUF"] = "4"
fantasy["Team21"][fantasy["Team21"] == "CAR"] = "5"
fantasy["Team21"][fantasy["Team21"] == "CHI"] = "6"
fantasy["Team21"][fantasy["Team21"] == "CIN"] = "7"
fantasy["Team21"][fantasy["Team21"] == "CLE"] = "8"
fantasy["Team21"][fantasy["Team21"] == "DAL"] = "9"
fantasy["Team21"][fantasy["Team21"] == "DEN"] = "10"
fantasy["Team21"][fantasy["Team21"] == "DET"] = "11"
fantasy["Team21"][fantasy["Team21"] == "GB"] = "12"
fantasy["Team21"][fantasy["Team21"] == "HOU"] = "13"
fantasy["Team21"][fantasy["Team21"] == "IND"] = "14"
fantasy["Team21"][fantasy["Team21"] == "JAX"] = "15"
fantasy["Team21"][fantasy["Team21"] == "KC"] = "16"
fantasy["Team21"][fantasy["Team21"] == "LAC"] = "17"
fantasy["Team21"][fantasy["Team21"] == "LAR"] = "18"
fantasy["Team21"][fantasy["Team21"] == "LV"] = "19"
fantasy["Team21"][fantasy["Team21"] == "MIA"] = "20"
fantasy["Team21"][fantasy["Team21"] == "MIN"] = "21"
fantasy["Team21"][fantasy["Team21"] == "NE"] = "22"
fantasy["Team21"][fantasy["Team21"] == "NO"] = "23"
fantasy["Team21"][fantasy["Team21"] == "NYG"] = "24"
fantasy["Team21"][fantasy["Team21"] == "NYJ"] = "25"
fantasy["Team21"][fantasy["Team21"] == "PHI"] = "26"
fantasy["Team21"][fantasy["Team21"] == "PIT"] = "27"
fantasy["Team21"][fantasy["Team21"] == "SEA"] = "28"
fantasy["Team21"][fantasy["Team21"] == "SF"] = "29"
fantasy["Team21"][fantasy["Team21"] == "TB"] = "30"
fantasy["Team21"][fantasy["Team21"] == "TEN"] = "31"
fantasy["Team21"][fantasy["Team21"] == "WAS"] = "32"
unique(fantasy$Team21)

#####change positionrank 17,18,19,20,21

fantasy1 = fantasy[!is.na(fantasy$Name), ] 
fantasy = fantasy1

fantasy$PositionRank17 = gsub("QB", "", fantasy$PositionRank17)
fantasy$PositionRank17 = gsub("RB", "", fantasy$PositionRank17)
fantasy$PositionRank17 = gsub("WR", "", fantasy$PositionRank17)
fantasy$PositionRank17 = gsub("TE", "", fantasy$PositionRank17)
fantasy$PositionRank17 = gsub("K", "", fantasy$PositionRank17)
fantasy$PositionRank17 = gsub("DST", "", fantasy$PositionRank17)

fantasy$PositionRank18 = gsub("QB", "", fantasy$PositionRank18)
fantasy$PositionRank18 = gsub("RB", "", fantasy$PositionRank18)
fantasy$PositionRank18 = gsub("WR", "", fantasy$PositionRank18)
fantasy$PositionRank18 = gsub("TE", "", fantasy$PositionRank18)
fantasy$PositionRank18 = gsub("K", "", fantasy$PositionRank18)
fantasy$PositionRank18 = gsub("DST", "", fantasy$PositionRank18)

fantasy$PositionRank19 = gsub("QB", "", fantasy$PositionRank19)
fantasy$PositionRank19 = gsub("RB", "", fantasy$PositionRank19)
fantasy$PositionRank19 = gsub("WR", "", fantasy$PositionRank19)
fantasy$PositionRank19 = gsub("TE", "", fantasy$PositionRank19)
fantasy$PositionRank19 = gsub("K", "", fantasy$PositionRank19)
fantasy$PositionRank19 = gsub("DST", "", fantasy$PositionRank19)

fantasy$PositionRank20 = gsub("QB", "", fantasy$PositionRank20)
fantasy$PositionRank20 = gsub("RB", "", fantasy$PositionRank20)
fantasy$PositionRank20 = gsub("WR", "", fantasy$PositionRank20)
fantasy$PositionRank20 = gsub("TE", "", fantasy$PositionRank20)
fantasy$PositionRank20 = gsub("K", "", fantasy$PositionRank20)
fantasy$PositionRank20 = gsub("DST", "", fantasy$PositionRank20)

fantasy$PositionRank21 = gsub("QB", "", fantasy$PositionRank21)
fantasy$PositionRank21 = gsub("RB", "", fantasy$PositionRank21)
fantasy$PositionRank21 = gsub("WR", "", fantasy$PositionRank21)
fantasy$PositionRank21 = gsub("TE", "", fantasy$PositionRank21)
fantasy$PositionRank21 = gsub("K", "", fantasy$PositionRank21)
fantasy$PositionRank21 = gsub("DST", "", fantasy$PositionRank21)

######check for and remove duplicates
ID = fantasy$PlayerID
duplicated(ID)

fantasy2 = subset(fantasy, duplicated(`PlayerID`),)
fantasy = fantasy[!duplicated(fantasy$PlayerID),]
ID1 = fantasy$PlayerID
duplicated(ID1)

#Destruct fantasy data frame into season based dataframes
colnames(fantasy)

fantasy17 = fantasy[c(1:4,13:16,33,34,43,44,53)]
fantasy18 = fantasy[c(1,2,5,6,17:20,35,36,45,46,54)]
fantasy19 = fantasy[c(1,2,7,8,21:24,37,38,47,48,55)]
fantasy20 = fantasy[c(1,2,9,10,25:28,39,40,49,50,56)]
fantasy21 = fantasy[c(1,2,11,12,29:32,41,42,51,52,57)]

fantasy17$Season = "2017"
fantasy18$Season = "2018"
fantasy19$Season = "2019"
fantasy20$Season = "2020"
fantasy21$Season = "2021"

##merge in new data
biowr = merge(`17.WR.PP.I`,`18.WR.PP.I`)
biowr1 = merge(`19.WR.PP.I`,`20.WR.PP.I`)
biowr2 = merge(`21.WR.PP.I`,biowr)
biowr0 = merge(biowr,biowr1)

ID1 = biowr0$Full.Name
duplicated(ID1)

biomerge = merge(`17.21.QB.PP.I`,`17.21.RB.PP.I`, by = c("Full.Name", "Position", "Athleticism.Score", "SPARQ.x", "Breakout.Year", "Breakout.Age"), all = TRUE)
biomerge1 = merge(biomerge, `17.21.TE.PP.I`, by = c("Full.Name", "Position", "Athleticism.Score", "SPARQ.x", "Breakout.Year", "Breakout.Age", "College.Dominator.Rating", "Special.Status"), all = TRUE)
playerbio = merge(biowr0, biomerge1, by = c("Full.Name", "Position", "Athleticism.Score", "SPARQ.x", "College.Dominator.Rating", "Special.Status", "Position.Type"), all = TRUE)

playerbio = playerbio[-c(6,14:18)]
playerbio = playerbio %>% rename(College.Receiver.Rating = Receiver.Rating)

ID2 = playerbio$Full.Name
duplicated(ID2)

###intersect and join all positions

common0 = intersect(colnames(`17.WR.PP`), colnames(`17.18.TE.PP`))
merge20 = join(`17.WR.PP`, `17.18.TE.PP`, by = "Full.Name", type = "full", match = "all")
common1 = intersect(colnames(merge20), colnames(`18.19.WR.PP`))
merge21 = join(merge20, `18.19.WR.PP`, by = "Full.Name", type = "full", match = "all")
common2 = intersect(colnames(merge21), colnames(`19.21.TE.PP`))
merge22 = join(merge21, `19.21.TE.PP`, by = "Full.Name", type = "full", match = "all")
common3 = intersect(colnames(merge22), colnames(`20.21.WR.PP`))
merge23 = join(merge22, `20.21.WR.PP`, by = "Full.Name", type = "full", match = "all")
merge23 = transform(merge23, `Snaps..2017.` = as.character(`Snaps..2017.`))
common4 = intersect(colnames(merge23), colnames(`2017.2018.RB.PP`))
merge24 = join(merge23, `2017.2018.RB.PP`, by = "Full.Name", type = "full", match = "all")
common5 = intersect(colnames(merge24), colnames(`2017.2021.QB.PP`))
merge25 = join(merge24, `2017.2021.QB.PP`, by = "Full.Name", type = "full", match = "all")
common6 = intersect(colnames(merge25), colnames(`2019.2021.RB.PP`))
merge26 = join(merge25, `2019.2021.RB.PP`, by = "Full.Name", type = "full", match = "all")

colnames(merge26)

fanstats17 = merge26[c(1:12,21,58,60,62,64,66,68,73,78,83,88)]
fanstats18 = merge26[c(1:2,13:20,22,23,30,59,61,63,65,67,69,74,79,84,89)]
fanstats19 = merge26[c(1:2,24:29,31:34,51,70,75,80,85,90,93,96,99,102,105)]
fanstats20 = merge26[c(1:2,35,37,39,41,43,45,47,49,52,54,56,71,76,81,86,91,94,97,100,103,106)]
fanstats21 = merge26[c(1:2,36,38,40,42,44,46,48,50,53,55,57,72,77,82,87,92,95,98,101,104,107)]

fanstats17$Season = "2017"
fanstats18$Season = "2018"
fanstats19$Season = "2019"
fanstats20$Season = "2020"
fanstats21$Season = "2021"

###rename columns

colnames(fanstats17)
fanstats17 = rename(fanstats17, c("Full.Name" = "Name",
                                  "Dominator.Rating..2017." = "Dominator",
                                  "VOS..2017." = "VOS",
                                  "Receiving.Yards..2017." = "RecYds",
                                  "Targets..2017." = "Targets",
                                  "Receptions..2017." = "Receptions",
                                  "Air.Yards..2017." = "AirYds",
                                  "Snaps..2017." = "Snaps",
                                  "Routes.Run..2017." = "RoutesRun",
                                  "Yards.After.Catch..2017." = "YAC",
                                  "Total.Touchdowns..2017." = "TDs",
                                  "Completed.Air.Yards..2017." = "CmpltdAirYds",
                                  "Carries..2017." = "Carries",
                                  "Weighted.Opportunities..2017." = "WghtdOpp",
                                  "Snap.Share..2017." = "SnapShare",
                                  "Opportunity.Share..2017." = "OppShare",
                                  "Rushing.Yards..2017." = "RushYds",
                                  "Passing.Touchdowns..2017." = "PassTD",
                                  "Passing.Yards..2017." = "PassYds",
                                  "Red.Zone.Attempts..2017." = "RZAtt",
                                  "Deep.Ball.Attempts..2017." = "DeepBallAtt",
                                  "Red.Zone.Carries..2017." = "RZCarries"))

fanstats18 = rename(fanstats18, c("Full.Name" = "Name",
                                  "Dominator.Rating..2018." = "Dominator",
                                  "VOS..2018." = "VOS",
                                  "Receiving.Yards..2018." = "RecYds",
                                  "Targets..2018." = "Targets",
                                  "Receptions..2018." = "Receptions",
                                  "Air.Yards..2018." = "AirYds",
                                  "Snaps..2018." = "Snaps",
                                  "Routes.Run..2018." = "RoutesRun",
                                  "Yards.After.Catch..2018." = "YAC",
                                  "Total.Touchdowns..2018." = "TDs",
                                  "Completed.Air.Yards..2018." = "CmpltdAirYds",
                                  "Carries..2018." = "Carries",
                                  "Weighted.Opportunities..2018." = "WghtdOpp",
                                  "Snap.Share..2018." = "SnapShare",
                                  "Opportunity.Share..2018." = "OppShare",
                                  "Rushing.Yards..2018." = "RushYds",
                                  "Passing.Touchdowns..2018." = "PassTD",
                                  "Passing.Yards..2018." = "PassYds",
                                  "Red.Zone.Attempts..2018." = "RZAtt",
                                  "Deep.Ball.Attempts..2018." = "DeepBallAtt",
                                  "Red.Zone.Carries..2018." = "RZCarries"))

fanstats19 = rename(fanstats19, c("Full.Name" = "Name",
                                  "Dominator.Rating..2019." = "Dominator",
                                  "VOS..2019." = "VOS",
                                  "Receiving.Yards..2019." = "RecYds",
                                  "Targets..2019." = "Targets",
                                  "Receptions..2019." = "Receptions",
                                  "Air.Yards..2019." = "AirYds",
                                  "Snaps..2019." = "Snaps",
                                  "Routes.Run..2019." = "RoutesRun",
                                  "Yards.After.Catch..2019." = "YAC",
                                  "Total.Touchdowns..2019." = "TDs",
                                  "Completed.Air.Yards..2019." = "CmpltdAirYds",
                                  "Carries..2019." = "Carries",
                                  "Weighted.Opportunities..2019." = "WghtdOpp",
                                  "Snap.Share..2019." = "SnapShare",
                                  "Opportunity.Share..2019." = "OppShare",
                                  "Rushing.Yards..2019." = "RushYds",
                                  "Passing.Touchdowns..2019." = "PassTD",
                                  "Passing.Yards..2019." = "PassYds",
                                  "Red.Zone.Attempts..2019." = "RZAtt",
                                  "Deep.Ball.Attempts..2019." = "DeepBallAtt",
                                  "Red.Zone.Carries..2019." = "RZCarries"))

fanstats20 = rename(fanstats20, c("Full.Name" = "Name",
                                  "Dominator.Rating..2020." = "Dominator",
                                  "VOS..2020." = "VOS",
                                  "Receiving.Yards..2020." = "RecYds",
                                  "Targets..2020." = "Targets",
                                  "Receptions..2020." = "Receptions",
                                  "Air.Yards..2020." = "AirYds",
                                  "Snaps..2020." = "Snaps",
                                  "Routes.Run..2020." = "RoutesRun",
                                  "Yards.After.Catch..2020." = "YAC",
                                  "Total.Touchdowns..2020." = "TDs",
                                  "Completed.Air.Yards..2020." = "CmpltdAirYds",
                                  "Carries..2020." = "Carries",
                                  "Weighted.Opportunities..2020." = "WghtdOpp",
                                  "Snap.Share..2020." = "SnapShare",
                                  "Opportunity.Share..2020." = "OppShare",
                                  "Rushing.Yards..2020." = "RushYds",
                                  "Passing.Touchdowns..2020." = "PassTD",
                                  "Passing.Yards..2020." = "PassYds",
                                  "Red.Zone.Attempts..2020." = "RZAtt",
                                  "Deep.Ball.Attempts..2020." = "DeepBallAtt",
                                  "Red.Zone.Carries..2020." = "RZCarries"))

fanstats21 = rename(fanstats21, c("Full.Name" = "Name",
                                  "Dominator.Rating..2021." = "Dominator",
                                  "VOS..2021." = "VOS",
                                  "Receiving.Yards..2021." = "RecYds",
                                  "Targets..2021." = "Targets",
                                  "Receptions..2021." = "Receptions",
                                  "Air.Yards..2021." = "AirYds",
                                  "Snaps..2021." = "Snaps",
                                  "Routes.Run..2021." = "RoutesRun",
                                  "Yards.After.Catch..2021." = "YAC",
                                  "Total.Touchdowns..2021." = "TDs",
                                  "Completed.Air.Yards..2021." = "CmpltdAirYds",
                                  "Carries..2021." = "Carries",
                                  "Weighted.Opportunities..2021." = "WghtdOpp",
                                  "Snap.Share..2021." = "SnapShare",
                                  "Opportunity.Share..2021." = "OppShare",
                                  "Rushing.Yards..2021." = "RushYds",
                                  "Passing.Touchdowns..2021." = "PassTD",
                                  "Passing.Yards..2021." = "PassYds",
                                  "Red.Zone.Attempts..2021." = "RZAtt",
                                  "Deep.Ball.Attempts..2021." = "DeepBallAtt",
                                  "Red.Zone.Carries..2021." = "RZCarries"))

common10 = intersect(colnames(`fantasy17`), colnames(`fanstats17`))
common10
together17 = join(fantasy17, fanstats17, by = c("Name", "Season"), type = "full", match = "all")
ID3 = together17$Name
duplicated(ID3)

common11 = intersect(colnames(`fantasy18`), colnames(`fanstats18`))
common11
together18 = join(fantasy18, fanstats18, by = c("Name", "Season"), type = "full", match = "all")

common12 = intersect(colnames(`fantasy19`), colnames(`fanstats19`))
common12
together19 = join(fantasy19, fanstats19, by = c("Name", "Season"), type = "full", match = "all")

common13 = intersect(colnames(`fantasy20`), colnames(`fanstats20`))
common13
together20 = join(fantasy20, fanstats20, by = c("Name", "Season"), type = "full", match = "all")

common14 = intersect(colnames(`fantasy21`), colnames(`fanstats21`))
common14
together21 = join(fantasy21, fanstats21, by = c("Name", "Season"), type = "full", match = "all")

###join in player information

playerbio = rename(`playerbio`, c("Full.Name" = "Name"))
common40 = intersect(colnames(`together17`), colnames(`playerbio`))
common40
fantasize17 = join(together17, playerbio, by = c("Name", "Position"), type = "full", match = "all")
fantasize18 = join(together18, playerbio, by = c("Name", "Position"), type = "full", match = "all")
fantasize19 = join(together19, playerbio, by = c("Name", "Position"), type = "full", match = "all")
fantasize20 = join(together20, playerbio, by = c("Name", "Position"), type = "full", match = "all")
fantasize21 = join(together21, playerbio, by = c("Name", "Position"), type = "full", match = "all")

colnames(fantasize17) = gsub(pattern = "17", replacement = "", colnames(fantasize17))
colnames(fantasize18) = gsub(pattern = "18", replacement = "", colnames(fantasize18))
colnames(fantasize19) = gsub(pattern = "19", replacement = "", colnames(fantasize19))
colnames(fantasize20) = gsub(pattern = "20", replacement = "", colnames(fantasize20))
colnames(fantasize21) = gsub(pattern = "21", replacement = "", colnames(fantasize21))

###join seasonal dataframes into one big dataframe

fanatic0 = join(fantasize17, fantasize18, by = c(), type = "full", match = "all")
fanatic1 = join(fanatic0, fantasize19, by = c(), type = "full", match = "all")
fanatic2 = join(fanatic1, fantasize20, by = c(), type = "full", match = "all")
fanatic = join(fanatic2, fantasize21, by = c(), type = "full", match = "all")

fanatic = fanatic %>% select("Season", everything())

###Join team preseason odds and offensive line rankings

colnames(`17Odds`) = gsub(pattern = "Year", replacement = "Season", colnames(`17Odds`))
colnames(`18Odds`) = gsub(pattern = "Year", replacement = "Season", colnames(`18Odds`))
colnames(`19Odds`) = gsub(pattern = "Year", replacement = "Season", colnames(`19Odds`))
colnames(`20Odds`) = gsub(pattern = "Year", replacement = "Season", colnames(`20Odds`))
colnames(`21Odds`) = gsub(pattern = "Year", replacement = "Season", colnames(`21Odds`))

####replace team name with number

unique(`17Odds`$Team)
`17Odds`["Team"][`17Odds`["Team"] == "NA"] = "0"
`17Odds`["Team"][`17Odds`["Team"] == "Arizona Cardinals"] = "1"
`17Odds`["Team"][`17Odds`["Team"] == "Atlanta Falcons"] = "2"
`17Odds`["Team"][`17Odds`["Team"] == "Baltimore Ravens"] = "3"
`17Odds`["Team"][`17Odds`["Team"] == "Buffalo Bills"] = "4"
`17Odds`["Team"][`17Odds`["Team"] == "Carolina Panthers"] = "5"
`17Odds`["Team"][`17Odds`["Team"] == "Chicago Bears"] = "6"
`17Odds`["Team"][`17Odds`["Team"] == "Cincinnati Bengals"] = "7"
`17Odds`["Team"][`17Odds`["Team"] == "Cleveland Browns"] = "8"
`17Odds`["Team"][`17Odds`["Team"] == "Dallas Cowboys"] = "9"
`17Odds`["Team"][`17Odds`["Team"] == "Denver Broncos"] = "10"
`17Odds`["Team"][`17Odds`["Team"] == "Detroit Lions"] = "11"
`17Odds`["Team"][`17Odds`["Team"] == "Green Bay Packers"] = "12"
`17Odds`["Team"][`17Odds`["Team"] == "Houston Texans"] = "13"
`17Odds`["Team"][`17Odds`["Team"] == "Indianapolis Colts"] = "14"
`17Odds`["Team"][`17Odds`["Team"] == "Jacksonville Jaguars"] = "15"
`17Odds`["Team"][`17Odds`["Team"] == "Kansas City Chiefs"] = "16"
`17Odds`["Team"][`17Odds`["Team"] == "Los Angeles Chargers"] = "17"
`17Odds`["Team"][`17Odds`["Team"] == "Los Angeles Rams"] = "18"
`17Odds`["Team"][`17Odds`["Team"] == "Oakland Raiders"] = "19"
`17Odds`["Team"][`17Odds`["Team"] == "Miami Dolphins"] = "20"
`17Odds`["Team"][`17Odds`["Team"] == "Minnesota Vikings"] = "21"
`17Odds`["Team"][`17Odds`["Team"] == "New England Patriots"] = "22"
`17Odds`["Team"][`17Odds`["Team"] == "New Orleans Saints"] = "23"
`17Odds`["Team"][`17Odds`["Team"] == "New York Giants"] = "24"
`17Odds`["Team"][`17Odds`["Team"] == "New York Jets"] = "25"
`17Odds`["Team"][`17Odds`["Team"] == "Philadelphia Eagles"] = "26"
`17Odds`["Team"][`17Odds`["Team"] == "Pittsburgh Steelers"] = "27"
`17Odds`["Team"][`17Odds`["Team"] == "Seattle Seahawks"] = "28"
`17Odds`["Team"][`17Odds`["Team"] == "San Francisco 49ers"] = "29"
`17Odds`["Team"][`17Odds`["Team"] == "Tampa Bay Buccaneers"] = "30"
`17Odds`["Team"][`17Odds`["Team"] == "Tennessee Titans"] = "31"
`17Odds`["Team"][`17Odds`["Team"] == "Washington Redskins"] = "32"
unique(`17Odds`$Team)

unique(`18Odds`$Team)
`18Odds`["Team"][`18Odds`["Team"] == "NA"] = "0"
`18Odds`["Team"][`18Odds`["Team"] == "Arizona Cardinals"] = "1"
`18Odds`["Team"][`18Odds`["Team"] == "Atlanta Falcons"] = "2"
`18Odds`["Team"][`18Odds`["Team"] == "Baltimore Ravens"] = "3"
`18Odds`["Team"][`18Odds`["Team"] == "Buffalo Bills"] = "4"
`18Odds`["Team"][`18Odds`["Team"] == "Carolina Panthers"] = "5"
`18Odds`["Team"][`18Odds`["Team"] == "Chicago Bears"] = "6"
`18Odds`["Team"][`18Odds`["Team"] == "Cincinnati Bengals"] = "7"
`18Odds`["Team"][`18Odds`["Team"] == "Cleveland Browns"] = "8"
`18Odds`["Team"][`18Odds`["Team"] == "Dallas Cowboys"] = "9"
`18Odds`["Team"][`18Odds`["Team"] == "Denver Broncos"] = "10"
`18Odds`["Team"][`18Odds`["Team"] == "Detroit Lions"] = "11"
`18Odds`["Team"][`18Odds`["Team"] == "Green Bay Packers"] = "12"
`18Odds`["Team"][`18Odds`["Team"] == "Houston Texans"] = "13"
`18Odds`["Team"][`18Odds`["Team"] == "Indianapolis Colts"] = "14"
`18Odds`["Team"][`18Odds`["Team"] == "Jacksonville Jaguars"] = "15"
`18Odds`["Team"][`18Odds`["Team"] == "Kansas City Chiefs"] = "16"
`18Odds`["Team"][`18Odds`["Team"] == "Los Angeles Chargers"] = "17"
`18Odds`["Team"][`18Odds`["Team"] == "Los Angeles Rams"] = "18"
`18Odds`["Team"][`18Odds`["Team"] == "Oakland Raiders"] = "19"
`18Odds`["Team"][`18Odds`["Team"] == "Miami Dolphins"] = "20"
`18Odds`["Team"][`18Odds`["Team"] == "Minnesota Vikings"] = "21"
`18Odds`["Team"][`18Odds`["Team"] == "New England Patriots"] = "22"
`18Odds`["Team"][`18Odds`["Team"] == "New Orleans Saints"] = "23"
`18Odds`["Team"][`18Odds`["Team"] == "New York Giants"] = "24"
`18Odds`["Team"][`18Odds`["Team"] == "New York Jets"] = "25"
`18Odds`["Team"][`18Odds`["Team"] == "Philadelphia Eagles"] = "26"
`18Odds`["Team"][`18Odds`["Team"] == "Pittsburgh Steelers"] = "27"
`18Odds`["Team"][`18Odds`["Team"] == "Seattle Seahawks"] = "28"
`18Odds`["Team"][`18Odds`["Team"] == "San Francisco 49ers"] = "29"
`18Odds`["Team"][`18Odds`["Team"] == "Tampa Bay Buccaneers"] = "30"
`18Odds`["Team"][`18Odds`["Team"] == "Tennessee Titans"] = "31"
`18Odds`["Team"][`18Odds`["Team"] == "Washington Redskins"] = "32"
unique(`18Odds`$Team)

`19Odds`["Team"][`19Odds`["Team"] == "NA"] = "0"
`19Odds`["Team"][`19Odds`["Team"] == "Arizona Cardinals"] = "1"
`19Odds`["Team"][`19Odds`["Team"] == "Atlanta Falcons"] = "2"
`19Odds`["Team"][`19Odds`["Team"] == "Baltimore Ravens"] = "3"
`19Odds`["Team"][`19Odds`["Team"] == "Buffalo Bills"] = "4"
`19Odds`["Team"][`19Odds`["Team"] == "Carolina Panthers"] = "5"
`19Odds`["Team"][`19Odds`["Team"] == "Chicago Bears"] = "6"
`19Odds`["Team"][`19Odds`["Team"] == "Cincinnati Bengals"] = "7"
`19Odds`["Team"][`19Odds`["Team"] == "Cleveland Browns"] = "8"
`19Odds`["Team"][`19Odds`["Team"] == "Dallas Cowboys"] = "9"
`19Odds`["Team"][`19Odds`["Team"] == "Denver Broncos"] = "10"
`19Odds`["Team"][`19Odds`["Team"] == "Detroit Lions"] = "11"
`19Odds`["Team"][`19Odds`["Team"] == "Green Bay Packers"] = "12"
`19Odds`["Team"][`19Odds`["Team"] == "Houston Texans"] = "13"
`19Odds`["Team"][`19Odds`["Team"] == "Indianapolis Colts"] = "14"
`19Odds`["Team"][`19Odds`["Team"] == "Jacksonville Jaguars"] = "15"
`19Odds`["Team"][`19Odds`["Team"] == "Kansas City Chiefs"] = "16"
`19Odds`["Team"][`19Odds`["Team"] == "Los Angeles Chargers"] = "17"
`19Odds`["Team"][`19Odds`["Team"] == "Los Angeles Rams"] = "18"
`19Odds`["Team"][`19Odds`["Team"] == "Oakland Raiders"] = "19"
`19Odds`["Team"][`19Odds`["Team"] == "Miami Dolphins"] = "20"
`19Odds`["Team"][`19Odds`["Team"] == "Minnesota Vikings"] = "21"
`19Odds`["Team"][`19Odds`["Team"] == "New England Patriots"] = "22"
`19Odds`["Team"][`19Odds`["Team"] == "New Orleans Saints"] = "23"
`19Odds`["Team"][`19Odds`["Team"] == "New York Giants"] = "24"
`19Odds`["Team"][`19Odds`["Team"] == "New York Jets"] = "25"
`19Odds`["Team"][`19Odds`["Team"] == "Philadelphia Eagles"] = "26"
`19Odds`["Team"][`19Odds`["Team"] == "Pittsburgh Steelers"] = "27"
`19Odds`["Team"][`19Odds`["Team"] == "Seattle Seahawks"] = "28"
`19Odds`["Team"][`19Odds`["Team"] == "San Francisco 49ers"] = "29"
`19Odds`["Team"][`19Odds`["Team"] == "Tampa Bay Buccaneers"] = "30"
`19Odds`["Team"][`19Odds`["Team"] == "Tennessee Titans"] = "31"
`19Odds`["Team"][`19Odds`["Team"] == "Washington Redskins"] = "32"
unique(`18Odds`$Team)

`20Odds`["Team"][`20Odds`["Team"] == "NA"] = "0"
`20Odds`["Team"][`20Odds`["Team"] == "Arizona Cardinals"] = "1"
`20Odds`["Team"][`20Odds`["Team"] == "Atlanta Falcons"] = "2"
`20Odds`["Team"][`20Odds`["Team"] == "Baltimore Ravens"] = "3"
`20Odds`["Team"][`20Odds`["Team"] == "Buffalo Bills"] = "4"
`20Odds`["Team"][`20Odds`["Team"] == "Carolina Panthers"] = "5"
`20Odds`["Team"][`20Odds`["Team"] == "Chicago Bears"] = "6"
`20Odds`["Team"][`20Odds`["Team"] == "Cincinnati Bengals"] = "7"
`20Odds`["Team"][`20Odds`["Team"] == "Cleveland Browns"] = "8"
`20Odds`["Team"][`20Odds`["Team"] == "Dallas Cowboys"] = "9"
`20Odds`["Team"][`20Odds`["Team"] == "Denver Broncos"] = "10"
`20Odds`["Team"][`20Odds`["Team"] == "Detroit Lions"] = "11"
`20Odds`["Team"][`20Odds`["Team"] == "Green Bay Packers"] = "12"
`20Odds`["Team"][`20Odds`["Team"] == "Houston Texans"] = "13"
`20Odds`["Team"][`20Odds`["Team"] == "Indianapolis Colts"] = "14"
`20Odds`["Team"][`20Odds`["Team"] == "Jacksonville Jaguars"] = "15"
`20Odds`["Team"][`20Odds`["Team"] == "Kansas City Chiefs"] = "16"
`20Odds`["Team"][`20Odds`["Team"] == "Los Angeles Chargers"] = "17"
`20Odds`["Team"][`20Odds`["Team"] == "Los Angeles Rams"] = "18"
`20Odds`["Team"][`20Odds`["Team"] == "Las Vegas Raiders"] = "19"
`20Odds`["Team"][`20Odds`["Team"] == "Miami Dolphins"] = "20"
`20Odds`["Team"][`20Odds`["Team"] == "Minnesota Vikings"] = "21"
`20Odds`["Team"][`20Odds`["Team"] == "New England Patriots"] = "22"
`20Odds`["Team"][`20Odds`["Team"] == "New Orleans Saints"] = "23"
`20Odds`["Team"][`20Odds`["Team"] == "New York Giants"] = "24"
`20Odds`["Team"][`20Odds`["Team"] == "New York Jets"] = "25"
`20Odds`["Team"][`20Odds`["Team"] == "Philadelphia Eagles"] = "26"
`20Odds`["Team"][`20Odds`["Team"] == "Pittsburgh Steelers"] = "27"
`20Odds`["Team"][`20Odds`["Team"] == "Seattle Seahawks"] = "28"
`20Odds`["Team"][`20Odds`["Team"] == "San Francisco 49ers"] = "29"
`20Odds`["Team"][`20Odds`["Team"] == "Tampa Bay Buccaneers"] = "30"
`20Odds`["Team"][`20Odds`["Team"] == "Tennessee Titans"] = "31"
`20Odds`["Team"][`20Odds`["Team"] == "Washington Football Team"] = "32"
unique(`20Odds`$Team)

`21Odds`["Team"][`21Odds`["Team"] == "NA"] = "0"
`21Odds`["Team"][`21Odds`["Team"] == "Arizona Cardinals"] = "1"
`21Odds`["Team"][`21Odds`["Team"] == "Atlanta Falcons"] = "2"
`21Odds`["Team"][`21Odds`["Team"] == "Baltimore Ravens"] = "3"
`21Odds`["Team"][`21Odds`["Team"] == "Buffalo Bills"] = "4"
`21Odds`["Team"][`21Odds`["Team"] == "Carolina Panthers"] = "5"
`21Odds`["Team"][`21Odds`["Team"] == "Chicago Bears"] = "6"
`21Odds`["Team"][`21Odds`["Team"] == "Cincinnati Bengals"] = "7"
`21Odds`["Team"][`21Odds`["Team"] == "Cleveland Browns"] = "8"
`21Odds`["Team"][`21Odds`["Team"] == "Dallas Cowboys"] = "9"
`21Odds`["Team"][`21Odds`["Team"] == "Denver Broncos"] = "10"
`21Odds`["Team"][`21Odds`["Team"] == "Detroit Lions"] = "11"
`21Odds`["Team"][`21Odds`["Team"] == "Green Bay Packers"] = "12"
`21Odds`["Team"][`21Odds`["Team"] == "Houston Texans"] = "13"
`21Odds`["Team"][`21Odds`["Team"] == "Indianapolis Colts"] = "14"
`21Odds`["Team"][`21Odds`["Team"] == "Jacksonville Jaguars"] = "15"
`21Odds`["Team"][`21Odds`["Team"] == "Kansas City Chiefs"] = "16"
`21Odds`["Team"][`21Odds`["Team"] == "Los Angeles Chargers"] = "17"
`21Odds`["Team"][`21Odds`["Team"] == "Los Angeles Rams"] = "18"
`21Odds`["Team"][`21Odds`["Team"] == "Las Vegas Raiders"] = "19"
`21Odds`["Team"][`21Odds`["Team"] == "Miami Dolphins"] = "20"
`21Odds`["Team"][`21Odds`["Team"] == "Minnesota Vikings"] = "21"
`21Odds`["Team"][`21Odds`["Team"] == "New England Patriots"] = "22"
`21Odds`["Team"][`21Odds`["Team"] == "New Orleans Saints"] = "23"
`21Odds`["Team"][`21Odds`["Team"] == "New York Giants"] = "24"
`21Odds`["Team"][`21Odds`["Team"] == "New York Jets"] = "25"
`21Odds`["Team"][`21Odds`["Team"] == "Philadelphia Eagles"] = "26"
`21Odds`["Team"][`21Odds`["Team"] == "Pittsburgh Steelers"] = "27"
`21Odds`["Team"][`21Odds`["Team"] == "Seattle Seahawks"] = "28"
`21Odds`["Team"][`21Odds`["Team"] == "San Francisco 49ers"] = "29"
`21Odds`["Team"][`21Odds`["Team"] == "Tampa Bay Buccaneers"] = "30"
`21Odds`["Team"][`21Odds`["Team"] == "Tennessee Titans"] = "31"
`21Odds`["Team"][`21Odds`["Team"] == "Washington Football Team"] = "32"
unique(`21Odds`$Team)

oddsm = join(`17Odds`, `18Odds`, by = c(), type = "full", match = "all")
oddsm1 = join(oddsm, `19Odds`, by = c(), type = "full", match = "all")
oddsm2 = join(oddsm1, `20Odds`, by = c(), type = "full", match = "all")
oddsm3 = join(oddsm2, `21Odds`, by = c(), type = "full", match = "all")

fanatic4 = join(fanatic, oddsm3, by = c(), type = "full", match = "all")
fanatic = fanatic4

###seperate into Player position dataframes

fanatix = subset(fanatic, ADP <=250)
fanatix = subset(fanatix, Points > 0)

colnames(fanatix)

fanatixqb = subset(fanatix, Position == 1)
fanatixrb = subset(fanatix, Position == 2)
fanatixwr = subset(fanatix, Position == 3)
fanatixte = subset(fanatix, Position == 4)
fanatixk = subset(fanatix, Position == 5)
fanatixdef = subset(fanatix, Position == 6)

###list each players age when season started

qbbday = qbbdaydata
bday = c(qbbday$Birth.Date)
qbbday$BDay = as.Date(bday, "%m/%d/%y")
qbbday$start17 = "2017-09-07" 
qbbday$age17 = as.numeric(difftime(qbbday$start17,qbbday$BDay, units = "weeks"))/52.25

qbbday$start18 = "2018-09-06" 
qbbday$age18 = as.numeric(difftime(qbbday$start18,qbbday$BDay, units = "weeks"))/52.25

qbbday$start19 = "2019-09-05" 
qbbday$age19 = as.numeric(difftime(qbbday$start19,qbbday$BDay, units = "weeks"))/52.25

qbbday$start20 = "2020-09-10" 
qbbday$age20 = as.numeric(difftime(qbbday$start20,qbbday$BDay, units = "weeks"))/52.25

qbbday$start21 = "2021-09-09" 
qbbday$age21 = as.numeric(difftime(qbbday$start21,qbbday$BDay, units = "weeks"))/52.25

colnames(qbbday17) = c("Name", "Age")
qbbday17$Season = "2017"
qbbday = qbbday[-c(2,3)]
qbbday17 = qbbday[c(1,4)]
qbbday18 = qbbday[c(1,6)]
qbbday19 = qbbday[c(1,8)]
qbbday20 = qbbday[c(1,10)]
qbbday21 = qbbday[c(1,12)]

colnames(qbbday17) = c("Name", "Age")
qbbday17$Season = "2017"
colnames(qbbday18) = c("Name", "Age")
qbbday18$Season = "2018"
colnames(qbbday19) = c("Name", "Age")
qbbday19$Season = "2019"
colnames(qbbday20) = c("Name", "Age")
qbbday20$Season = "2020"
colnames(qbbday21) = c("Name", "Age")
qbbday21$Season = "2021"

fanatixjoin = join(qbbday17, qbbday18, by =c(), type = "full", match = "all")
fanatixjoin1 = join(fanatixjoin, qbbday19, by =c(), type = "full", match = "all")
fanatixjoin2 = join(fanatixjoin1, qbbday20, by =c(), type = "full", match = "all")
fanatixjoin3 = join(fanatixjoin2, qbbday21, by =c(), type = "full", match = "all")

fanatixqb5 = join(fanatixqb, fanatixjoin3, by = c(), type = "full", match = "all")

###join in preseason projections for qbs

fanatixrtd = join(qbrushtd18, qbrushtd17, by =c(), type = "full", match = "all")
fanatixrtd1 = join(fanatixrtd, qbrushtd19, by =c(), type = "full", match = "all")
fanatixrtd2 = join(fanatixrtd1, qbrushtd20, by =c(), type = "full", match = "all")
fanatixrtd3 = join(fanatixrtd2, qbrushtd21, by =c(), type = "full", match = "all")

fanatixqb5 = join(fanatixqb5, fanatixrtd3, by = c(), type = "full", match = "all")

fanatixqb5 = subset(fanatixqb5, ADP <=250)
fanatixqb5 = subset(fanatixqb5, Points > 0)

tsdqb0 = join(qb17tsd, qb18tsd, by = c(), type = "full", match = "all")
tsdqb1 = join(tsdqb0, qb19tsd, by = c(), type = "full", match = "all")
tsdqb2 = join(tsdqb1, qb20tsd, by = c(), type = "full", match = "all")
tsdqb = join(tsdqb2, qb21tsd, by = c(), type = "full", match = "all")

fanatixqb5 = join(fanatixqb5, tsdqb, by = c(), type = "full", match = "all")

colnames(qbproj20) = c("PlayerID", "Name", "PassingTouchdowns", "RushingTouchdowns")
colnames(qbproj20) = c("PlayerID", "Name", "PassingTouchdowns", "RushingTouchdowns")
qbproj21 = qbproj021[-c(2,5:8,10,12:16)]
qbproj17$Season = "2017"
qbproj18$Season = "2018"
qbproj19$Season = "2019"
qbproj20$Season = "2020"
colnames(qbproj21) = c("Season", "PlayerID", "Name","PassingTouchdowns", "RushingTouchdowns")

qbprojw = join(qbproj17, qbproj18, by = c(), type = "full", match = "all")
qbprojx = join(qbprojw, qbproj19, by = c(), type = "full", match = "all")
qbprojy = join(qbprojx, qbproj20, by = c(), type = "full", match = "all")
qbprojz = join(qbprojy, qbproj21, by = c(), type = "full", match = "all")

colnames(qbprojz) = c("PlayerID", "Name", "ProjPassTD", "ProjRushTD", "Season")
fanatixqb6 = join(fanatixqb5, qbprojz, by = c(), type = "full", match = "all")

##qbformula

tsdformqb = fanatixqb6[-c(5:7, 9:11, 13:17, 19:31, 33, 35, 37)]
tsdformqb = tsdformqb[-c(7,10)]

tsdformqb$pTD = tsdformqb$ProjPassTD * .2763
tsdformqb$GPlay = tsdformqb$ProjGamesPlayed * .2523
tsdformqb$OLine = tsdformqb$OffLineRank * .0781
tsdformqb$Point = tsdformqb$ProjPoints * .1802
tsdformqb$WLOU = tsdformqb$PreW.L.O.U * .1201
tsdformqb$rTD = tsdformqb$ProjRushTD * .1291
tsdformqb$adpx = tsdformqb$ADP * -.3765
tsdformqb$TSD = rowSums(tsdformqb[, c(11:17)], na.rm = TRUE)

tsdformqb2 = tsdformqb[-c(4:17)]

fanatixqb6$TSDx = fanatixqb6$TSD
fanatixqb7 = fanatixqb6[-c(37)]
fanatixqb5 = join(fanatixqb7, tsdformqb2, by = c(), type = "full", match = "all")

fanatixqb5 = subset(fanatixqb5, ADP <=250)
fanatixqb5 = subset(fanatixqb5, Points > 0)

##RB

###join together rb projections, provide season, and change teams to numeric

rbproj17$Season = "2017"
rbproj18$Season = "2018"
rbproj19$Season = "2019"
rbproj20$Season = "2020"
rbproj21$Season = "2021"
rbprojecting = join(rbproj17,rbproj18, by = c(), type = "full", match = "all")
rbprojecting1 = join(rbprojecting,rbproj19, by = c(), type = "full", match = "all")
rbprojecting2 = join(rbprojecting1,rbproj20, by = c(), type = "full", match = "all")
rbprojecting3 = join(rbprojecting2,rbproj21, by = c(), type = "full", match = "all")

unique(rbprojecting3$Team)
rbprojecting3["Team"][rbprojecting3["Team"] == "NA"] = "0"
rbprojecting3["Team"][rbprojecting3["Team"] == "ARI"] = "1"
rbprojecting3["Team"][rbprojecting3["Team"] == "ATL"] = "2"
rbprojecting3["Team"][rbprojecting3["Team"] == "BAL"] = "3"
rbprojecting3["Team"][rbprojecting3["Team"] == "BUF"] = "4"
rbprojecting3["Team"][rbprojecting3["Team"] == "CAR"] = "5"
rbprojecting3["Team"][rbprojecting3["Team"] == "CHI"] = "6"
rbprojecting3["Team"][rbprojecting3["Team"] == "CIN"] = "7"
rbprojecting3["Team"][rbprojecting3["Team"] == "CLE"] = "8"
rbprojecting3["Team"][rbprojecting3["Team"] == "DAL"] = "9"
rbprojecting3["Team"][rbprojecting3["Team"] == "DEN"] = "10"
rbprojecting3["Team"][rbprojecting3["Team"] == "DET"] = "11"
rbprojecting3["Team"][rbprojecting3["Team"] == "GB"] = "12"
rbprojecting3["Team"][rbprojecting3["Team"] == "HOU"] = "13"
rbprojecting3["Team"][rbprojecting3["Team"] == "IND"] = "14"
rbprojecting3["Team"][rbprojecting3["Team"] == "JAX"] = "15"
rbprojecting3["Team"][rbprojecting3["Team"] == "KC"] = "16"
rbprojecting3["Team"][rbprojecting3["Team"] == "LAC"] = "17"
rbprojecting3["Team"][rbprojecting3["Team"] == "LAR"] = "18"
rbprojecting3["Team"][rbprojecting3["Team"] == "LV"] = "19"
rbprojecting3["Team"][rbprojecting3["Team"] == "MIA"] = "20"
rbprojecting3["Team"][rbprojecting3["Team"] == "MIN"] = "21"
rbprojecting3["Team"][rbprojecting3["Team"] == "NE"] = "22"
rbprojecting3["Team"][rbprojecting3["Team"] == "NO"] = "23"
rbprojecting3["Team"][rbprojecting3["Team"] == "NYG"] = "24"
rbprojecting3["Team"][rbprojecting3["Team"] == "NYJ"] = "25"
rbprojecting3["Team"][rbprojecting3["Team"] == "PHI"] = "26"
rbprojecting3["Team"][rbprojecting3["Team"] == "PIT"] = "27"
rbprojecting3["Team"][rbprojecting3["Team"] == "SEA"] = "28"
rbprojecting3["Team"][rbprojecting3["Team"] == "SF"] = "29"
rbprojecting3["Team"][rbprojecting3["Team"] == "TB"] = "30"
rbprojecting3["Team"][rbprojecting3["Team"] == "TEN"] = "31"
rbprojecting3["Team"][rbprojecting3["Team"] == "WAS"] = "32"
unique(rbprojecting3$Team)

rbprojecting4 = join(rbprojecting3, oddsm3, by = c(), type = "full", match = "all")

###rb age when season began
rbbday = rbage
bdayrb = c(rbbday$Birth.Date)
rbbday$BDay = as.Date(bdayrb, "%Y-%m-%d")
rbbday$start17 = "2017-09-07" 
rbbday$age17 = as.numeric(difftime(rbbday$start17,rbbday$BDay, units = "weeks"))/52.25

rbbday$start18 = "2018-09-06" 
rbbday$age18 = as.numeric(difftime(rbbday$start18,rbbday$BDay, units = "weeks"))/52.25

rbbday$start19 = "2019-09-05" 
rbbday$age19 = as.numeric(difftime(rbbday$start19,rbbday$BDay, units = "weeks"))/52.25

rbbday$start20 = "2020-09-10" 
rbbday$age20 = as.numeric(difftime(rbbday$start20,rbbday$BDay, units = "weeks"))/52.25

rbbday$start21 = "2021-09-09" 
rbbday$age21 = as.numeric(difftime(rbbday$start21,rbbday$BDay, units = "weeks"))/52.25

rbbday = rbbday[-c(2,3)]
rbbday17 = rbbday[c(1,4)]
rbbday18 = rbbday[c(1,6)]
rbbday19 = rbbday[c(1,8)]
rbbday20 = rbbday[c(1,10)]
rbbday21 = rbbday[c(1,12)]

colnames(rbbday17) = c("Name", "Age")
rbbday17$Season = "2017"
colnames(rbbday18) = c("Name", "Age")
rbbday18$Season = "2018"
colnames(rbbday19) = c("Name", "Age")
rbbday19$Season = "2019"
colnames(rbbday20) = c("Name", "Age")
rbbday20$Season = "2020"
colnames(rbbday21) = c("Name", "Age")
rbbday21$Season = "2021"

rbbdayjoin = join(rbbday17, rbbday18, by =c(), type = "full", match = "all")
rbbdayjoin1 = join(rbbdayjoin, rbbday19, by =c(), type = "full", match = "all")
rbbdayjoin2 = join(rbbdayjoin1, rbbday20, by =c(), type = "full", match = "all")
rbbdayjoin3 = join(rbbdayjoin2, rbbday21, by =c(), type = "full", match = "all")

rbprojecting5 = join(rbprojecting4, rbbdayjoin3, by = c(), type = "full", match = "all")

rbprojecting6 = rbprojecting5[-c(1,9,17:20,22)]

rbadp = fanatixrb[c(1:4,9)]
rbprojecting8 = join(rbprojecting6,rbadp, by = c(), type = "full", match = "all")

rbprojecting7 = subset(rbprojecting8, ADP <=250)
rbprojecting7 = subset(rbprojecting8, FantasyPoints > 0)



####rb correlations

rbprojecting7$Played <- as.numeric(as.character(rbprojecting7$Played))
rbprojecting7$Season <- as.numeric(as.character(rbprojecting7$Season))
rbprojecting7$OffLineRank <- as.numeric(as.character(rbprojecting7$OffLineRank))

rbcor1 <- cor.test(rbprojecting7$Points, rbprojecting7$Played, method = "pearson")
rbcor1

#####correlation = .0298

rbcor2 <- cor.test(rbprojecting7$Points, rbprojecting7$RushingAttempts, method = "pearson")
rbcor2

#####correlation = .6049

rbcor3 <- cor.test(rbprojecting7$Points, rbprojecting7$RushingYards, method = "pearson")
rbcor3

#####correlation = .6098

rbcor4 <- cor.test(rbprojecting7$Points, rbprojecting7$RushingTouchdowns, method = "pearson")
rbcor4

#####correlation =.5871

rbcor5 <- cor.test(rbprojecting7$Points, rbprojecting7$ReceivingTargets, method = "pearson")
rbcor5

#####correlation = .4737

rbcor6 <- cor.test(rbprojecting7$Points, rbprojecting7$Receptions, method = "pearson")
rbcor6

#####correlation = .4675

rbcor7 <- cor.test(rbprojecting7$Points, rbprojecting7$ReceivingYards, method = "pearson")
rbcor7

#####correlation = .4743

rbcor8 <- cor.test(rbprojecting7$Points, rbprojecting7$ReceivingTouchdowns, method = "pearson")
rbcor8

#####correlation = .4110

rbcor9 <- cor.test(rbprojecting7$Points, rbprojecting7$FantasyPoints, method = "pearson")
rbcor9

#####correlation = .6369

rbcor10 <- cor.test(rbprojecting7$Points, rbprojecting7$PreW.L.O.U, method = "pearson")
rbcor10

#####correlation = .0137

rbcor11 <- cor.test(rbprojecting7$Points, rbprojecting7$OffLineRank, method = "pearson")
rbcor11

#####correlation = -.1156%

rbcor12 <- cor.test(rbprojecting7$Points, rbprojecting7$Age, method = "pearson")
rbcor12

#####correlation = -.1727

rbcor13 <- cor.test(rbprojecting7$Points, rbprojecting7$ADP, method = "pearson")
rbcor13

#####correlation = -.5932

####tsdrbformula

tsdformrb = rbprojecting7[-c(5:6, 8:10, 12, 15)]

tsdformrb$ruyds = tsdformrb$RushingYards * .7264
tsdformrb$rcyds = tsdformrb$ReceivingYards * .565
tsdformrb$fpts = tsdformrb$FantasyPoints * .7587
tsdformrb$olr = tsdformrb$OffLineRank* -.1377
tsdformrb$bday = tsdformrb$Age * -.2057
tsdformrb$adpos = tsdformrb$ADP * -.7066
tsdformrb$TSD = rowSums(tsdformrb[, c(13:18)], na.rm = TRUE)

rbcor14 <- cor.test(tsdformrb$Points, tsdformrb$TSD, method = "pearson")
rbcor14

#####correlation = .6419

####clean up df and join with fanatixrb and then clean up fanatix rb with players ADP<=250 and points scored, then change team names to numeric

tsdformrb2 = tsdformrb[-c(4:9,11:22)]

fanatixrb2 = join(fanatixrb, tsdformrb2, by = c(), type = "full", match = "all")

fanatixrb2 = subset(fanatixrb2, ADP <=250)
fanatixrb2 = subset(fanatixrb2, Points > 0)


rbproj22["Team"][rbproj22["Team"] == "NA"] = "0"
rbproj22["Team"][rbproj22["Team"] == "ARI"] = "1"
rbproj22["Team"][rbproj22["Team"] == "ATL"] = "2"
rbproj22["Team"][rbproj22["Team"] == "BAL"] = "3"
rbproj22["Team"][rbproj22["Team"] == "BUF"] = "4"
rbproj22["Team"][rbproj22["Team"] == "CAR"] = "5"
rbproj22["Team"][rbproj22["Team"] == "CHI"] = "6"
rbproj22["Team"][rbproj22["Team"] == "CIN"] = "7"
rbproj22["Team"][rbproj22["Team"] == "CLE"] = "8"
rbproj22["Team"][rbproj22["Team"] == "DAL"] = "9"
rbproj22["Team"][rbproj22["Team"] == "DEN"] = "10"
rbproj22["Team"][rbproj22["Team"] == "DET"] = "11"
rbproj22["Team"][rbproj22["Team"] == "GB"] = "12"
rbproj22["Team"][rbproj22["Team"] == "HOU"] = "13"
rbproj22["Team"][rbproj22["Team"] == "IND"] = "14"
rbproj22["Team"][rbproj22["Team"] == "JAX"] = "15"
rbproj22["Team"][rbproj22["Team"] == "KC"] = "16"
rbproj22["Team"][rbproj22["Team"] == "LAC"] = "17"
rbproj22["Team"][rbproj22["Team"] == "LAR"] = "18"
rbproj22["Team"][rbproj22["Team"] == "LV"] = "19"
rbproj22["Team"][rbproj22["Team"] == "MIA"] = "20"
rbproj22["Team"][rbproj22["Team"] == "MIN"] = "21"
rbproj22["Team"][rbproj22["Team"] == "NE"] = "22"
rbproj22["Team"][rbproj22["Team"] == "NO"] = "23"
rbproj22["Team"][rbproj22["Team"] == "NYG"] = "24"
rbproj22["Team"][rbproj22["Team"] == "NYJ"] = "25"
rbproj22["Team"][rbproj22["Team"] == "PHI"] = "26"
rbproj22["Team"][rbproj22["Team"] == "PIT"] = "27"
rbproj22["Team"][rbproj22["Team"] == "SEA"] = "28"
rbproj22["Team"][rbproj22["Team"] == "SF"] = "29"
rbproj22["Team"][rbproj22["Team"] == "TB"] = "30"
rbproj22["Team"][rbproj22["Team"] == "TEN"] = "31"
rbproj22["Team"][rbproj22["Team"] == "WAS"] = "32"
unique(rbproj22$Team)

####add in rb team odds to rb proj22 and add ages when 22 season starts

rbproj22 = join(rbproj22, `22oddsrb`, by = c(), type = "full", match = "all")
rbproj22 = rbproj22[-c(12:14)]
rbproj22 = na.omit(rbproj22)

rbbday$start22 = "2022-09-09" 
rbbday$age22 = as.numeric(difftime(rbbday$start22,rbbday$BDay, units = "weeks"))/52.25
rbbday22 = rbbday[c(1,14)]
colnames(rbbday22) = c("Name", "Age")
rbproj22 = join(rbproj22, rbbday22, by = c(), type = "full", match = "all")

rbadp22 = rbadp22[-c(1,4:13)]
rbproj22 = join(rbproj22, rbadp22, by = c(), type = "full", match = "all")

rbproj22$ruyds = rbproj22$RushingYards * .7264
rbproj22$rcyds = rbproj22$ReceivingYards * .565
rbproj22$fpts = rbproj22$FantasyPoints * .7587
rbproj22$olr = rbproj22$OffLineRank* .1377
rbproj22$bday = rbproj22$Age * -.2057
rbproj22$adpos = rbproj22$AverageDraftPosition * -.7066
rbproj22$TSD = rowSums(rbproj22[, c(14:19)], na.rm = TRUE)

fanatixrbhtsd = tsdformrb[c(1,2,8,19)]
rbprojecting7 = join(rbprojecting7, fanatixrbhtsd, by = c(), type = "full", match = "all")

rbprojecting7 = subset(rbprojecting7, ADP <=250)
rbprojecting7 = subset(rbprojecting7, Points > 0)

####create new subset of RB Draft Round dependent on league size

fanatixrb8team = subset(rbprojecting7, ADP <=120)
fanatixrb10team = subset(rbprojecting7, ADP <=150)
fanatixrb12team = subset(rbprojecting7, ADP <=180)
fanatixrb8team = fanatixrb8team[c(1:4,7,11,13,14,16:20)]
fanatixrb10team = fanatixrb10team[c(1:4,7,11,13,14,16:20)]
fanatixrb12team = fanatixrb12team[c(1:4,7,11,13,14,16:20)]

#####8 team

RBADP8 = summary(fanatixrb8team)
RBADP8

fanatixrb81 = subset(fanatixrb8team, ADP <=8)
fanatixrb82 = subset(fanatixrb8team, ADP >=9 & ADP <=16)
fanatixrb83 = subset(fanatixrb8team, ADP >=17 & ADP <=24)
fanatixrb84 = subset(fanatixrb8team, ADP >=25 & ADP <=32)
fanatixrb85 = subset(fanatixrb8team, ADP >=33 & ADP <=40)
fanatixrb86 = subset(fanatixrb8team, ADP >=41 & ADP <=48)
fanatixrb87 = subset(fanatixrb8team, ADP >=49 & ADP <=56)
fanatixrb88 = subset(fanatixrb8team, ADP >=57 & ADP <=64)
fanatixrb89 = subset(fanatixrb8team, ADP >=65 & ADP <=72)
fanatixrb810 = subset(fanatixrb8team, ADP >=73 & ADP <=80)
fanatixrb811 = subset(fanatixrb8team, ADP >=81 & ADP <=88)
fanatixrb812 = subset(fanatixrb8team, ADP >=89 & ADP <=96)
fanatixrb813 = subset(fanatixrb8team, ADP >=97 & ADP <=104)
fanatixrb814 = subset(fanatixrb8team, ADP >=105 & ADP <=112)
fanatixrb815 = subset(fanatixrb8team, ADP >=113 & ADP <=120)

RBADP81 = summary(fanatixrb81)
RBADP81
RBADP82 = summary(fanatixrb82)
RBADP82
RBADP83 = summary(fanatixrb83)
RBADP83
RBADP84 = summary(fanatixrb84)
RBADP84
RBADP85 = summary(fanatixrb85)
RBADP85
RBADP86 = summary(fanatixrb86)
RBADP86
RBADP87 = summary(fanatixrb87)
RBADP87
RBADP88 = summary(fanatixrb88)
RBADP88
RBADP89 = summary(fanatixrb89)
RBADP89
RBADP810 = summary(fanatixrb810)
RBADP810
RBADP811 = summary(fanatixrb811)
RBADP811
RBADP812 = summary(fanatixrb812)
RBADP812
RBADP813 = summary(fanatixrb813)
RBADP813
RBADP814 = summary(fanatixrb814)
RBADP814
RBADP815 = summary(fanatixrb815)
RBADP815

#####10 Team

RBADP10 = summary(fanatixrb10team)
RBADP10

fanatixrb101 = subset(fanatixrb10team, ADP <=10)
fanatixrb102 = subset(fanatixrb10team, ADP >=11 & ADP <=20)
fanatixrb103 = subset(fanatixrb10team, ADP >=21 & ADP <=30)
fanatixrb104 = subset(fanatixrb10team, ADP >=31 & ADP <=40)
fanatixrb105 = subset(fanatixrb10team, ADP >=41 & ADP <=50)
fanatixrb106 = subset(fanatixrb10team, ADP >=51 & ADP <=60)
fanatixrb107 = subset(fanatixrb10team, ADP >=61 & ADP <=70)
fanatixrb108 = subset(fanatixrb10team, ADP >=71 & ADP <=80)
fanatixrb109 = subset(fanatixrb10team, ADP >=81 & ADP <=90)
fanatixrb1010 = subset(fanatixrb10team, ADP >=91 & ADP <=100)
fanatixrb1011 = subset(fanatixrb10team, ADP >=101 & ADP <=110)
fanatixrb1012 = subset(fanatixrb10team, ADP >=111 & ADP <=120)
fanatixrb1013 = subset(fanatixrb10team, ADP >=121 & ADP <=130)
fanatixrb1014 = subset(fanatixrb10team, ADP >=131 & ADP <=140)
fanatixrb1015 = subset(fanatixrb10team, ADP >=141 & ADP <=150)

RBADP101 = summary(fanatixrb101)
RBADP101
RBADP102 = summary(fanatixrb102)
RBADP102
RBADP103 = summary(fanatixrb103)
RBADP103
RBADP104 = summary(fanatixrb104)
RBADP104
RBADP105 = summary(fanatixrb105)
RBADP105
RBADP106 = summary(fanatixrb106)
RBADP106
RBADP107 = summary(fanatixrb107)
RBADP107
RBADP108 = summary(fanatixrb108)
RBADP108
RBADP109 = summary(fanatixrb109)
RBADP109
RBADP1010 = summary(fanatixrb1010)
RBADP1010
RBADP1011 = summary(fanatixrb1011)
RBADP1011
RBADP1012 = summary(fanatixrb1012)
RBADP1012
RBADP1013 = summary(fanatixrb1013)
RBADP1013
RBADP1014 = summary(fanatixrb1014)
RBADP1014
RBADP1015 = summary(fanatixrb1015)
RBADP1015

#####12 Team

RBADP12 = summary(fanatixrb12team)
RBADP12

fanatixrb121 = subset(fanatixrb12team, ADP <=12)
fanatixrb122 = subset(fanatixrb12team, ADP >=13 & ADP <=24)
fanatixrb123 = subset(fanatixrb12team, ADP >=25 & ADP <=36)
fanatixrb124 = subset(fanatixrb12team, ADP >=37 & ADP <=48)
fanatixrb125 = subset(fanatixrb12team, ADP >=49 & ADP <=60)
fanatixrb126 = subset(fanatixrb12team, ADP >=61 & ADP <=72)
fanatixrb127 = subset(fanatixrb12team, ADP >=73 & ADP <=84)
fanatixrb128 = subset(fanatixrb12team, ADP >=85 & ADP <=96)
fanatixrb129 = subset(fanatixrb12team, ADP >=97 & ADP <=108)
fanatixrb1210 = subset(fanatixrb12team, ADP >=109 & ADP <=120)
fanatixrb1211 = subset(fanatixrb12team, ADP >=121 & ADP <=132)
fanatixrb1212 = subset(fanatixrb12team, ADP >=133 & ADP <=144)
fanatixrb1213 = subset(fanatixrb12team, ADP >=145 & ADP <=156)
fanatixrb1214 = subset(fanatixrb12team, ADP >=157 & ADP <=168)
fanatixrb1215 = subset(fanatixrb12team, ADP >=169 & ADP <=180)

RBADP121 = summary(fanatixrb121)
RBADP121
RBADP122 = summary(fanatixrb122)
RBADP122
RBADP123 = summary(fanatixrb123)
RBADP123
RBADP124 = summary(fanatixrb124)
RBADP124
RBADP125 = summary(fanatixrb125)
RBADP125
RBADP126 = summary(fanatixrb126)
RBADP126
RBADP127 = summary(fanatixrb127)
RBADP127
RBADP128 = summary(fanatixrb128)
RBADP128
RBADP129 = summary(fanatixrb129)
RBADP129
RBADP1210 = summary(fanatixrb1210)
RBADP1210
RBADP1211 = summary(fanatixrb1211)
RBADP1211
RBADP1212 = summary(fanatixrb1212)
RBADP1212
RBADP1213 = summary(fanatixrb1213)
RBADP1213
RBADP1214 = summary(fanatixrb1214)
RBADP1214
RBADP1215 = summary(fanatixrb1215)
RBADP1215

##WR

###join together wr projections, provide season, and change teams to numeric

wrproj17$Season = "2017"
wrproj18$Season = "2018"
wrproj19$Season = "2019"
wrproj20$Season = "2020"
wrproj21$Season = "2021"
wrprojecting = join(wrproj17,wrproj18, by = c(), type = "full", match = "all")
wrprojecting1 = join(wrprojecting,wrproj19, by = c(), type = "full", match = "all")
wrprojecting2 = join(wrprojecting1,wrproj20, by = c(), type = "full", match = "all")
wrprojecting3 = join(wrprojecting2,wrproj21, by = c(), type = "full", match = "all")

unique(wrprojecting3$Team)
wrprojecting3["Team"][wrprojecting3["Team"] == "NA"] = "0"
wrprojecting3["Team"][wrprojecting3["Team"] == "ARI"] = "1"
wrprojecting3["Team"][wrprojecting3["Team"] == "ATL"] = "2"
wrprojecting3["Team"][wrprojecting3["Team"] == "BAL"] = "3"
wrprojecting3["Team"][wrprojecting3["Team"] == "BUF"] = "4"
wrprojecting3["Team"][wrprojecting3["Team"] == "CAR"] = "5"
wrprojecting3["Team"][wrprojecting3["Team"] == "CHI"] = "6"
wrprojecting3["Team"][wrprojecting3["Team"] == "CIN"] = "7"
wrprojecting3["Team"][wrprojecting3["Team"] == "CLE"] = "8"
wrprojecting3["Team"][wrprojecting3["Team"] == "DAL"] = "9"
wrprojecting3["Team"][wrprojecting3["Team"] == "DEN"] = "10"
wrprojecting3["Team"][wrprojecting3["Team"] == "DET"] = "11"
wrprojecting3["Team"][wrprojecting3["Team"] == "GB"] = "12"
wrprojecting3["Team"][wrprojecting3["Team"] == "HOU"] = "13"
wrprojecting3["Team"][wrprojecting3["Team"] == "IND"] = "14"
wrprojecting3["Team"][wrprojecting3["Team"] == "JAX"] = "15"
wrprojecting3["Team"][wrprojecting3["Team"] == "KC"] = "16"
wrprojecting3["Team"][wrprojecting3["Team"] == "LAC"] = "17"
wrprojecting3["Team"][wrprojecting3["Team"] == "LAR"] = "18"
wrprojecting3["Team"][wrprojecting3["Team"] == "LV"] = "19"
wrprojecting3["Team"][wrprojecting3["Team"] == "MIA"] = "20"
wrprojecting3["Team"][wrprojecting3["Team"] == "MIN"] = "21"
wrprojecting3["Team"][wrprojecting3["Team"] == "NE"] = "22"
wrprojecting3["Team"][wrprojecting3["Team"] == "NO"] = "23"
wrprojecting3["Team"][wrprojecting3["Team"] == "NYG"] = "24"
wrprojecting3["Team"][wrprojecting3["Team"] == "NYJ"] = "25"
wrprojecting3["Team"][wrprojecting3["Team"] == "PHI"] = "26"
wrprojecting3["Team"][wrprojecting3["Team"] == "PIT"] = "27"
wrprojecting3["Team"][wrprojecting3["Team"] == "SEA"] = "28"
wrprojecting3["Team"][wrprojecting3["Team"] == "SF"] = "29"
wrprojecting3["Team"][wrprojecting3["Team"] == "TB"] = "30"
wrprojecting3["Team"][wrprojecting3["Team"] == "TEN"] = "31"
wrprojecting3["Team"][wrprojecting3["Team"] == "WAS"] = "32"
unique(wrprojecting3$Team)

wrprojecting4 = join(wrprojecting3, oddsm3, by = c(), type = "full", match = "all")

###change wr age dependent on each season start date

wrbday = wrage
bdaywr = c(wrbday$Birth.Date)
wrbday$BDay = as.Date(bdaywr, "%m/%d/%Y")
wrbday$start17 = "2017-09-07" 
wrbday$age17 = as.numeric(difftime(wrbday$start17,wrbday$BDay, units = "weeks"))/52.25

wrbday$start18 = "2018-09-06" 
wrbday$age18 = as.numeric(difftime(wrbday$start18,wrbday$BDay, units = "weeks"))/52.25

wrbday$start19 = "2019-09-05" 
wrbday$age19 = as.numeric(difftime(wrbday$start19,wrbday$BDay, units = "weeks"))/52.25

wrbday$start20 = "2020-09-10" 
wrbday$age20 = as.numeric(difftime(wrbday$start20,wrbday$BDay, units = "weeks"))/52.25

wrbday$start21 = "2021-09-09" 
wrbday$age21 = as.numeric(difftime(wrbday$start21,wrbday$BDay, units = "weeks"))/52.25

wrbday = wrbday[-c(2,3)]
wrbday17 = wrbday[c(1,4)]
wrbday18 = wrbday[c(1,6)]
wrbday19 = wrbday[c(1,8)]
wrbday20 = wrbday[c(1,10)]
wrbday21 = wrbday[c(1,12)]

colnames(wrbday17) = c("Name", "Age")
wrbday17$Season = "2017"
colnames(wrbday18) = c("Name", "Age")
wrbday18$Season = "2018"
colnames(wrbday19) = c("Name", "Age")
wrbday19$Season = "2019"
colnames(wrbday20) = c("Name", "Age")
wrbday20$Season = "2020"
colnames(wrbday21) = c("Name", "Age")
wrbday21$Season = "2021"

wrbdayjoin = join(wrbday17, wrbday18, by =c(), type = "full", match = "all")
wrbdayjoin1 = join(wrbdayjoin, wrbday19, by =c(), type = "full", match = "all")
wrbdayjoin2 = join(wrbdayjoin1, wrbday20, by =c(), type = "full", match = "all")
wrbdayjoin3 = join(wrbdayjoin2, wrbday21, by =c(), type = "full", match = "all")

wrprojecting5 = join(wrprojecting4, wrbdayjoin3, by = c(), type = "full", match = "all")

####clean up df

wrprojecting6 = wrprojecting5[-c(1,17,19:21,24,26)]

wradp = fanatixwr[c(1:4,9)]
wrprojecting8 = join(wrprojecting6,wradp, by = c(), type = "full", match = "all")

wrprojecting7 = subset(wrprojecting8, ADP <=250)
wrprojecting7 = subset(wrprojecting8, FantasyPoints > 0)

###correlations

####wr

wrprojecting7$Played <- as.numeric(as.character(wrprojecting7$Played))
wrprojecting7$Season <- as.numeric(as.character(wrprojecting7$Season))
wrprojecting7$OffLineRank <- as.numeric(as.character(wrprojecting7$OffLineRank))

wrcor1 <- cor.test(wrprojecting7$Points, wrprojecting7$Played, method = "pearson")
wrcor1

####correlation = .0485

wrcor2 <- cor.test(wrprojecting7$Points, wrprojecting7$ReceivingTargets, method = "pearson")
wrcor2

####correlation = .5716

wrcor3 <- cor.test(wrprojecting7$Points, wrprojecting7$Receptions, method = "pearson")
wrcor3

####correlation = .6039

wrcor4 <- cor.test(wrprojecting7$Points, wrprojecting7$ReceivingYards, method = "pearson")
wrcor4

####correlation =.6184

wrcor5 <- cor.test(wrprojecting7$Points, wrprojecting7$ReceivingTouchdowns, method = "pearson")
wrcor5

####correlation = .5645

wrcor6 <- cor.test(wrprojecting7$Points, wrprojecting7$ReceivingYardsPerTarget, method = "pearson")
wrcor6

####correlation = .1901

wrcor7 <- cor.test(wrprojecting7$Points, wrprojecting7$ReceivingYardsPerReception, method = "pearson")
wrcor7

####correlation = .1148

wrcor8 <- cor.test(wrprojecting7$Points, wrprojecting7$RushingYards, method = "pearson")
wrcor8

####correlation = .0509

wrcor9 <- cor.test(wrprojecting7$Points, wrprojecting7$FantasyPoints, method = "pearson")
wrcor9

####correlation = .6169

wrcor10 <- cor.test(wrprojecting7$Points, wrprojecting7$PreW.L.O.U, method = "pearson")
wrcor10

####correlation = .1493

wrcor11 <- cor.test(wrprojecting7$Points, wrprojecting7$OffLineRank, method = "pearson")
wrcor11

####correlation = -.0371%

wrcor12 <- cor.test(wrprojecting7$Points, wrprojecting7$Age, method = "pearson")
wrcor12

####correlation = -.0247

wrcor13 <- cor.test(wrprojecting7$Points, wrprojecting7$ADP, method = "pearson")
wrcor13

####correlation = -.6237

###tsdwrformula

tsdformwr = wrprojecting7[-c(5:6, 8,11:14,16, 20:21)]

tsdformwr$rec = tsdformwr$ReceivingYards * .7857
tsdformwr$rcyds = tsdformwr$RushingYards * .0647
tsdformwr$rctd = tsdformwr$FantasyPoints * .7838
tsdformwr$fpt = tsdformwr$PreW.L.O.U * .1897
tsdformwr$wlou = tsdformwr$Age * -.0314
tsdformwr$adpos = tsdformwr$ADP * -.7924
tsdformwr$TSD = rowSums(tsdformwr[, c(14:18)], na.rm = TRUE)

wrcor14 <- cor.test(tsdformwr$Points, tsdformwr$TSD, method = "pearson")
wrcor14

####correlation = .6260

###clean up df

tsdformwr2 = tsdformwr[-c(4:9,11:18)]

fanatixwr2 = join(fanatixwr, tsdformwr2, by = c(), type = "full", match = "all")

fanatixwr2 = subset(fanatixwr2, ADP <=250)
fanatixwr2 = subset(fanatixwr2, Points > 0)

###change teams to numeric

wrproj22["Team"][wrproj22["Team"] == "NA"] = "0"
wrproj22["Team"][wrproj22["Team"] == "ARI"] = "1"
wrproj22["Team"][wrproj22["Team"] == "ATL"] = "2"
wrproj22["Team"][wrproj22["Team"] == "BAL"] = "3"
wrproj22["Team"][wrproj22["Team"] == "BUF"] = "4"
wrproj22["Team"][wrproj22["Team"] == "CAR"] = "5"
wrproj22["Team"][wrproj22["Team"] == "CHI"] = "6"
wrproj22["Team"][wrproj22["Team"] == "CIN"] = "7"
wrproj22["Team"][wrproj22["Team"] == "CLE"] = "8"
wrproj22["Team"][wrproj22["Team"] == "DAL"] = "9"
wrproj22["Team"][wrproj22["Team"] == "DEN"] = "10"
wrproj22["Team"][wrproj22["Team"] == "DET"] = "11"
wrproj22["Team"][wrproj22["Team"] == "GB"] = "12"
wrproj22["Team"][wrproj22["Team"] == "HOU"] = "13"
wrproj22["Team"][wrproj22["Team"] == "IND"] = "14"
wrproj22["Team"][wrproj22["Team"] == "JAX"] = "15"
wrproj22["Team"][wrproj22["Team"] == "KC"] = "16"
wrproj22["Team"][wrproj22["Team"] == "LAC"] = "17"
wrproj22["Team"][wrproj22["Team"] == "LAR"] = "18"
wrproj22["Team"][wrproj22["Team"] == "LV"] = "19"
wrproj22["Team"][wrproj22["Team"] == "MIA"] = "20"
wrproj22["Team"][wrproj22["Team"] == "MIN"] = "21"
wrproj22["Team"][wrproj22["Team"] == "NE"] = "22"
wrproj22["Team"][wrproj22["Team"] == "NO"] = "23"
wrproj22["Team"][wrproj22["Team"] == "NYG"] = "24"
wrproj22["Team"][wrproj22["Team"] == "NYJ"] = "25"
wrproj22["Team"][wrproj22["Team"] == "PHI"] = "26"
wrproj22["Team"][wrproj22["Team"] == "PIT"] = "27"
wrproj22["Team"][wrproj22["Team"] == "SEA"] = "28"
wrproj22["Team"][wrproj22["Team"] == "SF"] = "29"
wrproj22["Team"][wrproj22["Team"] == "TB"] = "30"
wrproj22["Team"][wrproj22["Team"] == "TEN"] = "31"
wrproj22["Team"][wrproj22["Team"] == "WAS"] = "32"
unique(wrproj22$Team)

###join in team season odds and add age for 22 season

wrproj22 = join(wrproj22, `22Oddswr`, by = c(), type = "full", match = "all")
wrproj22 = na.omit(wrproj22)

wrbday$start22 = "2022-09-09" 
wrbday$age22 = as.numeric(difftime(wrbday$start22,wrbday$BDay, units = "weeks"))/52.25
wrbday22 = wrbday[c(1,14)]
colnames(wrbday22) = c("Name", "Age")
wrproj22 = join(wrproj22, wrbday22, by = c(), type = "full", match = "all")

wradp22 = wradp22[-c(1,4:13)]
wrproj22 = join(wrproj22, wradp22, by = c(), type = "full", match = "all")

wrproj22$rec = wrproj22$ReceivingYards * .7857
wrproj22$rcyds = wrproj22$RushingYards * .0647
wrproj22$rctd = wrproj22$FantasyPoints * .7838
wrproj22$fpt = wrproj22$PreW.L.O.U * .1897
wrproj22$wlou = wrproj22$Age * -.0314
wrproj22$adpos = wrproj22$AverageDraftPosition * -.7924
wrproj22$TSD = rowSums(wrproj22[, c(27:31,33)], na.rm = TRUE)

fanatixwrtsd = tsdformwr[c(1,2,10,19)]
wrprojecting7 = join(wrprojecting7, fanatixwrtsd, by = c(), type = "full", match = "all")

wrprojecting7 = subset(wrprojecting7, ADP <=250)
wrprojecting7 = subset(wrprojecting7, Points > 0)

###WR Draft Round per fantasy league size

fanatixwr8team = subset(wrprojecting7, ADP <=120)
fanatixwr10team = subset(wrprojecting7, ADP <=150)
fanatixwr12team = subset(wrprojecting7, ADP <=180)
fanatixwr8team = fanatixwr8team[c(1:4,17,22:24)]
fanatixwr10team = fanatixwr10team[c(1:4,17,22:24)]
fanatixwr12team = fanatixwr12team[c(1:4,17,22:24)]

####8 team

WRADP8 = summary(fanatixwr8team)
WRADP8

fanatixwr81 = subset(fanatixwr8team, ADP <=8)
fanatixwr82 = subset(fanatixwr8team, ADP >=9 & ADP <=16)
fanatixwr83 = subset(fanatixwr8team, ADP >=17 & ADP <=24)
fanatixwr84 = subset(fanatixwr8team, ADP >=25 & ADP <=32)
fanatixwr85 = subset(fanatixwr8team, ADP >=33 & ADP <=40)
fanatixwr86 = subset(fanatixwr8team, ADP >=41 & ADP <=48)
fanatixwr87 = subset(fanatixwr8team, ADP >=49 & ADP <=56)
fanatixwr88 = subset(fanatixwr8team, ADP >=57 & ADP <=64)
fanatixwr89 = subset(fanatixwr8team, ADP >=65 & ADP <=72)
fanatixwr810 = subset(fanatixwr8team, ADP >=73 & ADP <=80)
fanatixwr811 = subset(fanatixwr8team, ADP >=81 & ADP <=88)
fanatixwr812 = subset(fanatixwr8team, ADP >=89 & ADP <=96)
fanatixwr813 = subset(fanatixwr8team, ADP >=97 & ADP <=104)
fanatixwr814 = subset(fanatixwr8team, ADP >=105 & ADP <=112)
fanatixwr815 = subset(fanatixwr8team, ADP >=113 & ADP <=120)

WRADP81 = summary(fanatixwr81)
WRADP81
WRADP82 = summary(fanatixwr82)
WRADP82
WRADP83 = summary(fanatixwr83)
WRADP83
wRADP84 = summary(fanatixwr84)
WRADP84
WRADP85 = summary(fanatixwr85)
WRADP85
WRADP86 = summary(fanatixwr86)
WRADP86
WRADP87 = summary(fanatixwr87)
WRADP87
WRADP88 = summary(fanatixwr88)
WRADP88
WRADP89 = summary(fanatixwr89)
WRADP89
WRADP810 = summary(fanatixwr810)
WRADP810
WRADP811 = summary(fanatixwr811)
WRADP811
WRADP812 = summary(fanatixwr812)
wRADP812
WRADP813 = summary(fanatixwr813)
WRADP813
WRADP814 = summary(fanatixwr814)
WRADP814
WRADP815 = summary(fanatixwr815)
WRADP815

####10 Team
WRADP10 = summary(fanatixwr10team)
WRADP10

fanatixwr101 = subset(fanatixwr10team, ADP <=10)
fanatixwr102 = subset(fanatixwr10team, ADP >=11 & ADP <=20)
fanatixwr103 = subset(fanatixwr10team, ADP >=21 & ADP <=30)
fanatixwr104 = subset(fanatixwr10team, ADP >=31 & ADP <=40)
fanatixwr105 = subset(fanatixwr10team, ADP >=41 & ADP <=50)
fanatixwr106 = subset(fanatixwr10team, ADP >=51 & ADP <=60)
fanatixwr107 = subset(fanatixwr10team, ADP >=61 & ADP <=70)
fanatixwr108 = subset(fanatixwr10team, ADP >=71 & ADP <=80)
fanatixwr109 = subset(fanatixwr10team, ADP >=81 & ADP <=90)
fanatixwr1010 = subset(fanatixwr10team, ADP >=91 & ADP <=100)
fanatixwr1011 = subset(fanatixwr10team, ADP >=101 & ADP <=110)
fanatixwr1012 = subset(fanatixwr10team, ADP >=111 & ADP <=120)
fanatixwr1013 = subset(fanatixwr10team, ADP >=121 & ADP <=130)
fanatixwr1014 = subset(fanatixwr10team, ADP >=131 & ADP <=140)
fanatixwr1015 = subset(fanatixwr10team, ADP >=141 & ADP <=150)

WRADP101 = summary(fanatixwr101)
WRADP101
WRADP102 = summary(fanatixwr102)
WRADP102
WRADP103 = summary(fanatixwr103)
WRADP103
WRADP104 = summary(fanatixwr104)
WRADP104
WRADP105 = summary(fanatixwr105)
WRADP105
WRADP106 = summary(fanatixwr106)
WRADP106
WRADP107 = summary(fanatixwr107)
WRADP107
WRADP108 = summary(fanatixwr108)
WRADP108
WRADP109 = summary(fanatixwr109)
WRADP109
WRADP1010 = summary(fanatixwr1010)
WRADP1010
WRADP1011 = summary(fanatixwr1011)
WRADP1011
WRADP1012 = summary(fanatixwr1012)
WRADP1012
WRADP1013 = summary(fanatixwr1013)
WRADP1013
WRADP1014 = summary(fanatixwr1014)
WRADP1014
WRADP1015 = summary(fanatixwr1015)
WRADP1015

####12 Team
WRADP12 = summary(fanatixwr12team)
WRADP12

fanatixwr121 = subset(fanatixwr12team, ADP <=12)
fanatixwr122 = subset(fanatixwr12team, ADP >=13 & ADP <=24)
fanatixwr123 = subset(fanatixwr12team, ADP >=25 & ADP <=36)
fanatixwr124 = subset(fanatixwr12team, ADP >=37 & ADP <=48)
fanatixwr125 = subset(fanatixwr12team, ADP >=49 & ADP <=60)
fanatixwr126 = subset(fanatixwr12team, ADP >=61 & ADP <=72)
fanatixwr127 = subset(fanatixwr12team, ADP >=73 & ADP <=84)
fanatixwr128 = subset(fanatixwr12team, ADP >=85 & ADP <=96)
fanatixwr129 = subset(fanatixwr12team, ADP >=97 & ADP <=108)
fanatixwr1210 = subset(fanatixwr12team, ADP >=109 & ADP <=120)
fanatixwr1211 = subset(fanatixwr12team, ADP >=121 & ADP <=132)
fanatixwr1212 = subset(fanatixwr12team, ADP >=133 & ADP <=144)
fanatixwr1213 = subset(fanatixwr12team, ADP >=145 & ADP <=156)
fanatixwr1214 = subset(fanatixwr12team, ADP >=157 & ADP <=168)
fanatixwr1215 = subset(fanatixwr12team, ADP >=169 & ADP <=180)

WRADP121 = summary(fanatixwr121)
WRADP121
WRADP122 = summary(fanatixwr122)
WRADP122
WRADP123 = summary(fanatixwr123)
WRADP123
WRADP124 = summary(fanatixwr124)
WRADP124
WRADP125 = summary(fanatixwr125)
WRADP125
WRADP126 = summary(fanatixwr126)
WRADP126
WRADP127 = summary(fanatixwr127)
WRADP127
WRADP128 = summary(fanatixwr128)
WRADP128
WRADP129 = summary(fanatixwr129)
WRADP129
WRADP1210 = summary(fanatixwr1210)
WRADP1210
WRADP1211 = summary(fanatixwr1211)
WRADP1211
WRADP1212 = summary(fanatixwr1212)
WRADP1212
WRADP1213 = summary(fanatixwr1213)
WRADP1213
WRADP1214 = summary(fanatixwr1214)
WRADP1214
WRADP1215 = summary(fanatixwr1215)
WRADP1215

##TE

###create history for TE position, add season years, join into one df, and change teams to numeric

teproj17$Season = "2017"
teproj18$Season = "2018"
teproj19$Season = "2019"
teproj20$Season = "2020"
teproj21$Season = "2021"
teprojecting = join(teproj17,teproj18, by = c(), type = "full", match = "all")
teprojecting1 = join(teprojecting,teproj19, by = c(), type = "full", match = "all")
teprojecting2 = join(teprojecting1,teproj20, by = c(), type = "full", match = "all")
teprojecting3 = join(teprojecting2,teproj21, by = c(), type = "full", match = "all")

unique(teprojecting3$Team)
teprojecting3["Team"][teprojecting3["Team"] == "NA"] = "0"
teprojecting3["Team"][teprojecting3["Team"] == "ARI"] = "1"
teprojecting3["Team"][teprojecting3["Team"] == "ATL"] = "2"
teprojecting3["Team"][teprojecting3["Team"] == "BAL"] = "3"
teprojecting3["Team"][teprojecting3["Team"] == "BUF"] = "4"
teprojecting3["Team"][teprojecting3["Team"] == "CAR"] = "5"
teprojecting3["Team"][teprojecting3["Team"] == "CHI"] = "6"
teprojecting3["Team"][teprojecting3["Team"] == "CIN"] = "7"
teprojecting3["Team"][teprojecting3["Team"] == "CLE"] = "8"
teprojecting3["Team"][teprojecting3["Team"] == "DAL"] = "9"
teprojecting3["Team"][teprojecting3["Team"] == "DEN"] = "10"
teprojecting3["Team"][teprojecting3["Team"] == "DET"] = "11"
teprojecting3["Team"][teprojecting3["Team"] == "GB"] = "12"
teprojecting3["Team"][teprojecting3["Team"] == "HOU"] = "13"
teprojecting3["Team"][teprojecting3["Team"] == "IND"] = "14"
teprojecting3["Team"][teprojecting3["Team"] == "JAX"] = "15"
teprojecting3["Team"][teprojecting3["Team"] == "KC"] = "16"
teprojecting3["Team"][teprojecting3["Team"] == "LAC"] = "17"
teprojecting3["Team"][teprojecting3["Team"] == "LAR"] = "18"
teprojecting3["Team"][teprojecting3["Team"] == "LV"] = "19"
teprojecting3["Team"][teprojecting3["Team"] == "MIA"] = "20"
teprojecting3["Team"][teprojecting3["Team"] == "MIN"] = "21"
teprojecting3["Team"][teprojecting3["Team"] == "NE"] = "22"
teprojecting3["Team"][teprojecting3["Team"] == "NO"] = "23"
teprojecting3["Team"][teprojecting3["Team"] == "NYG"] = "24"
teprojecting3["Team"][teprojecting3["Team"] == "NYJ"] = "25"
teprojecting3["Team"][teprojecting3["Team"] == "PHI"] = "26"
teprojecting3["Team"][teprojecting3["Team"] == "PIT"] = "27"
teprojecting3["Team"][teprojecting3["Team"] == "SEA"] = "28"
teprojecting3["Team"][teprojecting3["Team"] == "SF"] = "29"
teprojecting3["Team"][teprojecting3["Team"] == "TB"] = "30"
teprojecting3["Team"][teprojecting3["Team"] == "TEN"] = "31"
teprojecting3["Team"][teprojecting3["Team"] == "WAS"] = "32"
unique(teprojecting3$Team)

teprojecting4 = join(teprojecting3, oddsm3, by = c(), type = "full", match = "all")

###create ages for each te based on season start date

tebday = teage
bdayte = c(tebday$Birth.Date)
tebday$BDay = as.Date(bdayte, "%m/%d/%y")
tebday$start17 = "2017-09-07" 
tebday$age17 = as.numeric(difftime(tebday$start17,tebday$BDay, units = "weeks"))/52.25

tebday$start18 = "2018-09-06" 
tebday$age18 = as.numeric(difftime(tebday$start18,tebday$BDay, units = "weeks"))/52.25

tebday$start19 = "2019-09-05" 
tebday$age19 = as.numeric(difftime(tebday$start19,tebday$BDay, units = "weeks"))/52.25

tebday$start20 = "2020-09-10" 
tebday$age20 = as.numeric(difftime(tebday$start20,tebday$BDay, units = "weeks"))/52.25

tebday$start21 = "2021-09-09" 
tebday$age21 = as.numeric(difftime(tebday$start21,tebday$BDay, units = "weeks"))/52.25

tebday = tebday[-c(2,3)]
tebday17 = tebday[c(1,4)]
tebday18 = tebday[c(1,6)]
tebday19 = tebday[c(1,8)]
tebday20 = tebday[c(1,10)]
tebday21 = tebday[c(1,12)]

colnames(tebday17) = c("Name", "Age")
tebday17$Season = "2017"
colnames(tebday18) = c("Name", "Age")
tebday18$Season = "2018"
colnames(tebday19) = c("Name", "Age")
tebday19$Season = "2019"
colnames(tebday20) = c("Name", "Age")
tebday20$Season = "2020"
colnames(tebday21) = c("Name", "Age")
tebday21$Season = "2021"

tebdayjoin = join(tebday17, tebday18, by =c(), type = "full", match = "all")
tebdayjoin1 = join(tebdayjoin, tebday19, by =c(), type = "full", match = "all")
tebdayjoin2 = join(tebdayjoin1, tebday20, by =c(), type = "full", match = "all")
tebdayjoin3 = join(tebdayjoin2, tebday21, by =c(), type = "full", match = "all")

teprojecting5 = join(teprojecting4, tebdayjoin3, by = c(), type = "full", match = "all")

###clean up df and join with prior df

teprojecting6 = teprojecting5[-c(1,12,15:21,24,26)]

teadp = fanatixte[c(1:4,9)]
teprojecting8 = join(teprojecting6,teadp, by = c(), type = "full", match = "all")

teprojecting7 = subset(teprojecting8, ADP <=250)
teprojecting7 = subset(teprojecting8, FantasyPoints > 0)

###correlations

####te

teprojecting7$Played <- as.numeric(as.character(teprojecting7$Played))
teprojecting7$Season <- as.numeric(as.character(teprojecting7$Season))
teprojecting7$OffLineRank <- as.numeric(as.character(teprojecting7$OffLineRank))

tecor1 <- cor.test(teprojecting7$Points, teprojecting7$Played, method = "pearson")
tecor1

####correlation = .1047

tecor2 <- cor.test(teprojecting7$Points, teprojecting7$ReceivingTargets, method = "pearson")
tecor2

####correlation = .3613

tecor3 <- cor.test(teprojecting7$Points, teprojecting7$Receptions, method = "pearson")
tecor3

####correlation = .3693

tecor4 <- cor.test(teprojecting7$Points, teprojecting7$ReceivingYards, method = "pearson")
tecor4

####correlation =.4565

tecor5 <- cor.test(teprojecting7$Points, teprojecting7$ReceivingTouchdowns, method = "pearson")
tecor5

####correlation = .2850

tecor6 <- cor.test(teprojecting7$Points, teprojecting7$ReceivingYardsPerTarget, method = "pearson")
tecor6

####correlation = .1934

tecor7 <- cor.test(teprojecting7$Points, teprojecting7$ReceivingYardsPerReception, method = "pearson")
tecor7

####correlation = .3304

tecor8 <- cor.test(teprojecting7$Points, teprojecting7$FantasyPoints, method = "pearson")
tecor8

####correlation = .4392

tecor9 <- cor.test(teprojecting7$Points, teprojecting7$PreW.L.O.U, method = "pearson")
tecor9

####correlation = .2778

tecor10 <- cor.test(teprojecting7$Points, teprojecting7$OffLineRank, method = "pearson")
tecor10

####correlation = -.1950

tecor11 <- cor.test(teprojecting7$Points, teprojecting7$ADP, method = "pearson")
tecor11

####correlation = -.3855

fanatixte$Athleticism.Score <- as.numeric(as.character(fanatixte$Athleticism.Score))

fanatixtease = fanatixte[c(1:3, 36:37)]
teprojecting7 = join(teprojecting7, fanatixtease, by = c(), type = "full", match = "all")

teprojecting7 = subset(teprojecting7, ADP <=250)
teprojecting7 = subset(teprojecting7, FantasyPoints > 0)

tecor12 = cor.test(teprojecting7$Points, teprojecting7$Athleticism.Score, method = "pearson")
tecor12

#### correlation = .2087

###tsdteformula

tsdformte = teprojecting7

tsdformte$rcyds = tsdformte$ReceivingYards * .6835
tsdformte$fpt = tsdformte$FantasyPoints * .6576
tsdformte$wlou = tsdformte$PreW.L.O.U * .4159
tsdformte$ath = tsdformte$Athleticism.Score * .3125
tsdformte$olr = tsdformte$OffLineRank * -.4923
tsdformte$adpos = tsdformte$ADP * -.5772
tsdformte$TSD = rowSums(tsdformte[, c(22:27)], na.rm = TRUE)

tecor14 <- cor.test(tsdformte$Points, tsdformte$TSD, method = "pearson")
tecor14

###correlation = .4595

###clean up df

tsdformte2 = tsdformte[-c(4:13,15:27)]

fanatixte2 = join(fanatixte, tsdformte2, by = c(), type = "full", match = "all")

fanatixte2 = subset(fanatixte2, ADP <=250)
fanatixte2 = subset(fanatixte2, Points > 0)

###create 22 season for projecting, change teams to numeric

teproj22["Team"][teproj22["Team"] == "NA"] = "0"
teproj22["Team"][teproj22["Team"] == "ARI"] = "1"
teproj22["Team"][teproj22["Team"] == "ATL"] = "2"
teproj22["Team"][teproj22["Team"] == "BAL"] = "3"
teproj22["Team"][teproj22["Team"] == "BUF"] = "4"
teproj22["Team"][teproj22["Team"] == "CAR"] = "5"
teproj22["Team"][teproj22["Team"] == "CHI"] = "6"
teproj22["Team"][teproj22["Team"] == "CIN"] = "7"
teproj22["Team"][teproj22["Team"] == "CLE"] = "8"
teproj22["Team"][teproj22["Team"] == "DAL"] = "9"
teproj22["Team"][teproj22["Team"] == "DEN"] = "10"
teproj22["Team"][teproj22["Team"] == "DET"] = "11"
teproj22["Team"][teproj22["Team"] == "GB"] = "12"
teproj22["Team"][teproj22["Team"] == "HOU"] = "13"
teproj22["Team"][teproj22["Team"] == "IND"] = "14"
teproj22["Team"][teproj22["Team"] == "JAX"] = "15"
teproj22["Team"][teproj22["Team"] == "KC"] = "16"
teproj22["Team"][teproj22["Team"] == "LAC"] = "17"
teproj22["Team"][teproj22["Team"] == "LAR"] = "18"
teproj22["Team"][teproj22["Team"] == "LV"] = "19"
teproj22["Team"][teproj22["Team"] == "MIA"] = "20"
teproj22["Team"][teproj22["Team"] == "MIN"] = "21"
teproj22["Team"][teproj22["Team"] == "NE"] = "22"
teproj22["Team"][teproj22["Team"] == "NO"] = "23"
teproj22["Team"][teproj22["Team"] == "NYG"] = "24"
teproj22["Team"][teproj22["Team"] == "NYJ"] = "25"
teproj22["Team"][teproj22["Team"] == "PHI"] = "26"
teproj22["Team"][teproj22["Team"] == "PIT"] = "27"
teproj22["Team"][teproj22["Team"] == "SEA"] = "28"
teproj22["Team"][teproj22["Team"] == "SF"] = "29"
teproj22["Team"][teproj22["Team"] == "TB"] = "30"
teproj22["Team"][teproj22["Team"] == "TEN"] = "31"
teproj22["Team"][teproj22["Team"] == "WAS"] = "32"
unique(teproj22$Team)

###add in team odds

teproj22 = join(teproj22, `22Oddste`, by = c(), type = "full", match = "all")
teproj22 = na.omit(teproj22)

###add age for season start date

tebday$start22 = "2022-09-09" 
tebday$age22 = as.numeric(difftime(tebday$start22,tebday$BDay, units = "weeks"))/52.25
tebday22 = tebday[c(1,14)]
colnames(tebday22) = c("Name", "Age")
teproj22 = join(teproj22, tebday22, by = c(), type = "full", match = "all")

teadp22 = teadp22[-c(1,4:13)]
teproj22 = join(teproj22, teadp22, by = c(), type = "full", match = "all")
teproj22 = join(teproj22, fanatixtease, by = c(), type = "full", match = "all")

teproj22$rcyds = teproj22$ReceivingYards * .6835
teproj22$fpt = teproj22$FantasyPoints * .6576
teproj22$wlou = teproj22$PreW.L.O.U * .4159
teproj22$ath = teproj22$Athleticism.Score * .3125
teproj22$olr = teproj22$OffLineRank * -.4923
teproj22$adpos = teproj22$AverageDraftPosition * -.5772
teproj22$TSD = rowSums(teproj22[, c(31:36)], na.rm = TRUE)

###clean up df

fanatixtetsd = tsdformte[c(1,2,14,28)]
teprojecting7 = join(teprojecting7, fanatixtetsd, by = c(), type = "full", match = "all")

teprojecting7 = subset(teprojecting7, ADP <=250)
teprojecting7 = subset(teprojecting7, Points > 0)

###TE Draft Round based on league size
fanatixte8team = subset(teprojecting7, ADP <=120)
fanatixte10team = subset(teprojecting7, ADP <=150)
fanatixte12team = subset(teprojecting7, ADP <=180)
fanatixte8team = fanatixte8team[c(1:4,13,18:19,22)]
fanatixte10team = fanatixte10team[c(1:4,13,18:19,22)]
fanatixte12team = fanatixte12team[c(1:4,13,18:19,22)]

####8 team

teADP8 = summary(fanatixte8team)
teADP8

fanatixte81 = subset(fanatixte8team, ADP <=8)
fanatixte82 = subset(fanatixte8team, ADP >=9 & ADP <=16)
fanatixte83 = subset(fanatixte8team, ADP >=17 & ADP <=24)
fanatixte84 = subset(fanatixte8team, ADP >=25 & ADP <=32)
fanatixte85 = subset(fanatixte8team, ADP >=33 & ADP <=40)
fanatixte86 = subset(fanatixte8team, ADP >=41 & ADP <=48)
fanatixte87 = subset(fanatixte8team, ADP >=49 & ADP <=56)
fanatixte88 = subset(fanatixte8team, ADP >=57 & ADP <=64)
fanatixte89 = subset(fanatixte8team, ADP >=65 & ADP <=72)
fanatixte810 = subset(fanatixte8team, ADP >=73 & ADP <=80)
fanatixte811 = subset(fanatixte8team, ADP >=81 & ADP <=88)
fanatixte812 = subset(fanatixte8team, ADP >=89 & ADP <=96)
fanatixte813 = subset(fanatixte8team, ADP >=97 & ADP <=104)
fanatixte814 = subset(fanatixte8team, ADP >=105 & ADP <=112)
fanatixte815 = subset(fanatixte8team, ADP >=113 & ADP <=120)

teADP81 = summary(fanatixte81)
teADP81
teADP82 = summary(fanatixte82)
teADP82
teADP83 = summary(fanatixte83)
teADP83
teADP84 = summary(fanatixte84)
teADP84
teADP85 = summary(fanatixte85)
teADP85
teADP86 = summary(fanatixte86)
teADP86
teADP87 = summary(fanatixte87)
teADP87
teADP88 = summary(fanatixte88)
teADP88
teADP89 = summary(fanatixte89)
teADP89
teADP810 = summary(fanatixte810)
teADP810
teADP811 = summary(fanatixte811)
teADP811
teADP812 = summary(fanatixte812)
teADP812
teADP813 = summary(fanatixte813)
teADP813
teADP814 = summary(fanatixte814)
teADP814
teADP815 = summary(fanatixte815)
teADP815

####10 Team
teADP10 = summary(fanatixte10team)
teADP10

fanatixte101 = subset(fanatixte10team, ADP <=10)
fanatixte102 = subset(fanatixte10team, ADP >=11 & ADP <=20)
fanatixte103 = subset(fanatixte10team, ADP >=21 & ADP <=30)
fanatixte104 = subset(fanatixte10team, ADP >=31 & ADP <=40)
fanatixte105 = subset(fanatixte10team, ADP >=41 & ADP <=50)
fanatixte106 = subset(fanatixte10team, ADP >=51 & ADP <=60)
fanatixte107 = subset(fanatixte10team, ADP >=61 & ADP <=70)
fanatixte108 = subset(fanatixte10team, ADP >=71 & ADP <=80)
fanatixte109 = subset(fanatixte10team, ADP >=81 & ADP <=90)
fanatixte1010 = subset(fanatixte10team, ADP >=91 & ADP <=100)
fanatixte1011 = subset(fanatixte10team, ADP >=101 & ADP <=110)
fanatixte1012 = subset(fanatixte10team, ADP >=111 & ADP <=120)
fanatixte1013 = subset(fanatixte10team, ADP >=121 & ADP <=130)
fanatixte1014 = subset(fanatixte10team, ADP >=131 & ADP <=140)
fanatixte1015 = subset(fanatixte10team, ADP >=141 & ADP <=150)

teADP101 = summary(fanatixte101)
teADP101
teADP102 = summary(fanatixte102)
teADP102
teADP103 = summary(fanatixte103)
teADP103
teADP104 = summary(fanatixte104)
teADP104
teADP105 = summary(fanatixte105)
teADP105
teADP106 = summary(fanatixte106)
teADP106
teADP107 = summary(fanatixte107)
teADP107
teADP108 = summary(fanatixte108)
teADP108
teADP109 = summary(fanatixte109)
teADP109
teADP1010 = summary(fanatixte1010)
teADP1010
teADP1011 = summary(fanatixte1011)
teADP1011
teADP1012 = summary(fanatixte1012)
teADP1012
teADP1013 = summary(fanatixte1013)
teADP1013
teADP1014 = summary(fanatixte1014)
teADP1014
teADP1015 = summary(fanatixte1015)
teADP1015

####12 Team
teADP12 = summary(fanatixte12team)
teADP12

fanatixte121 = subset(fanatixte12team, ADP <=12)
fanatixte122 = subset(fanatixte12team, ADP >=13 & ADP <=24)
fanatixte123 = subset(fanatixte12team, ADP >=25 & ADP <=36)
fanatixte124 = subset(fanatixte12team, ADP >=37 & ADP <=48)
fanatixte125 = subset(fanatixte12team, ADP >=49 & ADP <=60)
fanatixte126 = subset(fanatixte12team, ADP >=61 & ADP <=72)
fanatixte127 = subset(fanatixte12team, ADP >=73 & ADP <=84)
fanatixte128 = subset(fanatixte12team, ADP >=85 & ADP <=96)
fanatixte129 = subset(fanatixte12team, ADP >=97 & ADP <=108)
fanatixte1210 = subset(fanatixte12team, ADP >=109 & ADP <=120)
fanatixte1211 = subset(fanatixte12team, ADP >=121 & ADP <=132)
fanatixte1212 = subset(fanatixte12team, ADP >=133 & ADP <=144)
fanatixte1213 = subset(fanatixte12team, ADP >=145 & ADP <=156)
fanatixte1214 = subset(fanatixte12team, ADP >=157 & ADP <=168)
fanatixte1215 = subset(fanatixte12team, ADP >=169 & ADP <=180)

teADP121 = summary(fanatixte121)
teADP121
teADP122 = summary(fanatixte122)
teADP122
teADP123 = summary(fanatixte123)
teADP123
teADP124 = summary(fanatixte124)
teADP124
teADP125 = summary(fanatixte125)
teADP125
teADP126 = summary(fanatixte126)
teADP126
teADP127 = summary(fanatixte127)
teADP127
teADP128 = summary(fanatixte128)
teADP128
teADP129 = summary(fanatixte129)
teADP129
teADP1210 = summary(fanatixte1210)
teADP1210
teADP1211 = summary(fanatixte1211)
teADP1211
teADP1212 = summary(fanatixte1212)
teADP1212
teADP1213 = summary(fanatixte1213)
teADP1213
teADP1214 = summary(fanatixte1214)
teADP1214
teADP1215 = summary(fanatixte1215)
teADP1215

#TSD Means

##find means of TSD of each df and change each df to "master"position

TETSDmean = mean(teprojecting7$TSD)
QBTSDmean = mean(fanatixqb5$TSD)
RBTSDmean = mean(rbprojecting7$TSD)
WRTSDmean = mean(wrprojecting7$TSD)

masterqb = fanatixqb5
masterrb = rbprojecting7
masterwr = wrprojecting7
masterte = teprojecting7

###make all positions have the same mean

masterte$TSDx = masterte$TSD * 0.902512
masterqb$TSDx = masterqb$TSD * 6.877553
masterrb$TSDx = masterrb$TSD * 0.749588
masterwr$TSDx = masterwr$TSD * 0.707957

TETSDmeanm = mean(masterte$TSDx)
QBTSDmeanm = mean(masterqb$TSDx)
RBTSDmeanm = mean(masterrb$TSDx)
WRTSDmeanm = mean(masterwr$TSDx)
TETSDmeanm
QBTSDmeanm
RBTSDmeanm
WRTSDmeanm

TETSDsum = summary(masterte$TSDx)
TETSDsum
masterte$TSDx = masterte$TSDx / 9.7447
TETSDsum = summary(masterte$TSDx)
TETSDsum

QBTSDsum = summary(masterqb$TSDx)
QBTSDsum
masterqb$TSDx = masterqb$TSDx / 11.0670
QBTSDsum = summary(masterqb$TSDx)
QBTSDsum

WRTSDsum = summary(masterwr$TSDx)
WRTSDsum
masterwr$TSDx = masterwr$TSDx / 11.3406
WRTSDsum = summary(masterwr$TSDx)
WRTSDsum

RBTSDsum = summary(masterrb$TSDx)
RBTSDsum
masterrb$TSDx = masterrb$TSDx / 12.1030
RBTSDsum = summary(masterrb$TSDx)
RBTSDsum

###check correlation of TSDx

rbcortsd <- cor.test(masterrb$Points, masterrb$TSDx, method = "pearson")
rbcortsd

wrcortsd <- cor.test(masterwr$Points, masterwr$TSDx, method = "pearson")
wrcortsd

qbcortsd <- cor.test(masterqb$Points, masterqb$TSDx, method = "pearson")
qbcortsd

tecortsd <- cor.test(masterte$Points, masterte$TSDx, method = "pearson")
tecortsd

summary(masterqb$ProjPassTD)
summary(masterqb$ProjGamesPlayed)
summary(masterqb$OffLineRank)
summary(masterqb$ProjPoints)
summary(masterqb$PreW.L.O.U)
summary(masterqb$ProjRushTD)
summary(masterqb$dftrd8)
summary(masterqb$ADP)

##Does qbTSD translate to points scored?

###test for linearity
ggplot(masterqb, aes(x = TSDx, y = Points)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)

###test for homoscedasticity
lmModqb = lm(Points~TSDx, data = masterqb)

par(mfrow=c(2,2))
plot(lmModqb)

lmtest::bptest(lmModqb)
car::ncvTest(lmModqb)

####did not violate assumption of Homoscedasticity

gvlma(lmModqb)

### heteroscedasticity acceptable

###test for outliers in x space

CookD(lmModqb, group = NULL, plot = TRUE, idn = 3, newwd = TRUE)

####4,74,112 are outliers but not over .1
levqb = hat(model.matrix(lmModqb))
plot(levqb)

####nothing over .1
masterqb[levqb>.1,]

####no outliers in x space

###test for outliers in y space

car::outlierTest(lmModqb)

####no outliers in y space

###test for outliers in x and y space

summary(influence.measures(lmModqb))
####no outliers

###Interpreting Output
summary(lmModqb)
####fstat = 97.25, pvalue <2.2e-16
####TSDx does predict qb fantasy points


##Does rbTSD translate to points scored?

###test for linearity
ggplot(masterrb, aes(x = TSDx, y = Points)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)

###test for homoscedasticity
lmModrb = lm(Points~TSDx, data = masterrb)

par(mfrow=c(2,2))
plot(lmModrb)

lmtest::bptest(lmModrb)
car::ncvTest(lmModrb)

####did violate assumption of Homoscedasticity

gvlma(lmModrb)

#### heteroscedasticity acceptable

###test for outliers in x space

CookD(lmModrb, group = NULL, plot = TRUE, idn = 3, newwd = TRUE)
####1,449,450 are outliers but not over .2
levrb = hat(model.matrix(lmModrb))
plot(levrb)
####nothing over .1
masterrb[levrb>.2,]

####no outliers in x space

###test for outliers in y space

car::outlierTest(lmModrb)

####no outliers in y space

###test for outliers in x and y space

summary(influence.measures(lmModrb))
####no outliers

###Interpreting Output
summary(lmModrb)
####fstat = 263.6, pvalue <2.2e-16
####TSDx does predict rb fantasy points

##Does wrTSD translate to points scored?

###test for linearity
ggplot(masterwr, aes(x = TSDx, y = Points)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)

###test for homoscedasticity
lmModwr = lm(Points~TSDx, data = masterwr)

par(mfrow=c(2,2))
plot(lmModwr)

lmtest::bptest(lmModwr)
car::ncvTest(lmModwr)

####did violate assumption of Homoscedasticity

gvlma(lmModwr)

#### heteroscedasticity acceptable

###test for outliers in x space

CookD(lmModwr, group = NULL, plot = TRUE, idn = 3, newwd = TRUE)
####604,835,852 are outliers but not over .1
levwr = hat(model.matrix(lmModwr))
plot(levwr)
####nothing over .1
masterwr[levwr>.1,]

####no outliers in x space

###test for outliers in y space

car::outlierTest(lmModwr)

####outliers in y space

###test for outliers in x and y space

summary(influence.measures(lmModwr))
####no outliers

###Interpreting Output
summary(lmModwr)
####fstat = 538.9, pvalue <2.2e-16
####TSDx does predict wr fantasy points



##Does teTSD translate to points scored?

###test for linearity
ggplot(masterte, aes(x = TSDx, y = Points)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)

###test for homoscedasticity
lmModte = lm(Points~TSDx, data = masterte)

par(mfrow=c(2,2))
plot(lmModte)

lmtest::bptest(lmModte)
car::ncvTest(lmModte)

####did violate assumption of Homoscedasticity

###correcting
distBCModte = caret::BoxCoxTrans(masterte$TSDx)
print(distBCModte)

masterte1 = cbind(masterte, dist_newte = predict(distBCModte, masterte$TSDx))

lmMod_te = lm(Points~dist_newte, data = masterte1)
lmtest::bptest(lmMod_te)

####Corrected, pvalue 0.05632

gvlma(lmMod_te)

#### heteroscedasticity acceptable

###test for outliers in x space

CookD(lmMod_te, group = NULL, plot = TRUE, idn = 3, newwd = TRUE)
####2,68,108 are outliers with 108 possibly over .2
levte = hat(model.matrix(lmMod_te))
plot(levte)
####nothing over .1
masterte1[levte>.1,]

####no outliers in x space but pat freirmuth is >.1

###test for outliers in y space

car::outlierTest(lmMod_te)

####no outliers in y space

###test for outliers in x and y space

summary(influence.measures(lmMod_te))
####no outliers

###Interpreting Output
summary(lmMod_te)
####fstat = 26.35, pvalue <1.299e-06
####TSDx does predict te fantasy points

tecortsd1 <- cor.test(masterte1$dist_newte, masterte1$Points, method = "pearson")
tecortsd1

QBADP8
QBADP81
QBADP82
QBADP83
QBADP84
QBADP85
QBADP86
QBADP87
QBADP88
QBADP89
QBADP810
QBADP811
QBADP812
QBADP813
QBADP814
QBADP815
QBADP8

QBADP101
QBADP102
QBADP103
QBADP104
QBADP105
QBADP106
QBADP107
QBADP108
QBADP109
QBADP1010
QBADP1011
QBADP1012
QBADP1013
QBADP1014
QBADP1015
QBADP10

QBADP121
QBADP122
QBADP123
QBADP124
QBADP125
QBADP126
QBADP127
QBADP128
QBADP129
QBADP1210
QBADP1211
QBADP1212
QBADP1213
QBADP1214
QBADP1215
QBADP12

RBADP81
RBADP82
RBADP83
RBADP84
RBADP85
RBADP86
RBADP87
RBADP88
RBADP89
RBADP810
RBADP811
RBADP812
RBADP813
RBADP814
RBADP815
RBADP8

RBADP101
RBADP102
RBADP103
RBADP104
RBADP105
RBADP106
RBADP107
RBADP108
RBADP109
RBADP1010
RBADP1011
RBADP1012
RBADP1013
RBADP1014
RBADP1015
RBADP10

RBADP121
RBADP122
RBADP123
RBADP124
RBADP125
RBADP126
RBADP127
RBADP128
RBADP129
RBADP1210
RBADP1211
RBADP1212
RBADP1213
RBADP1214
RBADP1215
RBADP12

WRADP81
WRADP82
WRADP83
wRADP84
WRADP85
WRADP86
WRADP87
WRADP88
WRADP89
WRADP810
WRADP811
WRADP812
WRADP813
WRADP814
WRADP815
WRADP8

WRADP101
WRADP102
WRADP103
WRADP104
WRADP105
WRADP106
WRADP107
WRADP108
WRADP109
WRADP1010
WRADP1011
WRADP1012
WRADP1013
WRADP1014
WRADP1015
WRADP10

WRADP121
WRADP122
WRADP123
WRADP124
WRADP125
WRADP126
WRADP127
WRADP128
WRADP129
WRADP1210
WRADP1211
WRADP1212
WRADP1213
WRADP1214
WRADP1215
WRADP12

teADP81
teADP82
teADP83
teADP84
teADP85
teADP86
teADP87
teADP88
teADP89
teADP810
teADP811
teADP812
teADP813
teADP814
teADP815
teADP8

teADP101
teADP102
teADP103
teADP104
teADP105
teADP106
teADP107
teADP108
teADP109
teADP1010
teADP1011
teADP1012
teADP1013
teADP1014
teADP1015
teADP10

teADP121
teADP122
teADP123
teADP124
teADP125
teADP126
teADP127
teADP128
teADP129
teADP1210
teADP1211
teADP1212
teADP1213
teADP1214
teADP1215
teADP12

##create columns for draft rounds for each position and league size

masterqb$ADP = round(masterqb$ADP)

masterqb$draftround8 = ifelse(masterqb$ADP<=8, 1,
                              ifelse(masterqb$ADP >=9 & masterqb$ADP <=16, 2,
                                     ifelse(masterqb$ADP >=17 & masterqb$ADP <=24, 3,
                                            ifelse(masterqb$ADP >=25 & masterqb$ADP <=32, 4,
                                                   ifelse(masterqb$ADP >=33 & masterqb$ADP <=40, 5,
                                                          ifelse(masterqb$ADP >=41 & masterqb$ADP <=48, 6,
                                                                 ifelse(masterqb$ADP >=49 & masterqb$ADP <=56, 7,
                                                                        ifelse(masterqb$ADP >=57 & masterqb$ADP <=64, 8,
                                                                               ifelse(masterqb$ADP >=65 & masterqb$ADP <=72, 9,
                                                                                      ifelse(masterqb$ADP >=73 & masterqb$ADP <=80, 10,
                                                                                             ifelse(masterqb$ADP >=81 & masterqb$ADP <=88, 11,
                                                                                                    ifelse(masterqb$ADP >=89 & masterqb$ADP <=96, 12,
                                                                                                           ifelse(masterqb$ADP >=97 & masterqb$ADP <=104, 13,
                                                                                                                  ifelse(masterqb$ADP >=105 & masterqb$ADP <=112, 14,
                                                                                                                         ifelse(masterqb$ADP >=113 & masterqb$ADP <=120, 15,NA)))))))))))))))

masterqb$draftround10 = ifelse(masterqb$ADP<=10, 1,
                               ifelse(masterqb$ADP >=11 & masterqb$ADP <=20, 2,
                                      ifelse(masterqb$ADP >=21 & masterqb$ADP <=30, 3,
                                             ifelse(masterqb$ADP >=31 & masterqb$ADP <=40, 4,
                                                    ifelse(masterqb$ADP >=41 & masterqb$ADP <=50, 5,
                                                           ifelse(masterqb$ADP >=51 & masterqb$ADP <=60, 6,
                                                                  ifelse(masterqb$ADP >=61 & masterqb$ADP <=70, 7,
                                                                         ifelse(masterqb$ADP >=71 & masterqb$ADP <=80, 8,
                                                                                ifelse(masterqb$ADP >=81 & masterqb$ADP <=90, 9,
                                                                                       ifelse(masterqb$ADP >=91 & masterqb$ADP <=100, 10,
                                                                                              ifelse(masterqb$ADP >=101 & masterqb$ADP <=110, 11,
                                                                                                     ifelse(masterqb$ADP >=111 & masterqb$ADP <=120, 12,
                                                                                                            ifelse(masterqb$ADP >=121 & masterqb$ADP <=130, 13,
                                                                                                                   ifelse(masterqb$ADP >=131 & masterqb$ADP <=140, 14,
                                                                                                                          ifelse(masterqb$ADP >=141 & masterqb$ADP <=150, 15,NA)))))))))))))))

masterqb$draftround12 = ifelse(masterqb$ADP<=12, 1,
                               ifelse(masterqb$ADP >=13 & masterqb$ADP <=24, 2,
                                      ifelse(masterqb$ADP >=25 & masterqb$ADP <=36, 3,
                                             ifelse(masterqb$ADP >=37 & masterqb$ADP <=48, 4,
                                                    ifelse(masterqb$ADP >=49 & masterqb$ADP <=60, 5,
                                                           ifelse(masterqb$ADP >=61 & masterqb$ADP <=72, 6,
                                                                  ifelse(masterqb$ADP >=73 & masterqb$ADP <=84, 7,
                                                                         ifelse(masterqb$ADP >=85 & masterqb$ADP <=96, 8,
                                                                                ifelse(masterqb$ADP >=97 & masterqb$ADP <=108, 9,
                                                                                       ifelse(masterqb$ADP >=109 & masterqb$ADP <=120, 10,
                                                                                              ifelse(masterqb$ADP >=121 & masterqb$ADP <=132, 11,
                                                                                                     ifelse(masterqb$ADP >=133 & masterqb$ADP <=144, 12,
                                                                                                            ifelse(masterqb$ADP >=145 & masterqb$ADP <=156, 13,
                                                                                                                   ifelse(masterqb$ADP >=157 & masterqb$ADP <=168, 14,
                                                                                                                          ifelse(masterqb$ADP >=169 & masterqb$ADP <=180, 15,NA)))))))))))))))


masterrb$ADP = round(masterrb$ADP)

masterrb$draftround8 = ifelse(masterrb$ADP<=8, 1,
                              ifelse(masterrb$ADP >=9 & masterrb$ADP <=16, 2,
                                     ifelse(masterrb$ADP >=17 & masterrb$ADP <=24, 3,
                                            ifelse(masterrb$ADP >=25 & masterrb$ADP <=32, 4,
                                                   ifelse(masterrb$ADP >=33 & masterrb$ADP <=40, 5,
                                                          ifelse(masterrb$ADP >=41 & masterrb$ADP <=48, 6,
                                                                 ifelse(masterrb$ADP >=49 & masterrb$ADP <=56, 7,
                                                                        ifelse(masterrb$ADP >=57 & masterrb$ADP <=64, 8,
                                                                               ifelse(masterrb$ADP >=65 & masterrb$ADP <=72, 9,
                                                                                      ifelse(masterrb$ADP >=73 & masterrb$ADP <=80, 10,
                                                                                             ifelse(masterrb$ADP >=81 & masterrb$ADP <=88, 11,
                                                                                                    ifelse(masterrb$ADP >=89 & masterrb$ADP <=96, 12,
                                                                                                           ifelse(masterrb$ADP >=97 & masterrb$ADP <=104, 13,
                                                                                                                  ifelse(masterrb$ADP >=105 & masterrb$ADP <=112, 14,
                                                                                                                         ifelse(masterrb$ADP >=113 & masterrb$ADP <=120, 15,NA)))))))))))))))

masterrb$draftround10 = ifelse(masterrb$ADP<=10, 1,
                               ifelse(masterrb$ADP >=11 & masterrb$ADP <=20, 2,
                                      ifelse(masterrb$ADP >=21 & masterrb$ADP <=30, 3,
                                             ifelse(masterrb$ADP >=31 & masterrb$ADP <=40, 4,
                                                    ifelse(masterrb$ADP >=41 & masterrb$ADP <=50, 5,
                                                           ifelse(masterrb$ADP >=51 & masterrb$ADP <=60, 6,
                                                                  ifelse(masterrb$ADP >=61 & masterrb$ADP <=70, 7,
                                                                         ifelse(masterrb$ADP >=71 & masterrb$ADP <=80, 8,
                                                                                ifelse(masterrb$ADP >=81 & masterrb$ADP <=90, 9,
                                                                                       ifelse(masterrb$ADP >=91 & masterrb$ADP <=100, 10,
                                                                                              ifelse(masterrb$ADP >=101 & masterrb$ADP <=110, 11,
                                                                                                     ifelse(masterrb$ADP >=111 & masterrb$ADP <=120, 12,
                                                                                                            ifelse(masterrb$ADP >=121 & masterrb$ADP <=130, 13,
                                                                                                                   ifelse(masterrb$ADP >=131 & masterrb$ADP <=140, 14,
                                                                                                                          ifelse(masterrb$ADP >=141 & masterrb$ADP <=150, 15,NA)))))))))))))))

masterrb$draftround12 = ifelse(masterrb$ADP<=12, 1,
                               ifelse(masterrb$ADP >=13 & masterrb$ADP <=24, 2,
                                      ifelse(masterrb$ADP >=25 & masterrb$ADP <=36, 3,
                                             ifelse(masterrb$ADP >=37 & masterrb$ADP <=48, 4,
                                                    ifelse(masterrb$ADP >=49 & masterrb$ADP <=60, 5,
                                                           ifelse(masterrb$ADP >=61 & masterrb$ADP <=72, 6,
                                                                  ifelse(masterrb$ADP >=73 & masterrb$ADP <=84, 7,
                                                                         ifelse(masterrb$ADP >=85 & masterrb$ADP <=96, 8,
                                                                                ifelse(masterrb$ADP >=97 & masterrb$ADP <=108, 9,
                                                                                       ifelse(masterrb$ADP >=109 & masterrb$ADP <=120, 10,
                                                                                              ifelse(masterrb$ADP >=121 & masterrb$ADP <=132, 11,
                                                                                                     ifelse(masterrb$ADP >=133 & masterrb$ADP <=144, 12,
                                                                                                            ifelse(masterrb$ADP >=145 & masterrb$ADP <=156, 13,
                                                                                                                   ifelse(masterrb$ADP >=157 & masterrb$ADP <=168, 14,
                                                                                                                          ifelse(masterrb$ADP >=169 & masterrb$ADP <=180, 15,NA)))))))))))))))

masterwr$ADP = round(masterwr$ADP)

masterwr$draftround8 = ifelse(masterwr$ADP<=8, 1,
                              ifelse(masterwr$ADP >=9 & masterwr$ADP <=16, 2,
                                     ifelse(masterwr$ADP >=17 & masterwr$ADP <=24, 3,
                                            ifelse(masterwr$ADP >=25 & masterwr$ADP <=32, 4,
                                                   ifelse(masterwr$ADP >=33 & masterwr$ADP <=40, 5,
                                                          ifelse(masterwr$ADP >=41 & masterwr$ADP <=48, 6,
                                                                 ifelse(masterwr$ADP >=49 & masterwr$ADP <=56, 7,
                                                                        ifelse(masterwr$ADP >=57 & masterwr$ADP <=64, 8,
                                                                               ifelse(masterwr$ADP >=65 & masterwr$ADP <=72, 9,
                                                                                      ifelse(masterwr$ADP >=73 & masterwr$ADP <=80, 10,
                                                                                             ifelse(masterwr$ADP >=81 & masterwr$ADP <=88, 11,
                                                                                                    ifelse(masterwr$ADP >=89 & masterwr$ADP <=96, 12,
                                                                                                           ifelse(masterwr$ADP >=97 & masterwr$ADP <=104, 13,
                                                                                                                  ifelse(masterwr$ADP >=105 & masterwr$ADP <=112, 14,
                                                                                                                         ifelse(masterwr$ADP >=113 & masterwr$ADP <=120, 15,NA)))))))))))))))

masterwr$draftround10 = ifelse(masterwr$ADP<=10, 1,
                               ifelse(masterwr$ADP >=11 & masterwr$ADP <=20, 2,
                                      ifelse(masterwr$ADP >=21 & masterwr$ADP <=30, 3,
                                             ifelse(masterwr$ADP >=31 & masterwr$ADP <=40, 4,
                                                    ifelse(masterwr$ADP >=41 & masterwr$ADP <=50, 5,
                                                           ifelse(masterwr$ADP >=51 & masterwr$ADP <=60, 6,
                                                                  ifelse(masterwr$ADP >=61 & masterwr$ADP <=70, 7,
                                                                         ifelse(masterwr$ADP >=71 & masterwr$ADP <=80, 8,
                                                                                ifelse(masterwr$ADP >=81 & masterwr$ADP <=90, 9,
                                                                                       ifelse(masterwr$ADP >=91 & masterwr$ADP <=100, 10,
                                                                                              ifelse(masterwr$ADP >=101 & masterwr$ADP <=110, 11,
                                                                                                     ifelse(masterwr$ADP >=111 & masterwr$ADP <=120, 12,
                                                                                                            ifelse(masterwr$ADP >=121 & masterwr$ADP <=130, 13,
                                                                                                                   ifelse(masterwr$ADP >=131 & masterwr$ADP <=140, 14,
                                                                                                                          ifelse(masterwr$ADP >=141 & masterwr$ADP <=150, 15,NA)))))))))))))))

masterwr$draftround12 = ifelse(masterwr$ADP<=12, 1,
                               ifelse(masterwr$ADP >=13 & masterwr$ADP <=24, 2,
                                      ifelse(masterwr$ADP >=25 & masterwr$ADP <=36, 3,
                                             ifelse(masterwr$ADP >=37 & masterwr$ADP <=48, 4,
                                                    ifelse(masterwr$ADP >=49 & masterwr$ADP <=60, 5,
                                                           ifelse(masterwr$ADP >=61 & masterwr$ADP <=72, 6,
                                                                  ifelse(masterwr$ADP >=73 & masterwr$ADP <=84, 7,
                                                                         ifelse(masterwr$ADP >=85 & masterwr$ADP <=96, 8,
                                                                                ifelse(masterwr$ADP >=97 & masterwr$ADP <=108, 9,
                                                                                       ifelse(masterwr$ADP >=109 & masterwr$ADP <=120, 10,
                                                                                              ifelse(masterwr$ADP >=121 & masterwr$ADP <=132, 11,
                                                                                                     ifelse(masterwr$ADP >=133 & masterwr$ADP <=144, 12,
                                                                                                            ifelse(masterwr$ADP >=145 & masterwr$ADP <=156, 13,
                                                                                                                   ifelse(masterwr$ADP >=157 & masterwr$ADP <=168, 14,
                                                                                                                          ifelse(masterwr$ADP >=169 & masterwr$ADP <=180, 15,NA)))))))))))))))


masterte$ADP = round(masterte$ADP)

masterte$draftround8 = ifelse(masterte$ADP<=8, 1,
                              ifelse(masterte$ADP >=9 & masterte$ADP <=16, 2,
                                     ifelse(masterte$ADP >=17 & masterte$ADP <=24, 3,
                                            ifelse(masterte$ADP >=25 & masterte$ADP <=32, 4,
                                                   ifelse(masterte$ADP >=33 & masterte$ADP <=40, 5,
                                                          ifelse(masterte$ADP >=41 & masterte$ADP <=48, 6,
                                                                 ifelse(masterte$ADP >=49 & masterte$ADP <=56, 7,
                                                                        ifelse(masterte$ADP >=57 & masterte$ADP <=64, 8,
                                                                               ifelse(masterte$ADP >=65 & masterte$ADP <=72, 9,
                                                                                      ifelse(masterte$ADP >=73 & masterte$ADP <=80, 10,
                                                                                             ifelse(masterte$ADP >=81 & masterte$ADP <=88, 11,
                                                                                                    ifelse(masterte$ADP >=89 & masterte$ADP <=96, 12,
                                                                                                           ifelse(masterte$ADP >=97 & masterte$ADP <=104, 13,
                                                                                                                  ifelse(masterte$ADP >=105 & masterte$ADP <=112, 14,
                                                                                                                         ifelse(masterte$ADP >=113 & masterte$ADP <=120, 15,NA)))))))))))))))

masterte$draftround10 = ifelse(masterte$ADP<=10, 1,
                               ifelse(masterte$ADP >=11 & masterte$ADP <=20, 2,
                                      ifelse(masterte$ADP >=21 & masterte$ADP <=30, 3,
                                             ifelse(masterte$ADP >=31 & masterte$ADP <=40, 4,
                                                    ifelse(masterte$ADP >=41 & masterte$ADP <=50, 5,
                                                           ifelse(masterte$ADP >=51 & masterte$ADP <=60, 6,
                                                                  ifelse(masterte$ADP >=61 & masterte$ADP <=70, 7,
                                                                         ifelse(masterte$ADP >=71 & masterte$ADP <=80, 8,
                                                                                ifelse(masterte$ADP >=81 & masterte$ADP <=90, 9,
                                                                                       ifelse(masterte$ADP >=91 & masterte$ADP <=100, 10,
                                                                                              ifelse(masterte$ADP >=101 & masterte$ADP <=110, 11,
                                                                                                     ifelse(masterte$ADP >=111 & masterte$ADP <=120, 12,
                                                                                                            ifelse(masterte$ADP >=121 & masterte$ADP <=130, 13,
                                                                                                                   ifelse(masterte$ADP >=131 & masterte$ADP <=140, 14,
                                                                                                                          ifelse(masterte$ADP >=141 & masterte$ADP <=150, 15,NA)))))))))))))))

masterte$draftround12 = ifelse(masterte$ADP<=12, 1,
                               ifelse(masterte$ADP >=13 & masterte$ADP <=24, 2,
                                      ifelse(masterte$ADP >=25 & masterte$ADP <=36, 3,
                                             ifelse(masterte$ADP >=37 & masterte$ADP <=48, 4,
                                                    ifelse(masterte$ADP >=49 & masterte$ADP <=60, 5,
                                                           ifelse(masterte$ADP >=61 & masterte$ADP <=72, 6,
                                                                  ifelse(masterte$ADP >=73 & masterte$ADP <=84, 7,
                                                                         ifelse(masterte$ADP >=85 & masterte$ADP <=96, 8,
                                                                                ifelse(masterte$ADP >=97 & masterte$ADP <=108, 9,
                                                                                       ifelse(masterte$ADP >=109 & masterte$ADP <=120, 10,
                                                                                              ifelse(masterte$ADP >=121 & masterte$ADP <=132, 11,
                                                                                                     ifelse(masterte$ADP >=133 & masterte$ADP <=144, 12,
                                                                                                            ifelse(masterte$ADP >=145 & masterte$ADP <=156, 13,
                                                                                                                   ifelse(masterte$ADP >=157 & masterte$ADP <=168, 14,
                                                                                                                          ifelse(masterte$ADP >=169 & masterte$ADP <=180, 15,NA)))))))))))))))

##clean up position dataframes and join dataset with position points mean

RBdraftpos8 = na.omit(RBdraftpos8)
RBdraftpos10 = na.omit(RBdraftpos10)
RBdraftpos12 = na.omit(RBdraftpos12)

RBdraftpos8 = RBdraftpos8[-c(2:6)]
RBdraftpos10 = RBdraftpos10[-c(2:6)]
RBdraftpos12 = RBdraftpos12[-c(2:6)]

masterrb = join(masterrb, RBdraftpos8, by = c(), type = "full", match = "all")
masterrb = join(masterrb, RBdraftpos10, by = c(), type = "full", match = "all")
masterrb = join(masterrb, RBdraftpos12, by = c(), type = "full", match = "all")

QBdraftpos8 = na.omit(QBdraftpos8)
QBdraftpos10 = na.omit(QBdraftpos10)
QBdraftpos12 = na.omit(QBdraftpos12)

QBdraftpos8 = QBdraftpos8[-c(2:5)]
QBdraftpos10 = QBdraftpos10[-c(2:5)]
QBdraftpos12 = QBdraftpos12[-c(2:5)]

QBdraftpos8$meanpoints8 = QBdraftpos8$`+/-points/mean`
QBdraftpos10$meanpoints10 = QBdraftpos10$`+/-points/mean`
QBdraftpos12$meanpoints12 = QBdraftpos12$`+/-points/mean`

QBdraftpos8 = QBdraftpos8[-c(2)]
QBdraftpos10 = QBdraftpos10[-c(2)]
QBdraftpos12 = QBdraftpos12[-c(2)]

masterqb = join(masterqb, QBdraftpos8, by = c(), type = "full", match = "all")
masterqb = join(masterqb, QBdraftpos10, by = c(), type = "full", match = "all")
masterqb = join(masterqb, QBdraftpos12, by = c(), type = "full", match = "all")

WRdraftpos8 = na.omit(WRdraftpos8)
WRdraftpos10 = na.omit(WRdraftpos10)
WRdraftpos12 = na.omit(WRdraftpos12)

WRdraftpos8$meanpoints8 = WRdraftpos8$`+/-points/mean`
WRdraftpos10$meanpoints10 = WRdraftpos10$`+/-points/mean`
WRdraftpos12$meanpoints12 = WRdraftpos12$`+/-points/mean`

WRdraftpos8$draftround8 = WRdraftpos8$Round
WRdraftpos10$draftround10 = WRdraftpos10$Round
WRdraftpos12$draftround12 = WRdraftpos12$Round

WRdraftpos8 = WRdraftpos8[-c(1:9)]
WRdraftpos10 = WRdraftpos10[-c(1:9)]
WRdraftpos12 = WRdraftpos12[-c(1:9)]

masterwr = masterwr[-c(29:31)]

masterwr = join(masterwr, WRdraftpos8, by = c(), type = "full", match = "all")
masterwr = join(masterwr, WRdraftpos10, by = c(), type = "full", match = "all")
masterwr = join(masterwr, WRdraftpos12, by = c(), type = "full", match = "all")

TEdraftpos8 = na.omit(TEdraftpos8)
TEdraftpos10 = na.omit(TEdraftpos10)
TEdraftpos12 = na.omit(TEdraftpos12)

TEdraftpos8$meanpoints8 = TEdraftpos8$`+/-points/mean`
TEdraftpos10$meanpoints10 = TEdraftpos10$`+/-points/mean`
TEdraftpos12$meanpoints12 = TEdraftpos12$`+/-points/mean`

TEdraftpos8$draftround8 = TEdraftpos8$Round
TEdraftpos10$draftround10 = TEdraftpos10$Round
TEdraftpos12$draftround12 = TEdraftpos12$Round

TEdraftpos8 = TEdraftpos8[-c(1:9)]
TEdraftpos10 = TEdraftpos10[-c(1:9)]
TEdraftpos12 = TEdraftpos12[-c(1:9)]


masterte = join(masterte, TEdraftpos8, by = c(), type = "full", match = "all")
masterte = join(masterte, TEdraftpos10, by = c(), type = "full", match = "all")
masterte = join(masterte, TEdraftpos12, by = c(), type = "full", match = "all")

masterte[90, 28] = 68.19
masterqb[115, 45] = 15.81

##test correlation of meanpoints

qbcormean8 = cor.test(masterqb$Points, masterqb$meanpoints8, method = "pearson")
qbcormean8
qbcormean10 = cor.test(masterqb$Points, masterqb$meanpoints10, method = "pearson")
qbcormean10
qbcormean12 = cor.test(masterqb$Points, masterqb$meanpoints12, method = "pearson")
qbcormean12

rbcormean8 = cor.test(masterrb$Points, masterrb$meanpoints8, method = "pearson")
rbcormean8
rbcormean10 = cor.test(masterrb$Points, masterrb$meanpoints10, method = "pearson")
rbcormean10
rbcormean12 = cor.test(masterrb$Points, masterrb$meanpoints12, method = "pearson")
rbcormean12

wrcormean8 = cor.test(masterwr$Points, masterwr$meanpoints8, method = "pearson")
wrcormean8
wrcormean10 = cor.test(masterwr$Points, masterwr$meanpoints10, method = "pearson")
wrcormean10
wrcormean12 = cor.test(masterwr$Points, masterwr$meanpoints12, method = "pearson")
wrcormean12

tecormean8 = cor.test(masterte$Points, masterte$meanpoints8, method = "pearson")
tecormean8
tecormean10 = cor.test(masterte$Points, masterte$meanpoints10, method = "pearson")
tecormean10
tecormean12 = cor.test(masterte$Points, masterte$meanpoints12, method = "pearson")
tecormean12

masterqb$ProjGamesPlayed <- as.numeric(as.character(masterqb$ProjGamesPlayed))

###add meanpoints correlations into TSD equation

masterqb$pTD8 = masterqb$ProjPassTD * .3241
masterqb$GPlay8 = masterqb$ProjGamesPlayed * .1099
masterqb$OLine8 = masterqb$OffLineRank * -.1665
masterqb$projp8 = masterqb$ProjPoints * .3853
masterqb$WLOU8 = masterqb$PreW.L.O.U * .2540
masterqb$rTD8 = masterqb$ProjRushTD * .2114
masterqb$dftrd8 = masterqb$meanpoints8 * .2747
masterqb$adpx8 = masterqb$ADP * -.3928
masterqb$TSD8 = rowSums(masterqb[, c(39:46)], na.rm = TRUE)

###correlation test for each TSD aspect

qbcor8new = cor.test(masterqb$Points, masterqb$TSD8, method = "pearson")
qbcor8new
qbcorptd = cor.test(masterqb$Points, masterqb$ProjPassTD, method = "pearson")
qbcorptd
qbcorpgp = cor.test(masterqb$Points, masterqb$ProjGamesPlayed, method = "pearson")
qbcorpgp
qbcorolr = cor.test(masterqb$Points, masterqb$OffLineRank, method = "pearson")
qbcorolr
qbcorpp = cor.test(masterqb$Points, masterqb$ProjPoints, method = "pearson")
qbcorpp
qbcorwlou = cor.test(masterqb$Points, masterqb$PreW.L.O.U, method = "pearson")
qbcorwlou
qbcorrtd = cor.test(masterqb$Points, masterqb$ProjRushTD, method = "pearson")
qbcorrtd
qbcoradp = cor.test(masterqb$Points, masterqb$ADP, method = "pearson")
qbcoradp
qbcormean8 = cor.test(masterqb$Points, masterqb$meanpoints8, method = "pearson")
qbcormean8

###perform same operations for 8, 10, 12 team and all positions

masterqb$pTD10 = masterqb$ProjPassTD * .3111
masterqb$GPlay10 = masterqb$ProjGamesPlayed * .1055
masterqb$OLine10 = masterqb$OffLineRank * -.1598
masterqb$projp10 = masterqb$ProjPoints * .3699
masterqb$WLOU10 = masterqb$PreW.L.O.U * .2439
masterqb$rTD10 = masterqb$ProjRushTD * .2029
masterqb$dftrd10 = masterqb$meanpoints10 * .3038
masterqb$adpx10 = masterqb$ADP * -.3771
masterqb$TSD10 = rowSums(masterqb[, c(48:55)], na.rm = TRUE)

qbcor10new = cor.test(masterqb$Points, masterqb$TSD10, method = "pearson")
qbcor10new

masterqb$pTD12 = masterqb$ProjPassTD * .2985
masterqb$GPlay12 = masterqb$ProjGamesPlayed * .1012
masterqb$OLine12 = masterqb$OffLineRank * -.1534
masterqb$projp12 = masterqb$ProjPoints * .3549
masterqb$WLOU12 = masterqb$PreW.L.O.U * .234
masterqb$rTD12 = masterqb$ProjRushTD * .1947
masterqb$dftrd12 = masterqb$meanpoints12 * .3319
masterqb$adpx12 = masterqb$ADP * -.3618
masterqb$TSD12 = rowSums(masterqb[, c(57:64)], na.rm = TRUE)

qbcor12new = cor.test(masterqb$Points, masterqb$TSD12, method = "pearson")
qbcor12new

masterrb$ryd8 = masterrb$RushingYards * .4384
masterrb$recyd8 = masterrb$ReceivingYards * .3410
masterrb$fpt8 = masterrb$FantasyPoints * .4578
masterrb$dftrd8 = masterrb$meanpoints8 * .3965
masterrb$olr8 = masterrb$OffLineRank * -.0831
masterrb$bday8 = masterrb$Age * -.1241
masterrb$adpos8 = masterrb$ADP * -.4264
masterrb$TSD8 = rowSums(masterrb[, c(28:34)], na.rm = TRUE)

rbcor8new = cor.test(masterrb$Points, masterrb$TSD8, method = "pearson")
rbcor8new

masterrb$ryd10 = masterrb$RushingYards * .4243
masterrb$recyd10 = masterrb$ReceivingYards * .33
masterrb$fpt10 = masterrb$FantasyPoints * .4431
masterrb$dftrd10 = masterrb$meanpoints10 * .4159
masterrb$olr10 = masterrb$OffLineRank * -.0804
masterrb$bday10 = masterrb$Age * -.1202
masterrb$adpos10 = masterrb$ADP * -.4127
masterrb$TSD10 = rowSums(masterrb[, c(36:42)], na.rm = TRUE)

rbcor10new = cor.test(masterrb$Points, masterrb$TSD10, method = "pearson")
rbcor10new

masterrb$ryd12 = masterrb$RushingYards * .4110
masterrb$recyd12 = masterrb$ReceivingYards * .3197
masterrb$fpt12 = masterrb$FantasyPoints * .4293
masterrb$dftrd12 = masterrb$meanpoints12 * .4342
masterrb$olr12 = masterrb$OffLineRank * -.0779
masterrb$bday12 = masterrb$Age * -.1164
masterrb$adpos12 = masterrb$ADP * -.3998
masterrb$TSD12 = rowSums(masterrb[, c(44:50)], na.rm = TRUE)

rbcor12new = cor.test(masterrb$Points, masterrb$TSD12, method = "pearson")
rbcor12new

masterwr$ryd8 = masterwr$ReceivingYards * .4993
masterwr$rushyd8 = masterwr$RushingYards * .0411
masterwr$fpt8 = masterwr$FantasyPoints * .4981
masterwr$wlou8 = masterwr$PreW.L.O.U * .1205
masterwr$dftrd8 = masterwr$meanpoints8 * .3645
masterwr$bday8 = masterwr$Age * -.0199
masterwr$adpos8 = masterwr$ADP * -.5036
masterwr$TSD8 = rowSums(masterwr[, c(32:38)], na.rm = TRUE)

wrcor8new = cor.test(masterwr$Points, masterwr$TSD8, method = "pearson")
wrcor8new

masterwr$ryd10 = masterwr$ReceivingYards * .4887
masterwr$rushyd10 = masterwr$RushingYards * .0402
masterwr$fpt10 = masterwr$FantasyPoints * .4876
masterwr$wlou10 = masterwr$PreW.L.O.U * .1180
masterwr$dftrd10 = masterwr$meanpoints10 * .3779
masterwr$bday10 = masterwr$Age * -.0195
masterwr$adpos10 = masterwr$ADP * -.4929
masterwr$TSD10 = rowSums(masterwr[, c(40:46)], na.rm = TRUE)

wrcor10new = cor.test(masterwr$Points, masterwr$TSD10, method = "pearson")
wrcor10new

masterwr$ryd12 = masterwr$ReceivingYards * .4646
masterwr$rushyd12 = masterwr$RushingYards * .0382
masterwr$fpt12 = masterwr$FantasyPoints * .4635
masterwr$wlou12 = masterwr$PreW.L.O.U * .1122
masterwr$dftrd12 = masterwr$meanpoints12 * .4086
masterwr$bday12 = masterwr$Age * -.0186
masterwr$adpos12 = masterwr$ADP * -.4686
masterwr$TSD12 = rowSums(masterwr[, c(48:54)], na.rm = TRUE)

wrcor12newx = cor.test(masterwr$Points, masterwr$dftrd12, method = "pearson")
wrcor12newx
wrcor12new = cor.test(masterwr$Points, masterwr$TSD12, method = "pearson")
wrcor12new


masterte$ryd8 = masterte$ReceivingYards * .3082
masterte$fpt8 = masterte$FantasyPoints * .2966
masterte$wlou8 = masterte$PreW.L.O.U * .1579
masterte$olr8 = masterte$OffLineRank * -.2299
masterte$dftrd8 = masterte$meanpoints8 * .3952
masterte$adpos8 = masterte$ADP * -.3519
masterte$TSD8 = masterte$dftrd8

tecor8new = cor.test(masterte$Points, masterte$TSD8, method = "pearson")
tecor8new

masterte$TSD10 = masterte$meanpoints10

tecor10new = cor.test(masterte$Points, masterte$TSD10, method = "pearson")
tecor10new

masterte$TSD12 = masterte$meanpoints12

tecor12new = cor.test(masterte$Points, masterte$TSD12, method = "pearson")
tecor12new

#create projection key
##join masteradp and masterproj and change teams to numeric

masteradp2 = masteradp[-c(1,4:13)]
masterprojection = join(masterproj,masteradp2, by = c(), type = "full", match = "all")

masterprojection$TeamName = masterprojection$Team

masterprojection["Team"][masterprojection["Team"] == "NA"] = "0"
masterprojection["Team"][masterprojection["Team"] == "ARI"] = "1"
masterprojection["Team"][masterprojection["Team"] == "ATL"] = "2"
masterprojection["Team"][masterprojection["Team"] == "BAL"] = "3"
masterprojection["Team"][masterprojection["Team"] == "BUF"] = "4"
masterprojection["Team"][masterprojection["Team"] == "CAR"] = "5"
masterprojection["Team"][masterprojection["Team"] == "CHI"] = "6"
masterprojection["Team"][masterprojection["Team"] == "CIN"] = "7"
masterprojection["Team"][masterprojection["Team"] == "CLE"] = "8"
masterprojection["Team"][masterprojection["Team"] == "DAL"] = "9"
masterprojection["Team"][masterprojection["Team"] == "DEN"] = "10"
masterprojection["Team"][masterprojection["Team"] == "DET"] = "11"
masterprojection["Team"][masterprojection["Team"] == "GB"] = "12"
masterprojection["Team"][masterprojection["Team"] == "HOU"] = "13"
masterprojection["Team"][masterprojection["Team"] == "IND"] = "14"
masterprojection["Team"][masterprojection["Team"] == "JAX"] = "15"
masterprojection["Team"][masterprojection["Team"] == "KC"] = "16"
masterprojection["Team"][masterprojection["Team"] == "LAC"] = "17"
masterprojection["Team"][masterprojection["Team"] == "LAR"] = "18"
masterprojection["Team"][masterprojection["Team"] == "LV"] = "19"
masterprojection["Team"][masterprojection["Team"] == "MIA"] = "20"
masterprojection["Team"][masterprojection["Team"] == "MIN"] = "21"
masterprojection["Team"][masterprojection["Team"] == "NE"] = "22"
masterprojection["Team"][masterprojection["Team"] == "NO"] = "23"
masterprojection["Team"][masterprojection["Team"] == "NYG"] = "24"
masterprojection["Team"][masterprojection["Team"] == "NYJ"] = "25"
masterprojection["Team"][masterprojection["Team"] == "PHI"] = "26"
masterprojection["Team"][masterprojection["Team"] == "PIT"] = "27"
masterprojection["Team"][masterprojection["Team"] == "SEA"] = "28"
masterprojection["Team"][masterprojection["Team"] == "SF"] = "29"
masterprojection["Team"][masterprojection["Team"] == "TB"] = "30"
masterprojection["Team"][masterprojection["Team"] == "TEN"] = "31"
masterprojection["Team"][masterprojection["Team"] == "WAS"] = "32"
unique(masterprojection$Team)

###join in 22 team odds and rankings

masterprojection = join(masterprojection, `22Oddsmast`, by = c(), type = "full", match = "all")

###join in adp and draftrounds for league sizes

masterprojection$ADP = round(masterprojection$AverageDraftPosition)

masterprojection$draftround8 = ifelse(masterprojection$ADP<=8, 1,
                                      ifelse(masterprojection$ADP >=9 & masterprojection$ADP <=16, 2,
                                             ifelse(masterprojection$ADP >=17 & masterprojection$ADP <=24, 3,
                                                    ifelse(masterprojection$ADP >=25 & masterprojection$ADP <=32, 4,
                                                           ifelse(masterprojection$ADP >=33 & masterprojection$ADP <=40, 5,
                                                                  ifelse(masterprojection$ADP >=41 & masterprojection$ADP <=48, 6,
                                                                         ifelse(masterprojection$ADP >=49 & masterprojection$ADP <=56, 7,
                                                                                ifelse(masterprojection$ADP >=57 & masterprojection$ADP <=64, 8,
                                                                                       ifelse(masterprojection$ADP >=65 & masterprojection$ADP <=72, 9,
                                                                                              ifelse(masterprojection$ADP >=73 & masterprojection$ADP <=80, 10,
                                                                                                     ifelse(masterprojection$ADP >=81 & masterprojection$ADP <=88, 11,
                                                                                                            ifelse(masterprojection$ADP >=89 & masterprojection$ADP <=96, 12,
                                                                                                                   ifelse(masterprojection$ADP >=97 & masterprojection$ADP <=104, 13,
                                                                                                                          ifelse(masterprojection$ADP >=105 & masterprojection$ADP <=112, 14,
                                                                                                                                 ifelse(masterprojection$ADP >=113, 15,NA)))))))))))))))

masterprojection$draftround10 = ifelse(masterprojection$ADP<=10, 1,
                                       ifelse(masterprojection$ADP >=11 & masterprojection$ADP <=20, 2,
                                              ifelse(masterprojection$ADP >=21 & masterprojection$ADP <=30, 3,
                                                     ifelse(masterprojection$ADP >=31 & masterprojection$ADP <=40, 4,
                                                            ifelse(masterprojection$ADP >=41 & masterprojection$ADP <=50, 5,
                                                                   ifelse(masterprojection$ADP >=51 & masterprojection$ADP <=60, 6,
                                                                          ifelse(masterprojection$ADP >=61 & masterprojection$ADP <=70, 7,
                                                                                 ifelse(masterprojection$ADP >=71 & masterprojection$ADP <=80, 8,
                                                                                        ifelse(masterprojection$ADP >=81 & masterprojection$ADP <=90, 9,
                                                                                               ifelse(masterprojection$ADP >=91 & masterprojection$ADP <=100, 10,
                                                                                                      ifelse(masterprojection$ADP >=101 & masterprojection$ADP <=110, 11,
                                                                                                             ifelse(masterprojection$ADP >=111 & masterprojection$ADP <=120, 12,
                                                                                                                    ifelse(masterprojection$ADP >=121 & masterprojection$ADP <=130, 13,
                                                                                                                           ifelse(masterprojection$ADP >=131 & masterprojection$ADP <=140, 14,
                                                                                                                                  ifelse(masterprojection$ADP >=141, 15,NA)))))))))))))))

masterprojection$draftround12 = ifelse(masterprojection$ADP<=12, 1,
                                       ifelse(masterprojection$ADP >=13 & masterprojection$ADP <=24, 2,
                                              ifelse(masterprojection$ADP >=25 & masterprojection$ADP <=36, 3,
                                                     ifelse(masterprojection$ADP >=37 & masterprojection$ADP <=48, 4,
                                                            ifelse(masterprojection$ADP >=49 & masterprojection$ADP <=60, 5,
                                                                   ifelse(masterprojection$ADP >=61 & masterprojection$ADP <=72, 6,
                                                                          ifelse(masterprojection$ADP >=73 & masterprojection$ADP <=84, 7,
                                                                                 ifelse(masterprojection$ADP >=85 & masterprojection$ADP <=96, 8,
                                                                                        ifelse(masterprojection$ADP >=97 & masterprojection$ADP <=108, 9,
                                                                                               ifelse(masterprojection$ADP >=109 & masterprojection$ADP <=120, 10,
                                                                                                      ifelse(masterprojection$ADP >=121 & masterprojection$ADP <=132, 11,
                                                                                                             ifelse(masterprojection$ADP >=133 & masterprojection$ADP <=144, 12,
                                                                                                                    ifelse(masterprojection$ADP >=145 & masterprojection$ADP <=156, 13,
                                                                                                                           ifelse(masterprojection$ADP >=157 & masterprojection$ADP <=168, 14,
                                                                                                                                  ifelse(masterprojection$ADP >=169, 15,NA)))))))))))))))

###clean up masterprojection             

masterprojection = subset(masterprojection, ADP <=250)
masterprojection = subset(masterprojection, FantasyPoints > 0)

masterprojection = masterprojection[c(1:6,10,13:14,17,20:28)]

###add ages for 22 season start date

masterbday = NFLBirthdays
Bday = c(masterbday$Bday)
masterbday$BDay = as.Date(Bday, "%m/%d/%Y")
masterbday$start22 = "2022-09-09" 
masterbday$age22 = as.numeric(difftime(masterbday$start22,masterbday$BDay, units = "weeks"))/52.25

masterbday = masterbday[c(1,2,6)]

masterprojection = join(masterprojection, masterbday, by = c(), type = "full", match = "all")

###clean up dataset again

masterprojection = subset(masterprojection, ADP <=250)
masterprojection = subset(masterprojection, FantasyPoints > 0)

###add in ages for players that are NA

masterprojection[2,20] = 26.94258
masterprojection[98,20] = 21.85714
masterprojection[145,20] = 29.29665
masterprojection[226,20] = 29.3486
masterprojection[234,20] = 26.6
masterprojection[225,20] = 26.5

##join together all positions

dftrd8join = join(masterdraftpos8qb, masterdraftpos8rb, by = c(), type = "full", match = "all")
dftrd8join1 = join(dftrd8join, masterdraftpos8wr, by = c(), type = "full", match = "all")
dftrd8join2 = join(dftrd8join1, masterdraftpos8te, by = c(), type = "full", match = "all")

masterprojection1 = join(masterprojection, dftrd8join2, by = c(), type = "full", match = "all")

masterprojection1 = join(masterprojection1, masterdraftpos12, by = c(), type = "full", match = "all")

masterprojection = masterprojection1

###create TSD equation for each position and league size

masterprojection1$TSD8 = ifelse(masterprojection$Position == "QB", 
                                ((masterprojection$PassingTouchdowns *.3241)+
                                   (masterprojection$Played *.1099)+(masterprojection$OffLineRank*-.1665)+
                                   (masterprojection$FantasyPoints*.3853)+(masterprojection$PreW.L.O.U*.2540)+
                                   (masterprojection$RushingTouchdowns*.3262)+(masterprojection$dftrd8*.2747)+
                                   (masterprojection$ADP*-.3928)),
                                ifelse(masterprojection$Position == "RB", 
                                       ((masterprojection$RushingYards *.4384)+
                                          (masterprojection$ReceivingYards *.3410)+(masterprojection$FantasyPoints*.4578)+
                                          (masterprojection$dftrd8*.3965)+(masterprojection$OffLineRank*-.0831)+
                                          (masterprojection$age22*-.1241)+(masterprojection$ADP*-.4264)),
                                       ifelse(masterprojection$Position == "WR", 
                                              ((masterprojection$ReceivingYards *.4993)+
                                                 (masterprojection$RushingYards *.0411)+(masterprojection$FantasyPoints*.4981)+
                                                 (masterprojection$PreW.L.O.U*.1205)+(masterprojection$dftrd8*.3645)+
                                                 (masterprojection$age22*-.0199)+(masterprojection$ADP*-.5036)),
                                              ifelse(masterprojection$Position == "TE", masterprojection$dftrd8,
                                                     NA
                                              ))))

masterprojection1$TSD10 = ifelse(masterprojection1$Position == "QB", 
                                 ((masterprojection1$PassingTouchdowns *.3111)+
                                    (masterprojection1$Played *.1055)+(masterprojection1$OffLineRank*-.1598)+
                                    (masterprojection1$FantasyPoints*.3699)+(masterprojection1$PreW.L.O.U*.2439)+
                                    (masterprojection1$RushingTouchdowns*.2029)+(masterprojection1$dftrd10*.3038)+
                                    (masterprojection1$ADP*-.3771)),
                                 ifelse(masterprojection1$Position == "RB", 
                                        ((masterprojection1$RushingYards *.4243)+
                                           (masterprojection1$ReceivingYards *.33)+(masterprojection1$FantasyPoints*.4431)+
                                           (masterprojection1$dftrd10*.4159)+(masterprojection1$OffLineRank*-.0804)+
                                           (masterprojection1$age22*-.1202)+(masterprojection1$ADP*-.4127)),
                                        ifelse(masterprojection1$Position == "WR", 
                                               ((masterprojection1$ReceivingYards *.4887)+
                                                  (masterprojection1$RushingYards *.0402)+(masterprojection1$FantasyPoints*.4876)+
                                                  (masterprojection1$PreW.L.O.U*.118)+(masterprojection1$dftrd10*.3779)+
                                                  (masterprojection1$age22*-.0195)+(masterprojection1$ADP*-.4929)),
                                               ifelse(masterprojection1$Position == "TE", masterprojection1$dftrd10,
                                                      NA
                                               ))))

masterprojection1$TSD12 = ifelse(masterprojection1$Position == "QB", 
                                 ((masterprojection1$PassingTouchdowns *.2985)+
                                    (masterprojection1$Played *.1012)+(masterprojection1$OffLineRank*-.1534)+
                                    (masterprojection1$FantasyPoints*.3549)+(masterprojection1$PreW.L.O.U*.2340)+
                                    (masterprojection1$RushingTouchdowns*.1947)+(masterprojection1$dftrd12*.3319)+
                                    (masterprojection1$ADP*-.3618)),
                                 ifelse(masterprojection1$Position == "RB", 
                                        ((masterprojection1$RushingYards *.4110)+
                                           (masterprojection1$ReceivingYards *.3197)+(masterprojection1$FantasyPoints*.4293)+
                                           (masterprojection1$dftrd12*.4342)+(masterprojection1$OffLineRank*-.0779)+
                                           (masterprojection1$age22*-.1164)+(masterprojection1$ADP*-.3998)),
                                        ifelse(masterprojection1$Position == "WR", 
                                               ((masterprojection1$ReceivingYards *.4646)+
                                                  (masterprojection1$RushingYards *.0382)+(masterprojection1$FantasyPoints*.4635)+
                                                  (masterprojection1$PreW.L.O.U*.1122)+(masterprojection1$dftrd12*.4086)+
                                                  (masterprojection1$age22*-.0186)+(masterprojection1$ADP*-.4686)),
                                               ifelse(masterprojection1$Position == "TE", masterprojection1$dftrd12,
                                                      NA
                                               ))))

###clean up dataset

masterprojection1 = subset(masterprojection1, ADP <=250)
masterprojection1 = subset(masterprojection1, FantasyPoints > 0)

###see summary of each position for each league size

mpj = subset(masterprojection1, Position=="QB")
summary(mpj$TSD8)
summary(mpj$TSD10)
summary(mpj$TSD12)

mpr = subset(masterprojection1, Position=="RB")
summary(mpr$TSD8)
summary(mpr$TSD10)
summary(mpr$TSD12)

mpw = subset(masterprojection1, Position=="WR")
summary(mpw$TSD8)
summary(mpw$TSD10)
summary(mpw$TSD12)

mpt = subset(masterprojection1, Position=="TE")
summary(mpt$TSD8)
summary(mpt$TSD10)
summary(mpt$TSD12)

###make all positions means the same

masterprojection1$TSDx8 = ifelse(masterprojection1$Position == "QB", ((masterprojection1$TSD8*100)/119),
                                 ifelse(masterprojection1$Position == "RB", ((masterprojection1$TSD8*100)/137.5),
                                        ifelse(masterprojection1$Position == "WR", ((masterprojection1$TSD8*100)/131),
                                               ifelse(masterprojection1$Position == "TE", ((masterprojection1$TSD8*100)/112.5),
                                                      NA
                                               ))))

masterprojection1$TSDx10 = ifelse(masterprojection1$Position == "QB", ((masterprojection1$TSD10*100)/119),
                                  ifelse(masterprojection1$Position == "RB", ((masterprojection1$TSD10*100)/137.5),
                                         ifelse(masterprojection1$Position == "WR", ((masterprojection1$TSD10*100)/131),
                                                ifelse(masterprojection1$Position == "TE", ((masterprojection1$TSD10*100)/112.5),
                                                       NA
                                                ))))

masterprojection1$TSDx12 = ifelse(masterprojection1$Position == "QB", ((masterprojection1$TSD12*100)/119),
                                  ifelse(masterprojection1$Position == "RB", ((masterprojection1$TSD12*100)/137.5),
                                         ifelse(masterprojection1$Position == "WR", ((masterprojection1$TSD12*100)/131),
                                                ifelse(masterprojection1$Position == "TE", ((masterprojection1$TSD12*100)/112.5),
                                                       NA
                                                ))))


masterprojection1$TSDxx8 = ifelse(masterprojection1$Position == "QB", masterprojection1$TSDx8*.785063993,
                                  ifelse(masterprojection1$Position == "RB", masterprojection1$TSDx8*.195757403,
                                         ifelse(masterprojection1$Position == "WR", masterprojection1$TSDx8*.189922481,
                                                ifelse(masterprojection1$Position == "TE", masterprojection1$TSDx8*1.61124001,
                                                       NA
                                                ))))

masterprojection1$TSDxx10 = ifelse(masterprojection1$Position == "QB", masterprojection1$TSDx10*.92348285,
                                   ifelse(masterprojection1$Position == "RB", masterprojection1$TSDx10*.200841636,
                                          ifelse(masterprojection1$Position == "WR", masterprojection1$TSDx10*.193707894,
                                                 ifelse(masterprojection1$Position == "TE", masterprojection1$TSDx10*1.85603748,
                                                        NA
                                                 ))))

masterprojection1$TSDxx12 = ifelse(masterprojection1$Position == "QB", masterprojection1$TSDx12*.906321401,
                                   ifelse(masterprojection1$Position == "RB", masterprojection1$TSDx12*.206255156,
                                          ifelse(masterprojection1$Position == "WR", masterprojection1$TSDx12*.201245881,
                                                 ifelse(masterprojection1$Position == "TE", masterprojection1$TSDx12*2.29986099,
                                                        NA
                                                 ))))

summary(mpj$TSDxx8)
summary(mpj$TSDxx10)
summary(mpj$TSDxx12)

summary(mpr$TSDxx8)
summary(mpr$TSDxx10)
summary(mpr$TSDxx12)

summary(mpw$TSDxx8)
summary(mpw$TSDxx10)
summary(mpw$TSDxx12)

summary(mpt$TSDxx8)
summary(mpt$TSDxx10)
summary(mpt$TSDxx12)

##create cleaned up version of masterprojection1

masterpiece = masterprojection1[c(1,3,5,6,11,12,13,17:19,30:32)]

byeweek$TeamName = byeweek$Team
byeweek = byeweek[-c(1)]

masterpiece = join(masterpiece, byeweek, by = c(), type = "full", match = "all")

write.csv(masterpiece, "C://Users/19403//Dropbox//My PC (LAPTOP-3FN89LB8)//Desktop//Final Project//masterpiece3.csv", row.names = FALSE)

###combine masters into one and project draft round for league size

masterqbmean = summary(masterqb$Points)
masterqbmean

masterrbmean = summary(masterrb$Points)
masterrbmean

masterwrmean = summary(masterwr$Points)
masterwrmean

mastertemean = summary(masterte$Points)
mastertemean


oamaster = join(masterqb, masterrb, by = c(), type = "full", match = "all")
oamaster = join(oamaster, masterte, by = c(), type = "full", match = "all")
oamaster = join(oamaster,masterwr, by = c(), type = "full", match = "all")

oomaster = oamaster[c(1:4,8:10,12,14,24,26,27,29:30,33:35,44,54,63,77,81,83)]

oomaster$draftround8 = ifelse(oomaster$ADP<=8, 1,
                              ifelse(oomaster$ADP >=9 & oomaster$ADP <=16, 2,
                                     ifelse(oomaster$ADP >=17 & oomaster$ADP <=24, 3,
                                            ifelse(oomaster$ADP >=25 & oomaster$ADP <=32, 4,
                                                   ifelse(oomaster$ADP >=33 & oomaster$ADP <=40, 5,
                                                          ifelse(oomaster$ADP >=41 & oomaster$ADP <=48, 6,
                                                                 ifelse(oomaster$ADP >=49 & oomaster$ADP <=56, 7,
                                                                        ifelse(oomaster$ADP >=57 & oomaster$ADP <=64, 8,
                                                                               ifelse(oomaster$ADP >=65 & oomaster$ADP <=72, 9,
                                                                                      ifelse(oomaster$ADP >=73 & oomaster$ADP <=80, 10,
                                                                                             ifelse(oomaster$ADP >=81 & oomaster$ADP <=88, 11,
                                                                                                    ifelse(oomaster$ADP >=89 & oomaster$ADP <=96, 12,
                                                                                                           ifelse(oomaster$ADP >=97 & oomaster$ADP <=104, 13,
                                                                                                                  ifelse(oomaster$ADP >=105 & oomaster$ADP <=112, 14,
                                                                                                                         ifelse(oomaster$ADP >=113 & oomaster$ADP <=120, 15,NA)))))))))))))))

oomaster$draftround10 = ifelse(oomaster$ADP<=10, 1,
                               ifelse(oomaster$ADP >=11 & oomaster$ADP <=20, 2,
                                      ifelse(oomaster$ADP >=21 & oomaster$ADP <=30, 3,
                                             ifelse(oomaster$ADP >=31 & oomaster$ADP <=40, 4,
                                                    ifelse(oomaster$ADP >=41 & oomaster$ADP <=50, 5,
                                                           ifelse(oomaster$ADP >=51 & oomaster$ADP <=60, 6,
                                                                  ifelse(oomaster$ADP >=61 & oomaster$ADP <=70, 7,
                                                                         ifelse(oomaster$ADP >=71 & oomaster$ADP <=80, 8,
                                                                                ifelse(oomaster$ADP >=81 & oomaster$ADP <=90, 9,
                                                                                       ifelse(oomaster$ADP >=91 & oomaster$ADP <=100, 10,
                                                                                              ifelse(oomaster$ADP >=101 & oomaster$ADP <=110, 11,
                                                                                                     ifelse(oomaster$ADP >=111 & oomaster$ADP <=120, 12,
                                                                                                            ifelse(oomaster$ADP >=121 & oomaster$ADP <=130, 13,
                                                                                                                   ifelse(oomaster$ADP >=131 & oomaster$ADP <=140, 14,
                                                                                                                          ifelse(oomaster$ADP >=141 & oomaster$ADP <=150, 15,NA)))))))))))))))

oomaster$draftround12 = ifelse(oomaster$ADP<=12, 1,
                               ifelse(oomaster$ADP >=13 & oomaster$ADP <=24, 2,
                                      ifelse(oomaster$ADP >=25 & oomaster$ADP <=36, 3,
                                             ifelse(oomaster$ADP >=37 & oomaster$ADP <=48, 4,
                                                    ifelse(oomaster$ADP >=49 & oomaster$ADP <=60, 5,
                                                           ifelse(oomaster$ADP >=61 & oomaster$ADP <=72, 6,
                                                                  ifelse(oomaster$ADP >=73 & oomaster$ADP <=84, 7,
                                                                         ifelse(oomaster$ADP >=85 & oomaster$ADP <=96, 8,
                                                                                ifelse(oomaster$ADP >=97 & oomaster$ADP <=108, 9,
                                                                                       ifelse(oomaster$ADP >=109 & oomaster$ADP <=120, 10,
                                                                                              ifelse(oomaster$ADP >=121 & oomaster$ADP <=132, 11,
                                                                                                     ifelse(oomaster$ADP >=133 & oomaster$ADP <=144, 12,
                                                                                                            ifelse(oomaster$ADP >=145 & oomaster$ADP <=156, 13,
                                                                                                                   ifelse(oomaster$ADP >=157 & oomaster$ADP <=168, 14,
                                                                                                                          ifelse(oomaster$ADP >=169 & oomaster$ADP <=180, 15,NA)))))))))))))))

###join masterdraftpos into main df

oomaster = join(oomaster, masterdraftpos8, by = c(), type = "full", match = "all")
oomaster = join(oomaster, masterdraftpos10, by = c(), type = "full", match = "all")
oomaster = join(oomaster, masterdraftpos12, by = c(), type = "full", match = "all")

###clean up df

oomaster = subset(oomaster, ADP <=250)
oomaster = subset(oomaster, Points > 0)

oomaster = subset(oomaster, draftround12 > 0)

###create TSD values

oomaster$TSD8 = ifelse(oomaster$Position == "1", 
                       ((oomaster$ProjPassTD *.3241)+
                          (oomaster$ProjGamesPlayed *.1099)+(oomaster$OffLineRank*-.1665)+
                          (oomaster$ProjPoints*.3853)+(oomaster$PreW.L.O.U*.2540)+
                          (oomaster$ProjRushTD*.2114)+(oomaster$dftrd8*.2747)+
                          (oomaster$ADP*-.3928)),
                       ifelse(oomaster$Position == "RB", 
                              ((oomaster$RushingYards *.4384)+
                                 (oomaster$ReceivingYards *.3410)+(oomaster$FantasyPoints*.4578)+
                                 (oomaster$dftrd8*.3965)+(oomaster$OffLineRank*-.0831)+
                                 (oomaster$Age*-.1241)+(oomaster$ADP*-.4264)),
                              ifelse(oomaster$Position == "WR", 
                                     ((oomaster$ReceivingYards *.4993)+
                                        (oomaster$RushingYards *.0411)+(oomaster$FantasyPoints*.4981)+
                                        (oomaster$PreW.L.O.U*.1205)+(oomaster$dftrd8*.3645)+
                                        (oomaster$Age*-.0199)+(oomaster$ADP*-.5036)),
                                     ifelse(oomaster$Position == "TE", oomaster$dftrd8,
                                            NA
                                     ))))

oomaster$TSD10 = ifelse(oomaster$Position == "1", 
                        ((oomaster$ProjPassTD *.3111)+
                           (oomaster$ProjGamesPlayed *.1055)+(oomaster$OffLineRank*-.1598)+
                           (oomaster$ProjPoints*.3699)+(oomaster$PreW.L.O.U*.2439)+
                           (oomaster$ProjRushTD*.2029)+(oomaster$dftrd10*.3038)+
                           (oomaster$ADP*-.3771)),
                        ifelse(oomaster$Position == "RB", 
                               ((oomaster$RushingYards *.4243)+
                                  (oomaster$ReceivingYards *.33)+(oomaster$FantasyPoints*.4431)+
                                  (oomaster$dftrd10*.4159)+(oomaster$OffLineRank*-.0804)+
                                  (oomaster$Age*-.1202)+(oomaster$ADP*-.4127)),
                               ifelse(oomaster$Position == "WR", 
                                      ((oomaster$ReceivingYards *.4887)+
                                         (oomaster$RushingYards *.0402)+(oomaster$FantasyPoints*.4876)+
                                         (oomaster$PreW.L.O.U*.118)+(oomaster$dftrd10*.3779)+
                                         (oomaster$Age*-.0195)+(oomaster$ADP*-.4929)),
                                      ifelse(oomaster$Position == "TE", oomaster$dftrd10,
                                             NA
                                      ))))

oomaster$TSD12 = ifelse(oomaster$Position == "1", 
                        ((oomaster$ProjPassTD *.2985)+
                           (oomaster$ProjGamesPlayed *.1012)+(oomaster$OffLineRank*-.1534)+
                           (oomaster$ProjPoints*.3549)+(oomaster$PreW.L.O.U*.2340)+
                           (oomaster$ProjRushTD*.1947)+(oomaster$dftrd12*.3319)+
                           (oomaster$ADP*-.3618)),
                        ifelse(oomaster$Position == "RB", 
                               ((oomaster$RushingYards *.4110)+
                                  (oomaster$ReceivingYards *.3197)+(oomaster$FantasyPoints*-.4293)+
                                  (oomaster$dftrd12*.4342)+(oomaster$OffLineRank*.0779)+
                                  (oomaster$Age*-.1164)+(oomaster$ADP*-.3998)),
                               ifelse(oomaster$Position == "WR", 
                                      ((oomaster$ReceivingYards *.4646)+
                                         (oomaster$RushingYards *.0382)+(oomaster$FantasyPoints*.4635)+
                                         (oomaster$PreW.L.O.U*.1122)+(oomaster$dftrd12*.4086)+
                                         (oomaster$Age*-.0186)+(oomaster$ADP*-.4686)),
                                      ifelse(oomaster$Position == "TE", oomaster$dftrd12,
                                             NA
                                      ))))

###balance all positions TSD values

oomaster$TSDx8 = ifelse(oomaster$Position == "1", ((oomaster$TSD8*100)/119),
                        ifelse(oomaster$Position == "RB", ((oomaster$TSD8*100)/137.5),
                               ifelse(oomaster$Position == "WR", ((oomaster$TSD8*100)/131),
                                      ifelse(oomaster$Position == "TE", ((oomaster$TSD8*100)/112.5),
                                             NA
                                      ))))

oomaster$TSDx10 = ifelse(oomaster$Position == "1", ((oomaster$TSD10*100)/119),
                         ifelse(oomaster$Position == "RB", ((oomaster$TSD10*100)/137.5),
                                ifelse(oomaster$Position == "WR", ((oomaster$TSD10*100)/131),
                                       ifelse(oomaster$Position == "TE", ((oomaster$TSD10*100)/112.5),
                                              NA
                                       ))))

oomaster$TSDx12 = ifelse(oomaster$Position == "1", ((oomaster$TSD12*100)/119),
                         ifelse(oomaster$Position == "RB", ((oomaster$TSD12*100)/137.5),
                                ifelse(oomaster$Position == "WR", ((oomaster$TSD12*100)/131),
                                       ifelse(oomaster$Position == "TE", ((oomaster$TSD12*100)/112.5),
                                              NA
                                       ))))


oomaster$TSDxx8 = ifelse(oomaster$Position == "1", oomaster$TSDx8*.785063993,
                         ifelse(oomaster$Position == "RB", oomaster$TSDx8*.195757403,
                                ifelse(oomaster$Position == "WR", oomaster$TSDx8*.189922481,
                                       ifelse(oomaster$Position == "TE", oomaster$TSDx8*1.61124001,
                                              NA
                                       ))))

oomaster$TSDxx10 = ifelse(oomaster$Position == "1", oomaster$TSDx10*.92348285,
                          ifelse(oomaster$Position == "RB", oomaster$TSDx10*.200841636,
                                 ifelse(oomaster$Position == "WR", oomaster$TSDx10*.193707894,
                                        ifelse(oomaster$Position == "TE", oomaster$TSDx10*1.85603748,
                                               NA
                                        ))))

oomaster$TSDxx12 = ifelse(oomaster$Position == "1", oomaster$TSDx12*.906321401,
                          ifelse(oomaster$Position == "RB", oomaster$TSDx12*.206255156,
                                 ifelse(oomaster$Position == "WR", oomaster$TSDx12*.201245881,
                                        ifelse(oomaster$Position == "TE", oomaster$TSDx12*2.29986099,
                                               NA
                                        ))))

###see correlation of TSD

ggplot(oomaster, aes(x = Points, y = TSDxx8)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)

oobaby8 = cor.test(oomaster$Points, oomaster$TSDxx8, method = "pearson")
oobaby8
####.3214
oobaby10 = cor.test(oomaster$Points, oomaster$TSDxx10, method = "pearson")
oobaby10
####.3473
oobaby12 = cor.test(oomaster$Points, oomaster$TSDxx12, method = "pearson")
oobaby12
####.3933

master = oomaster

###nonlinearmodel

quadModel8 <- lm(master$Points~master$TSDxx8)
summary(quadModel8)
quadModel10 <- lm(master$Points~master$TSDxx10)
summary(quadModel10)
quadModel12 <- lm(master$Points~master$TSDxx12)
summary(quadModel12)

mod8 = lm(log(master$Points)~master$TSDxx8)
summary(mod8)
####pvalue <.05, fstat 60.75 on 1, for every additional TSD an increase of.74% points

mod10 = lm(log(master$Points)~master$TSDxx10)
summary(mod10)
####pvalue <.05, fstat 67.88 on 1, for every additional TSD an increase of 1.04% points

mod12 = lm(log(master$Points)~master$TSDxx12)
summary(mod12)
####pvalue <.05, fstat 125.5 on 1, for every additional TSD an increase of 1.36% points

##machine learning for normal drafting

mastersub = master[c(1:3, 23)]
mastersub2 = master[c(1:3, 5)]
mastersub$ProjPoints = mastersub$FantasyPoints
mastersub = mastersub[-c(4)]
mastersub3 = join(mastersub, mastersub2, by = c(), type = "full", match = "all")
mastersub3 = subset(mastersub3, ProjPoints > 0)
master = join(master, mastersub3, by = c(), type = "full", match = "all")
master = subset(master, ProjPoints > 0)
master = master[-c(23)]

###8 team league draftround, projpoints, adp, and points

###create list of 60% of rows from original dataset
master8 <-subset(master, (!is.na(master$Points)) & (!is.na(master$TSDxx8)))
mastermodel8 = subset(master, (!is.na(master$Points)) & (!is.na(master$TSDxx8)))
mastermodel8 = mastermodel8[c(1:3,16)]

master8 = master8[c("draftround8", "ProjPoints", "ADP", "Points")]
master8 = subset(master8, Points > 0)
master8 = master8[ , apply(master8, 2, function(x) !any(is.na(x)))]

split = 0.60
validation8_index = createDataPartition(master8$Points, p= split, list=FALSE)
###select 40% for validation
validation8 = master8[-validation8_index,]
###use remaining60% to train and test models
master8a = master8[validation8_index,]

###dimension of dataset
dim(master8a)
####316 32

sapply(master8a, class)
head(master8a)

summary(master8a)


### Run algorithms using 10-fold cross validation
control8 <- trainControl(method="cv", number=10)
metric8 <- "Accuracy"

### b) nonlinear algorithms
### CART
set.seed(7)
fit.cart <- train(Points~., data=master8a, method="rpart")
### kNN
set.seed(7)
fit.knn <- train(Points~., data=master8a, method="knn")
### c) advanced algorithms
### SVM
set.seed(7)
fit.svm <- train(Points~., data=master8a, method="svmRadial")
### Random Forest
set.seed(7)
fit.rf <- train(Points~., data=master8a, method="rf")

results8 = resamples(list(cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results8)

dotplot(results8)

###summarize best model
print(fit.knn)

####model has 91.575 RMSE

predictions = predict(fit.knn, validation8)
predictions2 = predict(fit.knn, master8a)
predictions
predictions2

master8a$predictionsTSD = predictions2
validation8$predictionsTSD = predictions

masterplex8 = join(validation8, master8a, by = c(), type = "full", match = "all")
masterplex8 = join(oomaster, masterplex8, by = c(), type = "full", match = "all")
masterplex8 = subset(masterplex8, predictionsTSD > 0)

oobaby8a = cor.test(masterplex8$Points, masterplex8$predictionsTSD, method = "pearson")
oobaby8a

####correlation .2827

ggplot(masterplex8, aes(x = Points, y = predictionsTSD)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)

###10 team league draftround, projpoints, adp, and points

###create list of 60% of rows from original dataset
master10 <-subset(master, (!is.na(master$Points)) & (!is.na(master$TSDxx10)))
mastermodel10 = subset(master, (!is.na(master$Points)) & (!is.na(master$TSDxx10)))
mastermodel10 = mastermodel10[c(1:3,16)]

master10 = master10[c("draftround10", "ProjPoints", "ADP", "Points")]
master10 = subset(master10, Points > 0)
master10 = master10[ , apply(master10, 2, function(x) !any(is.na(x)))]

split = 0.60
validation10_index = createDataPartition(master10$Points, p= split, list=FALSE)
###select 40% for validation
validation10 = master10[-validation10_index,]
###use remaining60% to train and test models
master10a = master10[validation10_index,]

###dimension of dataset
dim(master10a)
####316 32

sapply(master10a, class)
head(master10a)

summary(master10a)


### Run algorithms using 10-fold cross validation
control10 <- trainControl(method="cv", number=10)
metric10 <- "Accuracy"

### b) nonlinear algorithms
### CART
set.seed(7)
fit.cart10 <- train(Points~., data=master10a, method="rpart")
### kNN
set.seed(7)
fit.knn10 <- train(Points~., data=master10a, method="knn")
### c) advanced algorithms
### SVM
set.seed(7)
fit.svm10 <- train(Points~., data=master10a, method="svmRadial")
### Random Forest
set.seed(7)
fit.rf10 <- train(Points~., data=master10a, method="rf")

results10 = resamples(list(cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results10)

dotplot(results10)

###summarize best model
print(fit.knn10)

####model has 90.84 RMSE

predictions10 = predict(fit.knn10, validation10)
predictions102 = predict(fit.knn10, master10a)
predictions10
predictions102

master10a$predictionsTSD10 = predictions102
validation10$predictionsTSD10 = predictions10

masterplex10 = join(validation10, master10a, by = c(), type = "full", match = "all")
masterplex10 = join(oomaster, masterplex10, by = c(), type = "full", match = "all")
masterplex10 = subset(masterplex10, predictionsTSD10 > 0)

oobaby10a = cor.test(masterplex10$Points, masterplex10$predictionsTSD10, method = "pearson")
oobaby10a

####correlation = .2882

ggplot(masterplex10, aes(x = Points, y = predictionsTSD10)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)

###12 team league draftround, projpoints, adp, and points

###create list of 60% of rows from original dataset
master12 <-subset(master, (!is.na(master$Points)) & (!is.na(master$TSDxx12)))
mastermodel12 = subset(master, (!is.na(master$Points)) & (!is.na(master$TSDxx12)))
mastermodel12 = mastermodel12[c(1:3,16)]

master12 = master12[c("draftround12", "ProjPoints", "ADP", "Points")]
master12 = subset(master12, Points > 0)
master12 = master12[ , apply(master12, 2, function(x) !any(is.na(x)))]

split = 0.60
validation12_index = createDataPartition(master12$Points, p= split, list=FALSE)
###select 40% for validation
validation12 = master12[-validation12_index,]
###use remaining60% to train and test models
master12a = master12[validation12_index,]

###dimension of dataset
dim(master12a)
####316 32

sapply(master12a, class)
head(master12a)

summary(master12a)


### Run algorithms using 10-fold cross validation
control12 <- trainControl(method="cv", number=10)
metric12 <- "Accuracy"

### b) nonlinear algorithms
### CART
set.seed(7)
fit.cart12 <- train(Points~., data=master12a, method="rpart")
### kNN
set.seed(7)
fit.knn12 <- train(Points~., data=master12a, method="knn")
### c) advanced algorithms
### SVM
set.seed(7)
fit.svm12 <- train(Points~., data=master12a, method="svmRadial")
### Random Forest
set.seed(7)
fit.rf12 <- train(Points~., data=master12a, method="rf")

results12 = resamples(list(cart = fit.cart12, knn = fit.knn12, svm = fit.svm12, rf = fit.rf12))
summary(results12)

dotplot(results12)

###summarize best model
print(fit.knn12)

####model has 87.55 RMSE

predictions12 = predict(fit.knn12, validation12)
predictions122 = predict(fit.knn12, master12a)
predictions12
predictions122

master12a$predictionsTSD12 = predictions122
validation12$predictionsTSD12 = predictions12

masterplex12 = join(validation12, master12a, by = c(), type = "full", match = "all")
masterplex12 = join(oomaster, masterplex12, by = c(), type = "full", match = "all")
masterplex12 = subset(masterplex12, predictionsTSD12 > 0)

oobaby12a = cor.test(masterplex12$Points, masterplex12$predictionsTSD12, method = "pearson")
oobaby12a

####correlation = .3376

ggplot(masterplex12, aes(x = Points, y = predictionsTSD12)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)

###run models against projection set

write.csv(master, "C://Users/19403//Dropbox//My PC (LAPTOP-3FN89LB8)//Desktop//Final Project//master.csv", row.names = FALSE)
write.csv(masterqb, "C://Users/19403//Dropbox//My PC (LAPTOP-3FN89LB8)//Desktop//Final Project//masterqb.csv", row.names = FALSE)
write.csv(masterwr, "C://Users/19403//Dropbox//My PC (LAPTOP-3FN89LB8)//Desktop//Final Project//masterwr.csv", row.names = FALSE)
write.csv(masterrb, "C://Users/19403//Dropbox//My PC (LAPTOP-3FN89LB8)//Desktop//Final Project//masterrb.csv", row.names = FALSE)
write.csv(masterte, "C://Users/19403//Dropbox//My PC (LAPTOP-3FN89LB8)//Desktop//Final Project//masterte.csv", row.names = FALSE)
