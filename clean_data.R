library(tidyverse)

# Used to filter older teams not in current NBA
nba_teams <- c("WAS", "UTA", "TOR", "SAS", "SAC", "POR", "PHO", "PHI", "ORL", "OKC", "NYK", "NOP", "MIN", "MIL", "MIA", "MEM", "LAL", "LAC", "IND", "HOU", "GSW", "DET", "DEN", "DAL", "CLE", "CHI", "CHA", "BOS", "ATL", "BRK")

# Create a dictionary of NBA teams and their colors
# CHECK FOR TEAMS Change PHX to PHO and BKN to BRK To align with CSV Creator
nba_teams_colors <- data.frame(
  ATL = "red",
  BOS = "green",
  BRK = "black",
  CHA = "teal",
  CHI = "darkred",
  CLE = "maroon",
  DAL = "blue",
  DEN = "goldenrod",
  DET = "darkblue",
  GSW = "goldenrod3",
  HOU = "firebrick",
  IND = "navy",
  LAC = "dodgerblue4",
  LAL = "purple",
  MEM = "mediumblue",
  MIA = "firebrick1",
  MIL = "green3",
  MIN = "blue4",
  NOP = "darkslateblue",
  NYK = "dodgerblue3",
  OKC = "dodgerblue2",
  ORL = "royalblue3",
  PHI = "deepskyblue4",
  PHO = "purple3",
  POR = "red3",
  SAC = "purple4",
  SAS = "black",
  TOR = "darkred",
  UTA = "darkblue",
  WAS = "navyblue"
)


### Analysis Starts Here
player_season_data_per_game <- read.csv("Data/Player Per Game.csv")

player_season_data_per_game <- player_season_data_per_game[, c(
  "seas_id",
  "season",
  "player_id",
  "player",
  "x2p_percent",
  "x3p_percent",
  "ft_percent",
  "tm",
  "x3p_per_game",
  "x3pa_per_game",
  "x2p_per_game",
  "x2pa_per_game",
  "ft_per_game",
  "fta_per_game"
)]


# Used to filter older teams not in current NBA
player_season_data_per_game <- player_season_data_per_game[player_season_data_per_game$tm %in% nba_teams, ]
min(player_season_data_per_game$season)


player_season_data_per_game <- gather(player_season_data_per_game,
  key = "distance", value = "sht_percentage",
  x2p_percent,
  x3p_percent,
  ft_percent
)

player_season_data_per_game <- group_by(player_season_data_per_game, tm)

player_season_data_per_game[player_season_data_per_game$distance == "x2p_percent", ]$distance <- "2P% ="
player_season_data_per_game[player_season_data_per_game$distance == "x3p_percent", ]$distance <- "3P% ="
player_season_data_per_game[player_season_data_per_game$distance == "ft_percent", ]$distance <- "FTP% ="

# Merge with the dictionary to add colors
player_season_data_per_game$color <- unlist(nba_teams_colors[player_season_data_per_game$tm])

# Make a name for each player and add their team name to avoid single season two team players
player_season_data_per_game$player_add_team_name <- paste0(player_season_data_per_game$player, " @ ", player_season_data_per_game$tm)
player_season_data_per_game$player_add_team_year_name <- paste0(player_season_data_per_game$player_add_team_name, ", ", player_season_data_per_game$season)
player_season_data_per_game$player_all_info_text <- paste0(
  player_season_data_per_game$player_add_team_year_name,
  "\n\n", player_season_data_per_game$ft_per_game, " / ", player_season_data_per_game$fta_per_game, " FT made/attempted Per Game",
  "\n\n", player_season_data_per_game$x2p_per_game, " / ", player_season_data_per_game$x2pa_per_game, " 2PTS made/attempted Per Game",
  "\n\n", player_season_data_per_game$x3p_per_game, " / ", player_season_data_per_game$x3pa_per_game, " 3PTS made/attempted Per Game"
)


write.csv(player_season_data_per_game, "project_deploy_files/player_info.csv")

# SEPARATE
# Data for Second Page
team_summ <- read.csv("Data/Team Summaries.csv")

# Repeat eliminating old teams and add colors
team_summ <- team_summ[(team_summ$abbreviation %in% nba_teams) & team_summ$season >= 1960, ]
team_summ$color <- unlist(nba_teams_colors[team_summ$abbreviation])

write.csv(team_summ, "project_deploy_files/team_summ.csv")

unique(team_summ$team)

selected_season_begin <- 2022
selected_season_end <- 2023
selected_teams <- c("Atlanta Hawks", "Boston Celtics", "Miami Heat")
