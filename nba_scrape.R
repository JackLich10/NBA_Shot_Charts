# load libraries
library(tidyverse)
library(httr)
library(hexbin)
library(jsonlite)
library(scales)
library(teamcolors)

# player_id, team_id lookup functions ------------------------------------------------
players_url = "http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=2019-20&IsOnlyCurrentSeason=0"

request_headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

request = GET(players_url, add_headers(request_headers))

players_data = fromJSON(content(request, as = "text"))
players = as_tibble(data.frame(players_data$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
names(players) = tolower(players_data$resultSets$headers[[1]])

players = mutate(players,
                 person_id = as.numeric(person_id),
                 rosterstatus = as.logical(as.numeric(rosterstatus)),
                 from_year = as.numeric(from_year),
                 to_year = as.numeric(to_year),
                 team_id = as.numeric(team_id)
)

if (Sys.Date() <= as.Date("2017-10-20")) {
  players = mutate(players, to_year = pmin(to_year, 2016))
}

players$name = sapply(players$display_last_comma_first, function(s) {
  paste(rev(strsplit(s, ", ")[[1]]), collapse = " ")
})

first_year_of_data = 1996
last_year_of_data = max(players$to_year)
season_strings = paste(first_year_of_data:last_year_of_data,
                       substr(first_year_of_data:last_year_of_data + 1, 3, 4),
                       sep = "-")

names(season_strings) = first_year_of_data:last_year_of_data

available_players = filter(players, to_year >= first_year_of_data)

names_table = table(available_players$name)
dupe_names = names(names_table[which(names_table > 1)])

available_players$name[available_players$name %in% dupe_names] = paste(
  available_players$name[available_players$name %in% dupe_names],
  available_players$person_id[available_players$name %in% dupe_names]
)

available_players$lower_name = tolower(available_players$name)
available_players = arrange(available_players, lower_name)

find_player_by_name <- function(name) {
  filter(available_players, lower_name == tolower(name))
}

find_player_id_by_name <- function(name) {
  find_player_by_name(name)$person_id
}

find_team_id_by_name <- function(team) {
  available_players %>% 
    distinct(team_name, team_id) %>% 
    filter(team_name == team) %>% 
    pull(team_id)
}

player_photo_url = function(player_id) {
  paste0("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/", player_id, ".png")
}

team_photo_url <- function(team) {
  abbreviation <- available_players %>% 
    distinct(team_name, team_abbreviation = str_to_lower(team_abbreviation)) %>% 
    filter(team_name == team) %>% 
    pull(team_abbreviation)
  paste0("https://www.nba.com/.element/img/1.0/teamsites/logos/teamlogos_500x500/", abbreviation, ".png")
}

# shot location lookup functions  ------------------------------------------
# use player_id, team_id, opponent_id = 0 to gather all players, all teams, all opponents
fetch_shots = function(player_id, team_id, opponent_id, season, season_type) {
  request = GET(
    "http://stats.nba.com/stats/shotchartdetail",
    query = list(
      PlayerID = player_id,
      Season = season,
      SeasonType = season_type,
      PlayerPosition = "",
      ContextMeasure = "FGA",
      DateFrom = "",
      DateTo = "",
      GameID = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = opponent_id,
      Outcome = "",
      Period = 0,
      Position = "",
      RookieYear = "",
      SeasonSegment = "",
      TeamID = team_id,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers(request_headers)
  )
  
  stop_for_status(request)
  
  data = content(request)
  
  raw_shots_data = data$resultSets[[1]]$rowSet
  col_names = tolower(as.character(data$resultSets[[1]]$headers))
  
  if (length(raw_shots_data) == 0) {
    shots = data.frame(
      matrix(nrow = 0, ncol = length(col_names))
    )
  } else {
    shots = data.frame(
      matrix(
        unlist(raw_shots_data),
        ncol = length(col_names),
        byrow = TRUE
      )
    )
  }
  
  shots = as_tibble(shots)
  names(shots) = col_names
  
  shots = mutate(shots,
                 loc_x = as.numeric(as.character(loc_x)) / 10,
                 loc_y = as.numeric(as.character(loc_y)) / 10 + hoop_center_y,
                 shot_distance = as.numeric(as.character(shot_distance)),
                 shot_made_numeric = as.numeric(as.character(shot_made_flag)),
                 shot_made_flag = factor(shot_made_flag, levels = c("1", "0"), labels = c("made", "missed")),
                 shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
                 shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2),
                 game_date = as.Date(game_date, format = "%Y%m%d")
  )
  
  raw_league_avg_data = data$resultSets[[2]]$rowSet
  league_avg_names = tolower(as.character(data$resultSets[[2]]$headers))
  league_averages = tbl_df(data.frame(
    matrix(unlist(raw_league_avg_data), ncol = length(league_avg_names), byrow = TRUE)
  ))
  names(league_averages) = league_avg_names
  league_averages = mutate(league_averages,
                           fga = as.numeric(as.character(fga)),
                           fgm = as.numeric(as.character(fgm)),
                           fg_pct = as.numeric(as.character(fg_pct)),
                           shot_value = ifelse(shot_zone_basic %in% c("Above the Break 3", "Backcourt", "Left Corner 3", "Right Corner 3"), 3, 2)
  )
  return(list(shots = shots, league_averages = league_averages))
}

# function to convert shots to averages to apply to 'league_averages' input for hex chart
convert_to_avgs <- function(shots) {
  shots %>% 
    group_by(shot_zone_basic, shot_zone_area, shot_zone_range, shot_value) %>% 
    summarise(fga = n(),
              fgm = sum(shot_made_numeric)) %>% 
    mutate(fg_pct = fgm/fga) %>% 
    ungroup()
}

# function to retrieve shot location data
get_data <- function(player_id, team_id, opponent_id, seasons, season_type, bubble = F, binwidths = c(1.5, 1.5), min_radius_factor = .2) {
  df <- fetch_shots(player_id = player_id, team_id = team_id, 
                    opponent_id = opponent_id, season = seasons, 
                    season_type = season_type)
  
  shots <- as.data.frame(df[1])
  league_averages <- as.data.frame(df[2])
  
  names(shots) <- sub(".*\\.", "", names(shots))
  names(league_averages) <- sub(".*\\.", "", names(league_averages))
  
  shots <- shots %>% 
    mutate(loc_x = loc_x + 25)
  
  if (opponent_id != 0) {
    # bug where opponent id returns wrong league averages
    df <- fetch_shots(player_id = 0, team_id = 1610612751, 
                      opponent_id = 0, season = seasons, 
                      season_type = season_type)
    league_averages <- as.data.frame(df[2])
    names(league_averages) <- sub(".*\\.", "", names(league_averages))
  }
  
  if (bubble == T) {
    shots <- shots %>% 
      mutate(bubble = ifelse(game_date > as.Date("2020-03-20"), "Bubble", "Pre-Bubble"))
    
    league_averages <- team_shots %>% 
      filter(bubble == "Pre-Bubble") %>% 
      convert_to_avgs()
    
    shots <- shots %>% 
      filter(bubble == "Bubble")
  }
  
  df <- calculate_hexbins_from_shots(shots, league_averages, 
                                     binwidths = binwidths, min_radius_factor = min_radius_factor, 
                                     fg_diff_limits = c(-0.15, 0.15), fg_freq_limits = c(-0.075, 0.075),
                                     fg_pct_limits = c(0.2, 0.7), pps_limits = c(0.5, 1.5))  
  
  df <- as.data.frame(df[1])
  
  df$season <- seasons
  df$player_id <- player_id
  df$team_id <- team_id
  df$opponent_id <- opponent_id
  
  names(df) <- sub(".*\\.", "", names(df))
  
  return(df)
}

