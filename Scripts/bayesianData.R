library(readr)
library(dplyr)
library(tidyr)

# Read data
stats <- read_csv("../Data/player_stats_NHL.csv", show_col_types=FALSE)

# Add shooting style for players without one
missing_shoots <- stats %>% filter(is.na(Shoots)) %>% 
  dplyr::select(PlayerId, Name) %>% distinct() %>% 
  mutate(Shoots = case_when(Name == "Barry Legge" ~ "L",
                            Name == "Roger Lemelin" ~ "R",
                            Name == "Dale Lewis" ~ "L",
                            Name == "Nick Libett" ~ "L",
                            Name == "Dwayne Lowdermilk" ~ "R",
                            Name == "George Lyle" ~ "L",
                            Name == "Donald Maciver" ~ "L",
                            Name == "Paul MacKinnon" ~ "R",
                            Name == "Darryl Maggs" ~ "R",
                            Name == "Dan Mandich" ~ "R",
                            Name == "John Marks" ~ "L",
                            Name == "Paul Marshall" ~ "L",
                            Name == "Mike Marson" ~ "L",
                            # Name == "Patrick Mayer" ~ NA,
                            Name == "Kevin Maxwell" ~ "R",
                            Name == "William McCreary" ~ "R",
                            # Name == "Robert McLean" ~ NA,
                            Name == "Joe Micheletti" ~ "L",
                            Name == "Perry Miller" ~ "L",
                            Name == "Warren Miller" ~ "R",
                            Name == "Lyle Moffatt" ~ "L",
                            Name == "Gary Morrison" ~ "R",
                            Name == "Kevin Morrison" ~ "L",
                            Name == "Grant Mulvey" ~ "R",
                            Name == "Chris Oddleifson" ~ "R",
                            Name == "John Paddock" ~ "R",
                            Name == "Douglas Patey" ~ "R",
                            Name == "Dennis Patterson" ~ "L",
                            Name == "Alex Pirus" ~ "R",
                            Name == "Paul Pooley" ~ "R",
                            Name == "Thomas Price" ~ "L",
                            Name == "Bill Riley" ~ "R",
                            Name == "Doug Risebrough" ~ "L",
                            # Name == "Morris Robinson" ~ NA,
                            Name == "Don Saleski" ~ "R",
                            Name == "Gary Sampson" ~ "L",
                            Name == "Gary Sargent" ~ "L",
                            Name == "Peter Scamurra" ~ "L",
                            Name == "Timothy Sheehy" ~ "R",
                            Name == "Al Sims" ~ "L",
                            Name == "Gord Smith" ~ "L",
                            Name == "Greg Smith" ~ "L",
                            Name == "Steve Smith" ~ "L",
                            Name == "John Smrke" ~ "L",
                            # Name == "Lorne Stamler" ~ NA,
                            Name == "Robert Stephenson" ~ "R",
                            Name == "Paul Stewart" ~ "L",
                            Name == "Bob Stewart" ~ "L",
                            Name == "Blair Stewart" ~ "L",
                            Name == "Tony Stiles" ~ "L",                            
                            Name == "Steve Stoyanovich" ~ "R",                          
                            Name == "Jeffrey Teal" ~ "L",
                            Name == "Glenn Tomalty" ~ "L",
                            Name == "Dean Turner" ~ "L",
                            Name == "Gregory Vaydik" ~ "L",
                            Name == "James Walsh" ~ "R",
                            Name == "David Watson" ~ "L",
                            Name == "Tony White" ~ "L",
                            Name == "Rick Wilson" ~ "L",
                            Name == "Murray Wilson" ~ "L",
                            Name == "Behn Wilson" ~ "L",
                            Name == "Paul Woods" ~ "L",
                            Name == "Bennett Wolf" ~ "R",
                            Name == "Filip Roos" ~ "L"
  ))

# Update shooting style for the missing players
stats <- stats %>% 
  left_join(missing_shoots, by=c("PlayerId", "Name")) %>% 
  mutate(Shoots = ifelse(is.na(Shoots.x), Shoots.y, Shoots.x)) %>% 
  dplyr::select(-c(Shoots.x, Shoots.y)) %>% 
  filter(!is.na(Shoots) & Season < 20222023)

# Shooting stats for single-seasons
stats %>%
  filter(!is.na(Shots) & Shots > 0) %>%
  group_by(PlayerId, Name, Shoots, Position, Season) %>% 
  summarise(Goals = sum(Goals), Shots=sum(Shots), Games=sum(Games)) %>% 
  mutate(ShotPct = Goals/Shots, Season = as.integer(substr(as.character(Season), 1, 4))) %>%
  ungroup() %>% 
  filter(ShotPct > 0 & ShotPct < 1) %>%
  dplyr::select(PlayerId, Name, Shoots, Position, Season, Goals, Shots, ShotPct, Games)  %>%
  mutate(PositionGroup = ifelse(Position == "D", "D", "F"), 
         PositionGroup = factor(PositionGroup)) -> season_shots

# Shooting stats for an entire career
stats %>% 
  filter(!is.na(Shots) & Shots > 0) %>% 
  mutate(Season = as.integer(substr(as.character(Season), 1, 4))) %>% 
  group_by(PlayerId, Name, Shoots, Position) %>% 
  summarise(Goals = sum(Goals), Shots=sum(Shots), Season=round(mean(Season)), Games=sum(Games)) %>% 
  mutate(ShotPct = Goals/Shots) %>% 
  filter(ShotPct > 0 & ShotPct < 1) %>%  
  ungroup() %>% 
  dplyr::select(PlayerId, Name, Shoots, Position, Season, Goals, Shots, ShotPct, Games)  %>%
  mutate(PositionGroup = ifelse(Position == "D", "D", "F"), 
         PositionGroup = factor(PositionGroup)) -> career_shots

# Find teams represented for each season 
player_teams_season <- stats %>% 
  filter(Season > 19581959) %>% 
  dplyr::select(PlayerId, Team, Season) %>% 
  distinct() %>% 
  mutate(SeasonFull = paste(substr(as.character(Season), 1, 4), substr(as.character(Season), 5, 8), sep="-"),
         Season = as.integer(substr(as.character(Season), 1, 4))) %>% 
  group_by(PlayerId, Season) %>% 
  summarise(Teams = paste0(Team, collapse = " "),
            SeasonTeams = paste(Team, SeasonFull, collapse = " ")) %>% 
  ungroup() 

# Find teams represented for the entire career
player_teams_career <- stats %>% 
  filter(Season > 19581959) %>% 
  group_by(PlayerId, Team) %>%
  slice(c(1, n()))  %>% 
  group_by(PlayerId, Team, Season) %>% 
  distinct() %>% 
  group_by(PlayerId, Team) %>%
  mutate(StartSeason = as.integer(substr(as.character(Season), 1, 4)),
         EndSeason = as.integer(substr(as.character(Season), 5, 8))) %>% 
  summarise(SeasonFull = paste(min(StartSeason), max(EndSeason), sep="-")) %>%
  arrange(PlayerId, SeasonFull) %>% 
  summarise(Teams = paste0(Team, collapse = " "),
            SeasonTeams = paste(Team, SeasonFull, collapse = " ")) %>% 
  ungroup() %>% 
  dplyr::select(PlayerId, Teams, SeasonTeams) %>% 
  left_join(
    # Get the entire length of the player's career
    stats %>% 
      group_by(PlayerId) %>%
      mutate(StartSeason = as.integer(substr(as.character(Season), 1, 4)),
             EndSeason = as.integer(substr(as.character(Season), 5, 8))) %>% 
      summarise(Career = paste(min(StartSeason), max(EndSeason), sep="-")),
    by="PlayerId"
  )
