library(ggplot2)
library(mgcv)
library(gamlss)
library(gamlss.add)
library(stringr)

# Specify the working directory
if(length(str_split(getwd(), "/Scripts")[[1]]) != 2){
  setwd("Scripts")
}

# Suppress summarise information
options(dplyr.summarise.inform = FALSE)

# Specify default theme
theme_set(theme_classic())

# Update theme
theme_update(legend.position = "top", panel.grid = element_blank(),
             axis.line.x.bottom = element_line(color="#C0C0C0"),
             axis.line.y.left = element_line(color="#C0C0C0"),
             axis.text = element_text(size=12, color="black"), 
             axis.title = element_text(size=12),
             legend.text = element_text(size=12))

#' Perform a check on possible predictors.
#'
#' @param shots data frame for each single-season or career shots
#'
#' @return plots saved to disk
#' @export
#'
predictor_check <- function(shots){
  
  # Capture the name of the input data
  input_data <- deparse(substitute(shots))
  
  # Get the name of the data
  name <- str_split(input_data, "_")[[1]][1]
  
  # Plot shooting percentage by handedness
  print(ggplot(shots %>% mutate(Shoots = factor(Shoots, labels=c("Left", "Right"))), 
               aes(Shoots, ShotPct)) + 
          geom_jitter(alpha=0.2, height=0, width=0.2, color="#00539CFF") + 
          geom_violin(alpha=0) + geom_boxplot(width=0.3, alpha=0.5, outlier.shape = NA) +
          scale_y_continuous(limits=c(0, 0.8)) + 
          ylab("S%") + guides(label = "none"))
  
  # Save the figure
  ggsave(paste0("../Figures/shot_percent_hand_", name, ".png"), width = 12, height = 8)
  
  # Plot shooting percentage by position group
  print(ggplot(shots %>% mutate(PositionGroup = factor(PositionGroup, labels=c("Defenders", "Forwards"))), 
               aes(PositionGroup, ShotPct)) + 
          geom_jitter(alpha=0.2, height=0, width=0.2, color="#00539CFF") + 
          geom_violin(alpha=0) + geom_boxplot(width=0.3, alpha=0.5, outlier.shape = NA) +
          scale_y_continuous(limits=c(0, 0.8)) + 
          xlab("") + ylab("S%") + guides(label = "none"))
  
  # Save the figure
  ggsave(paste0("../Figures/shot_percent_position_", name, ".png"), width = 12, height = 8)
  
  # Plot shooting percentage by season
  print(ggplot(shots %>% group_by(Season) %>% summarise(ShotPct = mean(ShotPct)), 
               aes(Season, ShotPct)) + 
          geom_smooth(aes(color="Smooth"), se=FALSE, alpha=0.4) + 
          geom_smooth(aes(color="Smooth"), show.legend=FALSE, alpha=0.4) + 
          geom_line(aes(color="Data"), size=1) + 
          xlab("Season") + ylab("Mean S%") + guides(label = "none") + 
          scale_y_continuous(limits=c(0.05, 0.15)) + 
          scale_color_manual(name="", values=c("#00539CFF", "#EEA47FFF")))
  
  # Save the figure
  ggsave(paste0("../Figures/shot_percent_season_", name, ".png"), width = 12, height = 8)
  
  # Plot shooting percentage by number of games played
  print(ggplot(shots %>% group_by(Games) %>% summarise(ShotPct = mean(ShotPct)), 
               aes(Games, ShotPct)) + 
          {if(name=="career") geom_line(aes(color="Data"), size=1)} + 
          geom_smooth(aes(color="Smooth"), se=FALSE, alpha=0.4) + 
          geom_smooth(aes(color="Smooth"), show.legend=FALSE, alpha=0.4) + 
          {if(name=="season") geom_line(aes(color="Data"), size=1)} + 
          xlab("Games") + ylab("Mean S%") + guides(label = "none") + 
          scale_y_continuous(limits=c(0, 0.42)) + 
          scale_color_manual(name="", values=c("#00539CFF", "#EEA47FFF")))
  
  # Save the figure
  ggsave(paste0("../Figures/shot_percent_games_", name, ".png"), width = 12, height = 8)
  
  # Plot shooting percentage by number of shots taken
  print(ggplot(shots %>% group_by(Shots) %>% summarise(ShotPct = mean(ShotPct)), 
               aes(Shots, ShotPct)) + 
          {if(name=="career") geom_line(aes(color="Data"), size=1)} + 
          geom_smooth(aes(color="Smooth"), se=FALSE, alpha=0.4) + 
          geom_smooth(aes(color="Smooth"), show.legend=FALSE, alpha=0.4) + 
          {if(name=="season") geom_line(aes(color="Data"), size=1)} + 
          xlab("Shots") + ylab("Mean S%") + guides(label = "none") + 
          scale_y_continuous(limits=c(0, 0.5)) + 
          scale_color_manual(name="", values=c("#00539CFF", "#EEA47FFF")))
  
  # Save the figure
  ggsave(paste0("../Figures/shot_percent_shots_", name, ".png"), width = 12, height = 8)
}

#' Compute the posterior
#'
#' @param fit a model fit by gamlss: either season or career
#' @param shots the data used to fit the model
#'
#' @return data frame with posterior
#' @export
#'
compute_posterior <- function(fit, shots){
  
  # Extract the (linear) terms for the mu parameter
  mu_coef <- predict(fit, what="mu", type="terms")
  
  # Capture the name of the input data
  input_data <- deparse(substitute(shots))
  
  # Get the name of the data
  name <- str_split(input_data, "_")[[1]][1]
  
  # Show how season coefficient varies by position 
  print(shots %>%
          mutate(PositionGroup = factor(PositionGroup, labels=c("Defenders", "Forwards"))) %>% 
          dplyr::select(Season, PositionGroup) %>%
          mutate(`Season coefficient` = boot::inv.logit(mu_coef[, 2]),
                 `-Season coefficient` = boot::inv.logit(-mu_coef[, 2])) %>%
          pivot_longer(-c(Season, PositionGroup)) %>%
          mutate(name = factor(name, levels=c("Season coefficient", "-Season coefficient"))) %>% 
          ggplot(aes(Season, value, color=PositionGroup)) + geom_line(size=1, show.legend = FALSE) + 
          facet_grid(PositionGroup~name) + ylab("Coefficient") + 
          scale_color_manual(values=c("#00539CFF", "#F95700FF")))
  
  # Save the figure
  ggsave(paste0("../Figures/season_coefficient_", name, ".png"), width = 12, height = 8)
  
  # Compute the response variable with adjusted values for the season
  mu <- boot::inv.logit(attr(mu_coef, 'constant') + mu_coef[, 1] - mu_coef[, 2] + mu_coef[, 3])
  
  # Extract fitted values
  sigma <- fitted(fit, "sigma")
  
  # Compute an empirical Bayes estimator after the regression
  eb_posterior <- shots %>% 
    mutate(mu = mu,
           sigma = sigma,
           alpha0 = mu / sigma,
           beta0 = (1 - mu) / sigma,
           alpha1 = alpha0 + Goals,
           beta1 = beta0 + Shots - Goals,
           EmpBayes = alpha1 / (alpha1 + beta1)) %>% 
    arrange(-EmpBayes)
  
  # Extract the desired columns to keep only the essentials
  examine <- eb_posterior %>% 
    dplyr::select(PlayerId, Name, Season, PositionGroup, Goals, Shots, Games, ShotPct, EmpBayes) %>% 
    mutate(ShotPctRank = rank(-ShotPct, ties.method = "first"), EmpBayesRank = rank(-EmpBayes))
  
  return(examine)
}

# Read data
source("bayesianData.R")

# EDA
predictor_check(season_shots)
predictor_check(career_shots)

# Fit prior (MLE) with a Beta-Binomial distribution
season_fit <- gamlss(cbind(Goals, Shots - Goals) ~ PositionGroup + ga(~s(Season, by = PositionGroup), method="REML") + Shots,
                     sigma.formula = ~ PositionGroup + Shots,
                     data = season_shots,
                     family = BB(mu.link = "logit", sigma.link = "log"),
                     control = gamlss.control(n.cyc=100))

career_fit <- gamlss(cbind(Goals, Shots - Goals) ~ PositionGroup + ga(~s(Season, by = PositionGroup), method="REML") + Shots,
                     sigma.formula = ~ PositionGroup + Shots,
                     data = career_shots,
                     family = BB(mu.link = "logit", sigma.link = "log"),
                     control = gamlss.control(n.cyc=100))

# Print model details
summary(season_fit)
summary(career_fit)

# Compute the posterior
season_posterior <- compute_posterior(season_fit, season_shots)
career_posterior <- compute_posterior(career_fit, career_shots)

# Add teams
season_posterior <- season_posterior %>% 
  left_join(player_teams_season, by=c("PlayerId", "Season")) %>% 
  mutate(Season = paste(Season, Season+1, sep="-")) %>% 
  dplyr::select(-SeasonTeams)

career_posterior <- career_posterior %>% 
  left_join(player_teams_career, by="PlayerId")

# Read player data to add nationality
players <- read_csv("../Data/Players.csv", show_col_types = FALSE) %>% 
  dplyr::select(id, nationality) %>% 
  rename(PlayerId = id, Nationality = nationality)

# Add nationality 
season_posterior <- season_posterior %>% 
  left_join(players, by="PlayerId") 

career_posterior <- career_posterior %>% 
  left_join(players, by="PlayerId")

# Save as csv
write_csv(season_posterior, "../Data/bayes_season.csv")
write_csv(career_posterior, "../Data/bayes_career.csv")
