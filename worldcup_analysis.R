#### LOAD LIBRARIES ####
library(tidyverse)
library(DataExplorer)
library(png)
library(grid)

#### LOAD DATA ####
data <- read_csv("worldcup_results.csv")

# functions to get an overview of the data
str(data)
introduce(data)
summary(data)
plot_intro(data)

#### CLEAN DATA ####
## Adding variables that will be used in the visualisations ##

# get year from date column (as a numeric value)
data$year <- format(data$date, format = "%Y")
data$year <- as.numeric(data$year)

# make binary columns for game results
data$home_win <- ifelse(data$home_score > data$away_score, 1, 0)
data$away_win <- ifelse(data$home_score < data$away_score, 1, 0)
data$draw <- ifelse(data$home_score == data$away_score, 1, 0)

# use summarise() function to get average goals per game and number of games, goals, ... 
# ... wins, draws, and losses. (for both home and away games)
home_data <- data %>% 
  # group by home team to get information on each team
  group_by(home_team) %>% 
  summarise(
    # get number of home games per team
    n_home_games = n(),
    # get number of 'goals for' per team for home games
    home_gf = sum(home_score),
    # get number of 'goals against' per team for home games
    home_ga = sum(away_score),
    # get average goals scored per home game for each team
    avg_goals_homegame = home_gf/n_home_games,
    # get number of wins per team for home games
    n_home_win = sum(home_win),
    # get number of draws per team for home games
    n_home_draw = sum(draw),
    # get number of losses per team for home games
    n_home_lose = n_home_games - n_home_win - n_home_draw)
away_data <- data %>% 
  # group by away team to get information for each team
  group_by(away_team) %>% 
  summarise(
    # get number of away games per team
    n_away_games = n(),
    # get number of 'goals for' per team for away games
    away_gf = sum(away_score),
    # get number of 'goals against' per team for away games
    away_ga = sum(home_score),
    # get average goals scored per away game for each team
    avg_goals_awaygame = away_gf/n_away_games,
    # get number of wins per team for away games
    n_away_win = sum(away_win),
    # get number of draws per team for away games
    n_away_draw = sum(draw),
    # get number of losses per team for away games
    n_away_lose = n_away_games - n_away_win - n_away_draw)

# merge home & away data, calculate totals, and select the variables of interest
new_data <- merge(
  # specify which data frames to merge
  home_data, away_data,
  # specify which columns to merge by
  by.x="home_team", by.y = "away_team", 
  # to keep all rows (even if the team isn't in both data frames)
  all.x = TRUE, all.y = TRUE) %>% 
  mutate(
    team = home_team,
    # get total number of games
    n_games = n_home_games + n_away_games,
    # get total goals for
    gf = home_gf + away_gf,
    # get total goals against
    ga = home_ga + away_ga,
    # get average goals scored per game
    avg_goals_game = gf/n_games,
    # get total number of wins
    n_win = n_home_win + n_away_win,
    # get total number of draws
    n_draw = n_home_draw + n_away_draw,
    # get total number of losses
    n_lose = n_home_lose + n_away_lose) %>% 
  # select the variables of interest
  select(team, n_games, n_home_games, n_away_games, gf, ga, avg_goals_game, n_win, n_draw, n_lose)

# mutate and select variables, and transform from variable to observation
results_proportion <- new_data %>% 
  mutate(country = team, 
         # calculate proportion of games won by team (to 2 s.f)
         win_proportion = signif(n_win/n_games, 3),
         # calculate proportion of games lost by team (to 2 s.f)
         lose_proportion = signif(n_lose/n_games, 3),
         # calculate proportion of games drawn by team (to 2 s.f)
         draw_proportion = signif(n_draw/n_games, 3)) %>%
  # select the variables of interest
  select(country, win_proportion, lose_proportion, draw_proportion) %>% 
  # gather columns into key-value pairs
  gather(Proportion, Value, win_proportion:draw_proportion, -country)
# view top rows
head(results_proportion)


#### NUMBER OF MATCHES EACH YEAR #### 

# load images and render raster objects
img <- readPNG("Picture.png")
g <- rasterGrob(img, interpolate=TRUE)
img2 <- readPNG("Picture2.png")
g2 <- rasterGrob(img2, interpolate=TRUE)
img3 <- readPNG("Picture1.png")
g3 <- rasterGrob(img3, interpolate=TRUE)
img4 <- readPNG("Picture3.png")
g4 <- rasterGrob(img4, interpolate=TRUE)

# create an object with the years in which the FIFA world cup was held
world_cup_year <- c(1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019)

plot01 <- data %>% 
  # choose aesthetics (year) and fill (to highlight select years)
  ggplot(aes(year, fill = factor(ifelse(year %in% world_cup_year, "Highlighted", "Normal")))) +
  # add bar chart
  geom_bar() +
  # add custom annotations of country flag logos at chosen coordinates
  annotation_custom(g, xmin=1990, xmax=1992, ymin=95, ymax=125) +
  annotation_custom(g2, xmin=1994, xmax=1996, ymin=125, ymax=155) +
  annotation_custom(g, xmin=1998, xmax=2000, ymin=140, ymax=170) +
  annotation_custom(g3, xmin=2002, xmax=2004, ymin=180, ymax=210) +
  annotation_custom(g3, xmin=2006, xmax=2008, ymin=130, ymax=160) +
  annotation_custom(g4, xmin=2010, xmax=2012, ymin=150, ymax=180) +
  annotation_custom(g, xmin=2014, xmax=2016, ymin=165, ymax=195) +
  annotation_custom(g, xmin=2018, xmax=2020, ymin=80, ymax=110) +
  # select colour to fill the bars
  scale_fill_manual(name = "year", values=c("red","grey50")) +
  # change theme to classic (for a cleaner looking plot)
  theme_classic() +
  # add labels for title, subtitle, and x-axis
  labs(title = "Number of Matches Each Year", 
       subtitle = "Years with FIFA World Cup highlighted, with flags indicating the winners", 
       x = "Year") +
  # change feature of the plot (to make the background transparent and other adjustments)
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none", 
        axis.title.y = element_blank(),
        axis.text.y = element_text(colour = "black", face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold")) +
  scale_y_continuous(expand = c(0,2))
# view plot
plot01
# save plot
ggsave(plot01, filename = "plot01.png")


#### RESULT PERCENTAGE PLOT ####
plot02 <- results_proportion %>%
  # filter to show select countries/teams
  filter(country %in% c("United States", "Sweden", "France", "Germany")) %>% 
  # choose aesthetics
  ggplot(aes(country, Value)) +
  # add stacked column chart, fill by proportion
  geom_col(aes(fill = Proportion), position="stack") +
  # select colours for fill
  scale_fill_manual(values = c("lose_proportion" = "#ff3b3b", 
                               "draw_proportion" = "#ffc800", 
                               "win_proportion" = "#5bf05b")) +
  # add text labels and adjust their position
  geom_text(aes(label=Value), size = 3, position = position_stack(vjust = 0.5)) +
  # add title and subtitle
  labs(title = "Proportion of Game Results",
       subtitle = "For the 4 top ranked teams - USA, Sweden, France, and Germany") +
  # change feature of the plot (to make the background transparent and other adjustments)
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(colour = "black", face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold"))
# view plot
plot02
# save plot
ggsave(plot02, filename = "plot02.png")





