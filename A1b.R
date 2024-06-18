# Set the working directory and verify it
setwd('E:\\ASSIGNMENT\\Data')
getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("readxl", "dplyr", "ggplot2", "fitdistrplus", "tidyverse")
lapply(libraries, install_and_load)

# Load data sets  
ipl_data <- read.csv("IPL_ball_by_ball_updated till 2024.csv")
salary_data <- read_excel("IPL SALARIES 2024.xlsx")


# Renaming columns to match code requirements
ipl_data <- ipl_data %>%
  rename(
    Match_id = `Match.id`,
    Batting_team = `Batting.team`,
    Bowling_team = `Bowling.team`,
    Innings_No = `Innings.No`,
    Ball_No = `Ball.No`
  )





# Arranging the data IPL round-wise and batsman, ball, runs, and wickets per player per match
ipl_rounds <- ipl_data %>%
  group_by(Match_id, Date, Season, Batting_team, Bowling_team, Innings_No, Ball_No, Bowler, Striker) %>%
  summarize(
    runs = sum(runs_scored),
    wickets = sum(wicket_confirmation, na.rm = TRUE),
    .groups = 'drop'
  )

# Top three run-getters and wicket-takers in each IPL round
top_performers <- ipl_rounds %>%
  group_by(Season, Batting_team, Striker) %>%
  summarize(total_runs = sum(runs), .groups = 'drop') %>%
  arrange(desc(total_runs)) %>%
  top_n(3, total_runs)

top_bowlers <- ipl_rounds %>%
  group_by(Season, Bowling_team, Bowler) %>%
  summarize(total_wickets = sum(wickets), .groups = 'drop') %>%
  arrange(desc(total_wickets)) %>%
  top_n(3, total_wickets)

# Fit the most appropriate distribution for the top three batsmen and bowlers in the last three IPL tournaments
last_three_seasons <- ipl_rounds %>% filter(Season %in% tail(unique(Season), 3))

# Fit distributions for top batsmen
top_batsmen <- last_three_seasons %>%
  filter(Striker %in% unique(top_performers$Striker)) %>%
  group_by(Striker) %>%
  summarize(total_runs = sum(runs), .groups = 'drop')

top_batsmen_dist <- fitdist(top_batsmen$total_runs, "norm")

# Fit distributions for top bowlers
top_bowlers <- last_three_seasons %>%
  filter(Bowler %in% unique(top_bowlers$Bowler)) %>%
  group_by(Bowler) %>%
  summarize(total_wickets = sum(wickets), .groups = 'drop')

top_bowlers_dist <- fitdist(top_bowlers$total_wickets, "pois")

# Fit distribution for A Nortje
A_Nortje_runs <- last_three_seasons %>%
  filter(Striker == "A Nortje") %>%
  dplyr::select(runs)

# Check if the resulting runs are numeric and have more than one element
if (is.numeric(A_Nortje_runs$runs) && length(A_Nortje_runs$runs) > 1) {
  A_Nortje_dist <- fitdist(A_Nortje_runs$runs, "norm")
  print(summary(A_Nortje_dist))
} else {
  print("A Nortje runs are not a numeric vector of length greater than 1.")
}


# Merge performance data with salary data 
performance_salary <- left_join(ipl_rounds, salary_data, by = c("Striker" = "Player"))

# Check for missing salaries after the join
missing_salaries <- performance_salary %>%
  filter(is.na(Salary))

# Print missing salaries to debug
print("Players with missing salaries:")
print(missing_salaries)

# Summarize total runs and wickets with salary
performance_summary <- performance_salary %>%
  filter(!is.na(Salary)) %>%
  group_by(Striker, Salary) %>%
  summarize(total_runs = sum(runs), total_wickets = sum(wickets), .groups = 'drop')

# Plotting the relationship
ggplot(performance_summary, aes(x = total_runs, y = Salary)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Runs Scored and Salary")

ggplot(performance_summary, aes(x = total_wickets, y = Salary)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Wickets Taken and Salary")

# Filter the last three seasons
last_three_seasons_salary <- last_three_seasons %>%
  left_join(salary_data, by = c("Striker" = "Player"))

# Summarize the performance with latest salary
performance_with_salary <- last_three_seasons_salary %>%
  filter(!is.na(Salary)) %>%
  group_by(Striker) %>%
  summarize(total_runs = sum(runs), total_wickets = sum(wickets), latest_salary = max(Salary), .groups = 'drop')

# Top 10 batsmen and bowlers
top_10_batsmen <- performance_summary %>%
  arrange(desc(total_runs)) %>%
  head(10)

top_10_bowlers <- performance_summary %>%
  arrange(desc(total_wickets)) %>%
  head(10)

# Print top 10 batsmen and bowlers to verify data
print(top_10_batsmen)
print(top_10_bowlers)

# Perform t-test only if both groups have sufficient data
if (nrow(top_10_batsmen) > 1 && nrow(top_10_bowlers) > 1) {
  # Perform t-test
  t_test_result <- t.test(top_10_batsmen$Salary, top_10_bowlers$Salary)
  # Display results
  print(t_test_result)
} else {
  print("Not enough observations for the t-test.")
}
