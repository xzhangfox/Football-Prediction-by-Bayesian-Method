############################################################################################################
# Setup
############################################################################################################
library(coda)
library(rjags)
library(mcmcplots)
library(stringr)
library(plyr)
library(xtable)
library(ggplot2)
source("plotPost.R")
set.seed(12345)
#-----------------------------------------------------------------------------------------------------------
# Away: -1, Draw: 0 , Home: 1
fmel <- df
fmel$match_result <- sign(fmel$homeGoals - fmel$awayGoals) 
fmel$home=as.character(fmel$home)
fmel$away=as.character(fmel$away)
fmel$year=as.numeric(fmel$year)
# Dataframe fmel with only the completly matched results
fmel <- na.omit(fmel)

teams <- unique(c(fmel$home, fmel$away))
year <- unique(fmel$year)

# JAGS list with the data from fmel in integer strings
DATAlist <- list(homeGoals = fmel$homeGoals, awayGoals = fmel$awayGoals, 
                  home = as.numeric(factor(fmel$home, levels=teams)),
                  away = as.numeric(factor(fmel$away, levels=teams)),
                  year = as.numeric(factor(fmel$year, levels=year)),
                  n_teams = length(teams), n_games = nrow(fmel), 
                  n_year = length(year))

# Generator for the type of column names Jags outputs.
column_name <- function(name, ...) {
  paste0(name, "[", paste(..., sep=",") , "]")
}

############################################################################################################
# Check the actual distribution of the number of scored goals and a Poisson distribution
############################################################################################################

oldParam <- par(mfcol=c(2,1), mar=rep(2.2, 4))
hist(c(fmel$awayGoals, fmel$homeGoals), xlim=c(-0.5, 8), breaks = -1:20 + 0.5,
     main = "Distribution of the Goals")
Goal_Mean <- mean(c(fmel$awayGoals, fmel$homeGoals))
hist(rpois(9999, Goal_Mean), xlim=c(-0.5, 8), breaks = -1:9 + 0.5,
     main = "Poisson Distribution with the Same Mean")
par(oldParam)

############################################################################################################
# Model 1
############################################################################################################
model1_string <- "model {
for(i in 1:n_games) {
  homeGoals[i] ~ dpois(lambda_home[home[i],away[i]])
  awayGoals[i] ~ dpois(lambda_away[home[i],away[i]])
}

for(home_i in 1:n_teams) {
  for(away_i in 1:n_teams) {
    lambda_home[home_i, away_i] <- exp(base_line + skill[home_i] - skill[away_i])
    lambda_away[home_i, away_i] <- exp(base_line + skill[away_i] - skill[home_i])
  }
}

skill[1] <- 0
for(j in 2:n_teams) {
  skill[j] ~ dnorm(group_skill, group_tau)
}  

group_skill ~ dnorm(0, 0.0625)
group_tau <- 1 / pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)
base_line ~ dnorm(0, 0.0625)
}"

#-----------------------------------------------------------------------------------------------------------
# Compiling model 1
model1 <- jags.model(textConnection(model1_string), data=DATAlist, n.chains=3, n.adapt=1000)
# Burning some samples on the altar of the MCMC god
update(model1, 1000)
# Generating MCMC samples
s1 <- coda.samples(model1, variable.names=c("base_line", "skill", "group_skill", "group_sigma"), n.iter=1000, thin=2)
# Merging the three MCMC chains into one matrix
ms1 <- as.matrix(s1)

# Skill plot of Valencia
plot(s1[,column_name("skill", which(teams == "Valencia"))])
# Skill plot of Sevilla
plot(s1[,column_name("skill", which(teams == "Sevilla"))])

#-----------------------------------------------------------------------------------------------------------
# Plots histograms over Goals_H, Goals_A, the difference in goals and a barplot over the credible match results.
plot_goals <- function(Goals_H, Goals_A) {
  n_matches <- length(Goals_H)
  Goal_difference <- Goals_H - Goals_A
  match_result <- ifelse(Goal_difference < 0, "away_win", ifelse(Goal_difference > 0, "home_win", "equal"))
  hist(Goals_H, xlim=c(-0.5, 10), breaks=(0:100) - 0.5)
  hist(Goals_A, xlim=c(-0.5, 10), breaks=(0:100) - 0.5)
  hist(Goal_difference, xlim=c(-6, 6), breaks=(-100:100) - 0.5 )
  barplot(table(match_result) / n_matches , ylim=c(0, 1))
}

# Simulates game goals scores using the MCMC samples from the model1 model.
plotPrediction1 <- function(home_team, away_team, ms) {
  oldParam <- par(mfrow = c(2, 4))
  base_line <- ms[, "base_line"]
  skill_H <- ms[, column_name("skill", which(teams == home_team))]
  skill_A <- ms[, column_name("skill", which(teams == away_team))]
  Goals_H <- rpois(nrow(ms),  exp(base_line +  skill_H - skill_A))
  Goals_A <- rpois(nrow(ms),  exp(base_line +  skill_A - skill_H))
  plot_goals(Goals_H, Goals_A)
  Goals_H <- fmel$homeGoals[ fmel$home == home_team & fmel$away == away_team]
  Goals_A <- fmel$awayGoals[ fmel$home == home_team & fmel$away == away_team]
  plot_goals(Goals_H, Goals_A)
  par(oldParam)
}

plotPrediction1("Valencia", "Sevilla", ms1)


plotPrediction1("Sevilla", "Valencia",ms1)

############################################################################################################
# Model 2 with Home Advantages
############################################################################################################
model2_string <- "model {
for(i in 1:n_games) {
  homeGoals[i] ~ dpois(lambda_home[home[i],away[i]])
  awayGoals[i] ~ dpois(lambda_away[home[i],away[i]])
}

for(home_i in 1:n_teams) {
  for(away_i in 1:n_teams) {
    lambda_home[home_i, away_i] <- exp( home_base_line + skill[home_i] - skill[away_i])
    lambda_away[home_i, away_i] <- exp( away_base_line + skill[away_i] - skill[home_i])
  }
}

skill[1] <- 0 
for(j in 2:n_teams) {
  skill[j] ~ dnorm(group_skill, group_tau)
}

group_skill ~ dnorm(0, 0.0625)
group_tau <- 1/pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)

home_base_line ~ dnorm(0, 0.0625)
away_base_line ~ dnorm(0, 0.0625)
}"


model2 <- jags.model(textConnection(model2_string), data=DATAlist, n.chains=3, n.adapt=5000)
update(model2, 5000)
#s2 <- coda.samples(model2, variable.names=c("home_base_line", "away_base_line","skill", "group_sigma", "group_skill"), n.iter=10000, thin=2)
s2 <- coda.samples(model2, variable.names=c("home_base_line", "away_base_line","skill", "group_sigma", "group_skill"), n.iter=5000, thin=2)
ms2 <- as.matrix(s2)


plot(s2[,"home_base_line"])
plot(s2[,"away_base_line"])

#-----------------------------------------------------------------------------------------------------------
plotPost(exp(ms2[,"home_base_line"]) - exp(ms2[,"away_base_line"]), compVal=0, 
         xlab="Extra Goals in Home Advantage")


## @knitr 
#dic_model1 <- dic.samples(model1, 10000, "pD")
#dic_model2 <- dic.samples(model2, 10000, "pD")
dic_model1 <- dic.samples(model1, 1000, "pD")
dic_model2 <- dic.samples(model2, 1000, "pD")
diffdic(dic_model1, dic_model2)

plotPrediction2 <- function(home_team, away_team, ms) {
  oldParam <- par(mfrow = c(2, 4))
  home_base_line <- ms[, "home_base_line"]
  away_base_line <- ms[, "away_base_line"]
  skill_H <- ms[, column_name("skill", which(teams == home_team))]
  skill_A <- ms[, column_name("skill", which(teams == away_team))]
  Goals_H <- rpois(nrow(ms),  exp(home_base_line +  skill_H - skill_A))
  Goals_A <- rpois(nrow(ms),  exp(away_base_line +  skill_A - skill_H))
  plot_goals(Goals_H, Goals_A)
  Goals_H <- fmel$homeGoals[ fmel$home == home_team & fmel$away == away_team]
  Goals_A <- fmel$awayGoals[ fmel$home == home_team & fmel$away == away_team]
  plot_goals(Goals_H, Goals_A)
  par(oldParam)
}

plotPrediction2("Valencia", "Sevilla", ms2)

plotPrediction2("Sevilla", "Valencia",ms2)


############################################################################################################
# Model 3 with Skill Cariability
############################################################################################################
model3_string <- "model {
for(i in 1:n_games) {
  homeGoals[i] ~ dpois(lambda_home[year[i], home[i],away[i]])
  awayGoals[i] ~ dpois(lambda_away[year[i], home[i],away[i]])
}

for(year_i in 1:n_year) {
  for(home_i in 1:n_teams) {
    for(away_i in 1:n_teams) {
      lambda_home[year_i, home_i, away_i] <- exp( home_base_line[year_i] + skill[year_i, home_i] - skill[year_i, away_i])
      lambda_away[year_i, home_i, away_i] <- exp( away_base_line[year_i] + skill[year_i, away_i] - skill[year_i, home_i])
    }
  }
}

skill[1, 1] <- 0 
for(j in 2:n_teams) {
  skill[1, j] ~ dnorm(group_skill, group_tau)
}

group_skill ~ dnorm(0, 0.0625)
group_tau <- 1/pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)

home_base_line[1] ~ dnorm(0, 0.0625)
away_base_line[1] ~ dnorm(0, 0.0625)

for(year_i in 2:n_year) {
  skill[year_i, 1] <- 0 
  for(j in 2:n_teams) {
    skill[year_i, j] ~ dnorm(skill[year_i - 1, j], year_tau)
  }
  home_base_line[year_i] ~ dnorm(home_base_line[year_i - 1], year_tau)
  away_base_line[year_i] ~ dnorm(away_base_line[year_i - 1], year_tau)
}

year_tau <- 1/pow(year_sigma, 2) 
year_sigma ~ dunif(0, 3) 
}"


#model3 <- jags.model(textConnection(model3_string), data=DATAlist, n.chains=3, n.adapt=10000)
#update(model3, 10000)
#s3 <- coda.samples(model3, variable.names=c("home_base_line", "away_base_line","skill", "year_sigma", "group_sigma", "group_skill"), n.iter=10000, thin=8)
#ms3 <- as.matrix(s3)

model3 <- jags.model(textConnection(model3_string), data=DATAlist, n.chains=3, n.adapt=3500)
update(model3, 3500)
s3 <- coda.samples(model3, variable.names=c("home_base_line", "away_base_line","skill", "year_sigma", "group_sigma", "group_skill"), n.iter=10000, thin=8)
ms3 <- as.matrix(s3)


plot(s3[,"year_sigma"])


## Ranking Plot
team_skill <- ms3[, str_detect(string=colnames(ms3), "skill\\[5,")]
team_skill <- (team_skill - rowMeans(team_skill)) + ms3[, "home_base_line[5]"]
team_skill <- exp(team_skill)
colnames(team_skill) <- teams
team_skill <- team_skill[,order(colMeans(team_skill), decreasing=T)]
oldParam <- par(mar=c(2,0.7,0.7,0.7), xaxs='i')
caterplot(team_skill, labels.loc="above", val.lim=c(0.7, 3.8))
par(oldParam)

plotPost(team_skill[, "Real Madrid"] - team_skill[, "Barcelona"], compVal = 0, xlab = "← Barcelona   vs    Real Madrid →")

############################################################################################################
# Prediction
############################################################################################################
n <- nrow(ms3)
model3_pred <- sapply(1:nrow(fmel), function(i) {
  home_team <- which(teams == fmel$home[i])
  away_team <- which(teams == fmel$away[i])
  year <- which(year == fmel$year[i])
  skill_H <- ms3[, column_name("skill", year, home_team)] 
  skill_A <- ms3[, column_name("skill", year, away_team)]
  home_base_line <- ms3[, column_name("home_base_line", year)]
  away_base_line <- ms3[, column_name("away_base_line", year)]
  
  Goals_H <- rpois(n, exp(home_base_line + skill_H - skill_A))
  Goals_A <- rpois(n, exp(away_base_line + skill_A - skill_H))
  Goals_H_table <- table(Goals_H)
  Goals_A_table <- table(Goals_A)
  match_results <- sign(Goals_H - Goals_A)
  match_results_table <- table(match_results)
  
  mode_home_goal <- as.numeric(names(Goals_H_table)[ which.max(Goals_H_table)])
  mode_away_goal <- as.numeric(names(Goals_A_table)[ which.max(Goals_A_table)])
  match_result <-  as.numeric(names(match_results_table)[which.max(match_results_table)])
  rand_i <- sample(seq_along(Goals_H), 1)
  
  c(mode_home_goal = mode_home_goal, mode_away_goal = mode_away_goal, match_result = match_result,
    mean_home_goal = mean(Goals_H), mean_away_goal = mean(Goals_A),
    rand_home_goal = Goals_H[rand_i], rand_away_goal = Goals_A[rand_i],
    rand_match_result = match_results[rand_i])
})
model3_pred <- t(model3_pred)


hist(fmel$homeGoals, breaks= (-1:10) + 0.5, xlim=c(-0.5, 10))


hist(model3_pred[ , "mode_home_goal"], breaks= (-1:10) + 0.5, xlim=c(-0.5, 10))


hist(model3_pred[ , "mean_home_goal"], breaks= (-1:10) + 0.5, xlim=c(-0.5, 10))


hist(model3_pred[ , "rand_home_goal"], breaks= (-1:12) + 0.5, xlim=c(-0.5, 10))


mean(fmel$homeGoals == model3_pred[ , "mode_home_goal"], na.rm=T)
mean((fmel$homeGoals - model3_pred[ , "mean_home_goal"])^2, na.rm=T)


hist(fmel$match_result, breaks= (-2:1) + 0.5)


hist(model3_pred[ , "match_result"], breaks= (-2:1) + 0.5)


hist(model3_pred[ , "rand_match_result"], breaks= (-2:1) + 0.5)

mean(fmel$match_result == model3_pred[ , "match_result"], na.rm=T)

############################################################################################################

