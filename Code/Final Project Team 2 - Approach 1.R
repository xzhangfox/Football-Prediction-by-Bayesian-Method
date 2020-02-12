# DATS 6450 Bayesian Final Project Team 2
# Team Member: Hao Ning, Xi Zhang
# Approach 1 by Hao Ning
############################################################################################################
# load packages
library('dplyr')
library('stringr')
library('ggplot2')
library('rjags')
library('coda')
library('data.table')
library('reshape2')
source('DBDA2E-utilities.R')

############################################################################################################
# LoadData
df_raw = read.csv('FMEL_Dataset.csv')
sum(is.na(df_raw$col))
# checked, no missing values

# Preprocessing, only include division 1
df_division1 = filter(df_raw, division == '1')

# After 1997, there are total 20 teams in divison1
df_division1$year = paste(str_sub(df_division1$season, start = 1, end = 4))
df_division1 = filter(df_division1, year>=1997)
df = select(df_division1,-c(id,season,division,date,timestamp))

# rename columns
df = rename(df, home=localTeam, away=visitorTeam, homeGoals = localGoals, awayGoals = visitorGoals)
df$score_diff = df$homeGoals-df$awayGoals
# results 1: home win, 0: draw, -1: away win
#df$results_code = sign(df$homeGoals - df$awayGoals)
df = df %>% mutate(results = ifelse(df$score_diff > 0, "Home", ifelse(df$score_diff < 0,"Away","Draw")))
str(df)
df$home = as.character(df$home)
df$away = as.character(df$away)
str(df)
unique(df$year)


############################################################################################################
# Home advantage analysis
# plot home_win/draw/away_win distribution
df_results =  df %>% group_by(results, year) %>% summarise(games = n()) %>% arrange(year)
df_results = full_join(df_results, df_results %>% group_by(year) %>% summarise(total_games_year = sum(games)), by = "year")
df_results = full_join(df_results, df_results %>% group_by(year) %>% filter(results == "Home") %>% select(year,Home_wins = games), by = "year")
df_results$Home_wins_percent = paste0(as.character(round(df_results$Home_wins*100/df_results$total_games_year,2)),"%")

openGraph( width=12 , height=8 )
ggplot(df_results) + geom_bar(aes(year, games, fill=results), stat = 'identity') +
  geom_text(aes(x = year, y = games, label = Home_wins_percent), size = 3, hjust = 0.5, vjust = 1, position = "fill") + 
  ggtitle('Game Results Since 1997') + theme(plot.title = element_text(hjust = 0.5))
saveGraph( file="Game Results Since 1997" , type="png" )

home_win = df_results$Home_wins*100/df_results$total_games_year
print(mean(home_win))

############################################################################################################
# Work on season/year from 2015 to 2017
mydata = filter(df, year >= '2015')

# our target is to predict home win probability
mydata = mydata %>% mutate(home_win = ifelse(mydata$score_diff > 0, 1, 0))

teams = unique(c(mydata$home, mydata$away))
seasons <- unique(mydata$year)
n_teams = length(teams)
n_games = nrow(mydata)
n_seasons = length(seasons)

############################################################################################################
#############               pre processing and EDA completed                                  ##############
############################################################################################################


# start to build model
# Approch 1 - Using home win 0,1 for modelling and prediction - by Hao Ning
# assign ID for home and away team
mydata$home_ID = as.numeric(factor(mydata$home, levels=teams))
mydata$away_ID = as.numeric(factor(mydata$away, levels=teams))
str(mydata)

mydata_17 = filter(mydata, year==2017)
############################################################################################################
# first, we will work on all 3 seasons from 2015-2017
# we will compare the team abilities for good teams vs good teams and good teams vs normal team
# find out the best teams !?
# compare if there's significant differece between the teams

datalist1 = list(
  home_ID = mydata$home_ID,
  away_ID = mydata$away_ID,
  home_win = mydata$home_win
)


############################################################################################################

modelString_RM = "
model{

# team ability, log ability ~ dnorm
for (i in 1:24)
{
  log_ability[i] ~ dnorm(0,1/performance^2)
  ability[i] = exp(log_ability[i])
  
}

# using 900 games of 1180 total for likelihood
for (i in 1:900)
{
  p[i] = (ability[home_ID[i]]*h)/(ability[home_ID[i]]*h + ability[away_ID[i]])
  home_win[i] ~ dbin(p[i], 1)
}
  
# a team mihgt perform better/equal/worse than their ability/expectations
# we assume they usualy perform with some ups and downs, but generally as they are
# there will be variations, define here
performance ~ dunif(0,2)

# home advantage truely exist, define a h factor
h ~ dunif(1,1.5)

# predict results, when Real Madrid (ID 15) playing at home

for (away in 1:24){
  p_vs[away] = (ability[15]*h)/(ability[15]*h + ability[away])
  h_win[away] ~ dbin(p_vs[away],1)
}
  
}
"
writeLines( modelString_RM , con="TEMPmodel_RM.txt")

burn_in = 1000
steps = 10000
thin = 1

variable_names_RM=c("h_win","ability")
jagsModel_RM = jags.model( "TEMPmodel_RM.txt",data = datalist1)

s_RM <- coda.samples(jagsModel_RM, 20000, thin = 1, variable.names = variable_names_RM)

# trace plot of ability of teams
for(i in seq(1, 24, by = 4)){
  openGraph( width=8 , height=8 )
  plot(s_RM[, i:(i+3)])
}

####################################################################################################
####################################################################################################
pred_RM = s_RM %>% as.matrix() %>% as.data.frame()
pred_RM = select(pred_RM,-c('h_win[15]'))
##########################################################################
# fun part, Barcelona and Real Madrid, which team is better in 2015-2017 3 seasons ? 
ms_RM = as.matrix(s_RM)

##compare all team, I commend this part, otherwise there'll be too many plots
# for (i in 1:24){
#   for (j in 1:24){
#     if (i==j) next
#     openGraph( width=5 , height=5 )
#     plotPost(ms_RM[, i] - ms_RM[,j],  compVal = 0)
#   }
# }

# team ability comparison
# Barcelona vs Real Madrid, two good teams
openGraph( width= 7, height=7)
plotPost(ms_RM[, 13] - ms_RM[,15],  compVal = 0, main= "Barcelona - Real Madrid 2015-2017",xlab="Team Abilities Difference" )
saveGraph( file="Barcelona - Real Madrid 2015-2017" , type="png" )
# Espanol vs Real Madrid, normal vs good team
openGraph( width= 7, height=7)
plotPost(ms_RM[, 3] - ms_RM[,15],  compVal = 0, main= "Espanol  - Real Madrid 2015-2017",xlab="Team Abilities Difference" )
saveGraph( file="Espanol - Real Madrid 2015-2017" , type="png" )
####################################################################################################
# BOXPLOT
#openGraph( width=12 , height=8)
#ggplot(data = melt(ability_teams), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable)) + 
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  ggtitle("Boxplot of Team Abilities") + theme(plot.title = element_text(hjust = 0.5)) 
#saveGraph( file="Boxplot of Team Abilities 2015-2017" , type="png" )
####################################################################################################

away_RM  = paste("h_win[",1:24,"]", sep="")
away_RM = away_RM[ away_RM != 'h_win[15]' ] 

h_win_RM = select(pred_RM, away_RM)
p_RM_win = colMeans(h_win_RM) 

for (i in 1:23){
  if(i==15) next
  print(paste("Real Madrid playing at home, winning chance against team", i , ':',p_RM_win [i]*100 ,"%"))
}

####################################################################################################

# study on season 2017 for all teams
# We will model the first 300 games then make predictions using the rest of the games (80)
datalist2 = list(
  home_ID = mydata_17$home_ID,
  away_ID = mydata_17$away_ID,
  home_win = mydata_17$home_win
)


modelString_all = "
model{

# team ability
for (i in 1:24)
{
  log_ability[i] ~ dnorm(0,1/performance^2)
  ability[i] = exp(log_ability[i])

}

# likelihood
for (i in 1:200)
{
  p[i] = (ability[home_ID[i]]*h)/(ability[home_ID[i]]*h + ability[away_ID[i]])
  home_win[i] ~ dbin(p[i], 1)
}

# a team mihgt perform better/equal/worse than their ability/expectations
# we assume they usualy perform with some ups and downs, but generally as they are
# there will be variations, define here
performance ~ dunif(0,2)
# home advantage truely exist, define a h factor
h ~ dunif(1,1.5)

# predict
for (home in 1:24){
  for (away in 1:24){
      p_vs[home,away] = (ability[home]*h)/(ability[home]*h + ability[away])
      h_win[home,away]~ dbin(p_vs[home,away],1)
    }
}
}
"
writeLines( modelString_all , con="TEMPmodel_all.txt")

burn_in = 1000
steps = 10000
thin = 1

variable_names_all=c("h_win","ability")
jagsModel_all = jags.model( "TEMPmodel_all.txt",data = datalist2)

s_all = coda.samples(jagsModel_all, 20000, thin = 1, variable.names = variable_names_all)
ms_all = s_all%>% as.matrix()
pred_all = s_all %>% as.matrix() %>% as.data.frame()
pred_all_home_away = pred_all[25:600]

# this is the win probability of home vs away for all teams !
pred_home_win_all = colMeans(pred_all_home_away) %>% as.data.frame()
HvsA = rownames(pred_home_win_all)
pred_home_win_all = data.frame(HvsA,pred_home_win_all[1], row.names = NULL)
pred_home_win_all = rename(pred_home_win_all, HvsA_pred=.)

mydata_17$HvsA = paste('h_win[',mydata_17$home_ID,',',mydata_17$away_ID,']', sep='')

# we use first 200 games (20 round) for modelling, use the rest for prediction
# When predicted probability > 0.6, > 0.7, we believe the home team will win, check the accuracy
df_pred_0.6 = merge(mydata_17, pred_home_win_all, by = "HvsA") %>% filter(round>20 & HvsA_pred>0.6)
df_pred_0.7 = merge(mydata_17, pred_home_win_all, by = "HvsA") %>% filter(round>20 & HvsA_pred>0.7)
# the dataframe is are the subset of predicted probability that >0.6
# thus the accuracy can be calculated by the sum of home_win/nrow
accuracy_0.6 = sum(df_pred_0.6$home_win)/nrow(df_pred_0.6)
print(paste('P_pred >0.6, we bet the home team win, prediction accuracy :',round(accuracy_0.6*100,2) ,"%"))
accuracy_0.7= sum(df_pred_0.7$home_win)/nrow(df_pred_0.7)
print(paste('P_pred >0.7, we bet the home team win, prediction accuracy :',round(accuracy_0.7*100,2) ,"%"))

####################################################################################################
# final ability rank for season 2017
# We will compare with the final team rank of this season
ability_teams_17 = select(pred_all, c(1:24))
old_names_17 = colnames(ability_teams_17 )
new_names_17 = c(teams)
ability_teams_17 = setnames(ability_teams_17, old=old_names_17, new=new_names_17) 
ability_teams_17 = ability_teams_17[-c(5,7,10,21)]
ability_teams_avg_17 = colMeans(ability_teams_17)%>% sort(decreasing = TRUE) %>% as.data.frame()
ability_teams_avg_17$rank = seq.int(nrow(ability_teams_avg_17))
ability_teams_avg_17 = rename(ability_teams_avg_17, ability_avg=.)
print(ability_teams_avg_17)
####################################################################################################

# Approach 1 complete

# Summary
# Game results prediction is not an easy field since many factors can impact the outcomes.
# Approach 1 that we have demonstrated could set things straight by only considering home team winning probability 
# with parameters of team abilities, performance variations and home advantages.
# We can get a pretty good accuracy using this model
# Including more factors will be very interesting and for future works to dive deeper!
# Thank you!