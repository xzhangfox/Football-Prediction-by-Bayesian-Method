# Predicting Football Match Results of Spanish League using Bayesian Hierarchical Model
Established Bayesian models and leveraged historical data to predict home and away teams’ performance. Achieved over 80% prediction accuracy of winning percentage and 53% of the goal prediction.
<div align="center">
<img src="https://github.com/f0000000x/Football-Prediction-by-Bayesian-Method/blob/master/Images/Banner.png" align=center />
</div>

Team Members:

Xi Zhang, Hao Ning

## Requirements
* R
* JAGS
* mcmcplots
* [HDIofMCMC.R]()
* [plotPost.R]()

## Background
Soccer(or internationally, football) is one of the most popular sports on our planet, with a well-developed industry worth more than $400 billion and billions of fans(estimated) around the world1. Predicting the matches results have always attracted many attentions and analyzing the game results proved to be very helpful with the business growth and player training. 

## Motivation
There are many machine learning approaches for game prediction, however, we believe the Bayesian approach could be very helpful in this scenario (given reliable historical data). We will work on the data from the Spanish league, specifically the season 2015-2017.

## Data Description
Data Set with the football matches of the Spanish league of the 1st and 2nd division from the 1970-71 to 2016-17 season, has been created with the aim of opening a line of research in the Machine Learning, for the prediction of results (1X2) of football matches.
[Kaggle](https://www.kaggle.com/ricardomoya/football-matches-of-spanish-league)

## Models
* [Approach 1](https://github.com/f0000000x/Football-Prediction-by-Bayesian-Method/blob/master/Code/Final%20Project%20Team%202%20-%20Approach%201.R)
Using home win 0,1 for modelling and prediction
* [Approach 2](https://github.com/f0000000x/Football-Prediction-by-Bayesian-Method/blob/master/Code/Final%20Project%20Team%202%20-%20Approach%202.R)
Using Hierarchical Model with Poisson Distribution for Predicting Goals

## References
* Bååth, Rasmus. "Modeling Match Results in Soccer using a Hierarchical Bayesian Poisson Model." 2015, Sumsar.
* Sawe, Benjamin Elisha. "The Most Popular Sports in the World." WorldAtlas, Apr. 5, 2018, worldatlas.com/articles/what-are-the-most-popular-sports-in-the-world.html
* Shahzeb, Farheen. "The Evolution and Future of Analytics in Sport". June 22, 2017, Proem Sports

