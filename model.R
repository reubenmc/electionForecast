# Check for libraries and install if needed #
listOfPackages <- c("MCMCpack", "dplyr", "magrittr", "ggplot2")
NewPackages <- listOfPackages[!(listOfPackages %in% installed.packages()[,"Package"])]
if(length(NewPackages)>0) {install.packages(NewPackages,repos="http://cran.rstudio.com/")}

library(MCMCpack)
library(dplyr)
library(magrittr)
library(ggplot2)
set.seed(3)

# data #
ev = read.csv("/Users/Reubenm/Documents/surveyMonkey/data/Election2016_Electoral_Votes.csv")
ev = arrange(ev, state)
estimates = read.csv("/Users/Reubenm/Documents/surveyMonkey/data/Election2016_Estimates.csv")

# split data between last week and first 3 weeks #
lastWeek = estimates %>% 
  arrange(state) %>% 
  filter(date == "Nov 1 thru 7")

firstWeeks = estimates %>% 
  arrange(state) %>% 
  filter(date != "Nov 1 thru 7")

# create counts of votes from proportions and number surveyed #
voteCounts = round(lastWeek[,2:7]*lastWeek[,8])

## create Bayesian dirchelt-multinomial model to a single state  ##
## inputs are prior parameters as vector = lenght of num states  ##
## votes as counts for each cat and number of times to run       ##
voteModel = function(priorParam, votes, nsims) {
  # prior #
  prior_alpha = unlist(priorParam)
  # posterior parameters # 
  post_alpha = prior_alpha + unlist(votes)
  res = rdirichlet(nsims, post_alpha)
  # 95% CI #
  ci = apply(res, 2, quantile, probs = c(0.025, 0.975))
  # remove obs not in CI #
  res = res[res[,1] >= ci[1,1] & res[,1] <= ci[2,1],]
  return(list(res, ci))
}

# find prior based as mean of estimates #
# for past 4 weeks for each state       #
priorData = firstWeeks %>% 
  dplyr::select(-Unweighted.n, -date) %>% 
  group_by(state) %>% 
  summarize(Clinton = mean(Clinton), Trump = mean(Trump), 
            Johnson = mean(Johnson), Stein = mean(Stein), 
            McMullin = mean(McMullin), No.Answer = mean(No.Answer))

# for each state calculate posterior estimates #
# find how many times Hilary wins each state   #
nSims = 10000
winHil = as.data.frame(matrix(NA, nrow = 51, ncol = nSims))
winTrump = as.data.frame(matrix(NA, nrow = 51, ncol = nSims))
ciHil = as.data.frame(matrix(NA, nrow = 51, ncol = 2))
ciTrump = as.data.frame(matrix(NA, nrow = 51, ncol = 2))

for (i in 1:nrow(lastWeek)) {
  # find prior from previous data #
  prior = priorData[i, 2:7]
  res = voteModel(prior, voteCounts[i,], 10528)
  winHil[i,] = apply(res[[1]], 1, function(x) ifelse(which.max(x) == 1, 1, 0))
  winTrump[i,] = apply(res[[1]], 1, function(x) ifelse(which.max(x) == 2, 1, 0))
  ciHil[i,] = res[[2]][,1]
  ciTrump[i,] = res[[2]][,2]
}
# find total votes for each simulation #
votes = winHil * as.matrix(ev[,2])
votesTot = apply(votes, 2, sum)
# find out how many sims result in winning election #
pWin = sum(votesTot >= 270)/nSims
pWin

# examine CIs #
CIs = cbind(ciHil, ciTrump)
rownames(CIs) = ev$state
overCIs = CIs[c('Arizona', 'Colorado', 'Delaware', 
                'Florida', 'Georgia', 'Maine', 'Michigan', 
                'Mississippi', 'Nevada', 'New Hampshire', 
                'New Mexico', 'Pennsylvania', 'South Carolina', 
                'Wisconsin'),]
# see how many EVs these represent #
sum(ev$ev[ev$state %in% rownames(overCIs)])

####################################################################################

# find which states Hilary is most likely to win when she wins election #
# P(win state x | Hillary wins election) #
rownames(winHil) = ev[,1]
# only inculde winning sims #
loseHil = winHil[,which(votesTot < 270)]
loseHil = apply(loseHil, 1, sum)/nSims
winHil = winHil[,which(votesTot >= 270)]
winHil = as.data.frame(apply(winHil, 1, sum))
winHil$probability = winHil$`apply(winHil, 1, sum)`/nSims
colnames(winHil) = c('Number of wins', 'Probability')


winHil$probability2 = winHil$`Number of wins`/max(winHil$`Number of wins`)

# find which states Trump is most likely to win when he wins election #
# P(win state x | Trump wins election) #
rownames(winTrump) = ev[,1]
# only inculde winning sims #
loseTrump = winTrump[,which(votesTot >= 270)]
loseTrump = apply(loseTrump, 1, sum)/nSims
winTrump = winTrump[,which(votesTot < 270)]
winTrump = as.data.frame(apply(winTrump, 1, sum))
winTrump$probability = winTrump$`apply(winTrump, 1, sum)`/nSims
colnames(winTrump) = c('Number of wins', 'Probability')

winTrump$probability2 = winTrump$`Number of wins`/max(winTrump$`Number of wins`)


# calculate full Bayes rule for Hilary #
condProb = (winHil$probability2*pWin) / ((winHil$probability2*pWin) + (loseHil*(1-pWin)))

# calculate full Bayes rule for Trump #
condProb = (winTrump$probability2*(1-pWin)) / ((winHil$probability2*(1-pWin)) + (loseTrump*(pWin)))
 