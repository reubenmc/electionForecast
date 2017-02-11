# Check for libraries and install if needed #
listOfPackages <- c("MCMCpack", "dplyr", "magrittr", "ggplot2")
NewPackages <- listOfPackages[!(listOfPackages %in% installed.packages()[,"Package"])]
if(length(NewPackages)>0) {install.packages(NewPackages,repos="http://cran.rstudio.com/")}

library(MCMCpack)
library(dplyr)
library(magrittr)

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
  return(res)
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
nSims = 1e4
win = as.data.frame(matrix(NA, nrow = 51, ncol = nSims))
for (i in 1:nrow(lastWeek)) {
  prior = priorData[i, 2:7]
  res = voteModel(prior, voteCounts[i,], nSims)
  win[i,] = apply(res, 1, function(x) ifelse(which.max(x) == 1, 1, 0))
}
# find total votes for each simulation #
votes = win * as.matrix(ev[,2])
votesTot = apply(votes, 2, sum)
# find out how many sims result in winning election #
pWin = sum(votesTot >= 270)/nSims



# for quantiles #
#res = apply(rdirichlet(nsims, post_alpha), 2, quantile, probs = c(0.025, 0.975))

####################################################################################

# find which states Hilary is most likely to win #



# for each state find conditional probability of  #
# clinton winning given that she won the election #
resClinton = win
rownames(resClinton) = ev[,1]

# P(win state x | Hillary wins election) #
winHill = resClinton[,which(votesTot >= 270)]
winState = apply(winHill, 1, sum)/nSims

# P(win state x | Hillary doesn't win election) #
loseHill = resClinton[,which(votesTot < 270)]
loseState = apply(loseHill, 1, sum)/nSims

# calculate full Bayes rule #
condProb = (winState*pWin) / ((winState*pWin) + (loseState*(1-pWin)))

############################
######## Trump ############
############################

# for each state calculate posterior estimates #
# find how many times Trump wins each state   #
nSims = 1e4
win = as.data.frame(matrix(NA, nrow = 51, ncol = nSims))
for (i in 1:nrow(lastWeek)) {
  prior = priorData[i, 2:7]
  res = voteModel(prior, voteCounts[i,], nSims)
  win[i,] = apply(res, 1, function(x) ifelse(which.max(x) == 2, 1, 0))
}
# find total votes for each simulation #
votes = win * as.matrix(ev[,2])
votesTot = apply(votes, 2, sum)
# find out how many sims result in winning election #
pWin = sum(votesTot >= 270)/nSims

# for each state find conditional probability of  #
# clinton winning given that she won the election #
resTrump = win
rownames(resTrump) = ev[,1]

# P(win state x | Trump wins election) #
winTrump = resTrump[,which(votesTot >= 270)]
winState = apply(winTrump, 1, sum)/nSims

# P(win state x | Trump doesn't win election) #
loseHill = resTrump[,which(votesTot < 270)]
loseState = apply(loseHill, 1, sum)/nSims

# calculate full Bayes rule #
condProb = (winState*pWin) / ((winState*pWin) + (loseState*(1-pWin)))
