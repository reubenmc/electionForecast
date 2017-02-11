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
win = as.data.frame(matrix(NA, nrow = 51, ncol = 1e6))
for (i in 1:nrow(lastWeek)) {
  prior = priorData[i, 2:7]
  res = voteModel(prior, voteCounts[i,], 1e6)
  win[i,] = apply(res, 1, function(x) ifelse(which.max(x) == 1, 1, 0))
}
# find total votes for each simulation #
votes = win * as.matrix(ev[,2])
votes = apply(votes, 2, sum)
# find out how many sims result in winning election #
sum(votes >= 270)/1e6



# for quantiles #
#res = apply(rdirichlet(nsims, post_alpha), 2, quantile, probs = c(0.025, 0.975))


win = as.data.frame(matrix(NA, nrow = 51, ncol = 1e4))

prior = priorData[1, 2:7]
res = voteModel(prior, voteCounts[1,], 1e4)
win[1,] = apply(res, 1, function(x) ifelse(which.max(x) == 1, 1, 0))

