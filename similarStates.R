library(hclust)
library(lattice)
library(dplyr)
library(ape)

estimates = read.csv("/Users/Reubenm/Documents/surveyMonkey/data/Election2016_Estimates.csv")

# subset data #
df = estimates %>% 
  arrange(state) %>% 
  filter(date == "Nov 1 thru 7")

voters = df %>% 
  dplyr::select(-Unweighted.n, -date)

rownames(voters) = voters$state
voters = voters[,2:ncol(voters)]

# perform clustering #
clusters = hclust(dist(voters))

# plot dendogram #
# tweeking parameters
op = par(bg = "#DDE3CA")
plot(clusters, col = "#487AA1", col.main = "#45ADA8", col.lab = "#7C8071", 
     col.axis = "#F38630", lwd = 3, lty = 3, sub = "", hang = -1, axes = FALSE)
# add axis
axis(side = 2, at = seq(0, 400, 100), col = "#F38630", labels = FALSE, 
     lwd = 2)
# add text in margin
mtext(seq(0, 400, 100), side = 2, at = seq(0, 400, 100), line = 1, 
      col = "#A38630", las = 2)
