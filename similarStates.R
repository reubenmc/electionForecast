library(hclust)
library(lattice)

estimates = read.csv("/Users/Reubenm/Documents/surveyMonkey/data/Election2016_Estimates.csv")

df = estimates %>% 
  arrange(state) %>% 
  filter(date == "Nov 1 thru 7")

voters = df %>% 
  dplyr::select(-Unweighted.n, -date)

splom(~voters,groups=voters$state,auto.key=FALSE)

cor(unlist(voters[1,-1]), unlist(voters[5,-1]))

#cor(as.matrix(voters[1,-1]),t(as.matrix(voters[2:5,-1])))

ggplot(df, aes(Clinton, Trump)) + 
  geom_point()

rownames(voters) = voters$state
voters = voters[,2:ncol(voters)]

clusters = hclust(dist(voters))
plot(clusters)
