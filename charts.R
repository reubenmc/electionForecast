df = read.csv("/Users/Reubenm/Documents/surveyMonkey/data/Election2016_Estimates.csv")

# key states #
target = c("Florida", "Ohio", "Pennsylvania", "Wisconsin", 
           "New Hampshire", "Minnesota", "Iowa", "Michigan", 
           "Nevada", "Colorado", "North Carolina")

# plot changes for Hilary for key states #
clintonDf = df %>% 
  dplyr::select(state, Clinton, date) %>% 
  filter(state %in% target) %>%
  group_by(state)

# reorder factor levels by date #
clintonDf$date = factor(clintonDf$date, levels = c("Oct 4 thru 10", "Oct 11 thru 17", "Oct 18 thru 24", "Oct 25 thru 31", "Nov 1 thru 7"))

ggplot(data=clintonDf, aes(x=date, y=Clinton, group=state, colour = state)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 4, linetype="dotted", color = "red") +
  labs(x="Date", y="Clinton Voters")

# plot changes for Trump for key states #
trumpDf = df %>% 
  dplyr::select(state, Trump, date) %>% 
  filter(state %in% target) %>%
  group_by(state)

# reorder factor levels by date #
trumpDf$date = factor(trumpDf$date, levels = c("Oct 4 thru 10", "Oct 11 thru 17", "Oct 18 thru 24", "Oct 25 thru 31", "Nov 1 thru 7"))

ggplot(data=trumpDf, aes(x=date, y=Trump, group=state, colour = state)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 4, linetype="dotted", color = "red") +
  labs(x="Date", y="Trump Voters")
  

