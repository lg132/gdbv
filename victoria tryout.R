?catchdata 

#=============================================================================
# Question 1: Visualize who is catching how much globally:
#=============================================================================

#================== How to make a stacked line chart ========================
#online example 1: https://stackoverflow.com/questions/13644529/how-to-create-a-stacked-line-plot
set.seed(11)
df <- data.frame(a = rlnorm(30), b = 1:10, c = rep(LETTERS[1:3], each = 10))
#library(ggplot2)
ggplot(df, aes(x = b, y = a, fill = c)) + geom_area(position = 'stack')

#-----------------------
#online example 2 (better!): http://ggplot2.tidyverse.org/reference/position_stack.html
# Stacking is also useful for time series
series <- data.frame(
  time = c(rep(1, 4),rep(2, 4), rep(3, 4), rep(4, 4)),
  type = rep(c('a', 'b', 'c', 'd'), 4),
  value = rpois(16, 10)
)
ggplot(series, aes(time, value)) +
  geom_area(aes(fill = type))
#------------------------

#===============
# DATA FRAME: FOR EVERY COUNTRY CATCH TIME SERIES

regiondf
countrynames <- regiondf$title
countryids <- regiondf$id

df_fishing_all<- catchdata('fishing-entity', countryids[1], measure='tonnage', dimension='country')
View(df_fishing_all)

for (i in 2:length(countryids)){
  newcol<- catchdata('fishing-entity', countryids[i], measure='tonnage', dimension='country')
  df_fishing_all[countrynames[i]] <-newcol[,2]
}

# which country catches in average less than 2,000,000 tonnes of fish per year?





