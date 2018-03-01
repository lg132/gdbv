<<<<<<< HEAD
?catchdata 

=======
#library
library(c(ggplot2,tidyr,dplyr))

regiondf <- listregions("fishing-entity")
>>>>>>> cfb2f9d6e42c0ee1c42d52b3e78dfbacd1cece0d
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

<<<<<<< HEAD
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



=======
#==========================================================================================
# DATA FRAME: FOR EVERY COUNTRY CATCH TIME SERIES (df_fishing_all)

regiondf
#Error Message:
#Unknown Fishing Country: nr 192, id 213
#Fishing Country Unknown: nr.61, id 223
#Johnston Atoll nr.93, id 216

countrynames <- regiondf$title
countrynames <- countrynames[-c(61,192,93)]
countryids <- regiondf$id
countryids <- countryids[-c(61,192,93)]

df_fishing_all<- catchdata('fishing-entity', countryids[1], measure='tonnage', dimension='country')


for (i in 2:length(countryids)){ #beginning with 2.column (first one is year)
  newcol<- catchdata('fishing-entity', countryids[i], measure='tonnage', dimension='country') 
          #every column is the time series of fish catches for one country
  df_fishing_all[countrynames[i]] <-newcol[,2]
          #add this column to the big data frame 
}

dump("df_fishing_all","df_fishing_all.Rdmpd")
bigdf <- source("df_fishing_all.Rdmpd")
bigdf <- bigdf[[1]]

#========which country catches in average less than 2,000,000 tonnes of fish per year?======
#library(tidyverse)
#fishing_high <- df_fishing_all %>% select(mean > 2000000)
#avg <- apply(df_fishing_all,2,mean)


# preparing data for stacked chart
df1 <- df_fishing_all
df2 <- df1 %>% gather(., key="country", value="tonnage", -c(years)) #im ggplot
df3 <- df2 %>% group_by(country) %>% summarise(avg=mean(tonnage))

df4 <- df3 %>% filter(avg<2000000) 
df5 <- df3 %>% filter(avg>2000000) 
summary(df5)

fishing_high <- df2 %>% filter(country %in% df5$country) #all countries with more than 2mio catches
others1 <- df2 %>% filter(country %in% df4$country)
others2 <- others1 %>% group_by(years) %>% summarise(tonnage = sum(tonnage))
others3 <- others2 %>% mutate(country = "Others") %>% select(years,country,tonnage)

fish.final <- bind_rows(fishing_high,others3)

#ggplot of fishing_high
ggplot(fishing_high, aes(x=years, y=tonnage))+
  geom_area(aes(fill=country))+
  theme(legend.position = "none")

#ggplot of fishing + others
ggplot(fish.final, aes(x= years, y=tonnage)) +
  geom_area(aes(fill=factor(country, levels=c(df5$country, "Others"))))+
  theme(legend.position = "right") +
  scale_fill_hue (l=30)



 
>>>>>>> cfb2f9d6e42c0ee1c42d52b3e78dfbacd1cece0d


