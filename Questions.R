#=============================================================================
# Question 1: Visualize who is catching how much globally:
#=============================================================================

library(gglot2)
library(tidyr)
library(dplyr)
library(seaaroundus)

#-------------------------------------------------------------------
# DATA FRAME: FOR EVERY COUNTRY CATCH TIME SERIES (df_fishing_all)
#-------------------------------------------------------------------
regiondf <- listregions("fishing-entity")

#Error Messages for this countries --> take them out
#Unknown Fishing Country: nr 192, id 213
#Fishing Country Unknown: nr.61, id 223
#Johnston Atoll nr.93, id 216

countrynames <- regiondf$title[-c(61,192,93)]
countryids <- regiondf$id[-c(61,192,93)]

df_fishing_all<- catchdata('fishing-entity', countryids[1], measure='tonnage', dimension='country')

#-------------for-loop----------------------------
for (i in 2:length(countryids)){ #beginning with 2.column (first one is year)
  newcol<- catchdata('fishing-entity', countryids[i], measure='tonnage', dimension='country') 
  #every column is the time series of fish catches for one country
  df_fishing_all[countrynames[i]] <-newcol[,2]
  #add this column to the big data frame 
}
#--------------------------------------------------
#command to save data frame and read in:
#dump("df_fishing_all","df_fishing_all.Rdmpd")
#df_fishing_all <- source("df_fishing_all.Rdmpd")
#df_fishing_all <- df_fishing_all[[1]]

#-------------------------------------------------------------------
# PREPARING DATA FOR STACKED CHART
#-------------------------------------------------------------------
df1 <- df_fishing_all
df2 <- df1 %>% gather(., key="country", value="tonnage", -c(years)) 
df3 <- df2 %>% group_by(country) %>% summarise(avg=mean(tonnage))

df4 <- df3 %>% filter(avg<2000000) 
df5 <- df3 %>% filter(avg>2000000) 

fishing_high <- df2 %>% filter(country %in% df5$country) #all countries with more than 2mio catches
others1 <- df2 %>% filter(country %in% df4$country)
others2 <- others1 %>% group_by(years) %>% summarise(tonnage = sum(tonnage))
others3 <- others2 %>% mutate(country = "Others") %>% select(years,country,tonnage)

fish.final <- bind_rows(fishing_high,others3)

#-------------------------------------------------------------------
# PLOT STACKED CHART
#-------------------------------------------------------------------

ggplot(fish.final, aes(x= years, y=tonnage)) +
  geom_area(aes(fill=factor(country, levels=c(df5$country, "Others"))))+  
  #levels: to have "others" last (default of ggplot would be alphabetical order)
  theme(legend.position = "right") + #add legend on the right 
  guides(fill=guide_legend(title="Countries")) + #change the legend title 
  scale_fill_hue (l=30) #just darken/lighten the colours 
