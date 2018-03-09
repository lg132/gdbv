#library
library(c(ggplot2,tidyr,dplyr))

regiondf <- listregions("fishing-entity")
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
df_fishing_all <- source("df_fishing_all.Rdmpd")
df_fishing_all <- df_fishing_all[[1]]
write.table(df_fishing_all, "df_total_catch", sep="\t")

#========which country catches in average less than 2,000,000 tonnes of fish per year?======
#library(tidyverse)
#fishing_high <- df_fishing_all %>% select(mean > 2000000)
#avg <- apply(df_fishing_all,2,mean)


# preparing data for stacked chart
df1 <- df_fishing_all
df2 <- df1 %>% gather(., key="country", value="tonnage", -c(years)) #im ggplot
df2.1 <- df2 %>% filter(years>=1960, years<=1990)
df3 <- df2.1 %>% group_by(country) %>% summarise(avg=mean(tonnage))
View(df2)

df4 <- df3 %>% filter(avg<2000000) 
df5 <- df3 %>% filter(avg>2000000) 
summary(df5)

fishing_high <- df2.1 %>% filter(country %in% df5$country) #all countries with more than 2mio catches
others1 <- df2.1 %>% filter(country %in% df4$country)
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
  #levels: to have "others" last (default of ggplot would be alphabetical order)
  theme(legend.position = "right") + #add legend on the right 
  guides(fill=guide_legend(title="Countries")) + #change the legend title 
  scale_fill_hue (l=30) #just darken/lighten the colours 


#==============================================================
#                     05.03.18
#==============================================================
#Von Felix:
#Was wird gefischt?

View(listregions("lme"))
eezost <- catchdata(region="eez", 278, dimension = "country", measure="tonnage")

eezost2 <- eezost %>% gather(., key="country", value="tonnage", -c(years))

ggplot(eezost2, aes(x=years, y=tonnage))+
  geom_area(aes(fill=factor(country)))+
  theme(legend.position = "right")+
  scale_fill_hue(l=40)+
  guides(fill=guide_legend(title="Countries"))+
  labs(title="eez ostsee")

eeznord <- catchdata(region="eez", 277, dimension = "taxon", measure="tonnage")

eeznord2 <- eeznord %>% gather(., key="taxon", value="tonnage", -c(years))

ggplot(eeznord2, aes(x=years, y=tonnage))+
  geom_area(aes(fill=factor(taxon)))+
  theme(legend.position = "right")+
  scale_fill_hue(l=40)+
  guides(fill=guide_legend(title="Countries"))+
  labs(title="eez nordsee")
?catchdata

df3<-order(df3)

View(df2)
data.frame(arrange(df2,tonnage))

#==================================================================
# EEZ WORLD MAP STUFF
#==================================================================

attribute <- read.csv("~/Dropbox/Geovis/attribute.csv")
View(attribute)
df_eez <-read.delim("df_eez")
View(df_eez)
df_eez_avg <- df_eez %>% group_by(eez) %>% summarise(avg=mean(landings+discards))

att_names <- attribute$Country
is.element(att_names,eez_names) 

library(sf)

#------ plot map
eez_shp <- st_read("World_EEZ_v8_2014.shp")
plot(eez_shp$geometry)
#0--------------

listeez <- listregions("eez")
names(listeez) <- c("sau_id","eez")

sau_eez <- join(df_eez_avg,listeez, by="eez")
#write.csv(sau_eez, "sau_eez.csv")

saui <-read.delim("sau_eez_felix.txt") #HAENDISCH VERAENDERT!!
names(saui)[1] <- "Country"
country.sau_id <- saui[,-2]
write.table(country.sau_id, "country.sau_id", sep="\t")

#==== JOIN
mapjoin <- join(saui,attribute, by="Country",type="right")
write.table(mapjoin, "mapjoin", sep="\t")

eez_shp$avg <- mapjoin$avg #SHAPEFILE INKLUSIVE AVERAGE





#========RENAME!


write.table(df_fishing_all, "df_total_catch", sep="\t") 
#write.table(df_Q2, "df_discards", sep="\t") 
