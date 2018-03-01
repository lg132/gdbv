library(seaaroundus)
library(ggplot2)
library(tidyr)
#####
#regiondf <- listregions('fishing-entity')
#View(regiondf)
#lr <- as.data.frame(listregions("fishing-entity"))
#head(lr)
#af  <- sapply(X=lr, FUN=catchdata, region='fishing-entity', measure="tonnage", dimension = "country")
#af

#===============
# DATA FRAME: FOR EVERY COUNTRY CATCH TIME SERIES

regiondf <- listregions("fishing-entity")
countrynames <- regiondf$title[-c(61,93,192)]
countryids <- regiondf$id[-c(61,93,192)]

region <- gather(regiondf, key="title", value="id")

View(region)
catchdata('fishing-entity', 66, measure='tonnage', dimension='country')



#leonie:
#test <- eapply(X=countryids, FUN=catchdata, region="fishing-identity", dimension="country")
#View(test)




df_fishing_all<- catchdata('fishing-entity', countryids[1], measure='tonnage', dimension='country')
View(df_fishing_all)
#Calculation of Victorias DF
for (i in 2:length(countryids)){
  newcol<- catchdata('fishing-entity', countryids[i], measure='tonnage', dimension='country')
  df_fishing_all[countrynames[i]] <-newcol[,2]
}


# which country catches in average less than 2,000,000 tonnes of fish per year?

#load Victoria's data.frame
df_fishing_all <- source("/Users/Fex/Dropbox/GeoVis/df_fishing_all.Rdmpd")



#Victoria's data.frame
df_fishing_all <- source("/Users/Fex/Dropbox/GeoVis/df_fishing_all.Rdmpd")
library(ggplot2)
library(dplyr)
library(tidyr)
#####
# Example stacked chart: http://ggplot2.tidyverse.org/reference/position_stack.html
series <- data.frame(
  time = c(rep(1, 4),rep(2, 4), rep(3, 4), rep(4, 4)),
  type = rep(c('a', 'b', 'c', 'd'), 4),
  value = rpois(16, 10)
)
ggplot(series, aes(time, value)) +
  geom_area(aes(fill = type))  
#####
# preparing data for stacked chart
df1 <- df_fishing_all[[1]]
df2 <- df1 %>% gather(., key="country", value="tonnage", -c(years))
df3 <- df2 %>% group_by(country) %>% summarise(avg=mean(tonnage))

df4 <- df3 %>% filter(avg<2000000)
df5 <- df3 %>% filter(avg>2000000)

fishalot <- df2 %>% filter(country %in% df5$country)
fishother <- df2 %>%  filter(country %in% df4$country)

fishother2 <-  fishother %>%  group_by(years) %>% summarise(tonnage = sum(tonnage))

fishother3 <- fishother2 %>% mutate(country = "Others") %>% select(years, country, tonnage)

fish.final <- bind_rows(fishalot, fishother3)

ggplot(fish.final, aes(x=years, y=tonnage))+
  geom_area(aes(fill=factor(country, levels=c(df5$country, "Others"))))+
  theme(legend.position = "right")+
  scale_fill_hue (l=40)+
  guides(fill=guide_legend(title="Countries"))




dfleonie <- read.delim("/Users/Fex/Dropbox/GeoVis/df_Q2")
View(df.ex2)

df.ex2 <- dfleonie %>% group_by(years) %>%  summarise(landings =sum(landings), discards=sum(discards))
df.ex2 <- df.ex2 %>% mutate(total =( landings+ discards)) %>% 
  mutate(percilandi = (landings/total)) %>% 
  mutate(percidiscardi = (discards/total))

df.ex2.1 <- df.ex2 %>% select(-landings, -discards, -total) %>% gather(., key="catchtype", value="percentage", -years) 

ggplot(df.ex2.1, aes(x=years, y=percentage))+
  geom_area(aes(fill=catchtype))+
  scale_fill_brewer(palette = "Dark2")


?catchdata
#"taxon", "commercialgroup", "functionalgroup", "country", "sector", "catchtype", "reporting-status", "layer" 
