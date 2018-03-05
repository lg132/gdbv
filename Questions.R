#=============================================================================
# Question 1: Visualize who is catching how much globally:
#=============================================================================

library(ggplot2)
library(tidyr)
library(plyr)
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
df_fishing_all <- df_fishing_all[[1]]

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


#=============================================================================
# Question 2: Quantify by-catch ratios
#=============================================================================
# Plot again, this time grouped by “catch_type” and with a percentage scale
# on the y-axis (instead of tonnes): landings vs. discards. Years on the xaxis.

regiondf <- listregions("fishing-entity")
countrynames <- regiondf$title[-c(61,192,93)]
countryids <- regiondf$id[-c(61,192,93)]

#------  getting the data:
df_Q2n <- lapply(X = countryids, FUN = catchdata, region="fishing-entity", measure="tonnage", dimension="catchtype")

#------- initial data.frame
df_Q2 <- df_Q2n[[1]]
df_Q2$country <- countrynames[1]

#-------- get all entries of list into one data.frame
for (i in 1:length(df_Q2n)){
  df_new <- df_Q2n[[i]]
  
  if (ncol(df_new)<3)
    df_new$discards <- 0
  
  df_new$country <- countrynames[i]
  
  df_Q2 <- bind_rows(df_Q2, df_new)
}

#write and read in again:
#write.table(df_Q2, "df_Q2", sep="\t")
df_Q2 <- read.delim("~/Dropbox/GeoVis/df_Q2") 

# ------ calculate percentage of landings and discards

df_Q2p <- df_Q2 %>% group_by(years) %>% 
  summarise(landings =sum(landings), discards=sum(discards)) %>%  #for all countries
  mutate(total =( landings+ discards)) %>%  
  mutate(percilandi = (landings/total)) %>%        #landings in percent
  mutate(percidiscardi = (discards/total)) %>%     #discards in percent
  select(-landings, -discards, -total) %>% gather(., key="catchtype", value="percentage", -years) 
                                                   # preparing for ggplot
#------- plot 
ggplot(df_Q2p, aes(x=years, y=percentage))+
  geom_area(aes(fill=catchtype))+
  scale_fill_brewer(palette = "Dark2")


#=============================================================================
# Question 3: Filter data for the fishing_entity “Germany”.
#=============================================================================

lr_fish.ent <- regiondf # country-id Germany:276 <-> id: 66
lr_eez <- listregions("eez")

#data frame with values of every sector (within Germany):
sec_ger <- catchdata(region="fishing-entity", id=66, measure="value", dimension="sector") 
sec_ger <- bind_cols("id"=rownames(sec_ger), sec_ger)  

#without "industrial" to see the smaller sectors better:
sec_ger2 <- sec_ger %>% filter(years>=1960, years<=2010) %>% select(-industrial) %>% gather(., key="sector", value, -c(id, years))


#----------plot:
col_fac <- c("darkblue", "orange", "darkgreen")

ggplot(sec_ger2, aes(x=years, y=value))+
  geom_area(aes(fill=sector))+
  scale_fill_brewer(palette = "Dark2")



#===============OWN IDEAS==================

#==========================================
### Welche Länder fischen im Mittelmeer?
#==========================================


med <- catchdata(region="lme", 26, dimension="country", measure="tonnage")
black <- catchdata(region="lme", 62, dimension="country", measure="tonnage")

medblack <- join(med, black, by="years", type="full", match="first")
View(medblack)
medblack2 <- medblack %>% gather(., key="country", value="tonnage", -c(years))
med2 <- med %>% gather(., key="country", value="tonnage", -c(years))

ggplot(med2, aes(x=years, y=tonnage))+
  geom_area(aes(fill=factor(country)))+
  theme(legend.position = "right")+
  #scale_fill_hue (l=40)+
  guides(fill=guide_legend(title="Countries"))

medhigh <- catchdata(region="fao", 37, dimension="country", measure="tonnage")
medhigh2 <- medhigh %>% gather(., key="country", value="tonnage", -c(years))

ggplot(medhigh2, aes(x=years, y=tonnage))+
  geom_area(aes(fill=factor(country)))+
  theme(legend.position = "right")+
  scale_fill_hue(l=40)+
  guides(fill=guide_legend(title="Countries"))+
  labs(title="FAO areas")


ggplot(medblack2, aes(x=years, y=tonnage))+
  geom_area(aes(fill=factor(country)))+
  theme(legend.position = "right")+
  scale_fill_hue (l=40)+
  guides(fill=guide_legend(title="Countries"))+
  labs(title="LME - Large Marine Ecosystems")
