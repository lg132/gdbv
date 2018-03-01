library(seaaroundus)
library(ggplot2)
library(tidyverse)
library(viridis)

wkt <- "POLYGON((2.37 43.56,13.18 43.56,13.18 35.66,2.37 35.66,2.37 43.56))"
getcells(wkt)

getcelldata(2004, cells = 89568)
ex <- getcelldata(2008, cells = c(89568, 89569))
ex

?marinetrophicindex
marinetrophicindex(region = "eez", id = 76, chart=T)
?eezsvshighseas
eezsvshighseas(chart=T)

# ---- possible question 1 ----

regiondf <- listregions(region="fishing-entity")
countrynames <- regiondf$title
countrynames <- countrynames[-c(61,93,192)]
countryids <- regiondf$id
countryids <- countryids[-c(61,93,192)]

df_fishing_all<- catchdata('fishing-entity', countryids[1], measure='tonnage', dimension='country')
View(df_fishing_all)

for (i in 2:length(countryids)){ #beginning with 2.column (first one is year)
  newcol<- catchdata('fishing-entity', countryids[i], measure='tonnage', dimension='country') 
  #every column is the time series of fish catches for one country
  df_fishing_all[countrynames[i]] <-newcol[,2]
  #add this column to the big data frame 
}

#save data.frame
write.table(df_fishing_all, "fishing_all", sep="\t")

#Victoria's data.frame
#df_fishing_all <- source("/home/leonie/Dropbox/GeoVis/df_fishing_all.Rdmpd")


# Example stacked chart: http://ggplot2.tidyverse.org/reference/position_stack.html
series <- data.frame(
  time = c(rep(1, 4),rep(2, 4), rep(3, 4), rep(4, 4)),
  type = rep(c('a', 'b', 'c', 'd'), 4),
  value = rpois(16, 10)
)
ggplot(series, aes(time, value)) +
  geom_area(aes(fill = type))  

# preparing data for stacked chart
df1 <- df_fishing_all
df2 <- df1 %>% gather(., key="country", value="tonnage", -c(years))
df3 <- df2 %>% group_by(country) %>% summarise(avg=mean(tonnage))

ggplot(df2, aes(x=years, y=tonnage))+
  geom_area(aes(fill=country))+
  theme(legend.position = "none")

df4 <- df3 %>% filter(avg<2000000)
df5 <- df3 %>% filter(avg>2000000) 

fishing_high <- df2 %>% filter(country %in% df5$country) #all countries with more than 2mio catches
others <- df2 %>% filter(country %in% df4$country) %>%
  group_by(years) %>% summarise(tonnage = sum(tonnage)) %>%
  mutate(country = "Others") %>% select(years,country,tonnage)

fish.final <- bind_rows(fishing_high,others)

#ggplot of fishing_high
ggplot(fishing_high, aes(x=years, y=tonnage))+
  geom_area(aes(fill=country))+
  theme(legend.position = "none")

#ggplot of fishing + others
ggplot(fish.final, aes(x=years, y=tonnage))+
  geom_area(aes(fill=country))+
  theme(legend.position = "right")

#ggplot of fishing + others seperated
ggplot(fish.final, aes(x= years, y=tonnage)) +
  geom_area(aes(fill=factor(country, levels=c("Others", df5$country))))+
  theme(legend.position = "right")+
  guides(fill=guide_legend(title="Countries"))
  #scale_fill_hue(l=20)


# ---- possible question 2 ----

countrynames <- regiondf$title
countrynames <- countrynames[-c(61,93,192)]
countryids <- regiondf$id
countryids <- countryids[-c(61,93,192)]

list_Q2 <- lapply(X = countryids, FUN = catchdata, region="fishing-entity", measure="tonnage", dimension="catchtype")

write.table(list_Q2, "list_Q2", sep="\t")

# create initial data.frame to be filled with remaining list entries
df_Q2 <- list_Q2[[1]]
df_Q2$country <- countrynames[1]

#get all entries of list into one data.frame
for (i in 2:length(list_Q2)){
  df_new <- df_Q2n[[i]]
  
  if (ncol(df_new)<3)
    df_new$discards <- 0
  
  df_new$country <- countrynames[i]
  #df_new <- df_new[, -1]
  
  df_Q2 <- bind_rows(df_Q2, df_new)
}

write.table(df_Q2, "df_Q2", sep="\t")

df_Q2p <- df_Q2 %>% group_by(years) %>% 
  summarise(landings =sum(landings), discards=sum(discards)) %>%
  mutate(total =( landings+ discards)) %>% 
  mutate(percilandi = (landings/total)) %>% 
  mutate(percidiscardi = (discards/total)) %>%
  select(-landings, -discards, -total) %>% gather(., key="catchtype", value="percentage", -years) 

ggplot(df_Q2p, aes(x=years, y=percentage))+
  geom_area(aes(fill=catchtype))+
  scale_fill_brewer(palette = "Dark2")


# ---- possible question 3 ----

lr_fish.ent <- regiondf # country-id Germany:276 <-> id: 66
lr_eez <- listregions("eez")

sec_ger <- catchdata(region="fishing-entity", id=66, measure="value", dimension="sector")
sec_ger <- bind_cols("id"=rownames(sec_ger), sec_ger)

sec_ger2 <- sec_ger %>% filter(years>=1960, years<=2010) %>% select(-industrial) %>% gather(., key="sector", value, -c(id, years))

col_fac <- c("darkblue", "orange", "darkgreen")

ggplot(sec_ger2, aes(x=years, y=value))+
  geom_area(aes(fill=sector))+
  scale_fill_brewer(palette = "Dark2")


# ---- own thoughts ----

library(plyr)

# get catch data for Germany for fish species in Baltic and North sea 
eezB_ger <- catchdata(region="eez", id=278, measure="value", dimension="taxon")
eezB_ger <- bind_cols("id"=rownames(eezB_ger), eezB_ger)
eezN_ger <- catchdata(region="eez", id=277, measure="value", dimension="taxon")
eezN_ger <- bind_cols("id"=rownames(eezN_ger), eezN_ger)

join_ger <- join(sec_ger, eezB_ger, by="id", type="full", match="first")

fish_ent <- listregions(region="fishing-entity")
eez <- listregions(region="eez")
lme <- listregions(region="lme")
rfmo <- listregions(region="rfmo")
fao <- listregions(region="fao")

# get catchdata for fish species in NorthSea (id=22 in lme)
lme_NS <- catchdata(region="lme", id=22)

catchdata(region="eez", id=52, dimension="functionalgroup")
