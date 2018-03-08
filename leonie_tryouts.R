library(seaaroundus)
library(ggplot2)
library(tidyverse)
library(plyr)

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

#for (i in 2:length(countryids)){ #beginning with 2.column (first one is year)
#  newcol<- catchdata('fishing-entity', countryids[i], measure='tonnage', dimension='country') 
  #every column is the time series of fish catches for one country
#  df_fishing_all[countrynames[i]] <-newcol[,2]
  #add this column to the big data frame 
#}

#save data.frame
#write.table(df_fishing_all, "fishing_all", sep="\t")

#Victoria's data.frame
df_fishing_all <- source("df_fishing_all.Rdmpd")
df_fishing_all <- df_fishing_all[[1]]

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
  guides(fill=guide_legend(title="Countries"))+
  theme_light()
  #scale_fill_hue(l=20)

colnames(fish.final)[2]
# ---- possible question 2 ----

#list_Q2 <- lapply(X = countryids, FUN = catchdata, region="fishing-entity", measure="tonnage", dimension="catchtype")
#write.table(list_Q2, "list_Q2", sep="\t")
list_Q2 <- read.delim("list_Q2")

# create initial data.frame to be filled with remaining list entries
df_Q2 <- list_Q2[[1]]
df_Q2$country <- countrynames[1]

#get all entries of list into one data.frame
for (i in 2:length(list_Q2)){
  df_new <- list_Q2[[i]]
  
  if (ncol(df_new)<3)
    df_new$discards <- 0
  
  df_new$country <- countrynames[i]
  #df_new <- df_new[, -1]
  
  df_Q2 <- bind_rows(df_Q2, df_new)
}

#write.table(df_Q2, "df_Q2", sep="\t")

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

fish_ent <- listregions(region="fishing-entity")
eez <- listregions(region="eez")

sec_ger <- catchdata(region="fishing-entity", id=66, measure="value", dimension="sector")
sec_ger <- bind_cols("id"=rownames(sec_ger), sec_ger)

sec_ger2 <- sec_ger %>% filter(years>=1960, years<=2010) %>% select(-industrial) %>% gather(., key="sector", value, -c(id, years))

ggplot(sec_ger2, aes(x=years, y=value))+
  geom_area(aes(fill=sector))+
  scale_fill_brewer(palette = "Dark2")


# ---- own thoughts ----

# listregions
lme <- listregions(region="lme")
rfmo <- listregions(region="rfmo")
fao <- listregions(region="fao")
hs <- listregions("highseas")

# 1. Germany stuff ----
# get catch data for Germany for fish species in Baltic and North sea 
eezB_tax_ger <- catchdata(region="eez", id=278, measure="value", dimension="taxon")
eezB_tax_ger <- bind_cols("id"=rownames(eezB_ger), eezB_ger)
eezN_tax_ger <- catchdata(region="eez", id=277, measure="value", dimension="taxon")
eezN_tax_ger <- bind_cols("id"=rownames(eezN_ger), eezN_ger)

join_ger <- join(sec_ger, eezB_ger, by="id", type="full", match="first")

# get sectorwise catchdata for germany globally
glob_sec_ger <-catchdata(region="global", id=66, dimension="sector")

# get catchtypes for germany
glob_ct_ger <- catchdata(region="global", id=66, dimension="catchtype")
colnames(glob_ct_ger) <- c("years", "landings_global", "discards_global")
#glob_ct_ger <- glob_ct_ger %>% gather(., key="catchtype", value="tonnage", -years)
eezB_ct_ger <- catchdata(region="eez", id=278, dimension="catchtype")
colnames(eezB_ct_ger) <- c("years", "landings_BalticSea", "discards_BalticSea")
#eezB_ct_ger <- eezB_ct_ger %>% gather(., key="catchtype", value="tonnage", -years)
eezN_ct_ger <- catchdata(region="eez", id=277, dimension="catchtype")
colnames(eezN_ct_ger) <- c("years", "landings_NorthSea", "discards_NorthSea")
#eezN_ct_ger <- eezN_ct_ger %>% gather(., key="catchtype", value="tonnage", -years)

join_ct_ger <- join(glob_ct_ger, eezB_ct_ger, by="years", type="full", match="first")
join_ct_ger <- join(join_ct_ger, eezN_ct_ger, by="years", type="full", match="first")
join_ct_ger <- join_ct_ger %>% gather(., key="catchtype", value="tonnage", -years)

ggplot(join_ct_ger)+
  geom_area(aes(x=years, y=tonnage, fill=catchtype))
#  geom_col(eezB_ct_ger, aes(x=years, y=tonnage, fill=catchtype))

catchdata("global", 66, dimension="sector", chart=T)

# 2. North Sea stuff ----

# get catchdata for NorthSea (id=22 in lme)
lme_NS_repstat <- catchdata(region="lme", id=22, dimension="reporting-status")
lme_NS_tax <- catchdata(region="lme", id=22, dimension="taxon")
lme_NS_count <- catchdata(region="lme", id=22, dimension="country")
lme_NS_sec <- catchdata(region="lme", id=22, dimension="sector")

lme_NS_tax_p <- lme_NS_tax %>% gather(., key="taxon", value="tonnage", -years)

ggplot()+
  geom_area(data=lme_NS_tax_p, aes(x=years, y=tonnage, fill=taxon))+
  geom_line(data=lme_NS_repstat, aes(x=years, y=unreported))

lme_NS_count_p <- lme_NS_count %>% gather(., key="country", value = "tonnage", -years)

ggplot()+
  geom_area(data=lme_NS_count_p, aes(x=years, y=tonnage, fill=country))

#plot(lme_NS_tax$years, lme_NS_tax$`atlantic herring`)
#lines(lme_NS_tax$years, lme_NS_repstat$reported)

# import shapefile for north sea
library(sf)
shape_NS <- st_read("../gdbv/Maritime_Boundaries/MBEULSIV1.shp")
plot(shape_NS)



# eez map ----
eez_all <- eez$id
eez_names <- eez$title
list_eez <- lapply(X = eez_all, FUN = catchdata, region="eez", measure="tonnage", dimension="catchtype")

# create initial data.frame to be filled with remaining list entries
df_eez <- list_eez[[1]]
df_eez$eez <- eez_names[1]

#get all entries of list into one data.frame
for (i in 2:length(list_eez)){
  df_new <- list_eez[[i]]
  
  if (ncol(df_new)<3)
    df_new$discards <- 0
  
  df_new$eez <- eez_names[i]
  #df_new <- df_new[, -1]
  
  df_eez <- bind_rows(df_eez, df_new)
}

write.table(df_eez, "df_eez", sep="\t")
attribute <- read.csv("attribute.csv")

att %in% eez_names
popo <- intersect(attribute$Country, eez$title)

att <- attribute$Country
eez_names
att[6]
summary(is.element(att, eez_names))

eez_shp <- st_read("../../../../Dropbox/GeoVis/World_EEZ_v7_20121120_LR/World_EEZ_v7_2012.shp")
plot(eez_shp)
plot(eez_shp[3])

# ================== AB HIER TRYOUTS ZU JOIN =====================

EEZ <- data.frame(EEZ=eez_shp$EEZ)

EEZ$ID <- substr(EEZ$EEZ, start = 1, stop = 5)
length(unique(EEZ$id))

eez$ID <- substr(eez$title, start = 1, stop = 5)
length(unique(eez$ID))
# 
# test1 <- data.frame("ID"=unique(EEZ$id))
# test2 <- data.frame("ID"=unique(eez$ID))

test_join <- plyr::join(eez, EEZ, by="ID", type="left")

intersects(test1, test2)


# =======================================================
library(raster)
library(sf)
#install.packages("tmap")
library(tmap)
library(dplyr)

# eez_merge <- st_read("../../../../Dropbox/GeoVis/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp")
# plot(eezmerge[2])
# 
# saui <-read.delim("../../../../Dropbox/GeoVis/sau_eez_felix.txt")
# 
# mapjoin <- read.delim("mapjoin")
# View(eezmerge)
# 
# fakenews<-merge(eezmerge, saui, by.x="Country", by.y= "Country")
# View(fakenews)
# 
# qtm(fakenews, fill="avg")
# 
# tm_shape(fakenews, fill = "avg")+
#   tm_fill("avg", palette = "Blues", n=7, style = "jenks", legend.hist = T)

saui <-read.delim("sau_eez_felix.txt")

eez_shp <- st_read("../../../../Dropbox/GeoVis/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp")
names(saui)[1] <- "Country"
eez_merge <- merge(eez_shp, saui, by="Country", all.x=T)

tm_shape(eez_merge, fill="avg")+
  tm_fill("avg", palette = "Blues", n=5, style = "jenks", legend.hist = T)

sum(is.na(mapjoin$avg))

View(list_eez_c[[1]])
