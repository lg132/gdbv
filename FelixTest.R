library(seaaroundus)
library(ggplot2)
library(tidyr)

#===============
# DATA FRAME: FOR EVERY COUNTRY CATCH TIME SERIES

regiondf <- listregions("fishing-entity")
countrynames <- regiondf$title[-c(61,93,192)]
countryids <- regiondf$id[-c(61,93,192)]

region <- gather(regiondf, key="title", value="id")

View(region)
catchdata('fishing-entity', 66, measure='tonnage', dimension='country')


df_fishing_all<- catchdata('fishing-entity', countryids[1], measure='tonnage', dimension='country')
View(df_fishing_all)

#Calculation of Victorias DF
for (i in 2:length(countryids)){
  newcol<- catchdata('fishing-entity', countryids[i], measure='tonnage', dimension='country')
  df_fishing_all[countrynames[i]] <-newcol[,2]
}


# which country catches in average less than 2,000,000 tonnes of fish per year?

#Victoria's data.frame
df_fishing_all <- source("/Users/Fex/Dropbox/GeoVis/df_fishing_all.Rdmpd")
library(ggplot2)
library(dplyr)
library(plyr)
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

df.ex2 <- dfleonie %>% group_by(years) %>%  summarise(landings =sum(landings), discards=sum(discards))
df.ex2 <- df.ex2 %>% mutate(total =( landings+ discards)) %>% 
  mutate(percilandi = (landings/total)) %>% 
  mutate(percidiscardi = (discards/total))

View(df.ex2)
df.ex2.1 <- df.ex2 %>% select(-landings, -discards, -total) %>% gather(., key="catchtype", value="percentage", -years) 

ggplot(df.ex2.1, aes(x=years, y=percentage))+
  geom_area(aes(fill=catchtype))+
  scale_fill_brewer(palette = "Dark2")


?catchdata
#"taxon", "commercialgroup", "functionalgroup", "country", "sector", "catchtype", "reporting-status", "layer" 


lr_fish.ent <- regiondf # country-id Germany:276 <-> id: 66
lr_eez <- listregions("eez")
?catchdata
#data frame with values of every sector (within Germany):
#sec_ger <- catchdata(region="fishing-entity", id=66, measure="value", dimension="sector") 
#sec_ger <- bind_cols("id"=rownames(sec_ger), sec_ger)  

#without "industrial" to see the smaller sectors better:
#sec_ger2 <- sec_ger %>% filter(years>=1960, years<=2010) %>% select(-industrial) %>% gather(., #key="sector", value, -c(id, years))
lme <- listregions("lme")
rfmo <- listregions("rfmo")
highseas <- listregions("highseas")
fao <- listregions("fao")
eez <- listregions("eez")

View(listregions("lme"))

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
  labs(title="FAO")
  

ggplot(medblack2, aes(x=years, y=tonnage))+
  geom_area(aes(fill=factor(country)))+
  theme(legend.position = "right")+
  scale_fill_hue (l=40)+
  guides(fill=guide_legend(title="Countries"))



#==========================================
### Sushi
#==========================================
View(listregions("fishing-entity"))
jap <- catchdata(region="fishing-entity", 91, dimension="country", measure="tonnage")
View(jap)


#==========================================
### Nord/Ostseeküste
#==========================================
# Wer fischt wieviel?
med <- catchdata(region="lme", 26, dimension="country", measure="tonnage")

nord <- catchdata(region="lme", 22, dimension = "country", measure="tonnage")
ost <- catchdata(region="lme", 23, dimension = "country", measure="tonnage")
nord2 <- nord %>% gather(., key="country", value="tonnage", -c(years))
ost2 <- ost %>% gather(., key="country", value="tonnage", -c(years))

ggplot(nord2, aes(x=years, y=tonnage))+
  geom_area(aes(fill=factor(country)))+
  theme(legend.position = "right")+
  scale_fill_hue(l=40)+
  guides(fill=guide_legend(title="Countries"))+
  labs(title="nordsee LME")

ggplot(ost2, aes(x=years, y=tonnage))+
  geom_area(aes(fill=factor(country)))+
  theme(legend.position = "right")+
  scale_fill_hue(l=40)+
  guides(fill=guide_legend(title="Countries"))+
  labs(title="ostsee")
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

eeznord <- catchdata(region="eez", 277, dimension = "country", measure="tonnage")

eeznord2 <- eeznord %>% gather(., key="country", value="tonnage", -c(years))

ggplot(eeznord2, aes(x=years, y=tonnage))+
  geom_area(aes(fill=factor(country)))+
  theme(legend.position = "right")+
  scale_fill_hue(l=40)+
  guides(fill=guide_legend(title="Countries"))+
  labs(title="eez nordsee")
?catchdata



#countrynames <- regiondf$title[-c(61,192,93)]
#countryids <- regiondf$id[-c(61,192,93)]

map <- listregions('eez', id="32", measure="tonnage", dimension="")
map2 <- catchdata(region='eez', id="32", measure="tonnage", dimension="country")
attribute <- read.csv("~/Dropbox/Geovis/attribute.csv")
View(df_eez)
df_eez <- read.delim("df_eez")

df_eez_attr <- intersect(attribute$Country, df_eez$eez)

View(df_eez_attr)
summary(is.element(attribute$Country, eez_names))  

View(listregions("eez"))
View(catchdata(region="eez", measure="tonnage", dimension="country"))

attr <- read.csv("attribute.csv")
eez <- read.delim("df_eez")
sau_eez <- read.delim("~/Dropbox/Geovis/sau_eez")
View(sau_eez)
View(attr)




#### LEONIE
library(sf)
eez_shp <- st_read("~/Dropbox/GeoVis/World_EEZ_v7_20121120_LR/World_EEZ_v7_2012.shp")
View(eez_shp)
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
library(dplyr)
test_join <- plyr::join(eez, EEZ, by="ID", type="left")

intersects(test1, test2)
 ########### FELIX AGAIN #############

eez_shp <- st_read("/Users/Fex/Dropbox/GeoVis/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp")
plot(eez_shp$geometry)
plot(eez_shp[3])

list_eez <- listregions("eez")
View(listregions("eez"))
head(eez_shp)


saui <- read.delim("sau_eez_felix.txt")
names(saui)[1] <- "Country"
head(saui)

mapjoin <- plyr::join(saui, attribute, by="Country", type="right")

map <- plot(eez_shp$geometry)

#ggmap(map)
#+ geom_point(aes(x=lon, y=lat), data=mv_num_collisions, col="orange", alpha=0.4) 
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top

library("ggmap")
ggplot(eez_shp$geometry, color="red")


world <- map_data("world") # we already did this, but we can do it again
#ggplot() + 
#  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + coord_fixed(1.3) +
#  geom_polygon(data = eez_shp, aes(x=eez_shp$Longitude, y=eez_shp$Latitude), color="red") + coord_fixed(1.3)
 
ggplot(data = eez_shp) + 
  geom_polygon(aes(x = Longitude, y = Latitude, group = "Country"), color = "white") + 
  coord_fixed(1.3) 

eez_shp_big <- get_map(location = world, source = "google", maptype = "terrain")
ggmap(mapWorld) + 
  geom_point(data = eez_shp, mapping = aes(x = Longitude, y = Latitude))

ggplot(eez_shp, aes(x = Longitude, y = Latitude, group = 1)) + 
  geom_polygon(aes(fill = EEZ)) +
  #geom_text(data = pakistan.adm2.centroids.df, aes(label = NAME_2, x = long, y = lat, group = NAME_2), size = 3) + 
  labs(x=" ", y=" ") + 
  #theme_bw() + scale_fill_brewer('Unemployment Rate (Jan 2011)', palette  = 'PuRd') + 
  coord_map() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme(panel.border = element_blank())

autoplot(eez_shp)

library(ggplot2)

mapWorld <- borders("world", colour="gray50", fill="gray50")
install.packages('rgdal')
library(rgdal)
eez_shp2 <- spTransform(eez_shp, CRS("+proj=longlat +datum=WGS84"))

ggplot() + mapWorld #+
 # geom_polygon(aes(x=Longitude, y=Latitude) ,color="blue", size=1, data=fortify(eez_shp))

######################

  
library(rgdal)
library(ggplot2)
sfn = readOGR("/Users/Fex/Dropbox/GeoVis/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
sfn$avg <- mapjoin$avg
ggplot(data = sfn, aes(x = long, y = lat, group = group)) + geom_path()

names(sfn)
sfn.f = sfn %>% fortify(region = "avg")
names(sfn.f)

sf = merge(sfn$data, values, by.x='id')

ggplot(sfn.f, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = sfn.f$avg))
############## map again ###############

saui <-read.delim("sau_eez_felix.txt")
names(saui)[1] <- "Country"

eez_shp <- st_read("~/Dropbox/GeoVis/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp")
eez_shp_sau <- merge(eez_shp, saui[, -2], by="Country", all.x=T) %>% dplyr::select(Country, EEZ, sau_id, Longitude, Latitude, geometry)
eez_shp_sau <- eez_shp_sau %>% dplyr::select(Country, EEZ, sau_id, Longitude, Latitude, geometry)
#rgdal::writeOGR(obj=eez_shp_sau, dsn=".", layer="eez_shp_sau", driver="ESRI Shapefile")
dump("eez_shp_sau", "eez_shp_sau.Rdmpd")

eez_merge <- merge(eez_shp_sau, saui, by="Country", all.x=T)

tm_shape(eez_merge, fill="avg")+
  tm_fill("avg", palette = "Blues", n=5, style = "jenks", 
          legend.hist = F, title="Average Discards", textNA="NA")+
 # tm_text("iso_a3", size="AREA", root=5) + 
  tm_borders()+
  tm_compass()+
  tm_legend(position = c("left", "bottom"), frame = TRUE, bg.color="grey")

library(tmap)
  tm_shape(eez_merge)+
    tm_polygons("avg", palette = "Blues", textNA="NA", title="Average Discards", n=6, style="jenks")+
    tm_layout(
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            #legend.digits = 3,
            legend.bg.alpha = 1, 
            legend.frame="gray50", 
            #legend.format = list(digits=0)
            legend.format = list(scientific = FALSE)
            #legend.width = 0.4, legend.height = 0.9
            )
#  tm_text(legend.format = "f")
 #tm_fill(breaks = c(0, 10000, 20000, 30000, 40000, Inf))
  mapWorld <- borders("world", colour="gray50", fill="gray50")
  
  tm_polygon(mapWorld)
  
data(Europe)
tm_shape(Europe) +
  tm_polygons("well_being", textNA="Non-European countries", title="Well-Being Index") +
  tm_text("iso_a3", size="AREA", root=5) + 
  tm_format_Europe() +
  tm_style_grey()+
  tm_layout(
    legend.title.size = 1.1,
    legend.text.size = 0.7,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 1, 
    legend.frame="gray50", 
    legend.format = list(scientific = FALSE), 
    legend.outside = FALSE, 
    legend.width = 0.2,
    #legend.height = 2
    legend.stack = c("vertical"))

#### LEAFLET ####
install.packages("leaflet")
library(leaflet)
library(rgdal)
library(ggplot2)
library(tidyverse)

sfn = readOGR("/Users/Fex/Dropbox/GeoVis/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp")#%>% spTransform(CRS("+proj=longlat +datum=WGS84"))
#sfn.f = sfn %>% fortify(region = "avg")
#eez_merge <- merge(sfn, saui, by="Country", all.x=T)
sfn2 <- merge(sfn, saui, by="Country", all.x=T)

library(sf)
eez_shp <- st_read("~/Dropbox/GeoVis/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp")
eez_merge <- merge(eez_shp_sau, saui, by="Country", all.x=T)

#### LEAFLET - AB HIER ZÄHLTS ###
pal_fun <- colorQuantile("Blues", n = 4, domain = eez_merge$avg)

leaflet(eez_merge) %>%
  addPolygons(
    stroke = FALSE, # remove polygon borders
    fillColor = ~pal_fun(eez_merge$avg), # set fill color with function from above and value
    fillOpacity = 0.8, smoothFactor = 0.5)#,
   # highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)) %>%   
  addLegend("bottomright", pal = pal_fun, values = ~avg, title = "Discards", opacity = 1)

    
pal_fun <- colorQuantile("Blues", n = 4, domain = sfn2$avg, alpha=1.2)
leaflet(sfn2)  %>% addTiles() %>% 
  #setView(sfn2, min(sfn2$Longitude), min(sfn2$Latitude), max(sfn2$Longitude), max(sfn2$Latitude)) %>% 
  addProviderTiles("OpenSeaMap") %>%
  addPolygons(stroke = FALSE, fillColor = ~pal_fun(sfn2$avg),
              fillOpacity = 0.8, smoothFactor = 0.5) %>% 
  addLegend("bottomright", pal=pal_fun, values= sfn2$avg, title = 'Discards')

