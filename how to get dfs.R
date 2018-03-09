
# CREATING THE DATA FRAMES
#--------------------------

regiondf <- listregions("fishing-entity")

#Error Messages in:
#Unknown Fishing Country: row 192
#Fishing Country Unknown: row 61
#Johnston Atoll: row 93
# --> take them out 

countrynames <- regiondf$title[-c(61,93,192)]
countryids <- regiondf$id[-c(61,93,192)]

#------------------------------
# Dataframe df_total_catch: Total fish catch in tons for every country and every year
#------------------------------
df_total_catch<- catchdata('fishing-entity', countryids[1], measure='tonnage', dimension='country')

for (i in 2:length(countryids)){ 
  #beginning with 2.column (first one is year)
  #every new column is the time series of total fish catches for one country:
  newcol<- catchdata('fishing-entity', countryids[i], measure='tonnage', dimension='country')
  df_total_catch[countrynames[i]] <-newcol[,2]
}

#write.table(df_total_catch, "df_total_catch", sep="\t") 


#---------------------------
# Dataframe df_discards: Landings and discards of every country and every year 
#----------------------------
# Execute catchdata with all countryids; output as list
list_catchtype <- lapply(X = countryids, FUN = catchdata, region="fishing-entity", measure="tonnage", dimension="catchtype")

# for further processing the data has to be extracted from the list and put into one data.frame: ----

# initial data.frame to be filled with remaining list entries
df_discards <- list_catchtype[[1]]
df_discards$country <- countrynames[1]

# loop to get all entries of list into one data.frame
for (i in 2:length(list_catchtype)){
  
  df_new <- list_catchtype[[i]]
  
  if (ncol(df_new)<3)   # if clause to check for same amount of columns
    df_new$discards <- 0
  
  df_new$country <- countrynames[i]
  
  df_discards <- bind_rows(df_discards, df_new)
}

#---------------------------
# Dataframe df_eez: Landings and discards in every EEZ for every year
#----------------------------
eez <- listregions(region="eez")

#Execute catchdata for every EEZ; output as list:
list_eez <- lapply(X = eez_all, FUN = catchdata, region="eez", measure="tonnage", dimension="catchtype")

# create initial data.frame to be filled with remaining list entries
df_eez <- list_eez[[1]]
df_eez$eez <- eez$title[1]
df_eez$sau_id <- eez$id[1]

#get all entries of list into one data.frame
for (i in 2:length(list_eez)){
  df_new <- list_eez[[i]]
  
  if (ncol(df_new)<3)
    df_new$discards <- 0
  
  df_new$eez <- eez_names[i]
  df_new$id <- eez_all[i]
  
  df_eez <- bind_rows(df_eez, df_new)
}
#write.table(df_eez, "df_eez", sep="\t")

#---------------------------
# sau_id: changed the eez names of df_eez manually to be equal to the names in the shapefile
