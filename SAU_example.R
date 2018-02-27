# SEAAROUNDUS
# Note: Github mentions issues for plotting functions on Windows.
# Examples: https://github.com/ropensci/seaaroundus

#install required packages
install.packages("devtools")
devtools::install_github("ropensci/seaaroundus")

#load the seaaroundus package
library(seaaroundus)

öbshdfljbndslfjbnlksgjnbkljfgnlkbjsnflkgjbnslkfjgnbkljfgnlbksjnlgkjn


hallo i bims
#list the assigned IDs for each Exclusive Economic Zone (EEZ). NB: The EEZ is the zone adjacent to the coast, in which the corresponding country has the sovereign right to fish.
listregions('eez')

#pick e.g. Cuba ́s ID from the list and print catch data in tonnes, grouped by commercial taxon groups
catchdata("eez",192,measure= "tonnage",dimension = "commercialgroup")

#..or it ́s value in US dollar
catchdata("eez",192,measure="value",dimension = "commercialgroup")

#see which country is fishing how much within the EEZ of Cuba
catchdata("eez",192,measure = "tonnage",dimension = "country")

#plot Cuba ́s catch data via chart-function
catchdata("eez",192,measure="tonnage",dimension = "commercialgroup", chart=TRUE)

#see reporting status (visualize illegal, unreported and unregulated fishing activities)
catchdata("eez",192,measure = "tonnage",dimension = "reporting-status")

#see sector distributions:
#Industrial: large ships, commercial.
#Artisanal: small-scale, small boats, commercial (local markets, etc.).
#Recreational : Fishing for pleasure, small-scale.
#Subsistence: Fishing for food, no commercial interest.
catchdata("eez",192,measure = "tonnage",dimension = "sector")

#plot a map of Cuba ́s region
regionmap("eez", 192)
