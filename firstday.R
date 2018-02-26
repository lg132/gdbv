#install.packages("vegan")
library(vegan)

data("varechem")
data("varespec")

#neither has ID coloumn

varechem2 <- cbind("id"=rownames(varechem), varechem)
varespec2 <- cbind("id"=rownames(varespec), varespec)

head(varechem2)
head(varespec2)

# now we can join the two tables by id

opt1 <- merge(varechem2, varespec2, by.x="id", by.y="id", all=T) # id is default for join-coloumn (if existent); "all=T" takes all data also when uneven number of data -> want to keep all data? or only where data existent for one dataset e.g.
opt1 # note: sorted by id

library(plyr)
opt2 <- join(varechem2, varespec2, by="id", type="full", match="first")
opt2 # note: not sorted but following sequence of first data.frame
     # type is immaterial in this case as both data.frames are fully compatible

     # inner: only rows present in BOTH data.frames are used
     # left: alls rows of x

#install.packages("ade4")
library(ade4)
data(aviurba)
str(aviurba, 1)
#fau are sites x species ("faune"; 51x40)
#mil are sites x environment ("milieu"; 51x11=)
#traits are species x traits

aviurba$fau
aviurba$traits

no.species <- ncol(aviurba$fau)
no.sites <- nrow(aviurba$fau)
no.traits <- ncol(aviurba$traits)

# how many species of site are migratory?

fau.t.id <- data.frame(SpecID=colnames(aviurba$fau), t(aviurba$fau))
traits.id <- data.frame(TraitID=rownames(aviurba$traits), (aviurba$traits))
fau.trait <- merge(fau.t.id, traits.id, by.x="SpecID", by.y="TraitID") # necessary to make sure that the order is correct
# compare for example
fau.t.id
fau.trait
# Thus, from now on, alwas refer to the same data.frame and not an older version!
# If you want you can delete the intermediate step to avoid mistakes:
rm(fau.t.id, traits.id)
colnames(fau.trait)

# now we compute the number of migratory species at each site in two ways
# 1: fast one
migrants <- t(fau.trait[, -c(1, 53:56)] > 0) %*% ifelse(fau.trait$migratory == "migrant", 1, 0) # matrix multiplication
migrants

# 2: slow one
migrant.indicator.matrix <- matrix(ifelse(fau.trait$migratory == "migrant", 1, 0), nrow=no.species, ncol=no.sites) # yielding  a species x sites matrix with ls indicating that a species is migratory; check: each coloumn should be identical (because each row contains a species)
migrant.indicator.matrix
# this previous step requires great care! R is a bit funny about the way it puts vectors into matrices (row-first, not coloumn-first as we would think abour reading a text).
migrant.df <- migrant.indicator.matrix * (fau.trait[, -c(1, 53:56)] > 0)
# element-wise multiplication with presence/absence of a species-matrix
(migrants <- colSums(migrant.df)) # number of migrant species; same as in fast step

# now compute proportion of migrants:
nr.of.species <- colSums(fau.trait[, -c(1,53:56)] > 0) # total number of species
prop.of.migrants <- migrants/nr.of.species
prop.of.migrants

# different data formats have different times to respond -> system.time
# matrix: R flips the matrix to be faster in accessing
# data.frame: R goes through every coloumn until the specified one
# data.tables are supposed to be faster (e.g. at joins) and allow grouping within the square brackets, much slower in indexing!
# tibble (tbl): consistent in use of function names not so much use in terms of speed
  #ex.tbl <- tbl_df(ex.df) # converts from right to left
  #dataset: glimpse(starwars)


