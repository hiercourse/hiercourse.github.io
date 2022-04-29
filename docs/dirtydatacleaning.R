
# load libraries
library(tidyverse)

# First read the data.
dirty <- read.csv("data/dirty data.csv")
summary(dirty)
unique(dirty$max.temp)

# The na.strings argument lists values that should really be missing.
# These came up when inspecting summary() of each variable.
dirty <- read.csv("data/dirty data.csv",
                  na.strings=c("","xxxxxx","-9999"))

# The structure shows a few problems!
str(dirty)

# First rename some variables.
# Rename the third variable to lon
names(dirty)[3] <- "lon"
# Rename the variable that had the name 'max.temp' to Tmax
dirty <- dirty %>% rename(Tmax = max.temp)

# Here we keep variables that we want
dirty <- dirty %>% select(species, lon, lat, Country, AMT, AMP, Tmin, Tmax, Clay)

# Delete first row
dirty <- dirty[-1, ]
str(dirty)

# It turns out that longitude was read as character, but it should really be numeric!
# This means some of the values could not be converted to numeric.
# Let's find out which values, by seeing which values end up as NA if we convert them to numeric
dirty$lon[is.na(as.numeric(dirty$lon))]

# From the above we see that there is a space in longitude.
# Replace it with a "." using gsub.
# First, find the values that have a space using grep
grep(" ", dirty$lon, value=T)
# It's safe to replace spaces with decimals
dirty$lon <- gsub(" ", ".", dirty$lon)
dirty$lon <- as.numeric(dirty$lon)
str(dirty)

# Another solution is to find the observations that could not be converted to numeric,
# AND were not missing in the first place:
x <- as.numeric(as.character(dirty$lon))
ii <- which(is.na(x) & !is.na(dirty$lon))
dirty$lon[ii]
# Here, ii are the indices of observations that have a space in them

# Now, we can check whether the latitude and longitudes have reasonable values
range(dirty$lat, na.rm=TRUE)
range(dirty$lon, na.rm=TRUE)

# Longitude maximum is infinity! We should delete that point. 
# First, here is a trick to look at the n maximum points, using which.maxn from doBy
library(doBy)
dirty$lon[which.maxn(dirty$lon,10)]
# This doesn't tell us why it is infinity, let's identify the row and look in the spreadsheet
which(dirty$lon == Inf)

# And set infinite points to NA
dirty$lon[!is.finite(dirty$lon)] <- NA

# Check on a map.
library(maps)
map()
with(dirty, points(lon, lat, pch=16, col="red", cex=1))

# We found that AMP (annual mean precipitation) includes a few very low values.
dirty$AMP[which.minn(dirty$AMP,10)]
# We will assume these are in the wrong units and need to be converted.
ii <- which(dirty$AMP < 10 & !is.na(dirty$AMP))
dirty$AMP[ii] <- 1000 * dirty$AMP[ii]
summary(dirty)

# We also found that the three temperature variables are saved in multiples of ten
# This was intentional but we can backtransform them here
dirty$AMT <- dirty$AMT/10
dirty$Tmin <- dirty$Tmin/10
dirty$Tmax <- dirty$Tmax/10

# Let's look at the species variable.
unique(dirty$species)

# Why are there NA values? Let's check.
dirty[is.na(dirty$species), ]

# There are some more empty rows in the dataframe, possibly introduced during copy-paste
# Let's delete these (Note: don't use drop_na() or complete.cases())
dirty <- dirty[!is.na(dirty$species), ]


# Let's delete a number of bad characters, by replacement using gsub
# We need fixed=TRUE, because these special characters might be interpreted as
# regular expressions (fixed=TRUE avoids that).
dirty$species <- gsub("[", "", dirty$species, fixed=TRUE)
dirty$species <- gsub("]", "", dirty$species, fixed=TRUE)
dirty$species <- gsub("'", "", dirty$species, fixed=TRUE)
dirty$species <- gsub("(", "", dirty$species, fixed=TRUE)
dirty$species <- gsub(")", "", dirty$species, fixed=TRUE)

unique(dirty$species)

# There are still trailing and leading spaces. These can be removed with str_trim
# from the stringr package
dirty$species <- str_trim(dirty$species)

# Some species are not capitalized as the should be.
# capitalize() handles this well but the Hmisc package conflicts with the dplyr
# package so we use it without loading the library
dirty$species <- Hmisc::capitalize(dirty$species)

# Almost there!
unique(dirty$species)

# The final, and hardest problem, is to replace spaces 
# **only when there are at least two in a row**
# The following regular expression does the trick,
# where {2,} means "2 or more". For comparison {3} means 'exactly three',
# and {,10} means 'between zero and ten'.
dirty$species <- gsub("[[:space:]]{2,}", " ", dirty$species)

# And we are done.
unique(dirty$species)

# write clean data to file
write.csv(dirty, 'output/clean.csv', row.names=F)
