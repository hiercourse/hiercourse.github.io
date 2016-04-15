# First read the data.
# The na.strings argument lists values that should really be missing.
# These came up when inspecting summary() of each variable.
dirty <- read.csv("dirty_data.csv",
                  stringsAsFactors=FALSE,
                  na.strings=c("","xxxxxx","-9999"))

# The structure shows a few problems!
str(dirty)

# First rename some variables.
# Rename the third variable to lon
names(dirty)[3] <- "lon"
# Rename the variable that had the name 'max.temp' to Tmax
names(dirty)[names(dirty) == "max.temp"] <- "Tmax"

# Here we keep variables that we want
keepvars <- c("species","lon","lat","Country", "AMT","AMP","Tmin","Tmax","Clay")
dirty <- dirty[,keepvars]

# Delete first row
dirty <- dirty[-1,]

# It turns out that longitude was read as character, but it should really be numeric!
# This means some of the values could not be converted to numeric.
# Let's find out which values, by seeing which values end up as NA if we convert them to numeric
dirty$lon[is.na(as.numeric(dirty$lon))]

# From the above we see that there is a space in longitude.
# Replace it with a "." using gsub.
# First, find the values that have a space using grep (inspect the havespace object yourself!)
havespace <- grep(" ", dirty$lon)
dirty$lon[havespace] <- gsub(" ", ".", dirty$lon[havespace])
dirty$lon <- as.numeric(dirty$lon)

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

# And set infinite points to NA
dirty$lon[!is.finite(dirty$lon)] <- NA

# Check on a map.
library(maps)
map()
with(dirty, points(lon, lat, pch=16, col="red", cex=0.1))

# We found that AMP (annual mean precipitation) includes a few very low values.
# We will assume these are in the wrong units and need to be converted.
ii <- which(dirty$AMP < 100 & !is.na(dirty$AMP))
dirty$AMP[ii] <- 1000 * dirty$AMP[ii]


# Let's look at the species variable.
unique(dirty$species)

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
library(stringr)
dirty$species <- str_trim(dirty$species)

# Some species are not capitalized as the should be.
library(Hmisc)
dirty$species <- capitalize(dirty$species)

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
