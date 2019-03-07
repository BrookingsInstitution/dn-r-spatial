##=======================================================================##
# R for Spatial Data tutorial
#
# Data Network Brownbag
# 2019-02-28
##=======================================================================##

# run install.packages("packagename") once per package per computer

# run each library() function only once per session
library(sf)         # for dealing with spatial data
library(mapview)    # for interactively viewing spatial data
library(ggplot2)    # for static graphics
library(dplyr)      # for data wrangling

# specify and set a working directory where files will be read and written
# you will change this when working on your own computer
datadir <- "C:/Users/dharshbarger/Documents/spatial data"
setwd(datadir)

##=======================================================================##
# STEP 1 - READ IN DATA
##=======================================================================##

# read in towers shapefile
towers <- st_read("watertoren.shp")
# store the coordinate reference system so we can use it again
myproj <- st_crs(towers)

# read in places shapefile
places <- st_read("places.shp") %>%
  arrange(desc(population)) %>%       # sort in descending order by pop.
  st_transform(crs = myproj)          # change to the same crs as above

# we now have our two shapefiles loaded as sf objects,
# with matching projections

##=======================================================================##
# STEP 2 - EXPLORE VISUALLY
##=======================================================================##

# let's explore the data visually
# we can plot the towers with the plot() function from base R
plot(towers) # which automatically plots by each variable (too many!)
# we can plot by one variable by specifying its name, or with max.plot = 1
plot(towers["year"])
plot(towers, max.plot = 1)

# ok great, but what if we want to see the world around our points?
# or know which points are which?
# mapview to the rescue

mapview(towers)
mapview(towers, zcol = 'year') # we can also show a variable with color
mapview(towers, zcol = 'height')

# we can do our stats work alongside the GIS work!
# Try plotting year built against height of tower:
ggplot(data = towers,                # specify data to plot
       aes(x = year, y = height)) +  # specify axes
  geom_point() +                     # add points to plot
  geom_smooth(method = "lm",         # add best-fit line
              se = FALSE)            # don't show confidence intervals

# doesn't look like much of a correlation
# Do a quick correlation, using only pairwise complete observations
cor(towers$year, towers$height, use = "pairwise.complete.obs")
# yeah, not much.

# new question:
# how many watertowers are in each of the 100 largest cities?
# and which cities have the most?

##=======================================================================##
# STEP 3 - MANIPULATE GEOMETRIES
##=======================================================================##

# take a look at our cities shapefile to see what we have
plot(places, max.plot = 1) # automatically scales by first variable, "osm_id"

# our cities are currently point data
# what if we buffer the 100 largest by 5km?
# will need to check the crs info with st_crs() to know which units to use

# sort in descending order by population
places_desc <- arrange(places, desc(population))

# check the class of what we just created
class(places_desc) # yup, still an sf object. Great

# subset to the first 100 rows to get the 100 largest cities
top100 <- places_desc[1:100, ] # the empty space means we keep all columns

# use the st_buffer geoprocessing function on the sf object
# to create buffer zones at a distance of 5000m
top100_buf <- st_buffer(top100, dist = 5000)

# visually inspect to see if there's any overlap, using ggplot2
# we can use the super-handy geom_sf() function to plot the shapes
ggplot() +                                  # initialize the plot
  geom_sf(data = top100_buf) +              # add shape data (polygons)
  geom_sf(data = towers, color = "red")     # add shape data (points)

# there's definitely overlap
# Now use the sync functionality from mapview to get a closer look
# ...ready?
# create the two mapview objects to compare
m1 <- mapview(top100_buf)
m2 <- mapview(towers)
# and sync
sync(m1, m2)
# not bad, right?
# so which buffer zones contain which water towers? and how many?

##=======================================================================##
# STEP 4 - ANALYZE ATTRIBUTE DATA
##=======================================================================##

# We now have intersecting shapefiles in the same crs
# That means we can perform a spatial join with st_join()
overlap <- st_join(top100_buf, towers)
class(overlap) # this returns an sf object, as expected
dim(overlap)
# notice that the dimensions of the overlap have expanded beyond 100
# because some buffers overlap, so a few watertowers are in more than one

# use dplyr functions to count towers in each city
totals <- overlap %>%
  group_by(name) %>%
  # once a dataframe is "grouped", a function applied to it gets
  # applied to each group simultaneously
  # so the n() function, which counts rows, counts within groups
  # google "dplyr group_by" for more explanation
  summarise(n_towers = n())
dim(totals) # now we're back down to 100 obs, each one is a city/buffer


# view the result, colored by number of towers in each city
mapview(totals, zcol = "n_towers")

# looks good. Let's write it as a shapefile

##=======================================================================##
# STEP 5 - WRITE TO SHAPEFILE
##=======================================================================##

# write new shapefile
st_write(obj = totals, dsn = "cities_withtowers.shp")
# don't forget to name your shapefile something ending in ".shp"


# so to recap, we:
# 1 Read in each shapefile as an sf object and reprojected them
# 2 Explored the spatial data visually, with plot(), ggplot(), and mapview()
# 3 Created buffer zones around our point cities, and did a
#   spatial join between the towers (points) and cities (polygons)
# 4 Used dplyr on our spatial dataframe to count up the totals within each
# 5 Wrote a new shapefile of the 100 largest cities in the Netherlands
#   (buffered by 5km), along with their names and the number of watertowers
#   in each one

