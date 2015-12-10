# Maps in R: Plotting data points on a map
# from http://www.milanor.net/blog/?p=594
# Note that the data sets used in this sample program were moved to Github.
# And this caused some changes in the code.

# read the airport locations from Github
library(RCurl)
airports <- read.csv(text=getURL("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"),header=F)
colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST","tz")
head(airports)

# draw the map of Europe and lay the airports over the map
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)
points(airports$lon, airports$lat, col = "red", cex = .6)

# read the air flight routes data from Github
routes <- read.csv(text=getURL("https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"), header=F)
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")
head(routes)

# count the both number of routes departing from and arriving to a particular airport
library(plyr)
departures <- ddply(routes, .(sourceAirportID), "nrow")
names(departures)[2] <- "flights"
arrivals <- ddply(routes, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flights"

# add the info on departing and arriving flights to the airports dataset 
airportD <- merge(airports, departures, by.x = "ID", by.y = "sourceAirportID")
airportA <- merge(airports, arrivals, by.x = "ID", by.y = "destinationAirportID")

# get the map from GoogleMaps
library(ggmap)
map <- get_map(location = 'Europe', zoom = 4)
# The ggmap command prepares the drawing of the map. 
# The geom_point function adds the layer of data points
# - aes indicates how aesthetics (points in this case) are to be generated; the lon variable is associated to the x axis, lat to y, and the size of the points is proportional to the value of the variable flights (actually to its square root;)
# - data indicates the dataset where the variable passed to aes are to be found;
# - the alpha parameter controls the transparency of the plotted points (some degree of transparency will make the overlapping circles distinguishable.)
mapPoints <- ggmap(map) + geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportD, alpha = .5)
# A few tweaks to the legend (so that it does report the actual number of departures rather than the square root,) 
mapPointsLegend <- mapPoints + scale_size_area(breaks = sqrt(c(1, 5, 10, 50, 100, 500)), labels = c(1, 5, 10, 50, 100, 500), name = "departing routes")
mapPointsLegend

# Example of faceting: two panels (one reporting the departing flights, the other the incoming ones.)

# create the data set containing both departures and arrivals
airportD$type <- "departures"
airportA$type <- "arrivals"
airportDA <- rbind(airportD, airportA)

# map the data
# map + data points
mapPointsDA <- ggmap(map) + geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportDA, alpha = .5)
# adjust the legend
mapPointsLegendDA <- mapPointsDA + scale_size_area(breaks = sqrt(c(1, 5, 10, 50, 100, 500)), labels = c(1, 5, 10, 50, 100, 500), name = "routes")
# panels according to type (departure/arrival)
mapPointsFacetsDA <- mapPointsLegendDA + facet_grid(. ~ type)
# plot the map
mapPointsFacetsDA
