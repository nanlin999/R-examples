# Plot the Imagine Cup Earth competition data on the Google map of Bay of Bengal
setwd("/Users/nlin/Documents/Teaching/R/visualization")

# read data
ChlA.1 <- read.table("../data/2015BengalData/2015 01 Bay of Bengal Chlorophyll A Data.csv",sep=",",na.strings="NaN",header=F)
ChlA.2 <- read.table("../data/2015BengalData/2015 02 Bay of Bengal Chlorophyll A Data.csv",sep=",",na.strings="NaN",header=F)
ChlA.3 <- read.table("../data/2015BengalData/2015 03 Bay of Bengal Chlorophyll A Data.csv",sep=",",na.strings="NaN",header=F)
ChlA.4 <- read.table("../data/2015BengalData/2015 04 Bay of Bengal Chlorophyll A Data.csv",sep=",",na.strings="NaN",header=F)
ChlA.5 <- read.table("../data/2015BengalData/2015 05 Bay of Bengal Chlorophyll A Data.csv",sep=",",na.strings="NaN",header=F)
ChlA.6 <- read.table("../data/2015BengalData/2015 06 Bay of Bengal Chlorophyll A Data.csv",sep=",",na.strings="NaN",header=F)

# convert the data into a three column matrix
myreshape <- function(dat){
  dat = as.matrix(dat)
  lon = as.numeric(dat[1,-1])
  lat = as.numeric(dat[-1,1])
  intensity = dat[-1,-1]
  newdata = matrix(0,ncol=3,nrow=length(lat)*length(lon))
  for (i in 1:length(lat))
    for (j in 1:length(lon)){
        newdata[(i-1)*length(lon)+j,] = c(lon[j],lat[i],intensity[i,j])
    }
  nomissing = data.frame(newdata[complete.cases(newdata),])
  colnames(nomissing) = c("lon", "lat", "algae")
  return(nomissing)
}
for (i in 1:6){
  original = paste("ChlA.",i,sep="")
  new = paste("ChlA.",i,".reshaped",sep="")
  assign(new, myreshape(get(original)))
}

# plot the data
library(ggmap)
library(RColorBrewer)
map <- get_map(location = 'Bay of Bengal', zoom = 5,source="google")
for (i in 1:6){
  datname = paste("ChlA.",i,".reshaped",sep="")
  mapPoints <- ggmap(map) +
    geom_point(aes(x = lon, y = lat,colour=algae), data = get(datname)) +
    scale_colour_gradient(high = "yellow", low="lightblue", limit=c(0,85))
  ggsave(file = paste("./image/",i,".pdf",sep=""),plot = mapPoints,width=6, height=6)
}


