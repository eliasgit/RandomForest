
## Elias Gebremeskel
## Lesson 7 Assignments
rm(list = ls())
library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
data(tura)
tura <- tura/10000
dates <- substr(names(tura), 10, 16)
print(as.Date(dates, format="%Y%j"))
##
sceneID <- names(tura)
sceneinfo <- getSceneinfo(sceneID)
sceneinfo$date
sceneinfo$year <- factor(substr(sceneinfo$date, 1, 4), levels = c(2000,2005,2010))
year2000 <-which(sceneinfo$year==2000)
 tura2000 <- tura[[year2000]]
 plot(tura2000)
year2005 <- which(sceneinfo$year==2005)
 tura2005 <- tura[[year2005]]
plot(tura2005)
year2010 <- which(sceneinfo$year==2010)
tura2010 <- tura[[year2010]]
plot(tura2010)
## calculate Mean
## mean2000 <- mean(tura2000,na.rm = TRUE)
mean2000 <- calc(tura2000, fun=mean, na.rm = TRUE)
plot(mean2000)
mean2005 <- calc(tura2005, fun=mean, na.rm = TRUE)
plot(mean2005)
mean2010 <- calc(tura2010, fun=mean, na.rm = TRUE)
plot(mean2010)
## composite
composite <- brick(mean2000, mean2005,mean2010)
plot(composite)
plotRGB(composite, 3, 2, 1, stretch='hist')
## Extract and Draw extent
e1 <- drawExtent()
e2 <- drawExtent()
print(e1)
print(e2)
## (3) drawpoly and select average time series
e1 <- extent(c(819332 , 820480.2 , 829718.3 , 830509.4))
plot(e1)
e2 <- extent(c(820709.9 , 821824.1 , 828289.4 , 829174))
## Calculate mean
meanNDVITS1 <- extract(tura,e1, fun=mean, na.rm = TRUE)
meanNDVITS2 <- extract(tura,e2, fun=mean, na.rm = TRUE)
## make data frame
df1 <- data.frame(  date = sceneinfo$date,  sensor = sceneinfo$sensor,  ndvi = as.numeric(meanNDVITS1),   zone = "region1")
df2 <- data.frame(  date = sceneinfo$date,  sensor = sceneinfo$sensor,  ndvi = as.numeric(meanNDVITS2),   zone = "region2")
ts <- rbind(df1,df2)
## plots of the time series data
ggplot(data = tss, aes(x = date, y = ndvi)) +  
  geom_point() +
  geom_line() + 
  facet_wrap(~ zone, nrow = 2) + 
  theme_bw()
## plots of the time series with different colors
ggplot(data = tss, aes(x = date, y = ndvi, colour = zone)) + 
  geom_point() + 
  geom_line()

## End