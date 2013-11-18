## Elias Gebremeskel
## Date 17.11.2013
## Supervised(Random forest) Raster Image Classificatio
## Assignment 6
#################
setwd("M:/Geo_script/lesson6") ## working directory
#load the necessary packages
library(rasta)
library(igraph)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
# load in the training classes and Look-Up Table (LUT) of the Gewata area
data(lulcGewata)
data(LUTGewata)
## plot of the original lulcGewata                                                                    ters)
cols <- c("orange", "light green", "brown", "light pink", "dark green", "light blue")
plot(lulcGewata, col=cols, legend=FALSE)
legend("topright", legend=LUTGewata$Class, fill=cols)
## Draw Training Polygones
## Cropland
e <- drawExtent()
plot(lulcGewata, ext=e)
cropland <- drawPoly(sp=TRUE)
projection(cropland) <- projection(GewataB2)
## Forest
e <- drawExtent()
plot(lulcGewata, ext=e)
forest <- drawPoly(sp=TRUE)
projection(forest) <- projection(GewataB2)
## bamboo
e <- drawExtent()
plot(lulcGewata, ext=e)
bamboo <- drawPoly(sp=TRUE)
projection(bamboo) <- projection(GewataB2)
## baresoil
e <- drawExtent()
plot(lulcGewata, ext=e)
baresoil <- drawPoly(sp=TRUE)
projection(baresoil) <- projection(GewataB2)
## wetland
e <- drawExtent()
plot(lulcGewata, ext=e)
wetland <- drawPoly(sp=TRUE)
projection(wetland) <- projection(GewataB2)
## coffeeplant
e <- drawExtent()
plot(lulcGewata, ext=e)
coffeeplant<- drawPoly(sp=TRUE)
projection(coffeeplant) <- projection(GewataB2)
##convert the training polygones to SpatialPolygonsDataFrame
cropland <- SpatialPolygonsDataFrame(cropland, data=data.frame(
 class="agricultural land"), match.ID=FALSE)
forest <- SpatialPolygonsDataFrame(forest, data=data.frame(
 class="forest area"), match.ID=FALSE)
bamboo <- SpatialPolygonsDataFrame(bamboo, data=data.frame(
 class="bamboo tree "), match.ID=FALSE)
wetland <- SpatialPolygonsDataFrame(wetland, data=data.frame(
 class="wetland area "), match.ID=FALSE)
coffeeplant<- SpatialPolygonsDataFrame(coffeeplant, data=data.frame(
 class="coffeeplant invetment area "), match.ID=FALSE)
baresoil <- SpatialPolygonsDataFrame(baresoil, data=data.frame(
 class="bare soil area "), match.ID=FALSE)
## Add all polygones in to one " Fusing polygones" by giving a uniqe ID
cropland <- spChFIDs(cropland, "cropland")
forest <- spChFIDs(forest, "forest")
coffeeplant <- spChFIDs(coffeeplant, "bamboo")
bamboo <- spChFIDs(bamboo, "wetland")
wetland <- spChFIDs(wetland,"coffee" )
baresoil <- spChFIDs(baresoil, "baresoil")
## add/merge all as one object
library(maptools)
trainingPoly2 <- spRbind(cropland, forest)
trainingPoly2 <- spRbind(trainingPoly2, bamboo)
trainingPoly2 <- spRbind(trainingPoly2, wetland)
trainingPoly2 <- spRbind(trainingPoly2, coffeeplant)
trainingPoly2 <- spRbind(trainingPoly2, baresoil)
trainingPoly2@data
plot(lulcGewata)
plot(trainingPoly2, add=TRUE)
trainingPoly2@data$class
##preparing data for the classification: we will use the brick land sat of band2,band3,band4,NDVI and vcfGewata
## Pre-Processing of the data
data(GewataB2) #band 2
data(GewataB3)  # band 3
data(GewataB4)   #  band 3
## stack or Brick all the bands in to one
gewata <- brick(GewataB2, GewataB3, GewataB4)
## Compute the NDVI of the raster brick images of Gewata
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})
## filtering the data of NDVI using Focal Nighbourhood function
w <- matrix(1/9, nc=3, nr=3)
ndvi<- focal(ndvi, w=w)
## Add data of vegetation tree cover percentage
data(vcfGewata)
## Remove values greater than of 100 %
vcfGewata[vcfGewata > 100] <- NA
## Combined NDVI & VcfGewata & rescale the NDVI to 10000
ndvi <- calc(ndvi, fun = function(x) floor(x*10000))
dataType(ndvi) <- "INT2U"
## calculate the Covariates value of the raster brick,NDVI and tree % cover
covs <- addLayer(gewata, ndvi, vcfGewata)
plot(covs)
## plot of the training polygons
data(trainingPoly2)
plot(ndvi)
plot(trainingPoly2, add = TRUE)
trainingPoly2@data
trainingPoly2@data$class
## Reclass and assign CODES
reclass <- function(x){
which(x==levels(trainingPoly2@data$class))
}
trainingPoly2@data$Code <- sapply(trainingPoly2@data$class, FUN=reclass)
classes <- rasterize(trainingPoly2, ndvi, field='Code')
dataType(classes) <- "INT1U"
## Plot with legends
cols <- c("orange","dark green","light green","light blue","yellow", "brown")
plot(classes, col=cols, legend=FALSE)
legend("bottomright", legend=c("cropland", "forest", "bamboo","wetland","coffeeplant","baresoil"), fill=cols, bg="white")
## mask the rasterBrick which only representing the training pixels.
covmasked <- mask(covs, classes)
plot(covmasked)
names(classes) <- "class"
trainingbrick <- addLayer(covmasked, classes)
#plot(trainingbrick)
##Add All Values of the inputs for the random forest from the raster brick
valuetable <- getValues(trainingbrick)
valuetable <- as.data.frame(valuetable)
##Remove NA and keep only the valid data
valuetable <- valuetable[!is.na(valuetable$class),]
## Convert to factors
valuetable$class <- factor(valuetable$class, levels = c(1:6))
## Remove NA values from the covariates/predictor columns
valuetable <- na.omit(valuetable)
## RANDOM FOREST Supervised Classification of Gewata
library(randomForest)
modelRF <- randomForest(x=valuetable[,c(1:5)], y=valuetable$class,
 importance = TRUE)
names(modelRF)
summary(modelRF)
## Confusion Matrices
modelRF$confusion 
colnames(modelRF$confusion) <- c("cropland", "forest","bamboo", "wetland","coffeeplant","baresoil","class.error")
rownames(modelRF$confusion) <- c("cropland", "forest","bamboo", "wetland","coffeeplant","baresoil")
modelRF$confusion # Resulted 0.0.820512821 or 82% over all accuracy
## Which classes have the highest accuracy? Lowest? 
## the NDVI has the highest accuracy and gewataB3 has the lowest accuracy. on the other hand, gewataB2 has higher deacreasing Gini and VCf200Gewata has lower value.
varImpPlot(modelRF)
##Is the importance ranking of the input bands dierent in this case to the 3-class classi-
  ##cation we did earlier? If so, how, and what has changed in this case?
## the ranking has a difference.it shows the LULC classification has more similarity with the NDVI and Band 4 of the images, and also, the homogenity of the classes is more strong in band 2.
##########################################################
# predict the classification of land cover using the RF model
predLC <- predict(covs, model=modelRF, na.rm=TRUE)
cols <- c("orange","dark green","light green","light blue","yellow", "brown")
plot(predLC, col=cols, legend=FALSE)
legend("topright", legend=c("cropland", "forest", "bamboo","wetland","coffeeplant","baresoil"), fill=cols, bg="white")

## ggPlot each classes to compare with the land cover
valuetable$label <- with(valuetable, ifelse(class==1, "cropland",
                  ifelse(class==2, "forest",
                  ifelse(class==3,"bamboo",
                  ifelse(class==4,"wetland",    
                  ifelse(class==5,"coffeeplant","baresoil" ))))))
# 1. NDVI
 pndvi <- ggplot(data=valuetable, aes(x=NDVI)) +
   geom_histogram(binwidth=300) +
   facet_wrap(~ label) +
   theme_bw()
pndvi
# 2. VCF
pvcf <- ggplot(data=valuetable, aes(x=vcf2000Gewata)) +
   geom_histogram(binwidth=5) +
   labs(x="% Tree Cover") +
   facet_wrap(~ label) +
   theme_bw()
pvcf
# 4. Bands 3 and 4
 pB3B4 <- ggplot(data=valuetable, aes(x=gewataB3, y=gewataB4)) +
  stat_bin2d() +
  facet_wrap(~ label) +
  theme_bw()
pB3B4
# 4. Bands 2 and 3
 pB2B3 <- ggplot(data = valuetable, aes(x=gewataB2, y=gewataB3)) +
   stat_bin2d() +
   facet_wrap(~ label) +
   theme_bw()
 pB2B3
## the coffe plant and the wet land seems overlaped in the "NDVI value"
## the bambo and the  forest has a bit overlap in "VCF tree cover"
##End