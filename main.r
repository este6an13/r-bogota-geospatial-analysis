# name: "Diego Esteban Quintero Rey"
# program: "Ingeniería de Sistemas y Computación"
# course: "Introducción a la Biología Computacional, 2023-I"


#install.packages("sp")
#install.packages("rgdal")
#install.packages("raster")
#install.packages("dismo")
#install.packages("rgeos")
#install.packages("car")

rm(list = ls(all.names = TRUE)) 
gc()

library("sp")
library("rgdal")
library("raster")
library("dismo")
library("rgeos")
library("car")

# reads the shapefile into a SpatialPolygonsDataFrame object called LOCALIDADES
LOCALIDADES = readOGR(dsn = "data", layer = "localidades_BOGOTA_DC")

# calculates the centroids of each locality
CENTROIDS = gCentroid(LOCALIDADES, byid=T)

# plots the LOCALIDADES polygons
plot(LOCALIDADES)

# adds the CENTROIDS points to the plot using the add = TRUE argument
plot(CENTROIDS, add=TRUE, col = "red")

# creates a data frame called R_points from the CENTROIDS object, storing the longitude and latitude coordinates of the centroids
R_points = data.frame(CENTROIDS)

# assigns appropriate column names to the R_points data frame
names(R_points) = c("Long","Lat")

R_points

# creates object for coordinate reference system (CRS) as WGS84
WGS84 = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# creates a SpatialPointsDataFrame object from the R_points data frame 
SPDF = SpatialPointsDataFrame(coords = R_points[,c("Long","Lat")], data = R_points, proj4string = WGS84)

# loads a raster file into a RasterLayer object called SRTM_30
SRTM_30 = raster("data/raster/SRTM_30_BOGOTA_DC_CLIP.tif")

plot(SRTM_30)

plot(SPDF, col="red", add=TRUE)

# creates buffer polygons around each point in the SPDF object using the gBuffer
# function, with a buffer width of 0.01 (in the same units as the CRS)
BUFFERS <- gBuffer(SPDF, width = 0.01, byid = TRUE)

# plots the BUFFERS polygons
plot(BUFFERS)

# adds a new column called "ID" to the BUFFERS object, 
# assigning sequential IDs from 0 to 19
BUFFERS$ID = seq(0, 19, 1)
  
head(BUFFERS)

# masks the SRTM_30 raster with the BUFFERS polygons using the mask function, 
# creating a new raster object called mask
mask = mask(SRTM_30, BUFFERS)

plot(mask)

# splits the BUFFERS object into subsets 
# based on the unique values in the ID column
SUBSETS <- split(BUFFERS, BUFFERS$ID)

# applies anonymous function to SUBSETS list
results <- sapply(SUBSETS, function(subset) {
  mask_subset <- mask(SRTM_30, subset)
  values_subset <- getValues(mask_subset)
  sd_subset <- sd(values_subset, na.rm = TRUE) # excludes any missing values (NAs)
  return(sd_subset)
})

# vector, which contains the standard deviations 
# calculated for each subset in the SUBSETS list
results

# Get the names of the localidades from the LOCALIDADES object
localidades_names <- LOCALIDADES$Nombre_de_l

# Combine the names and results vectors into a data frame
results_df <- data.frame(localidades_names, results)

# Order the data frame by the results column in descending order
results_df <- results_df[order(results_df$results, decreasing = TRUE), ]

# Give names to the columns
colnames(results_df) <- c("Localidades", "Standard Deviation")

# Print the ordered localidades names
print(results_df)





SENT_2 = brick("data/raster/Sent2_mean_2020_Bog_clip.tif")

names = c('BLUE', 'GREEN', 'RED', 'RED_EDGE_1', 'RED_EDGE_2', 'RED_EDGE_3', 'NIR', 'RED_EDGE_4', 'SWIR_1', 'SWIR_2')

names(SENT_2) = names

NDVI_SENT_2 = (SENT_2$NIR - SENT_2$RED) / (SENT_2$NIR + SENT_2$RED)

plot(NDVI_SENT_2)

EVI_SENT_2 <- 2.5 * ((SENT_2$NIR - SENT_2$RED) / (SENT_2$NIR + 6 * SENT_2$RED - 7.5 * SENT_2$BLUE + 1))

plot(EVI_SENT_2)

L <- 0.5  # SAVI parameter, adjust as needed

SAVI_SENT_2 <- ((1 + L) * (SENT_2$NIR - SENT_2$RED)) / (SENT_2$NIR + SENT_2$RED + L)

plot(SAVI_SENT_2)

stack = stack(NDVI_SENT_2, EVI_SENT_2, SAVI_SENT_2)

plot(stack)

names = c('NDVI', 'EVI', 'SAVI')

names(stack) = names

stack

set.seed(42)

points = randomPoints(mask=SRTM_30, n=100)

points = data.frame(points)

names(points) = c("Long","Lat")

# creates object for coordinate reference system (CRS) as WGS84
WGS84 = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

SPDF = SpatialPointsDataFrame(coords = points[,c("Long","Lat")], data = points, proj4string = WGS84)

plotRGB(stack, r=3, g=2, b=1, stretch="hist")

plot(SPDF, col="red", add=TRUE)

TABLE = extract(stack, SPDF)

TABLE = data.frame(TABLE, SPDF)

head(TABLE)

cor.test(TABLE$NDVI, TABLE$EVI, method = "pearson")

scatterplot(TABLE$NDVI, TABLE$EVI, main="EVI vs NDVI", xlab="NDVI", ylab="EVI", pch=19, regLine = TRUE, smooth = FALSE)

cor.test(TABLE$NDVI, TABLE$SAVI, method = "pearson")

scatterplot(TABLE$NDVI, TABLE$SAVI, main="SAVI vs NDVI", xlab="NDVI", ylab="SAVI", pch=19, regLine = TRUE, smooth = FALSE)

cor.test(TABLE$EVI, TABLE$SAVI, method = "pearson")

scatterplot(TABLE$NDVI, TABLE$SAVI, main="SAVI vs EVI", xlab="EVI", ylab="SAVI", pch=19, regLine = TRUE, smooth = FALSE)
