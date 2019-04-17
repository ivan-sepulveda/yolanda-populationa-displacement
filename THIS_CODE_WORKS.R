data <- read.csv("TyphoonYol.csv")
data <- cbind(data[2],data[6:10])
data <- data[-4]

# install.packages("RgoogleMaps")
# install.packages('devtools')
library(RgoogleMaps)
# remove.packages("ggmap")
# devtools::install_github("dkahle/ggmap")
register_google(key = "AIzaSyDyMzCeX7tuq3aM9LIpxSSQ8rjWV1tvcq4")

#Encoding categorical values
data$Province = factor(data$Province, levels = c("AKLAN ", "ANTIQUE ", "BILIRAN ","CAPIZ",
                                                 "CEBU ", "DINAGAT ISLAND ", "EASTERN SAMAR ",
                                                 "ILOILO ", "LEYTE ", "MASBATE ", "NEGROS OCC. ", "PALAWAN ",
                                                 "SOUTHERN LEYTE ", "WESTERN SAMAR "), labels = c(1,2,3,4,5,6,
                                                                                                  7,8,9,10,11,12,
                                                                                                  13,14))

#Blank Column
data$Lat <- NA
data$Long <- NA

lat <- c(11.8166,11.3806,11.5833,11.3889,10.6079,10.1282,
         11.5001,11.0050,11.0891,12.3060,10.2926,9.8349,10.3849,12.0499)

# View(lat)

long <- c(122.0942,122.0635,124.4642,122.6277,123.8858,125.6095,125.5000,
          122.5373,124.8923,123.5589,123.0247,118.7384,124.9746,125.1166)

meanLat <- mean(lat)
meanLong <- mean(long)


prov_num <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)

coords_test <- matrix(data = coordinates, nrow=14,ncol = 3)
coords_test <- as.data.frame(as.matrix(coords_test))
coordinates <- c(prov_num,lat,long)


provinceColNum <- as.numeric(which(colnames(data) =="Province"))
latColNum_data <- as.numeric(which(colnames(data) =="Lat"))
longColNum_Data <-as.numeric(which(colnames(data) =="Long"))

colnames(coords_test) <-c("Province","Lat","Long")

for (currentRowNumber in 1:170) {
  currentProvinceNum <- as.numeric(data$Province[currentRowNumber])
  latitudeForThisProvince <- coords_test$Lat[currentProvinceNum]
  longitudeForThisProvince <- coords_test$Long[currentProvinceNum]
  
  data$Lat[currentRowNumber] <- latitudeForThisProvince
  data$Long[currentRowNumber] <- longitudeForThisProvince
}

# View(coordinates)

coords_test <- matrix(data = coordinates, nrow=14,ncol = 3)
coords_test <- as.data.frame(as.matrix(coords_test))

# coords <- colnames(coords) <- c("Lat","Long")

colnames(coords_test) <-c("Province","Lat","Long")

coordinates <- merge(lat,long)
colnames(coordinates)[1] <- "Lat"
colnames(coordinates)[2] <- "Long"

# create whole dataset, maybe call it damage_sum
# will have 12 rows, one for each province
# will have same num of columns, 6

damage_sum <- data.frame(as.matrix(data))
damage_sum <- damage_sum[1:12, ]
damage_sum[1:12] <- NA
damage_sum$Province <- c(1:12)

damage_sum <- damage_sum[1:12, 1:7]

# for loop to set it up below
# Summing up province data ratios

for(currentProvince in 1:12) {
  currentSumPartial <- 0
  currentSumTotally <- 0
  currentSumDispFam <- 0
  for (currentRow in 1:nrow(data)){
    if (data$Province[currentRow] == currentProvince){
      currentSumPartial = currentSumPartial + data$PARTIALLY_damaged[currentRow]
      currentSumTotally = currentSumPartial + data$TOTALLY_damaged[currentRow]
      # print(paste("cur row", currentRow))
      # 
      # print(paste("sum disp fam", currentSumDispFam))
      currentSumDispFam = currentSumDispFam + data$displaced_families[currentRow]
    }
  }
  damage_sum$PARTIALLY_damaged[currentProvince] <- currentSumPartial
  damage_sum$TOTALLY_damaged[currentProvince] <- currentSumTotally
  damage_sum$TOTAL_houses_damaged[currentProvince] <- (currentSumPartial + currentSumTotally)
  damage_sum$displaced_families[currentProvince] <- currentSumDispFam
  
  # print(paste("Prov: ", currentProvince))
  # print(paste("Lat: ", coords_test$Lat[currentProvince]))
  # print(paste("Lat: ", coords_test$Long[currentProvince]))
  damage_sum$Lat[currentProvince] = coords_test$Lat[currentProvince]
  damage_sum$Long[currentProvince] = coords_test$Long[currentProvince]
}
# install.packages("maps")
# install.packages("mapdata")

library(maps)
library(mapdata)

lat <- c(11.8166,11.3806,11.5833,11.3889,10.6079,10.1282,
         11.5001,11.0050,11.0891,12.3060,10.2926,9.8349,10.3849,12.0499)

# View(lat)

long <- c(122.0942,122.0635,124.4642,122.6277,123.8858,125.6095,125.5000,
          122.5373,124.8923,123.5589,123.0247,118.7384,124.9746,125.1166)

latLongFrame <- data.frame(matrix(NA, nrow = 14, ncol = 0))
latLongFrame$lat <- lat
latLongFrame$long <- long

rm(lat, long)

maxDamHouses <- max(damage_sum$TOTAL_houses_damaged)

damage_sum <- damage_sum[-3]
damage_sum[7] = scale(damage_sum[7])

#Code for Palawan Province
p <- ggmap(get_googlemap(center = c(long = meanLong, lat = meanLat),
                         destfile = "test.png" , 
                         zoom = 6,  sensor = TRUE,
                         maptype = "terrain" ,
                         API_console_key = Sys.getenv('GOOGLE_MAPS_API_KEY') , NEWMAP = TRUE))
p + geom_point(data = damage_sum, aes(x = Long, y = Lat, color = factor(Province)), size = damage_sum$TOTAL_houses_damaged + 2) +
  scale_color_manual(name = "Cluster", 
                     values = c(`1` = "yellow",
                                `2` = "orange",
                                `3` = "red",
                                `4` = "green",
                                `5` = "blue",
                                `6` = "yellow",
                                `7` = "orange",
                                `8` = "red",
                                `9` = "orange",
                                `10` = "green",
                                `11` = "blue",
                                `12` = "yellow"
                     ))

geom_point(data = damage_sum, aes(x = Lat, y = Long, colour="red", size=TOTAL_houses_damaged)) 
mapA <- get_googlemap(markers = latLongFrame, path = latLongFrame, scale = 2)