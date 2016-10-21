###This is the code the outputs the route level analysis in the westbound direction.

####################################################
####################################################
####                                            ####
####    Route Analysis.R                        ####
####                                            ####
####    Glick, Travis    tglick@pdx.edu         ####
####                                            ####
####    Date            July 2016               ####
####                                            ####
####################################################
####################################################
# 
# install.packages("maps")
# install.packages("xlsx")
# install.packages("chron")
# install.packages("ggmap")
# install.packages("rgdal")
# install.packages("taRifx")
# install.packages("maptools")
install.packages("rgeos")

install.packages("psych")

library(sp)
# library(maps)
library(plyr)
library(rowr)
# library(xlsx)
# library(chron)
# library(ggmap)
library(rgeos)
library(rgdal)
#library(taRifx)
library(psych)
library(maptools)
library(geosphere)


#################################################################################################
#### ## ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #####   
##### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ## ####
#################################################################################################

# setwd("//stash/marston/Active_Projects/14-10 Arterial PMs/_Analysis/_Regression")

#Original Data Location
read_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_Data/November 2014/Original_Data-Do_not_modify/"

#Names of Original Data
readname1 <- "Line 9 Stop Event Nov14 Whole Line.csv"
readname2 <- "stop definition.csv"
readname3 <- "Line 9 Cyclic Nov14 Whole Line.csv"
readname4 <- "TriMet stop IDs.csv" 
readname5 <- "stopid.csv"

hr <- read.csv(paste(read_path,readname3,sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
sl <- read.csv(paste(read_path,readname1,sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
def <- read.csv(paste(read_path,readname2,sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
stopid <- read.csv(paste(read_path,readname4,sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)


read_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/"
stopnums <- read.csv(paste(read_path,readname5,sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Process Original Data

# convert service date format
sl_serviceDate <- sl$SERVICE_DATE
sl_serviceDate <- as.Date(sl_serviceDate, format="%d%b%Y:%H:%M:%S")
sl$SERVICE_DATE <- as.Date(sl_serviceDate, format="%Y-%m-%d")


hr_serviceDate <- hr$OPD_DATE
hr_serviceDate <- as.Date(hr_serviceDate, format="%d%b%Y:%H:%M:%S")
hr$OPD_DATE <- as.Date(hr_serviceDate, format="%Y-%m-%d")



#####################################################################
########################    input parameters    #####################
#####################################################################

input_dates_all <- c("2014-11-03","2014-11-04","2014-11-05","2014-11-06","2014-11-07",
                     "2014-11-10","2014-11-11","2014-11-12","2014-11-13","2014-11-14",
                     "2014-11-17","2014-11-18","2014-11-19","2014-11-20","2014-11-21")

start_time <- "00:00:01"
end_time <- "23:59:59"


read_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/"
latlong <- read.csv(paste(read_path,"Route_latlong.csv",sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

latlongfull <- read.csv(paste(read_path,"alllatlong.csv",sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

i_1 <- 1
j_1 <- 1

# for (i_1 in 1:22) {
for (j_1 in 1:15) {
  input_date <- input_dates_all[j_1]
  
  latitude1 <- latlong$Lat_1[i_1]
  longitude1 <- latlong$Long_1[i_1]
  
  latitude2 <- latlong$Lat_2[i_1]
  longitude2 <- latlong$Long_2[i_1]
  
  number_POIs <- latlong$Points[i_1]
  
  Range_Downstream <- 160
  
  Range_Upstream <- 80
  
  #Defining Direction
  # 0 = Eastbound (outbound to Gresham)
  # 1 = Westbound (inbound to Portland)
  trip_direction <- 1
  
  #Output File number of rows
  Output_length <- 2000
  
  #Set Location ID for bus stops
  if (trip_direction == 0) {
    numstops1 <- latlong$EBS2[i_1] - latlong$EBS1[i_1]
    Location_ID <- stopnums$East[latlong$EBS1[i_1]]
    for (i in 1:numstops1) {
      Location_ID <- c(Location_ID, stopnums$East[latlong$EBS1[i_1]+i])
    }
    
  } else if (trip_direction == 1) {
    numstops1 <- latlong$WBS2[i_1] - latlong$WBS1[i_1]
    Location_ID <- stopnums$West[latlong$EBS1[i_1]]
    for (i in 1:numstops1) {
      Location_ID <- c(Location_ID, stopnums$West[latlong$EBS1[i_1]+i])
    }
  }
  
  #####################################################################
  ##########################  code_processing   #######################
  #####################################################################
  
  # Stop Level Data
  bus_sl <- sl
  
  input_date <- as.Date(input_date, format="%Y-%m-%d")
  bus_sl <- bus_sl[(bus_sl$SERVICE_DATE %in% input_date),]
  
  # aggregate duplicated records
  bus_sl <- ddply(bus_sl,.(SERVICE_DATE,VEHICLE_NUMBER,TRAIN,ROUTE_NUMBER,TRIP_NUMBER,DIRECTION,
                           LOCATION_ID,PUBLIC_LOCATION_DESCRIPTION,STOP_TIME),summarize,LEAVE_TIME=max(LEAVE_TIME),
                  ARRIVE_TIME=min(ARRIVE_TIME),DWELL=sum(DWELL),DOOR=sum(DOOR),LIFT=sum(LIFT),ONS=sum(ONS),
                  OFFS=sum(OFFS),ESTIMATED_LOAD=max(ESTIMATED_LOAD),MAXIMUM_SPEED=max(MAXIMUM_SPEED),
                  TRAIN_MILEAGE=max(TRAIN_MILEAGE),PATTERN_DISTANCE=max(PATTERN_DISTANCE),
                  X_COORDINATE=max(X_COORDINATE),Y_COORDINATE=max(Y_COORDINATE),TRIP_ID=max(TRIP_ID))
  
  bus_sl$GPS_Latitude <- NA
  bus_sl$GPS_Longitude <- NA
  
  data <- data.frame(long=bus_sl$X_COORDINATE, lat=bus_sl$Y_COORDINATE)
  coordinates(data) <- ~long+lat
  proj4string(data) <- CRS("+init=epsg:2913")
  data.proj <- spTransform(data, CRS("+init=epsg:4326"))
  bus_sl$GPS_Longitude <- data.proj$long
  bus_sl$GPS_Latitude <- data.proj$lat
  
  bus_sl <- subset(bus_sl, bus_sl$DIRECTION == trip_direction)
  bus_sl <- bus_sl[order(bus_sl$ROUTE,bus_sl$DIRECTION,bus_sl$SERVICE_DATE,bus_sl$VEHICLE_NUMBER,bus_sl$TRIP_NUMBER,bus_sl$ARRIVE_TIME),]
  
  
  #5-SR data
  bus_hr <- hr
  bus_hr <- bus_hr[,c("VEHICLE_ID","OPD_DATE","ACT_TIME","GPS_LONGITUDE","GPS_LATITUDE",
                      "BLOCK_CODE","EVENT_NO_TRIP","EVENT_NO_STOP","METERS")]
  colnames(bus_hr) <- c("vehicleNumber","serviceDate","actualTime","actual_GPS_Longitude","actual_GPS_Latitude","blockCode",
                        "eventNoTrip","eventNoStop","meters")
  
  bus_hr <- bus_hr[(bus_hr$serviceDate %in% input_date),]
  bus_hr <- bus_hr[order(bus_hr$serviceDate,bus_hr$vehicleNumber,bus_hr$actualTime),]
  
  
  
  #Assign Unique Trip Numbers to stop level
  bus_uniqueTripNumber <- NA
  bus_tripNumber <- bus_sl$TRIP_NUMBER
  j=1
  bus_uniqueTripNumber[1] <- j
  for (i in 2:nrow(bus_sl)) {
    if (bus_tripNumber[i] != bus_tripNumber[i-1]) {
      j <- j + 1 
    }
    bus_uniqueTripNumber[i] <- j
  }
  bus_sl$UID <- bus_uniqueTripNumber
  
  #assign unique trip numbers to high res
  
  i=1
  bus_sl_temp <- subset(bus_sl, bus_sl$UID == i)
  
  bus_hr_temp <- subset(bus_hr, bus_hr$vehicleNumber == bus_sl_temp$VEHICLE_NUMBER[1])
  
  bus_sl_min <- min(bus_sl_temp$ARRIVE_TIME)
  bus_sl_max <- max(bus_sl_temp$ARRIVE_TIME)
  
  bus_hr_temp <- subset(bus_hr_temp, bus_hr_temp$actualTime > bus_sl_min - 5 & bus_hr_temp$actualTime < bus_sl_max + 5)
  
  bus_hr_temp$UID <- i
  
  hrtemp <- bus_hr_temp
  
  for (i in 2:max(bus_sl$UID)) {
    bus_sl_temp <- subset(bus_sl, bus_sl$UID == i)
    
    bus_hr_temp <- subset(bus_hr, bus_hr$vehicleNumber == bus_sl_temp$VEHICLE_NUMBER[1])
    
    bus_hr_temp$UID <- i
    
    bus_sl_min <- min(bus_sl_temp$ARRIVE_TIME)
    bus_sl_max <- max(bus_sl_temp$ARRIVE_TIME)
    
    bus_hr_temp <- subset(bus_hr_temp, bus_hr_temp$actualTime > bus_sl_min - 5 & bus_hr_temp$actualTime < bus_sl_max + 5)
    
    if (nrow(bus_hr_temp)==0 | nrow(bus_hr_temp)==1 ) {
      next()
    }
    
    hrtemp <- rbind(hrtemp,bus_hr_temp)
  }
  
  bus_hr <- hrtemp
  
  bus_sl <- bus_sl[(bus_sl$UID %in% bus_hr$UID),]
  
  
  ### remove first and last stops
  
  uniquevalues <- unique(bus_sl$UID)
  
  for (i in 1:length(uniquevalues)) {
    k <- uniquevalues[i]
    bus_sl_temp <- subset(bus_sl, bus_sl$UID == k)
    
    # remove first stop
    bus_sl_temp <- bus_sl_temp[-1,]
    
    # remove last stop
    rowtemp <- nrow(bus_sl_temp)
    bus_sl_temp <- bus_sl_temp[-rowtemp,]
    
    if(i == 1) {
      bus_sl_temp2 <- bus_sl_temp
    } else {
      bus_sl_temp2 <- rbind(bus_sl_temp2, bus_sl_temp)
    }
  }
  
  bus_sl <- bus_sl_temp2
  
  
  
  ##### filter time stamps for hr again
  
  uniquevalues <- unique(bus_sl$UID)
  
  for (i in 1:length(uniquevalues)) {
    k <- uniquevalues[i]
    bus_sl_temp <- subset(bus_sl, bus_sl$UID == k)
    bus_hr_temp <- subset(bus_hr, bus_hr$UID == k)
    
    bus_sl_min <- min(bus_sl_temp$ARRIVE_TIME)
    bus_sl_max <- max(bus_sl_temp$ARRIVE_TIME)
    
    bus_hr_temp <- subset(bus_hr_temp, bus_hr_temp$actualTime > bus_sl_min - 5)
    bus_hr_temp <- subset(bus_hr_temp, bus_hr_temp$actualTime < bus_sl_max + 5)
    
    if (nrow(bus_hr_temp)==0 | nrow(bus_hr_temp)==1 ) {
      next()
    }
    
    if (i == 1) {
      hrtemp <- bus_hr_temp
    } else {
      hrtemp <- rbind(hrtemp,bus_hr_temp)
    }
  }
  
  bus_hr <- hrtemp
  bus_sl <- bus_sl[(bus_sl$UID %in% bus_hr$UID),]
  
  ##### filter time stamps for stop level 
  
  uniquevalues <- unique(bus_sl$UID)
  
  for (i in 1:length(uniquevalues)) {
    k <- uniquevalues[i]
    bus_sl_temp <- subset(bus_sl, bus_sl$UID == k)
    bus_hr_temp <- subset(bus_hr, bus_hr$UID == k)
    
    bus_hr_min <- min(bus_hr_temp$actualTime)
    bus_hr_max <- max(bus_hr_temp$actualTime)
    
    bus_sl_temp <- subset(bus_sl_temp, bus_sl_temp$ARRIVE_TIME > bus_hr_min)
    bus_sl_temp <- subset(bus_sl_temp, bus_sl_temp$ARRIVE_TIME < bus_hr_max)
    
    if (nrow(bus_sl_temp)==0 | nrow(bus_sl_temp)==1 ) {
      next()
    }
    
    if (i == 1) {
      sltemp <- bus_sl_temp
    } else {
      sltemp <- rbind(sltemp,bus_sl_temp)
    }
  }
  
  bus_sl <- sltemp
  bus_hr <- bus_hr[(bus_hr$UID %in% bus_sl$UID),]
  
  
  
  #### Apply Interval
  
  bus_hr$interval <- 0
  
  for (i in 1:(nrow(bus_hr)-1)) {
    if (bus_hr$UID[i] == bus_hr$UID[i+1]) {
      bus_hr$interval[i] <- bus_hr$actualTime[i+1] - bus_hr$actualTime[i]
    } else {
      bus_hr$interval[i] <- 5
    }
  }
  
  
  
  #Flag high resolution data
  
  # Apply point after
  
  bus_hr$GPS_Lat_1 <- NA
  bus_hr$GPS_Long_1 <- NA
  bus_hr$GPS_Lat_2 <- NA
  bus_hr$GPS_Long_2 <- NA
  
  for (i in 1:(nrow(bus_hr)-1)) {
    bus_hr$GPS_Lat_1[i] <- bus_hr$actual_GPS_Latitude[i]
    bus_hr$GPS_Long_1[i] <- bus_hr$actual_GPS_Longitude[i]
    
    if (bus_hr$UID[i] == bus_hr$UID[i+1]) {
      bus_hr$GPS_Lat_2[i] <- bus_hr$actual_GPS_Latitude[i+1]
      bus_hr$GPS_Long_2[i] <- bus_hr$actual_GPS_Longitude[i+1]
    }
  }
  
  # # remove actual GPS long
  # bus_hr <- bus_hr[,-4]
  # 
  # #remove actual gps lat
  # bus_hr <- bus_hr[,-4]
  # 
  # #remove block code
  # bus_hr <- bus_hr[,-4]
  # 
  # #remove meters
  # bus_hr <- bus_hr[,-6]
  
  
  # assign index to high resolution data that corresponds to a stop event data

  uniquevalues <- unique(bus_sl$UID)
  
  for (i in 1:length(uniquevalues)) {
    k <- uniquevalues[i]
    bus_hr_temp <- subset(bus_hr, bus_hr$UID == k)
    bus_sl_temp <- subset(bus_sl, bus_sl$UID == k)
    busstoprow <- NULL
    for (j in 1:nrow(bus_sl_temp)) {
      busstoprow <- c(busstoprow, which.min(abs(bus_hr_temp$actualTime - bus_sl_temp$ARRIVE_TIME[j])))
    }
    
    bus_hr_temp$BS <- 0
    
    for(j in 1:nrow(bus_sl_temp)) {
      bsr <- busstoprow[j]
      
      if (bsr == 1) {
        if(max(c(0,0,0,
                 bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                 bus_hr_temp$interval[bsr+3])) <= 5) {
          abc <- 4
        } else {
          abc <- (which.max(c(0,0,0,
                              bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                              bus_hr_temp$interval[bsr+3])))
        }
        if (abc == 4) {
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        } else {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        }
        
      } else if (bsr == 2) {
        if(max(c(0,0,bus_hr_temp$interval[bsr-1],
                 bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                 bus_hr_temp$interval[bsr+3])) <= 5) {
          abc <- 4
        } else {
          abc <- (which.max(c(0,0,bus_hr_temp$interval[bsr-1],
                              bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                              bus_hr_temp$interval[bsr+3])))
        }
        if (abc == 3) {
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        } else {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        }
        
      } else if (bsr == 3) {
        if(max(c(0,bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                 bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                 bus_hr_temp$interval[bsr+3])) <= 5) {
          abc <- 4
        } else {
          abc <- (which.max(c(0,bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                              bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                              bus_hr_temp$interval[bsr+3])))
        }
        if (abc == 2) {
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        } else {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        }
        
      } else if (bsr == 4) {
        if(max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                 bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                 bus_hr_temp$interval[bsr+3])) <= 5) {
          abc <- 4
        } else {
          abc <- (which.max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                              bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                              bus_hr_temp$interval[bsr+3])))
        }
        if (abc == 1) {
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        } else {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        }
      } else if (bsr == nrow(bus_hr_temp)) {
        if(max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                 bus_hr_temp$interval[bsr],0,0,0)) <= 5) {
          abc <- 4
        } else {
          abc <- (which.max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                              bus_hr_temp$interval[bsr],0,0,0)))
        } 
        if (abc == 4) {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
        } else {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        }
        
      } else if (bsr == (nrow(bus_hr_temp)-1)) {
        if(max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                 bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],0,0)) <= 5) {
          abc <- 4
        } else {
          abc <- (which.max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                              bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],0,0)))
        } 
        if (abc == 5) {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
        } else {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        }
        
      } else if (bsr == (nrow(bus_hr_temp)-2)) {
        if(max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                 bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],0)) <= 5) {
          abc <- 4
        } else {
          abc <- (which.max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                              bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],0)))
        } 
        if (abc == 6) {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
        } else {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        }
        
      } else if (bsr == (nrow(bus_hr_temp)-3)) {
        if(max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                 bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                 bus_hr_temp$interval[bsr+3])) <= 5) {
          abc <- 4
        } else {
          abc <- (which.max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                              bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                              bus_hr_temp$interval[bsr+3])))
        } 
        if (abc == 7) {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
        } else {
          bus_hr_temp$BS[bsr+abc-5] <- 1
          bus_hr_temp$BS[bsr+abc-4] <- 1
          bus_hr_temp$BS[bsr+abc-3] <- 1
        }
      } else {
          if(max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                   bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                   bus_hr_temp$interval[bsr+3])) <= 5) {
            abc <- 4
          } else {
            abc <- (which.max(c(bus_hr_temp$interval[bsr-3],bus_hr_temp$interval[bsr-2],bus_hr_temp$interval[bsr-1],
                                bus_hr_temp$interval[bsr],bus_hr_temp$interval[bsr+1],bus_hr_temp$interval[bsr+2],
                                bus_hr_temp$interval[bsr+3])))
          }
        bus_hr_temp$BS[bsr+abc-5] <- 1
        bus_hr_temp$BS[bsr+abc-4] <- 1
        bus_hr_temp$BS[bsr+abc-3] <- 1
        }
      }
    if(i == 1) {
      bus_hr_temp2 <- bus_hr_temp
    } else {
      bus_hr_temp2 <- rbind(bus_hr_temp2, bus_hr_temp)
    }
  }
    
    
  
  bus_hr_fin <- bus_hr_temp2

  ##### create line to snap points
  
  latlongfull <- subset(latlongfull,latlongfull$Set <= 22)
  
  lon <- latlongfull$Long
  lat <- latlongfull$Lat
  
  latlongline <- SpatialLines(list(Lines(Line(cbind(lon,lat)), ID="a")))
  
  
  #subset hr data to include only study area
  
  bus_hr_fin2 <- subset (bus_hr_fin, bus_hr_fin$GPS_Long_1 < max(lon) & bus_hr_fin$GPS_Long_1 > min(lon))
  
  
  #filter for complete cases
  
  bus_hr_fin3 <- bus_hr_fin2[complete.cases(bus_hr_fin2$GPS_Lat_1),]
  bus_hr_fin3 <- bus_hr_fin2[complete.cases(bus_hr_fin2$GPS_Long_1),]
  bus_hr_fin3 <- bus_hr_fin2[complete.cases(bus_hr_fin2$GPS_Lat_2),]
  bus_hr_fin3 <- bus_hr_fin2[complete.cases(bus_hr_fin2$GPS_Long_2),]
  
  ###### snap points to line
  bus_hr_fin3$GPS_Lat1_Snap <- NA
  bus_hr_fin3$GPS_Long1_Snap <- NA
  
  bus_hr_fin3$GPS_Lat2_Snap <- NA
  bus_hr_fin3$GPS_Long2_Snap <- NA
  
  lenghtrows <- nrow(bus_hr_fin3)
  
  for (i in 1:nrow(bus_hr_fin3)) {
    point1 <- cbind(bus_hr_fin3$GPS_Long_1[i],bus_hr_fin3$GPS_Lat_1[i])
    point1 <- SpatialPoints(point1)
    temppoint1 <- snapPointsToLines(point1, latlongline)
    temppoint1df <- as.data.frame(temppoint1)
    
    point2 <- cbind(bus_hr_fin3$GPS_Long_2[i],bus_hr_fin3$GPS_Lat_2[i])
    point2 <- SpatialPoints(point2)
    temppoint2 <- snapPointsToLines(point2, latlongline)
    temppoint2df <- as.data.frame(temppoint2)
    
    bus_hr_fin3$GPS_Lat1_Snap[i] <- temppoint1df$Y[1]
    bus_hr_fin3$GPS_Long1_Snap[i] <- temppoint1df$X[1]
    
    bus_hr_fin3$GPS_Lat2_Snap[i] <- temppoint2df$Y[1]
    bus_hr_fin3$GPS_Long2_Snap[i] <- temppoint2df$X[1]
    
    if(floor(i/1000)*1000 == i) {
      print(paste("Day ", j_1,"/15 - Part ", (i), "/", lenghtrows, sep=""))
    } 
  }
  
  bus_hr_fin4 <- bus_hr_fin3
  bus_hr_fin4$UID <- (bus_hr_fin4$UID + 1000*(j_1))
  
  
  if(j_1 == 1) {
    bus_hr_final <- bus_hr_fin4
  } else {
    bus_hr_final <- rbind(bus_hr_final, bus_hr_fin4)
  }
  
}

bus_hr_delay <- bus_hr_final

save_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/test/"
write.csv(bus_hr_delay, file= paste(save_path, "bus_hr_delay_temp1.csv",sep=""))


##############################################################################
#############################################################################

########## read in exported file


read_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/NoStops/"
bus_hr_delay2 <- read.csv(paste(read_path,"bus_hr_delay_temp1.csv",sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)


###### remove first collum numbers
bus_hr_delay2 <- bus_hr_delay2[-1]



###### calculate distances
bus_hr_delay2$dist <- NA

lengthrow <- nrow(bus_hr_delay2)
for (i in 1:nrow(bus_hr_delay2)){
  bus_hr_delay2$dist[i] <- (distVincentyEllipsoid(c(bus_hr_delay2$GPS_Long1_Snap[i],bus_hr_delay2$GPS_Lat1_Snap[i]),
                                                  c(bus_hr_delay2$GPS_Long2_Snap[i],bus_hr_delay2$GPS_Lat2_Snap[i])))/0.3048
  
  if(floor(i/10000)*10000 == i) {
    print(paste("Part ", (i), "/", lengthrow, sep=""))
  }
}


###### calculate speeds

bus_hr_delay2$speed <- NA

for (i in 1:nrow(bus_hr_delay2)){
  bus_hr_delay2$speed[i] <- bus_hr_delay2$dist[i]/bus_hr_delay2$interval[i]*(15/22)
  
  
  if(floor(i/10000)*10000 == i) {
    print(paste("Part ", (i), "/", lengthrow, sep=""))
  }
}


bus_hr_delay2 <- subset(bus_hr_delay2, bus_hr_delay2$dist > 0)
bus_hr_delay2 <- subset(bus_hr_delay2, bus_hr_delay2$interval > 3)
bus_hr_delay2 <- subset(bus_hr_delay2, bus_hr_delay2$dist < (mean(bus_hr_delay2$dist) + 4*sd(bus_hr_delay2$dist)))


save_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/NoStops/"
write.csv(bus_hr_delay2, file= paste(save_path, "bus_hr_delay_temp2.csv",sep=""))


########## read in exported file


read_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/NoStops/"
bus_hr_delay3 <- read.csv(paste(read_path,"bus_hr_delay_temp2.csv",sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

bus_hr_delay3 <- bus_hr_delay3[-1]

#### Create subset for no dwell


# calculate speeds for all points and put in other file
lengthrow <- nrow(latlongfull)

for (i in 2:(nrow(latlongfull)-1)) {
  bus_hr_POI <- subset(bus_hr_delay3, bus_hr_delay3$GPS_Long1_Snap > latlongfull$Long[i])
  bus_hr_POI <- subset(bus_hr_POI, bus_hr_POI$GPS_Long2_Snap < latlongfull$Long[i])
  
  df_temp <- as.data.frame(bus_hr_POI$speed)
  colnames(df_temp) <- i
  
  df_temp2 <- as.data.frame(bus_hr_POI$actualTime)
  colnames(df_temp2) <- i
  
  df_temp3 <- as.data.frame(bus_hr_POI$UID)
  colnames(df_temp3) <- i
  
  if(i == 2) {
    df_speed <- df_temp
    df_time <- df_temp2
    df_uid <- df_temp3
  } else {
    df_speed <- cbind.fill(df_speed,df_temp, fill=NA)
    df_time <- cbind.fill(df_time,df_temp2, fill=NA)
    df_uid <- cbind.fill(df_uid,df_temp3,fill=NA)
  }
  
  if(floor(i/100)*100 == i) {
    print(paste("Part ", (i), "/", lengthrow, sep=""))
  }
}

save_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/test/"
write.csv(df_speed, file= paste(save_path, "hr_speed_all.csv",sep=""))
write.csv(df_time, file= paste(save_path, "hr_times_all.csv",sep=""))
write.csv(df_uid, file= paste(save_path, "hr_uid_all.csv",sep=""))

speed_all <- df_speed
time_all <- df_time

bus_hr_nodwell <- subset(bus_hr_delay3, bus_hr_delay3$BS == 0)

lengthrow <- nrow(latlongfull)

for (i in 2:(nrow(latlongfull)-1)) {
  bus_hr_POI <- subset(bus_hr_nodwell, bus_hr_nodwell$GPS_Long1_Snap > latlongfull$Long[i])
  bus_hr_POI <- subset(bus_hr_POI, bus_hr_POI$GPS_Long2_Snap < latlongfull$Long[i])
  
  df_temp <- as.data.frame(bus_hr_POI$speed)
  colnames(df_temp) <- i
  
  df_temp2 <- as.data.frame(bus_hr_POI$actualTime)
  colnames(df_temp2) <- i
  
  df_temp3 <- as.data.frame(bus_hr_POI$UID)
  colnames(df_temp3) <- i
  
  if(i == 2) {
    df_speed <- df_temp
    df_time <- df_temp2
    df_uid <- df_temp3
  } else {
    df_speed <- cbind.fill(df_speed,df_temp, fill=NA)
    df_time <- cbind.fill(df_time,df_temp2, fill=NA)
    df_uid <- cbind.fill(df_uid,df_temp3,fill=NA)
  }
  
  if(floor(i/100)*100 == i) {
    print(paste("Part ", (i), "/", lengthrow, sep=""))
  }
}

save_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/test/"
write.csv(df_speed, file= paste(save_path, "hr_speed_nodwell.csv",sep=""))
write.csv(df_time, file= paste(save_path, "hr_times_nodwell.csv",sep=""))
write.csv(df_uid, file= paste(save_path, "hr_uid_all.csv",sep=""))

speed_nd <- df_speed
time_nd <- df_time

#################################
################################
##### Create graphs of delay per 25 ft.

read_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/test/"
hr_speed_all <- read.csv(paste(read_path,"hr_speed_all.csv",sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
hr_speed_nodwell <- read.csv(paste(read_path,"hr_speed_nodwell.csv",sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)


hr_delay_all <- (25/(22/15))*((1/hr_speed_all)-(1/35))
hr_delay_nodwell <- (25/(22/15))*((1/hr_speed_nodwell)-(1/35))


save_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/test/"
write.csv(hr_delay_all, file= paste(save_path, "hr_delay_all.csv",sep=""))
write.csv(hr_delay_nodwell, file= paste(save_path, "hr_delay_nodwell.csv",sep=""))



#############################
############################
#### intersection analysis


read_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/test/"
speed_all <- read.csv(paste(read_path,"hr_speed_all.csv",sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
speed_nd <- read.csv(paste(read_path,"hr_speed_nodwell.csv",sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
time_all <- read.csv(paste(read_path,"hr_times_all.csv",sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
time_nd <- read.csv(paste(read_path,"hr_times_nodwell.csv",sep=""), header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)



###############################
###############################
#Create matrix by time for all data

#range in minutes
range <- 1.5
window <- 15*60

numrows <- 26*(60/range)-4*(60/range)

speedtime <- as.data.frame(matrix(nrow=(numrows+1),ncol=(ncol(speed_all)+1)))
rownames(speedtime) <- 1:(numrows+1)
colnames(speedtime) <- 1:ncol(speed_all)

for (i in 1:(numrows+1)) {
  ctime <- 14400 + i*60*range -60*range
  speedtime[i,1] <- ctime
}

for (i in 2:ncol(speed_all)) {
  
  speed_temp <- speed_all[i]
  time_temp <- time_all[i]
  temp1 <- cbind(speed_temp,time_temp)
  
  for (j in 1:(numrows+1)) {
    ctime <- 14400 + j*60*range -60*range
    colnames(temp1) <- c("speed","time")
    temp2 <- subset(temp1, temp1$time > ctime-window/2)
    temp2 <- subset(temp2, temp2$time < ctime+window/2)
    speed1 <- harmonic.mean(temp2$speed)
    speedtime[j,i] <- speed1
  }
  if(floor(i/100)*100 == i) {
    print(paste("Part ", (i), "/", lengthrow, sep=""))
  }
}

save_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/test/"
write.csv(speedtime, file= paste(save_path, "speedtime_all_temp.csv",sep=""))


# install.packages("zoo")
library(zoo)

speedtime2 <- na.approx(speedtime)

write.csv(speedtime2, file= paste(save_path, "speedtime_all.csv",sep=""))

###############################
###############################
#Create matrix by time for no sl

speedtime <- as.data.frame(matrix(nrow=(numrows+1),ncol=(ncol(speed_nd)+1)))
rownames(speedtime) <- 1:(numrows+1)
colnames(speedtime) <- 1:ncol(speed_nd)

for (i in 1:(numrows+1)) {
  ctime <- 14400 + i*60*range -60*range
  speedtime[i,1] <- ctime
}

for (i in 2:ncol(speed_nd)) {
  
  speed_temp <- speed_nd[i]
  time_temp <- time_nd[i]
  temp1 <- cbind(speed_temp,time_temp)
  
  for (j in 1:(numrows+1)) {
    ctime <- 14400 + j*60*range -60*range
    colnames(temp1) <- c("speed","time")
    temp2 <- subset(temp1, temp1$time > ctime-window/2)
    temp2 <- subset(temp2, temp2$time < ctime+window/2)
    speed1 <- harmonic.mean(temp2$speed)
    speedtime[j,i] <- speed1
  }
  if(floor(i/100)*100 == i) {
    print(paste("Part ", (i), "/", lengthrow, sep=""))
  }
}

save_path <- "//stash/marston/Active_Projects/14-10 Arterial PMs/_2017TRB Analysis/Route Analysis/Codes/Westbound/test/"
write.csv(speedtime, file= paste(save_path, "speedtime_nd_temp.csv",sep=""))


# install.packages("zoo")
library(zoo)
library(missForest)

speedtime2 <- na.approx(speedtime)

write.csv(speedtime2, file= paste(save_path, "speedtime_nd.csv",sep=""))
