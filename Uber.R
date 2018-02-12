
#Loading Uber Data into R studio
uber_data <- read.csv("Uber Request Data.csv")
str(uber_data)

#Creating Alias of the data 
p<-as.character(uber_data$Request.timestamp)
q<-as.character(uber_data$Drop.timestamp)
require(dplyr)
require(ggplot2)
require(scales)
library("lubridate")
library(xlsx)

#Data Cleaning 
#One of the Date formats id 1/1/1990 and other is 1-1-1990
uber_data$Request.timestamp=parse_date_time(uber_data$Request.timestamp, orders="dmy hm")
uber_data$Request.timestamp1=parse_date_time(p, orders="dmy hms")

uber_data$Drop.timestamp=parse_date_time(uber_data$Drop.timestamp, orders="dmy hm")
uber_data$Drop.timestamp1=parse_date_time(q, orders="dmy hms")

#Extracting Date
uber_data$Req_Date1<-as.numeric(format(uber_data$Request.timestamp, "%d"))
uber_data$Req_Date2<-as.numeric(format(uber_data$Request.timestamp1, "%d"))

# Extracting Request Hour
uber_data$Hour1<-as.numeric(format(uber_data$Request.timestamp, "%H"))
uber_data$Hour2<-as.numeric(format(uber_data$Request.timestamp1, "%H"))

#Extracting Drop Time Hour
uber_data$Hour_Drop1<-as.numeric(format(uber_data$Drop.timestamp, "%H"))
uber_data$Hour_Drop2<-as.numeric(format(uber_data$Drop.timestamp1, "%H"))

#merging the columns to create Unique Columns
 uber_data$Hour_Request <- rowMeans(uber_data[, c("Hour1", "Hour2")], na.rm=TRUE)
 uber_data$Hour_Drop <- rowMeans(uber_data[, c("Hour_Drop1", "Hour_Drop2")], na.rm=TRUE)
 uber_data$Date <- rowMeans(uber_data[, c("Req_Date1", "Req_Date2")], na.rm=TRUE)
 
 
 
#write.xlsx(uber_data, "new_uber.xlsx")

 
  #Plot Demand of Uber Cabs per hour 
 perhr_req <- ggplot(uber_data,aes(x=factor(Hour_Request),fill=factor(Pickup.point)))
 
 p_1 <- perhr_req+geom_bar(stat='count',position = "dodge")+
   ggtitle("Demand of Cabs per Hour")+
   labs(x="Time ", y="Number of Cabs ")+
   labs(fill="Pickup Point")
 
 p_1
 
 

#Creating Time Slots
Hour_Request <- c(0:23)
Time_Slot1 <- c("Early_Morning","Morning_demand","Day_Time","Evening_Demand","Late_Night")
# How many times the time slots are to be repeated
times <- c(4,6,7,5,2)
Time_Slot <- rep(Time_Slot1,times)
 new_frame <- data.frame(Time_Slot,Hour_Request)
 #Merge uber with the newly created dataframe
 uber_data <- merge(uber_data,new_frame,by="Hour_Request",all.x=TRUE)
 
 #Create Subset of Trips completed
 trips_completed <- subset(uber_data,uber_data$Status=="Trip Completed")
 #Plot Demand Accordning to time slots-Trips Completed
 Timeslot_bar <- ggplot(trips_completed,aes(x=Time_Slot))
 p2 <- Timeslot_bar+geom_bar(stat="count" ,col="black",fill="pink")+
   ggtitle("Trips completed in Time Slots")+
   labs(x="Time Slots",y="Trips Completed")+
   geom_text(stat='count',aes(label=..count..),vjust=-1)+
   guides(fill=FALSE)+
   scale_x_discrete(limits=c("Early_Morning","Morning_demand","Day_Time",
                             "Evening_Demand","Late_Night"))
 
 

 p2
 
 #Trip Status Slot-wise
 
 timeslot_request_count <- ggplot(uber_data,aes(x=factor(Time_Slot),fill=factor(Status)))
 p3 <- timeslot_request_count+geom_bar(stat="count",position = "stack",col="black")+
   ggtitle("Trips Status Slot Wise")+
   scale_x_discrete(limits=c("Early_Morning","Morning_demand","Day_Time",
                             "Evening_Demand","Late_Night"))+
   labs(x="Time Slots",y="Number of Requests")+labs(fill="Trip Status")+
   scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled"))

 
 
 p3
 
 
 
 
 # Large number of Cab requests got cancelled during the Morning_Demand slot
 # Subset of the same 
  Morning_Problem <- subset(uber_data,uber_data$Time_Slot=="Morning_demand")
  # Plot Morning demand of cabs and Pickup point
 Morning_Problem_Plot <- ggplot(Morning_Problem,aes(x=factor(Status),fill=factor(Pickup.point)))
 p4 <-Morning_Problem_Plot+geom_bar(stat="count",position = "stack")+
   ggtitle("Morning Demand Cab")+
   labs(x="Status",y="Total count")+
   labs(fill="Pickup Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))+
   annotate("text", x=-Inf,y=Inf,label="Airport - 2.96% & City = 97.20%", hjust=-.1,vjust=1)

 p4
 
 #trips cancelled for the Morning 
 Sum_trip_cancel <- length(which(Morning_Problem$Status=="Cancelled"))
 
 # trips cancelled from airport for Morning 
 Sum_airport_trip_cancel <- length(which((Morning_Problem$Pickup.point=="Airport") & (Morning_Problem$Status == "Cancelled")))
 
 # trips cancelled from city for Morning 
 Sum_city_trip_cancel <- length(which((Morning_Problem$Pickup.point=="City") & (Morning_Problem$Status == "Cancelled")))

  #  Percent trips cancelled from city out of total trips cancelled - morning 
 percent_trip_cancel_city <- (Sum_city_trip_cancel/Sum_trip_cancel*100)

  # Percent of trips cancelled from airport 
 percent_trip_cancel_airport <- (Sum_airport_trip_cancel/Sum_trip_cancel*100)

  # trips requested from city to airport - morning 
 demand_trip_request_city <- length(which(Morning_Problem$Pickup.point=="City"))

  #trips completed from city to airport -morning 
 demand_trip_city_completed <- length(which((Morning_Problem$Pickup.point=="City")& (Morning_Problem$Status=="Trip Completed")))
 
 
 
 
 
 
 #subset Evening demand
 Evening_Problem<- subset(subset(uber_data,uber_data$Time_Slot=="Evening_Demand"))
 # Plot Evening demand of cabs and Pickup point
Evening_Problem_plot <- ggplot(Evening_Problem,aes(x=factor(Status),fill=factor(Pickup.point)))
 p5 <- Evening_Problem_plot+geom_bar(stat="count",position = "stack")+
   ggtitle("Evening Demand Plot")+
   labs(x="Status",y="Total count")+
   labs(fill="Pickup Point")+scale_x_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))+
   annotate("text", x=-Inf,y=Inf,label="Airport - 94.89% & City = 5.10%", hjust=-.1,vjust=1)  

 p5
 
 
 
 #  service requests with no cars available for evening time slot
 Sum_no_car_available <- length(which( Evening_Problem$Status=="No Cars Available"))
 
 # service requests with no cars available from airport during evening 
 Sum_airport_no_car_available <- length(which(( Evening_Problem$Pickup.point=="Airport") & ( Evening_Problem$Status == "No Cars Available")))
 
 # service requests with no cars available
 Sum_city_no_car_available <- length(which(( Evening_Problem$Pickup.point=="City") & ( Evening_Problem$Status == "No Cars Available")))

  # Percentage of no cars available status from city 
 percent_city_nocar <- (Sum_city_no_car_available/Sum_no_car_available*100)
 
 # Percentage of no cars available status from airport
 percent_airport_nocar <- (Sum_airport_no_car_available/Sum_no_car_available*100)
 
 #service requests from airport to city 
 demand_no_car_request_airport <- length(which(Evening_Problem$Pickup.point=="Airport"))
 
 #trips completed from airport to city 
 demand_no_car_request_airport_completed <- length(which((Evening_Problem$Pickup.point=="Airport") & (Evening_Problem$Status=="Trip Completed")))
 
 
 
 
 
 
 
 
 











