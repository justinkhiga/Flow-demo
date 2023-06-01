#####################################################
#                                                   #
#           Example Data Processing for             #
#         Continuous Flow Time-series Data          #
#                                                   #
#####################################################

#This code will read in the data from a USGS monitoring site.
#Data will be downloaded using USGS's dataRetrieval package

#This code will isolate storm events, return a completed storm catalog,
#and return an summary of annual rain and flow values, proportional to the amount of time
#that the monitoring took place over.

#clear environment so no data from previous analyses interferes
rm(list=ls())

#filepath of the working directory. This will be different for you!
filepath <- "C:/Users/justi/OneDrive/Desktop/Portfolio/R/Flow Data/"

#set the working environment to the desired path.
setwd(filepath)

#install packages if needed
#install.packages(c("lubridate", "tidyverse", "birk", "dataRetrieval", "readr") )

#We will be using lubridate to handle timestamps, tidyverse for chaining commands,
#birk for the "closest to" command.
#install these packages beforehand if needed.
library(lubridate)
library(tidyverse)
library(birk)
library(dataRetrieval)
library(readr)
library(gridExtra)

#We will now read in the data. We will be using the Moanalua Flow Gauge, 
#to compare with the rain data that we have manually downloaded earlier

site <- "16227500"
param <- "00060"
#The site only has data from 2013 onwards - we will look at May.2015- Apr.2020
start <- "2015-05-01T00:00"
end <- "2020-04-30T23:59"

data <- readNWISdata(
  sites = site, service = "iv",
  parameterCd = param,
  startDate = start, endDate = end,
  tz = "HST"
)

info <- readNWISsite(site)

da <- info$drain_area_va
#convert drainage area to acres
da.ac <- da * 640

#let's also have the rainfall data from the nearby gauge in our back pocket

raindata <- as.data.frame(readr::read_delim(
  "./raindata.txt", col_names = T,
  delim = "\t"))

#check data
head(data, 3) 
head(raindata, 3)


#now that we have the data, we will organize it for easier analysis.

###ORGANIZE DATA---------------------------------------------------------------------------------------

#mark process start time
process <- data.frame(start = as.POSIXct(NA) , end = as.POSIXct(NA))
process[1,1] <- Sys.time()

#make copy of original data to preserve
orig <- data
rainorig <- raindata

#remove unneeded columns
data <- data[-1, -c(1,2,5,6)]
raindata <- raindata[-1,-c(1,2,4,6)]

#Rename columns 
names(data)[c(1:2)] <- c("timestamp", "flow_cfs")
names(raindata)[c(1:2)] <- c("timestamp", "rain_in")

#what's the drainage area of the site?
da.sqmi <- info[1,30]
da.ac <- da.sqmi * 640

###########################################################################################

#change table values to correct format
data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M", tz = "HST")

raindata$timestamp <- as.character(raindata$timestamp) 
raindata$timestamp <- as.POSIXct(raindata$timestamp, format = "%Y-%m-%d %H:%M", tz = "HST")
raindata$rain_in <- as.character(raindata$rain_in)
raindata$rain_in <- as.numeric(raindata$rain_in)

#cut down rain data to match the span of flow data

raindata <- raindata[(raindata$timestamp >= data$timestamp[1]) & 
                      (raindata$timestamp <= data$timestamp[nrow(data)]),]

#let's get a calculation for the total number of days that data has been recorded.
#for the purposes of this analysis, we will subtract any gaps larger than two hours.
gapthreshold <- 120 ###(minutes)

daysr <- raindata %>% drop_na(timestamp)
daysr <- daysr %>% drop_na(rain_in)
daysr$gap <- c(NA, difftime(daysr$timestamp[-1] , daysr$timestamp[-nrow(daysr)], units="mins"))
grossdayr <- as.numeric(daysr$timestamp[nrow(daysr)] - daysr$timestamp[1])
rain.gap <- sum(daysr$gap[daysr$gap > gapthreshold], na.rm = TRUE)

daysf <- data %>% drop_na(timestamp)
daysf <- daysf %>% drop_na(flow_cfs)
daysf$gap <- c(NA, difftime(daysf$timestamp[-1] , daysf$timestamp[-nrow(daysf)], units="mins"))
grossdayf <- as.numeric(daysf$timestamp[nrow(daysf)] - daysf$timestamp[1])
flow.gap <- sum(daysf$gap[daysf$gap > gapthreshold], na.rm = TRUE)

#final number of days in the monitoring period
daynumr <- as.numeric(ddays(grossdayr) - dminutes(rain.gap)) / 86400
daynumf <- as.numeric(ddays(grossdayf) - dminutes(flow.gap)) / 86400

#let's get a volume calculation in daysf, and merge it into the basedata file
daysf$volume_cf <- ifelse(daysf$gap < 20, daysf$flow_cfs *60 * daysf$gap, daysf$flow_cfs * 60 * lag(daysf$gap))
data <- left_join(data, daysf[c(1,4)], by = c('timestamp'))

#adding in season. In Hawaii, May-Oct is Dry Season. Nov-Apr is Wet season.
raindata$season <- ifelse(between(month(raindata$timestamp), 5,10), "dry", "wet")

#let's get annual rain values in entirety and by season
annual_rain <- round(sum(raindata$rain_in, na.rm = TRUE) * 365 / daynumr, 2)
annual_rain_wet <- round(sum(raindata$rain_in[raindata$season == "wet"], na.rm = TRUE) * 365 / daynumr, 2)
annual_rain_dry <- round(sum(raindata$rain_in[raindata$season == "dry"], na.rm = TRUE) * 365 / daynumr, 2)

#check that it adds up
annual_rain_wet + annual_rain_dry - annual_rain
#it is roughly correct.

#we will now create a dataframe of rain events as done in the rain data code.

###STORM EVENTS------------------------------------------------------------

#create second dataframe of just timestamp and rain
sub_rain <- data.frame(timestamp = as.POSIXct(raindata$timestamp),
                       Rain = as.numeric(raindata$rain_in))

#remove rows that don't record rain to speed up the process
sub_rain <- sub_rain[!(sub_rain$Rain == 0), ]

#need to identify whether rainfall in the buffer period was 0 or not
sub_rain$last_six_hours <- sub_rain$next_six_hours <- NA

for(i in seq(NROW(sub_rain))) {
  sub_rain$last_six_hours[i] <- 
    sum(sub_rain$Rain[
      (sub_rain$timestamp >= sub_rain$timestamp[i] - dhours(6))&
        (sub_rain$timestamp < sub_rain$timestamp[i])])
  sub_rain$next_six_hours[i] <- 
    sum(sub_rain$Rain[
      (sub_rain$timestamp <= sub_rain$timestamp[i] + dhours(6))&
        (sub_rain$timestamp > sub_rain$timestamp[i])]
    )
}


#Filter dataset for start and end times of storms
#The start of a rain event will always be where rain in the prev. 6 hrs is 0
#The end of a rain event will always be where rain in the next 6 hrs is 0
Start = filter(sub_rain, last_six_hours == 0 & Rain > 0) %>% pull(timestamp)
End = filter(sub_rain, next_six_hours == 0 & Rain > 0) %>% pull(timestamp)

Rain_events <- data.frame(Start_time = Start, End_time = End)

#Find sum of rain for each storm event
Rain_events$Rain_in <- NA

for(i in seq(NROW(Rain_events))){
  Rain_events$Rain_in[i] <- 
    sum(raindata$rain_in[
      (which(raindata[,1] == Rain_events[i,1])):
        (which(raindata[,1] == Rain_events[i,2]))])
}


###END OF ORGANIZING DATA--------------------------------------------------------------------------------

###Hydrograph Generation
#let's say we want to see what the hydrologic response was to the first storm
#in the rain event data
#we will add three hours to examine recession

#suppose we would like to display the first storm in a hydrograph
storm_1_rain <- raindata[(raindata$timestamp >= Rain_events$Start_time[1]) &
                           (raindata$timestamp <= (Rain_events$End_time[1] + dhours(3))),]
storm_1_flow <- data[(data$timestamp >= Rain_events$Start_time[1]) &
                           (data$timestamp <= (Rain_events$End_time[1] + dhours(3))),]

#this method closely follows code by Chuliang Xiao, found here:
#https://rpubs.com/cxiao/hydrograph-ggplot2-plot


s1rain <- ggplot(storm_1_rain, aes(timestamp, rain_in)) +
  geom_line(color = "black") +
  ylab("Rain (in)")+
  scale_y_reverse()

s1flow <- ggplot(storm_1_flow, aes(timestamp, flow_cfs)) +
  geom_line(color = "blue") +
  ylab("Flow (cfs)")

grid.arrange(s1rain, s1flow, ncol=1, heights = c(1,3),
             top = paste("USGS Moanalua Stream", site,"\n",storm_1_rain$timestamp[1],
                         "to",storm_1_rain$timestamp[nrow(storm_1_rain)],sep=" "))

#It looks like how we want, so we will export the graph
g <- arrangeGrob(s1rain, s1flow, ncol=1, heights = c(1,3),
                 top = paste("USGS Moanalua Stream", site,"\n",storm_1_rain$timestamp[1],
                             "to",storm_1_rain$timestamp[nrow(storm_1_rain)],sep=" "))

ggsave(
  file = "storm_1.png", 
  g,
  dpi = 1200
)

#let's say we want to identify the storm with the largest amount of rain
#to account for a longer flow recedence time, we will add six hours to the end of the
#hydrograph
biggest <- which.max(Rain_events$Rain_in)

storm_big_rain <- raindata[(raindata$timestamp >= Rain_events$Start_time[biggest]) &
                           (raindata$timestamp <= (Rain_events$End_time[biggest]) + dhours(6)),]
storm_big_flow <- data[(data$timestamp >= Rain_events$Start_time[biggest]) &
                       (data$timestamp <= (Rain_events$End_time[biggest] + dhours(6))),]

sbigrain <- ggplot(storm_big_rain, aes(timestamp, rain_in)) +
  geom_line(color = "black") +
  ylab("Rain (in)")+
  scale_y_reverse()

sbigflow <- ggplot(storm_big_flow, aes(timestamp, flow_cfs)) +
  geom_line(color = "blue") +
  ylab("Flow (cfs)")

grid.arrange(sbigrain, sbigflow, ncol=1, heights = c(1, 3),
             top = paste("USGS Moanalua Stream", site,"\n", storm_big_rain$timestamp[1],
                         "to",storm_big_rain$timestamp[nrow(storm_big_rain)],sep=" "))

#It looks how we want, so we will export the graph
g2 <- arrangeGrob(sbigrain, sbigflow, ncol=1, heights = c(1,3),
                 top = paste("USGS Moanalua Stream", site,"\n",storm_big_rain$timestamp[1],
                             "to",storm_big_rain$timestamp[nrow(storm_big_rain)],sep=" "))

ggsave(
  file = "biggeststorm.png", 
  g2,
  dpi = 1200
)

#What if we want to look at only the largest peak? we will limit the window to
#Aug 27, 23:00 to Aug 28, 16:00 (2018)

#specify dates externally
d1 <- as.POSIXct("2018-08-27 23:00", format = "%Y-%m-%d %H:%M", tz = "HST")
d2 <- as.POSIXct("2018-08-28 16:00", format = "%Y-%m-%d %H:%M", tz = "HST")

sbigrain2 <- ggplot(storm_big_rain, aes(timestamp, rain_in)) +
  geom_line(color = "black") +
  ylab("Rain (in)")+
  xlim(c(d1, d2)) +
  scale_y_reverse()

sbigflow2 <- ggplot(storm_big_flow, aes(timestamp, flow_cfs)) +
  geom_line(color = "blue") +
  xlim(c(d1,d2)) +
  ylab("Flow (cfs)")

grid.arrange(sbigrain2, sbigflow2, ncol=1, heights = c(1, 3),
             top = paste("USGS Moanalua Stream", site,"\n", storm_big_rain$timestamp[1],
                         "to",storm_big_rain$timestamp[nrow(storm_big_rain)],
                         "\n Largest Flow Peak", d1, "to", d2, sep=" "))

#It looks how we want, so we will export the graph
g3 <- arrangeGrob(sbigrain2, sbigflow2, ncol=1, heights = c(1,3),
                  top = paste("USGS Moanalua Stream", site,"\n",storm_big_rain$timestamp[1],
                              "to",storm_big_rain$timestamp[nrow(storm_big_rain)],
                              "\n Largest Flow Peak", d1, "to", d2, sep=" "))

ggsave(
  file = "biggeststormzoom.png", 
  g3,
  dpi = 1200
)

###SOME BASIC STATS OF THE SITE------------------------------------------------------------------

#what is the annual flow volume at this site?
#cf/ac/yr
annual_flow_ac <- sum(data$volume_cf, na.rm = T) * 365 / daynumf / da.ac

#how has yield changed over the years?
annual_flow <- data %>%
  mutate(year = year(timestamp)) %>%
  group_by(year) %>% # group by the year column
  # calculate the SUM of all precipitation that occurred during each year
  summarise(ann_flow_vol = (sum(volume_cf, na.rm=T) / da.ac )) %>%  
  na.omit()

annual_rain <- sub_rain %>%
  mutate(year = year(timestamp)) %>%
  group_by(year) %>% # group by the year column
  # calculate the SUM of all precipitation that occurred during each year
  summarise(annual_rain= sum(Rain, na.rm=T)) %>%  
  na.omit()

annual_data <- left_join(annual_flow, annual_rain, by = "year")

#we need to scale the first and last year since the data starts and ends in may
#how many days are in the start and end years of the record?
daystart <- sum(year(unique(as.Date(data$timestamp, format="%Y-%m-%d"))) == min(annual_data$year))
daysend <- sum(year(unique(as.Date(data$timestamp, format="%Y-%m-%d"))) == max(annual_data$year))

annual_data$ann_flow_vol[annual_data$year == min(annual_data$year)] <- 
  annual_data$ann_flow_vol[annual_data$year == min(annual_data$year)] * 365 / daystart
annual_data$ann_flow_vol[annual_data$year == max(annual_data$year)] <- 
  annual_data$ann_flow_vol[annual_data$year == max(annual_data$year)] * 365 / daysend

##do the same for rain
#how many days are in the start and end years of the record?
daystart <- sum(year(unique(as.Date(data$timestamp, format="%Y-%m-%d"))) == min(annual_data$year))
daysend <- sum(year(unique(as.Date(data$timestamp, format="%Y-%m-%d"))) == max(annual_data$year))

#scale observed rainfall to an annual value
annual_data$annual_rain[annual_data$year == min(annual_data$year)] <- 
  annual_data$annual_rain[annual_data$year == min(annual_data$year)] * 365 / daystart
annual_data$annual_rain[annual_data$year == max(annual_data$year)] <- 
  annual_data$annual_rain[annual_data$year == max(annual_data$year)] * 365 / daysend

names(annual_data)[c(1:3)] <- c("Year", "annual flow (cf/ac/yr)", "annual rain (in)")



###EXPORT THE TABLES-----------------------------------------------------------------------------------

write.csv(annual_data, file = "./AnnualData.csv", row.names = F)

#let's find end time of process
process[1,2] <- Sys.time()

