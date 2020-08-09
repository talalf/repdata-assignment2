#load required packages
require(tidyverse)
require(dplyr)
require(ggplot2)

#download and read data
ifelse(!file.exists("stormdata.csv.bz2"),
   download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                 destfile =  "stormdata.csv.bz2",
                 method = "curl"),
   print("data file found"))

stormdata <- read.csv("stormdata.csv.bz2")

#extract needed variables
evtype <- stormdata$EVTYPE %>% as.factor()
fatalities <- stormdata$FATALITIES %>% as.numeric()
injuries <- stormdata$INJURIES %>% as.numeric()

#make dataframe for use in analysis
df <- data.frame(evtype,fatalities,injuries) %>% as_tibble() %>%
        group_by(evtype)

events <- table(df$evtype) %>%
        data.frame %>%
        as_tibble()

names(events)[1] <- "evtype"

summarydata <- summarise(df,
                         total_fatalities = sum(fatalities),
                         mean_fatalities = mean(fatalities),
                         total_injuries = sum(injuries),
                         mean_injuries = mean(injuries))

summarydata <- merge(events, summarydata, by = "evtype")
