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
propdmg <- stormdata$PROPDMG
cropdmg <- stormdata$CROPDMG
totaldmg <- propdmg+cropdmg

#make dataframe for use in analysis
df <- data.frame(evtype,fatalities,injuries,totaldmg) %>% as_tibble() %>%
        group_by(evtype)

events <- table(df$evtype) %>%
        data.frame %>%
        as_tibble()

names(events)[1] <- "evtype"

summarydata <- summarise(df,
                         total_fatl = sum(fatalities),
                         mean_fatl = mean(fatalities),
                         total_inj = sum(injuries),
                         mean_inj = mean(injuries),
                         total_dmg = sum(totaldmg),
                         mean_dmg = mean(totaldmg))

summarydata <- merge(events, summarydata, by = "evtype")

# sort by total fatality count
sumdata1 <- summarydata %>%
        arrange(desc(total_fatl))

# sort by total injury count
sumdata2 <- summarydata %>%
        arrange(desc(total_inj))

# analysis of economic impact

# sort by total damage
sumdata3 <- summarydata %>%
        arrange(desc(total_dmg))

# plotting health data

# plot data by total number of fatalities
sumdata1 <- slice_max(sumdata1, order_by = total_fatl, n=5)

ggplot(data = sumdata1,
       aes(x = reorder(evtype, -total_fatl), y = total_fatl)) +
        geom_bar(stat="identity") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1)) +
        labs(title = "Total fatalities per event type") +
        xlab("Event type") +
        ylab("Number of fatalities")

# plot data by total number of injuries
sumdata2 <- slice_max(sumdata2, order_by = total_inj, n=5)

ggplot(data = sumdata2,
       aes(x = reorder(evtype, -total_inj), y = total_inj)) +
        geom_bar(stat="identity") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1)) +
        labs(title = "Total injuries per event type") +
        xlab("Event type") +
        ylab("Number of injuries")
