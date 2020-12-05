
## 1: Synopsis

#The objective of this assignment is to analyze NOAA storm database to answer some severe weather events.

### The following investigation will address the below questions:

#1. What are the events that are harmful to population health?
#2. What types of events have greatest economic consequences?


#Duration of the events captured in database: 1950 - 2011


## 2: Data Processing

### 2.1: Data Loading

#Download data and extract the data

library("data.table")
library("ggplot2")

# Download data if file is not existing

if(!file.exists("StormData.csv.bz2")){
 download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","StormData.csv.bz2")
}

# Load
stormData<- read.csv("StormData.csv.bz2")

# Convert data.frame to data.table
stormDataTable <- as.data.table(stormData)

### 2.3 Data Subsetting

# Fetch the  data which is actually required

tidyStormDataTable <- stormDataTable[,c('EVTYPE','FATALITIES','INJURIES','PROPDMG','PROPDMGEXP','CROPDMG','CROPDMGEXP')]

### 2.4 Convert exponent components to actual components


# Map property damage exponents[H,K,M,B] to numeric values

tidyStormDataTable$PROPERTYDMGNUM = 0
tidyStormDataTable[tidyStormDataTable$PROPDMGEXP == "H",]$PROPERTYDMGNUM = tidyStormDataTable[tidyStormDataTable$PROPDMGEXP == "H",]$CROPDMG * 10^2
tidyStormDataTable[tidyStormDataTable$PROPDMGEXP == "K",]$PROPERTYDMGNUM = tidyStormDataTable[tidyStormDataTable$PROPDMGEXP == "K",]$CROPDMG * 10^3
tidyStormDataTable[tidyStormDataTable$PROPDMGEXP == "M",]$PROPERTYDMGNUM = tidyStormDataTable[tidyStormDataTable$PROPDMGEXP == "M",]$CROPDMG * 10^6
tidyStormDataTable[tidyStormDataTable$PROPDMGEXP == "B",]$PROPERTYDMGNUM = tidyStormDataTable[tidyStormDataTable$PROPDMGEXP == "B",]$CROPDMG * 10^9


# Map crop damage exponents [H,K,M,B] to numeric values

tidyStormDataTable$CROPDMGNUM = 0
tidyStormDataTable[tidyStormDataTable$CROPDMGEXP == "H",]$CROPDMGNUM = tidyStormDataTable[tidyStormDataTable$CROPDMGEXP == "H",]$PROPDMG * 10^2
tidyStormDataTable[tidyStormDataTable$CROPDMGEXP == "K",]$CROPDMGNUM = tidyStormDataTable[tidyStormDataTable$CROPDMGEXP == "K",]$PROPDMG * 10^3
tidyStormDataTable[tidyStormDataTable$CROPDMGEXP == "M",]$CROPDMGNUM = tidyStormDataTable[tidyStormDataTable$CROPDMGEXP == "M",]$PROPDMG * 10^6
tidyStormDataTable[tidyStormDataTable$CROPDMGEXP == "B",]$CROPDMGNUM = tidyStormDataTable[tidyStormDataTable$CROPDMGEXP == "B",]$PROPDMG * 10^9

## 3: Results

### 3.1 Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

# plot number of fatalities with most harmful event type

fatalities <- aggregate(FATALITIES ~ EVTYPE, data = tidyStormDataTable, sum)
fatalities <- fatalities[order(-fatalities$FATALITIES),][1:10,]
fatalities$EVTYPE <- factor(fatalities$EVTYPE, levels = fatalities$EVTYPE)

ggplot(fatalities, aes(x = EVTYPE, y = FATALITIES))+
  geom_bar(stat = "identity", fill = "orange")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Event Type")+ylab("Fatalities")+ggtitle("Number of fatalities by top 10 Weather events")

# plot number of injuries with most harmful event type

injuries <- aggregate(INJURIES ~ EVTYPE, data = tidyStormDataTable, sum)
injuries <- injuries[order(-injuries$INJURIES),][1:10,]
injuries$EVTYPE <- factor(injuries$EVTYPE, levels = injuries$EVTYPE)

ggplot(injuries, aes(x = EVTYPE, y = INJURIES))+
  geom_bar(stat = "identity", fill = "orange")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Event Type")+ylab("Fatalities")+ggtitle("Number of injuries by top 10 Weather events")

### 3.2: Across the United States, which types of events have the greatest economic consequences?


# plot number of damages with most harmful event type

damages <- aggregate(PROPERTYDMGNUM + CROPDMGNUM ~ EVTYPE, data=tidyStormDataTable, sum)
names(damages) = c("EVTYPE","TOTALDAMAGE")
damages <- damages[order(-damages$TOTALDAMAGE),][1:10,]
damages$EVTYPE <- factor(damages$EVTYPE, levels = damages$EVTYPE)

ggplot(damages, aes(x = EVTYPE, y = TOTALDAMAGE)) +
  geom_bar(stat = "identity", fill="orange")+
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  xlab("Event Type") + ylab("Damage ($)") + ggtitle("Property & Crop damage caused by top 10")
  
