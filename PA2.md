# Weather Events in the US and Their Economic and Health Impacts
##Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

The analysis below will answer the following questions:  
1. Across the United States, which types of events are most harmful with respect to population health?  
2. Across the United States, which types of events have the greatest economic consequences?

The data used in this analysis is available below:  
* [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

##Data Processing
###General Data Processing
First, download the data (if it isn't already available in the current directory):

```r
data_file_name <- "repdata-data-StormData.csv.bz2"
file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if(!file.exists(data_file_name)){
    print("Downloading Data")
    setInternet2(use = TRUE)
    download.file(file_url,data_file_name, mode = "wb")
}
```

Install and load the appropriate libraries:

```r
#plyr
if(!is.element("plyr", installed.packages()[,1])){
    install.packages("plyr", repos="http://cran.rstudio.com/")
}
library(plyr)

#ggplot2
if(!is.element("ggplot2", installed.packages()[,1])){
    install.packages("ggplot2", repos="http://cran.rstudio.com/")
}
library(ggplot2)

#reshape
if(!is.element("reshape", installed.packages()[,1])){
    install.packages("reshape", repos="http://cran.rstudio.com/")
}
library(reshape)

#gridExtra
if(!is.element("gridExtra", installed.packages()[,1])){
    install.packages("gridExtra", repos="http://cran.rstudio.com/")
}
library(gridExtra)
```

Load the data into a data frame:

```r
storm_data <- read.csv(bzfile("repdata-data-StormData.csv.bz2"))
```

Normalize the event types by eliminating special characters, spaces, and case discrepencies:

```r
storm_data2 <- storm_data
event_types <- tolower(storm_data2$EVTYPE)
event_types <- gsub("[[:blank:][:punct:]+]", " ", event_types)
event_types <- gsub("^ *|(?<= ) | *$", "", event_types, perl=T)
storm_data2$EVTYPE <- event_types
```

###Process data for health impact of weather events 
For this question, use the events that caused the most deaths AND injuries seperately.  This
data will be reported on in the 'Results' section.

```r
#fatal events
fatal_events <- melt(sort(tapply(storm_data2$FATALITIES,storm_data2$EVTYPE,sum),
                     decreasing=TRUE))

#injury events
injury_events <- melt(sort(tapply(storm_data2$INJURIES,storm_data2$EVTYPE,sum),
                     decreasing=TRUE))
```

###Process data for economic impact of weather events
In the data, property damage is represented with two fields: `PROPDMG` and `PROPDMGEXP`.
Crop damage is represented in the data with the following two fields: `CROPDMG` and `CROPDMGEXP`.  

First, normalize the cost by using the `PROPDMGEXP` and `CROPDMGEXP` fields:

```r
#Property Damage
storm_data2$PROPDMGEXP <- tolower(storm_data2$PROPDMGEXP)
storm_data2$PROPDMG[storm_data2$PROPDMGEXP == ""] <- storm_data2$PROPDMG[storm_data2$PROPDMGEXP == ""] * 1
storm_data2$PROPDMG[storm_data2$PROPDMGEXP == "h"] <- storm_data2$PROPDMG[storm_data2$PROPDMGEXP == "h"] * 100
storm_data2$PROPDMG[storm_data2$PROPDMGEXP == "k"] <- storm_data2$PROPDMG[storm_data2$PROPDMGEXP == "k"] * 1000
storm_data2$PROPDMG[storm_data2$PROPDMGEXP == "m"] <- storm_data2$PROPDMG[storm_data2$PROPDMGEXP == "m"] * 1e+06
storm_data2$PROPDMG[storm_data2$PROPDMGEXP == "b"] <- storm_data2$PROPDMG[storm_data2$PROPDMGEXP == "b"] * 1e+09

#Crop Damage
storm_data2$CROPDMGEXP <- tolower(storm_data2$CROPDMGEXP)
storm_data2$CROPDMG[storm_data2$CROPDMGEXP == ""] <- storm_data2$CROPDMG[storm_data2$CROPDMGEXP == ""] * 1
storm_data2$CROPDMG[storm_data2$CROPDMGEXP == "h"] <- storm_data2$CROPDMG[storm_data2$CROPDMGEXP == "h"] * 100
storm_data2$CROPDMG[storm_data2$CROPDMGEXP == "k"] <- storm_data2$CROPDMG[storm_data2$CROPDMGEXP == "k"] * 1000
storm_data2$CROPDMG[storm_data2$CROPDMGEXP == "m"] <- storm_data2$CROPDMG[storm_data2$CROPDMGEXP == "m"] * 1e+06
storm_data2$CROPDMG[storm_data2$CROPDMGEXP == "b"] <- storm_data2$CROPDMG[storm_data2$CROPDMGEXP == "b"] * 1e+09
```

Now create sorted data frames for crop damage and property damage seperately.  This data will be used in the results section below:

```r
#crop damage
crop_events <- melt(sort(tapply(storm_data2$CROPDMG,storm_data2$EVTYPE,sum),
                     decreasing=TRUE))

#property damage
property_events <- melt(sort(tapply(storm_data2$PROPDMG,storm_data2$EVTYPE,sum),
                     decreasing=TRUE))
```

##Results
###Which types of events are most harmful with respect to population health?

```r
#Plot the causes of fatalities by event type
p1 <- ggplot(data=head(fatal_events,10), aes(x=indices, y=value, fill=value)) +
    geom_bar(stat="identity") +
    ylab("Fatalaties") +
    xlab("Event type") +
    theme(legend.position="none") + 
    ggtitle("Causes of Fatalities by Event Type") + 
    coord_flip()

#Plot the causes of injuries by event type
p2 <- ggplot(data=head(injury_events,10), aes(x=indices, y=value, fill=value)) +
    geom_bar(stat="identity") +
    ylab("Injuries") +
    xlab("Event type") +
    theme(legend.position="none") +
    ggtitle("Causes of Injuries by Event Type") + 
    coord_flip()

grid.arrange(p1, p2)
```

![](PA2_files/figure-html/unnamed-chunk-8-1.png) 

Tornadoes are the leading cause of fatalities and injuries.  

###Which types of events have the greatest economic consequences?
Plot the crop damage by event type:

```r
#Plot the crop damage by event type
p3 <- ggplot(data=head(crop_events,10), aes(x=indices, y=value, fill=value)) +
    geom_bar(stat="identity") +
    ylab("Crop Damage") +
    xlab("Event type") +
    theme(legend.position="none") + 
    ggtitle("Causes of Crop Damage by Event Type") + 
    coord_flip()

#Plot the property damage by event type
p4 <- ggplot(data=head(property_events,10), aes(x=indices, y=value, fill=value)) +
    geom_bar(stat="identity") +
    ylab("Property Damage") +
    xlab("Event type") +
    theme(legend.position="none") +
    ggtitle("Causes of Property Damage by Event Type") + 
    coord_flip()

grid.arrange(p3, p4)
```

![](PA2_files/figure-html/unnamed-chunk-9-1.png) 

Drought is the leading cause of crop damage and floods are the leading cause of property damage.
