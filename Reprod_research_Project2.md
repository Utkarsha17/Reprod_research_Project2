Introduction
------------

Storms and other severe weather events can cause both public health and
economic problems for communities and municipalities. Many severe events
can result in fatalities, injuries, and property damage, and preventing
such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and
Atmospheric Administration's (NOAA) storm database. This database tracks
characteristics of major storms and weather events in the United States,
including when and where they occur, as well as estimates of any
fatalities, injuries, and property damage.

This report

Data
----

The data for this assignment come in the form of a comma-separated-value
file compressed via the bzip2 algorithm to reduce its size. The link for
data is given to download files.

    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

    download.file(url, "StormData.csv.bz2")

    library(R.utils)

    ## Warning: package 'R.utils' was built under R version 3.5.2

    ## Loading required package: R.oo

    ## Warning: package 'R.oo' was built under R version 3.5.2

    ## Loading required package: R.methodsS3

    ## Warning: package 'R.methodsS3' was built under R version 3.5.2

    ## R.methodsS3 v1.7.1 (2016-02-15) successfully loaded. See ?R.methodsS3 for help.

    ## R.oo v1.22.0 (2018-04-21) successfully loaded. See ?R.oo for help.

    ## 
    ## Attaching package: 'R.oo'

    ## The following objects are masked from 'package:methods':
    ## 
    ##     getClasses, getMethods

    ## The following objects are masked from 'package:base':
    ## 
    ##     attach, detach, gc, load, save

    ## R.utils v2.8.0 successfully loaded. See ?R.utils for help.

    ## 
    ## Attaching package: 'R.utils'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

    ## The following objects are masked from 'package:base':
    ## 
    ##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings

    bunzip2("StormData.csv.bz2", "StormData.csv", remove = FALSE, skip = TRUE)

    ## [1] "StormData.csv"
    ## attr(,"temporary")
    ## [1] FALSE

Data Processing
---------------

1.  Reading data in CSV format for further analysis

<!-- -->

    data <- read.csv("StormData.csv")
    head(data)

    ##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
    ## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
    ## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
    ## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
    ## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
    ## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
    ## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
    ##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
    ## 1 TORNADO         0                                               0
    ## 2 TORNADO         0                                               0
    ## 3 TORNADO         0                                               0
    ## 4 TORNADO         0                                               0
    ## 5 TORNADO         0                                               0
    ## 6 TORNADO         0                                               0
    ##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
    ## 1         NA         0                      14.0   100 3   0          0
    ## 2         NA         0                       2.0   150 2   0          0
    ## 3         NA         0                       0.1   123 2   0          0
    ## 4         NA         0                       0.0   100 2   0          0
    ## 5         NA         0                       0.0   150 2   0          0
    ## 6         NA         0                       1.5   177 2   0          0
    ##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
    ## 1       15    25.0          K       0                                    
    ## 2        0     2.5          K       0                                    
    ## 3        2    25.0          K       0                                    
    ## 4        2     2.5          K       0                                    
    ## 5        2     2.5          K       0                                    
    ## 6        6     2.5          K       0                                    
    ##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
    ## 1     3040      8812       3051       8806              1
    ## 2     3042      8755          0          0              2
    ## 3     3340      8742          0          0              3
    ## 4     3458      8626          0          0              4
    ## 5     3412      8642          0          0              5
    ## 6     3450      8748          0          0              6

2.Code needs to call following packages

    library(plyr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.5.2

    library(gridExtra)

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    library(grid)

3.While evaluating the health impact, this analysis requires only
following columns:

EVTYPE: Event type FATALITIES: Number of fatalities INJURIES: Number of
injuries PROPDMG: Property damage (numeric value) PROPDMGEXP: Property
damage (exponential indicator affecting the numeric value) CROPDMG: Crop
damage (numeric value) CROPDMGEXP: Crop damage (exponential indicator
affecting the numeric value)

    events <- data[,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

4.Summarizing fatalities and injuries with respective event type.

    harm <- ddply(events, .(EVTYPE), summarize,fatalities = sum(FATALITIES),injuries = sum(INJURIES))
    # Sorting data in decreasing order
    fatal <- harm[order(harm$fatalities, decreasing = T), ]
    injury <- harm[order(harm$injuries, decreasing = T), ]

1.  Converting Exponent Columns into Actual Exponents instead of (-,+,
    H, K, etc). In a first step a function replaces with a nuber that will implemented.

<!-- -->

    getExp <- function(exp) {
        if (exp %in% c("h", "H"))
            return(2)
        else if (exp %in% c("k", "K"))
            return(3)
        else if (exp %in% c("m", "M"))
            return(6)
        else if (exp %in% c("b", "B"))
            return(9)
        else if (!is.na(as.numeric(exp))) 
            return(as.numeric(exp))
        else if (exp %in% c("", "-", "?", "+"))
            return(0)
        else {
            stop("Invalid value")
        }
    }
    # using above function, calculating damage with crop and property
    prop <- sapply(events$PROPDMGEXP, FUN=getExp)
    events$prop_Damage <- events$PROPDMG * (10 ** prop)
    crop <- sapply(events$CROPDMGEXP, FUN=getExp)
    events$crop_Damage <- events$CROPDMG * (10 ** crop)

1.  Calculating economical damage for crop and property by summarizing
    crop damage and property damage with respect to event type

<!-- -->

    eco_Damage <- ddply(events, .(EVTYPE), summarize,prop_Damage = sum(prop_Damage), crop_Damage = sum(crop_Damage))

    #Excluding events that have not caused financial damage
    eco_Damage <- eco_Damage[(eco_Damage$prop_Damage > 0 | eco_Damage$crop_Damage > 0), ]

1.  Sorting damage data in decreasing order

<!-- -->

    prop_Damage_ord <- eco_Damage[order(eco_Damage$prop_Damage, decreasing = T), ]
    crop_Damage_ord <- eco_Damage[order(eco_Damage$crop_Damage, decreasing = T), ]

We have processed data for further analysis and conclusion.

Analysis
--------

1.  Displaying top 10 weather events that are harmful to population
    health

<!-- -->

    head(injury[, c("EVTYPE", "injuries")],10)

    ##                EVTYPE injuries
    ## 834           TORNADO    91346
    ## 856         TSTM WIND     6957
    ## 170             FLOOD     6789
    ## 130    EXCESSIVE HEAT     6525
    ## 464         LIGHTNING     5230
    ## 275              HEAT     2100
    ## 427         ICE STORM     1975
    ## 153       FLASH FLOOD     1777
    ## 760 THUNDERSTORM WIND     1488
    ## 244              HAIL     1361

    head(fatal[, c("EVTYPE", "fatalities")],10)

    ##             EVTYPE fatalities
    ## 834        TORNADO       5633
    ## 130 EXCESSIVE HEAT       1903
    ## 153    FLASH FLOOD        978
    ## 275           HEAT        937
    ## 464      LIGHTNING        816
    ## 856      TSTM WIND        504
    ## 170          FLOOD        470
    ## 585    RIP CURRENT        368
    ## 359      HIGH WIND        248
    ## 19       AVALANCHE        224

    # plotting top 10 weather events causing injuries
    plot_inj <- ggplot(data=head(injury,10), aes(x=reorder(EVTYPE, -injuries), y=injuries)) +
       geom_bar(fill="yellow",stat="identity")  + 
        ylab("Total number of injuries") + xlab("Event type") +
        ggtitle("Top 10 weather events causing injuries") +
        theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
    plot_inj

![](https://github.com/Utkarsha17/Reprod_research_Project2/blob/master/unnamed-chunk-11-1.png)

    # plotting top 10 weather events causing fatalities

    plot_fatal <- ggplot(data=head(fatal,10), aes(x=reorder(EVTYPE, -fatalities), y=fatalities)) +
        geom_bar(fill="red",stat="identity") +
        ylab("Top 10 weather events causing fatalities") + xlab("Event type") +
        theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
    plot_fatal

![](https://github.com/Utkarsha17/Reprod_research_Project2/blob/master/unnamed-chunk-11-2.png)

Plots and Tables show that tornado is most dangerous weather event that
impacted population health.

2.  Now displaying top 10 weather events that has impact on economy

<!-- -->

    #displaying top 10 events caused property damage
    head(prop_Damage_ord[, c("EVTYPE", "prop_Damage")], 10)

    ##                 EVTYPE  prop_Damage
    ## 153        FLASH FLOOD 6.820237e+13
    ## 786 THUNDERSTORM WINDS 2.086532e+13
    ## 834            TORNADO 1.078951e+12
    ## 244               HAIL 3.157558e+11
    ## 464          LIGHTNING 1.729433e+11
    ## 170              FLOOD 1.446577e+11
    ## 411  HURRICANE/TYPHOON 6.930584e+10
    ## 185           FLOODING 5.920826e+10
    ## 670        STORM SURGE 4.332354e+10
    ## 310         HEAVY SNOW 1.793259e+10

    #displaying top 10 events caused crop damage
    head(crop_Damage_ord[, c("EVTYPE", "crop_Damage")], 10)

    ##                EVTYPE crop_Damage
    ## 95            DROUGHT 13972566000
    ## 170             FLOOD  5661968450
    ## 590       RIVER FLOOD  5029459000
    ## 427         ICE STORM  5022113500
    ## 244              HAIL  3025974480
    ## 402         HURRICANE  2741910000
    ## 411 HURRICANE/TYPHOON  2607872800
    ## 153       FLASH FLOOD  1421317100
    ## 140      EXTREME COLD  1292973000
    ## 212      FROST/FREEZE  1094086000

    g1<- ggplot(data=head(prop_Damage_ord,10), aes(x=reorder(EVTYPE, -prop_Damage), y=log10(prop_Damage), fill=prop_Damage )) +
        geom_bar(fill="red",stat="identity") + 
        xlab("Event type") + ylab("Property damage in billion dollars") +
        ggtitle("Top 10 events caused property damage") +
        theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

    g1

![](https://github.com/Utkarsha17/Reprod_research_Project2/blob/master/unnamed-chunk-15-1.png)

    g2<- ggplot(data=head(crop_Damage_ord,10), aes(x=reorder(EVTYPE, -crop_Damage), y=log10(crop_Damage), fill=crop_Damage )) +
        geom_bar(fill="blue",stat="identity") + 
        xlab("Event type") + ylab("Crop damage in billion dollars") +
        ggtitle("Top 10 events caused crop damage") +
        theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

    g2

![](https://github.com/Utkarsha17/Reprod_research_Project2/blob/master/unnamed-chunk-16-1.png)

Results
=======

From above all plots, Tornados caused the maximum number of fatalities
and injuries. Followed by Excessive Heat for fatalities and Winds caused
injuries. Flash flood event caused most property damage and draught
caused most crop damage.
