Capstone\_ How Does a Bike-Share Navigate Speedy Success?
================
Mohamed KONE
2022-09-04

## R Markdown

## The main goal of our analysis is to use historical trend in order to discover the difference of usage of our Bike service by the different type of subscribtor.

## The insight we’ll discover here will allow to know how each kind of user is using the service and what should we include on our service in order to convince them that subscribing in our annual plan would be profitable for them.

# Our data are located on the the entreprise website and are organized per months

``` r
### Loading necessary library

library(tidyverse)
```

    ## Warning in as.POSIXlt.POSIXct(Sys.time()): unable to identify current timezone 'T':
    ## please set environment variable 'TZ'

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readr)
library(dplyr)
library(lubridate)
```

    ## 
    ## Attachement du package : 'lubridate'
    ## 
    ## Les objets suivants sont masqués depuis 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(hms)
```

    ## 
    ## Attachement du package : 'hms'
    ## 
    ## L'objet suivant est masqué depuis 'package:lubridate':
    ## 
    ##     hms

``` r
library(zoo)
```

    ## 
    ## Attachement du package : 'zoo'
    ## 
    ## Les objets suivants sont masqués depuis 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(data.table)
```

    ## 
    ## Attachement du package : 'data.table'
    ## 
    ## Les objets suivants sont masqués depuis 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## Les objets suivants sont masqués depuis 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## L'objet suivant est masqué depuis 'package:purrr':
    ## 
    ##     transpose

``` r
setwd("~/divvy-tripdata")

myfolder <- "divvy-tripdata"

allfiles <- list.files(path = myfolder, pattern="*-*-*.csv",full.names = TRUE)
allfiles
```

    ## character(0)

``` r
Bike_tripdata <- allfiles %>%
  lapply(read.csv)%>%
  bind_rows

glimpse(Bike_tripdata)
```

    ## Rows: 0
    ## Columns: 0

``` r
# Show an overview of member_casual and activities for each stations
library(dplyr)
library(readr)
library(ggplot2)

Bike_tripdata_clean <- read_csv("~/divvy-tripdata/Bike_tripdata_clean2.csv")
```

    ## Rows: 9258460 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (6): start_lat, start_lng, end_lat, end_lng, ride_length, month_year
    ## dttm (2): started_at, ended_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(Bike_tripdata_clean)
```

    ## Rows: 9,258,460
    ## Columns: 16
    ## $ ride_id            <chr> "DD06751C6019D865", "79973DC3B232048F", "0249AD4B25…
    ## $ rideable_type      <chr> "classic_bike", "classic_bike", "classic_bike", "cl…
    ## $ started_at         <dttm> 2021-08-08 17:21:26, 2021-08-27 08:53:52, 2021-08-…
    ## $ ended_at           <dttm> 2021-08-08 17:25:37, 2021-08-27 09:18:29, 2021-08-…
    ## $ start_station_name <chr> "Desplaines St & Kinzie St", "Larrabee St & Armitag…
    ## $ start_station_id   <chr> "TA1306000003", "TA1309000006", "13157", "13042", "…
    ## $ end_station_name   <chr> "Kingsbury St & Kinzie St", "Michigan Ave & Oak St"…
    ## $ end_station_id     <chr> "KA1503000043", "13042", "13157", "13042", "13042",…
    ## $ start_lat          <dbl> 41.88872, 41.91808, 41.87773, 41.90096, 41.90096, 4…
    ## $ start_lng          <dbl> -87.64445, -87.64375, -87.65479, -87.62378, -87.623…
    ## $ end_lat            <dbl> 41.88918, 41.90096, 41.87773, 41.90096, 41.90096, 4…
    ## $ end_lng            <dbl> -87.63851, -87.62378, -87.65479, -87.62378, -87.623…
    ## $ member_casual      <chr> "member", "member", "member", "casual", "casual", "…
    ## $ ride_length        <dbl> 251, 1477, 37, 282, 2156, 2402, 85, 3245, 3538, 123…
    ## $ month_year         <dbl> 2021.583, 2021.583, 2021.583, 2021.583, 2021.583, 2…
    ## $ weekdays           <chr> "dimanche", "vendredi", "dimanche", "jeudi", "lundi…

``` r
Station_name<-Bike_tripdata_clean %>%
  group_by(start_station_name,member_casual) %>%
  summarize(name_stations=nchar(start_station_name),average_length=mean(ride_length))
```

    ## `summarise()` has grouped output by 'start_station_name', 'member_casual'. You
    ## can override using the `.groups` argument.

``` r
 glimpse(Station_name)
```

    ## Rows: 9,258,460
    ## Columns: 4
    ## Groups: start_station_name, member_casual [2,420]
    ## $ start_station_name <chr> "111th St - Morgan Park Metra", "111th St - Morgan …
    ## $ member_casual      <chr> "casual", "casual", "casual", "casual", "member", "…
    ## $ name_stations      <int> 28, 28, 28, 28, 28, 28, 19, 19, 19, 19, 19, 19, 19,…
    ## $ average_length     <dbl> 281.500, 281.500, 281.500, 281.500, 392.000, 392.00…

``` r
 #Ploting my data
 ggplot(Station_name, aes(start_station_name,average_length))+
   geom_point(aes(fill=(member_casual)))+
   theme(axis.text = element_text(angle= 90))
```

![](Capstone-How-Does-a-Bike-Share-Navigate-Speedy-Success_files/figure-gfm/Comparing%20member%20per%20station%20activities-1.png)<!-- -->

``` r
 ##We can noticed generally in function of the average ride time that the casual are riding more member!

 ## Identifying the riders that are using more services in function of member type
 
 Best_riders<-Bike_tripdata_clean %>%
  group_by(ride_id,member_casual) %>%
  summarize(average_length=mean(ride_length)) %>%
   arrange(desc(average_length))
```

    ## `summarise()` has grouped output by 'ride_id'. You can override using the
    ## `.groups` argument.

``` r
 head(Best_riders)
```

    ## # A tibble: 6 × 3
    ## # Groups:   ride_id [6]
    ##   ride_id          member_casual average_length
    ##   <chr>            <chr>                  <dbl>
    ## 1 E25E58F5E94EE351 casual               2497750
    ## 2 A310C7270FD730B6 casual               2442301
    ## 3 23697816035F9A8F casual               2061244
    ## 4 BBDA8AFFE3BB3F61 casual               1971512
    ## 5 689016313584F621 casual               1953390
    ## 6 DC510E6F98003A94 casual               1922127

``` r
 # Depending to the the average length time per ride, the 6 first best riders are casual
```

``` r
# Show an overview of activities for last 12 months

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

Bike_trip_perL12M<-Bike_tripdata_clean %>%
  group_by(month_year,member_casual) %>%
  summarize(number_usage=nchar(month_year))
```

    ## `summarise()` has grouped output by 'month_year', 'member_casual'. You can
    ## override using the `.groups` argument.

``` r
 glimpse(Bike_trip_perL12M)
```

    ## Rows: 9,258,460
    ## Columns: 3
    ## Groups: month_year, member_casual [24]
    ## $ month_year    <dbl> 2021.583, 2021.583, 2021.583, 2021.583, 2021.583, 2021.5…
    ## $ member_casual <chr> "casual", "casual", "casual", "casual", "casual", "casua…
    ## $ number_usage  <int> 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, …

``` r
 #Plotting my data
 ggplot(Bike_trip_perL12M, aes(month_year))+
   geom_bar(aes(fill=(member_casual)))+
   theme(axis.text = element_text(angle=45))
```

![](Capstone-How-Does-a-Bike-Share-Navigate-Speedy-Success_files/figure-gfm/comparing%20member%20type%20activities%20for%20the%20latest%2012%20month-1.png)<!-- -->

``` r
## in function of month of latest 12 months, we can notice that casual are using the service like member. We can also notice that the grobal usage is going down from jully 2021 to March 2022, but this affect more casual user.It will be important to check this with marketing team!
```

``` r
# Show an overview of activities for weekdays

library(dplyr)
library(readr)
library(ggplot2)

Bike_trip_perweekdays<-Bike_tripdata_clean %>%
  group_by(weekdays,member_casual) %>%
  summarize(total_usage=nchar(weekdays))
```

    ## `summarise()` has grouped output by 'weekdays', 'member_casual'. You can
    ## override using the `.groups` argument.

``` r
 glimpse(Bike_trip_perweekdays)
```

    ## Rows: 9,258,460
    ## Columns: 3
    ## Groups: weekdays, member_casual [14]
    ## $ weekdays      <chr> "dimanche", "dimanche", "dimanche", "dimanche", "dimanch…
    ## $ member_casual <chr> "casual", "casual", "casual", "casual", "casual", "casua…
    ## $ total_usage   <int> 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,…

``` r
 #Ploting my data
 ggplot(Bike_trip_perweekdays, aes(weekdays))+
   geom_bar(aes(fill=(member_casual)))+
   theme(axis.text = element_text(angle=45))
```

![](Capstone-How-Does-a-Bike-Share-Navigate-Speedy-Success_files/figure-gfm/Comparing%20member%20type%20activities%20for%20weekdays-1.png)<!-- -->

``` r
 ## with the plot comparing activities per day, we can noticed that casual are mainly using more bike service the weekend days (sunday & saturday) compared to member.
```

``` r
# Show an overview of riders activities according with the rideable type they're using usualy

library(dplyr)
library(readr)
library(ggplot2)

Bike_trip_per_rideable_type<-Bike_tripdata_clean %>%
  group_by(rideable_type,member_casual) %>%
  summarize(average_length=mean(ride_length))
```

    ## `summarise()` has grouped output by 'rideable_type'. You can override using the
    ## `.groups` argument.

``` r
 glimpse(Bike_trip_per_rideable_type)
```

    ## Rows: 5
    ## Columns: 3
    ## Groups: rideable_type [3]
    ## $ rideable_type  <chr> "classic_bike", "classic_bike", "docked_bike", "electri…
    ## $ member_casual  <chr> "casual", "member", "casual", "casual", "member"
    ## $ average_length <dbl> 1489.8222, 787.5625, 3474.8611, 1091.7040, 676.2993

``` r
 #Ploting my data
 ggplot(Bike_trip_per_rideable_type, aes(rideable_type))+
   geom_bar(aes(fill=(member_casual)))+
   theme(axis.text = element_text(angle=45))
```

![](Capstone-How-Does-a-Bike-Share-Navigate-Speedy-Success_files/figure-gfm/Comparing%20member%20type%20in%20function%20of%20the%20rideable%20type%20they%20are%20using%20usualy-1.png)<!-- -->

``` r
 #the difference we can notice here is that member are using only 2/3 type of bike compared to casual
```

# Note

## Overall we notice during our analysis:

### the different member type are not using the service on the same way, casual are more active the weekend day and member are generally active everyday;

### In function of month we notice also that there are some period where casual are less active so it can explain why some of them are not using annual subscription.

### Comparing the rideable type for each user, we notice the casual are the only to use docked bike, we’ll check with marketing team to check if this offer is less expensive or if it’s exclude on annual payment plan.

### Comparing the mean length time, it appear that the user that have spent more time by using service are Casual, so it mean this user group could be also interested by a long time subscription

# Recommendations

### Taking in count the notes, as some casual are using just service for a casual month , in place of annual subscription, it could be also better to have bi annual subscription pack for this king of users;

### For casual that we identifying as best rider, we can suggest them the annual pack in order to show them the interest of this offer for them;

### it will be also better to have a survey in order to check with casual users that are most active to check why they are not choosing annual offer, it would allow to get more insight for future actions.
