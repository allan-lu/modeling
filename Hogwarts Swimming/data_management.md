---
title: "Homework 6"
author: "Allan Lu"
date: "March 17, 2020"
output: 
  html_document:
    keep_md: yes
    df_print: paged
    toc: yes
  html_notebook:
    df_print: paged
    toc: yes
---


# Loading Libraries
Load in necessary libraries in one chunk.


```r
library(tidyverse)
library(knitr)
library(stringr)
library(magrittr)
library(dplyr)
```


# Number of Swimmers in Each House

Find out which house in Hogwarts has the most swimmers in the swimming database.


```r
houses_df <- read_csv("Hogwarts_House_Students.csv")
swims_df <- read_csv("Hogwarts_swimming_2019_20.csv")

swims_df %<>%
  left_join(houses_df, by="ENTRIES")
```


```r
swimmers_df <- swims_df %>%
  select(ENTRIES, HOUSE = House.name) %>%
  distinct() %>%
  group_by(HOUSE) %>%
  count(HOUSE, name="NUMBER_SWIMMERS")

swimmers_df
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["HOUSE"],"name":[1],"type":["chr"],"align":["left"]},{"label":["NUMBER_SWIMMERS"],"name":[2],"type":["int"],"align":["right"]}],"data":[{"1":"Gryffindor","2":"48"},{"1":"Hufflepuff","2":"16"},{"1":"Ravenclaw","2":"20"},{"1":"Slytherin","2":"28"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
Gryffindor is the Hogwarts house with the most swimmers.


# Points per Swimmer

Find the points won per swimmer for each of the houses in Hogwarts.


```r
relays <- swims_df %>%
  filter(grepl("RELAY", EVENT)) %>%
  mutate(new_PTS = ifelse(PL == 1, 8/4, 
                          ifelse(PL == 2, 4/4, 
                                 ifelse(PL == 3, 2/4, 
                                        0))))

races_df <- swims_df %>%
  filter(!grepl("RELAY", EVENT)) %>%
  mutate(new_PTS = ifelse(PL == 1, 6,
                          ifelse(PL == 2, 4,
                                 ifelse(PL == 3, 3,
                                        ifelse(PL == 4, 2,
                                               ifelse(PL == 5, 1, 
                                                      0)))))) %>%
  rbind(relays) %>%
  drop_na()
```



```r
points_df <- races_df %>%
  select(HOUSE = House.name, new_PTS) %>%
  group_by(HOUSE) %>%
  summarize(TOTAL_POINTS = sum(new_PTS)) %>%
  left_join(swimmers_df, by="HOUSE") %>%
  mutate(PTS_PER_SWIMMER = TOTAL_POINTS / NUMBER_SWIMMERS)

points_df
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["HOUSE"],"name":[1],"type":["chr"],"align":["left"]},{"label":["TOTAL_POINTS"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["NUMBER_SWIMMERS"],"name":[3],"type":["int"],"align":["right"]},{"label":["PTS_PER_SWIMMER"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"Gryffindor","2":"802.5","3":"48","4":"16.71875"},{"1":"Hufflepuff","2":"291.0","3":"16","4":"18.18750"},{"1":"Ravenclaw","2":"420.5","3":"20","4":"21.02500"},{"1":"Slytherin","2":"477.0","3":"28","4":"17.03571"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


# Top 10 Fastest
Find the top 10 fastest swimmers in the `100 FREE`. 


```r
library(lubridate)

time_df <- swims_df %>%
  drop_na() %>%
  select(EVENT, ENTRIES, TIME, Date, Against, 
         HOUSE = House.name) %>%
  mutate(seconds = TIME %>% ms() %>% seconds() %>% as.numeric()) %>%
  filter(EVENT == "100 FREE") %>%
  top_n(-10, seconds) %>%
  arrange(seconds) %>%
  select(-seconds)

time_df
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["EVENT"],"name":[1],"type":["chr"],"align":["left"]},{"label":["ENTRIES"],"name":[2],"type":["chr"],"align":["left"]},{"label":["TIME"],"name":[3],"type":["chr"],"align":["left"]},{"label":["Date"],"name":[4],"type":["chr"],"align":["left"]},{"label":["Against"],"name":[5],"type":["chr"],"align":["left"]},{"label":["HOUSE"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"100 FREE","2":"Owen Cauldwell","3":"00:58.6","4":"20-Oct-19","5":"Beauxbatons","6":"Hufflepuff"},{"1":"100 FREE","2":"Hermione Granger","3":"00:59.4","4":"20-Oct-19","5":"Beauxbatons","6":"Gryffindor"},{"1":"100 FREE","2":"Kevin Entwhistle","3":"00:59.7","4":"20-Oct-19","5":"Ilvermorny","6":"Ravenclaw"},{"1":"100 FREE","2":"Filius Flitwick","3":"01:00.4","4":"19-Oct-19","5":"Castelobruxo","6":"Ravenclaw"},{"1":"100 FREE","2":"Parvati Patil","3":"01:01.2","4":"27-Oct-19","5":"Castelobruxo","6":"Gryffindor"},{"1":"100 FREE","2":"Michael Corner","3":"01:01.5","4":"19-Oct-19","5":"Mahoutokoro","6":"Ravenclaw"},{"1":"100 FREE","2":"Colin Creevey","3":"01:02.8","4":"20-Oct-19","5":"Beauxbatons","6":"Gryffindor"},{"1":"100 FREE","2":"Justin Finch-Fletchley","3":"01:03.0","4":"20-Oct-19","5":"Beauxbatons","6":"Hufflepuff"},{"1":"100 FREE","2":"Stephen Cornfoot","3":"01:03.2","4":"20-Oct-19","5":"Ilvermorny","6":"Slytherin"},{"1":"100 FREE","2":"Albus Dumbledore","3":"01:03.3","4":"19-Oct-19","5":"Durmstrang","6":"Gryffindor"},{"1":"100 FREE","2":"Rubeus Hagrid","3":"01:03.3","4":"19-Oct-19","5":"Durmstrang","6":"Gryffindor"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

