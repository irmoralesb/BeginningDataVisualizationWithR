Google Play Apps
================

Data Visualization of Google Play Apps
======================================

Dataset Description
-------------------

Dataset downloaded from: <https://www.kaggle.com/lava18/google-play-store-apps>

Loading the dataset

``` r
library(tidyverse)
library(ggplot2)
library(lattice)
library(latticeExtra)

apps_dataset = read.csv("../../data/googleplaystore.csv", header = TRUE, sep = ",")

#class(apps_dataset)
head(as.tibble(apps_dataset))
```

    ## # A tibble: 6 x 13
    ##   App   Category Rating Reviews Size  Installs Type  Price Content.Rating
    ##   <fct> <fct>     <dbl> <fct>   <fct> <fct>    <fct> <fct> <fct>         
    ## 1 Phot~ ART_AND~    4.1 159     19M   10,000+  Free  0     Everyone      
    ## 2 Colo~ ART_AND~    3.9 967     14M   500,000+ Free  0     Everyone      
    ## 3 U La~ ART_AND~    4.7 87510   8.7M  5,000,0~ Free  0     Everyone      
    ## 4 Sket~ ART_AND~    4.5 215644  25M   50,000,~ Free  0     Teen          
    ## 5 Pixe~ ART_AND~    4.3 967     2.8M  100,000+ Free  0     Everyone      
    ## 6 Pape~ ART_AND~    4.4 167     5.6M  50,000+  Free  0     Everyone      
    ## # ... with 4 more variables: Genres <fct>, Last.Updated <fct>,
    ## #   Current.Ver <fct>, Android.Ver <fct>

Column types
------------

Let's confirm the column types are correct.

How many apps are there?

``` r
rows_total <- nrow(apps_dataset)
rows_without_na <- nrow(na.omit(apps_dataset))

sprintf("Total apps %d", rows_total)
```

    ## [1] "Total apps 10841"

``` r
sprintf("Total apps %d, removing na", rows_without_na)
```

    ## [1] "Total apps 9367, removing na"

Data wrangling
--------------

Let's clean up our data.

NAs
---

Let's take a look at those features that contains NAs

``` r
colnames(apps_dataset)[colSums(is.na(apps_dataset)) > 0]
```

    ## [1] "Rating"

Let's confirm how many observations there are with NA

We see Rating column is numeric

``` r
class(apps_dataset$Rating)
```

    ## [1] "numeric"

Therefore those recods are missing, it may be due to:

    * They are new and there are no yet any rating.
    * There may be a new version with no ratings yet.
    * Messing value
    * It may be around for a while and it has no download (or almost none)

``` r
apps_without_ratings <-  apps_dataset %>% 
  filter(is.na(Rating)) %>%
  nrow()

sprintf("There are %d without ratings", apps_without_ratings)
```

    ## [1] "There are 1474 without ratings"

Let's see if they must be considered, or at least which ones...

``` r
newest_app_date <- apps_dataset %>%  select(Last.Updated) 
class(newest_app_date$Last.Updated)
```

    ## [1] "factor"

``` r
head(newest_app_date)
```

    ##       Last.Updated
    ## 1  January 7, 2018
    ## 2 January 15, 2018
    ## 3   August 1, 2018
    ## 4     June 8, 2018
    ## 5    June 20, 2018
    ## 6   March 26, 2017
