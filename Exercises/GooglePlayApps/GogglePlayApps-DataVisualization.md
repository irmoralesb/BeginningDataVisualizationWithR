Data Visualization
================

Data Load
=========

Let's start by taking a look to our current dataset

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.2

    ## -- Attaching packages ----------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0       v purrr   0.3.0  
    ## v tibble  2.0.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.2       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.5.2

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## Warning: package 'tidyr' was built under R version 3.5.2

    ## Warning: package 'readr' was built under R version 3.5.2

    ## Warning: package 'purrr' was built under R version 3.5.2

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## Warning: package 'stringr' was built under R version 3.5.2

    ## Warning: package 'forcats' was built under R version 3.5.2

    ## -- Conflicts -------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(dslabs)
```

    ## Warning: package 'dslabs' was built under R version 3.5.2

``` r
library(forcats)

load("rda/apps_dataset.rda")
ds_theme_set()
head(apps_dataset)
```

    ## # A tibble: 6 x 17
    ##   App   Category Rating Reviews Size  Installs Type  Price Content.Rating
    ##   <chr> <fct>     <dbl>   <dbl> <fct> <fct>    <fct> <fct> <fct>         
    ## 1 Phot~ ART_AND~    4.1    1183 19M   10,000+  Free  0     Everyone      
    ## 2 Colo~ ART_AND~    3.9    5924 14M   500,000+ Free  0     Everyone      
    ## 3 U La~ ART_AND~    4.7    5681 8.7M  5,000,0~ Free  0     Everyone      
    ## 4 Sket~ ART_AND~    4.5    1947 25M   50,000,~ Free  0     Teen          
    ## 5 Pixe~ ART_AND~    4.3    5924 2.8M  100,000+ Free  0     Everyone      
    ## 6 Pape~ ART_AND~    4.4    1310 5.6M  50,000+  Free  0     Everyone      
    ## # ... with 8 more variables: Genres <fct>, Last.Updated <fct>,
    ## #   Current.Ver <fct>, Android.Ver <fct>, SizeNumeric <dbl>,
    ## #   InstallsNumeric <int>, PriceNumeric <dbl>, Last.UpdatedDate <date>

Data Visualization
==================

So now our data is loaded, let's start by making questions that may fit on the course structure.

Qualitative Univariate Analysis
-------------------------------

**Questions to answer**

    * How many apps are in each app category?
    * How many apps are in each installation range?
    * What proportion of apps are paid apps?
    * How many apps are in each app content rating?
    * How many apps are in each app genre?
    * What proportion of apps are supporting the latest version?

### How many apps are in each app category?

#### Frequency chart

``` r
apps_dataset %>% 
  ggplot(aes(x =fct_infreq(Category))) +
  geom_bar() +
  ggtitle("Count of Apps by Category") +
  xlab("Category") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90))
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-2-1.png)

#### Horizontal frequency chart

We have too many categories, so it's a better approach to display the data horizontally

``` r
apps_dataset %>% 
  ggplot(aes(x =fct_rev(fct_infreq(Category)))) +
  coord_flip() +
  geom_bar() +
  geom_text(aes(label = ..count.., hjust = -.05), stat = "count") +
  ggtitle("Count of Apps by Category") + 
  xlab("Count") +
  ylab("Category") +
  scale_y_continuous(limits = c(0,2050)) +
  theme(axis.text.x = element_text(angle = 90))
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-3-1.png)

#### Cleveland plot

It is an alternative way to show the data, but horizontal bar chart is clearer

``` r
apps_dataset %>%
  ggplot(aes(x =fct_rev(fct_infreq(Category)))) +
  geom_point(stat = "count") +
  coord_flip() +
  ggtitle("Count of Apps by Category") +
  xlab("Count") + 
  ylab("Category") +
  geom_text(aes(label = ..count.., hjust = -0.25), stat = "count") +
  scale_y_continuous(limits = c(0,2050))
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
  theme(axis.text.x = element_text(angle = 90))
```

    ## List of 1
    ##  $ axis.text.x:List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : num 90
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi FALSE
    ##  - attr(*, "validate")= logi TRUE

#### Pie chart

*Disclamer: Pie chart is not a recomended way to display any data, and it must not be used in general, anyway, in very few scenarios may be useful.*

As we can see, this data visualization is not clear nor useful for this amount of categories, anyway, we can answer easily the question about what category has more apps.

``` r
apps_dataset %>% 
  ggplot(aes(x = "", fill = Category)) +
  geom_bar() +
  coord_polar(theta = "y") +
  ggtitle("Count of Apps by Category") +
  ylab("") +
  xlab("") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank()
  )
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-5-1.png)

#### Answer

Question: **How many apps are in each app category?**

Answer, from bar chart we can see easily how many apps have each category, for instance,

    * What is the category with more apps?
      - Family, by far, with 1972 apps.
    * What is the category with less apps?      
      - Beauty, with 53 apps
    * What is the range of apps that a category has?
      - it is a wide range, 100 to 400.

### How many apps are in each installation range?

#### Frequency chart

``` r
apps_dataset %>% 
  group_by(Installs) %>%
  select(Installs, App) %>%
  summarize(count = n())
```

    ## # A tibble: 20 x 2
    ##    Installs       count
    ##    <fct>          <int>
    ##  1 0+                15
    ##  2 1+                67
    ##  3 5+                82
    ##  4 10+              386
    ##  5 50+              205
    ##  6 100+             719
    ##  7 500+             330
    ##  8 1,000+           908
    ##  9 5,000+           477
    ## 10 10,000+         1054
    ## 11 50,000+          479
    ## 12 100,000+        1169
    ## 13 500,000+         539
    ## 14 1,000,000+      1579
    ## 15 5,000,000+       752
    ## 16 10,000,000+     1252
    ## 17 50,000,000+      289
    ## 18 100,000,000+     409
    ## 19 500,000,000+      72
    ## 20 1,000,000,000+    58

``` r
apps_dataset %>% 
  ggplot(aes(x = Installs)) +
  ggtitle("Count of Apps by App Installs") +
  xlab("Installs") +
  ylab("Count") +
  geom_bar() +
  geom_text(aes(label=..count.., vjust= -.3), stat = "count") +
  theme(
    axis.text.x = element_text(angle = 90))
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-6-1.png)

#### Horizontal frequency chart

This data is more clear if shown in horizontal orientation, due to the long number of installs levels

``` r
apps_dataset %>% 
  ggplot(aes(Installs), stat = "count") +
  geom_bar() +
  coord_flip() +
  ggtitle("Count of Apps by App Installs") +
  ylab("Count") +
  geom_text(aes(label = ..count.., hjust = -.1), stat = "count") +
  scale_y_continuous(limits = c(0,1650))
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-7-1.png)

#### Cleveland plot

``` r
apps_dataset %>%
  ggplot(aes(Installs)) +
  ggtitle("Count of Apps by App Installs") +
  geom_point(stat = "count") + 
  xlab("Installs") +
  ylab("Count") +
  coord_flip() +
  geom_text(aes(label = ..count.., vjust= -.1, hjust= -.2), stat="count") +
  scale_y_continuous(limits = c(0,1650))
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-8-1.png)

#### Pie chart

This visulization is also not the best for this data

``` r
apps_dataset %>%
  ggplot(aes(x = "", fill = Installs)) +
  geom_bar() + 
  coord_polar(theta = "y") +
  ggtitle("Count of Apps by App Installs") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-9-1.png)

#### Answer

Question: *How many apps are in each installation range?*

Answer, from bar chart we can see easily how many apps have each app install, for instance,

    * What is the most common number of installation for an app?
      - We limit the answer to a big range, but it is because the range size we have, and we can tell it is between $1,000,000 and $5,000,000
    * Are there apps with no Installs?
      - We can say, yes, and the number of apps without Installs are 15, later we can try to see if they are recent or old.

### What proportion of apps are paid apps?

#### Frequency chart

``` r
apps_dataset %>%
  ggplot(aes(x = Type)) +
  geom_bar() +
  ggtitle("Count of Apps by Type") +
  ylab("Count") +
  xlab("Type") 
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-10-1.png)

#### Horizontal frequency chart

In this case it is simpler to see the vertical orientation

``` r
apps_dataset %>%
  ggplot(aes(Type)) +
  ggtitle("Count of Apps by Type")+ 
  xlab("Type") +
  ylab("Count") +
  coord_flip() +
  geom_bar()
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-11-1.png)

#### Cleveland plot

``` r
apps_dataset %>% 
  ggplot(aes(x= Type)) +
  geom_point(stat="count") +
  coord_flip() +
  ggtitle("Count of App by Type") +
  xlab("Type") +
  ylab("Count") +
  geom_text(aes(label = ..count.., vjust=1.5),stat = "count")
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-12-1.png)

#### Pie chart

In this case this chart can give a quick look to the relationship between the app types

``` r
apps_dataset %>%
  ggplot(aes(x = "", fill=Type)) +
  geom_bar() +
  coord_polar(theta = "y") +
  ggtitle("Count of App by Type") + 
  xlab("") +
  ylab("") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-13-1.png)

#### Answer

Question: *What proportion of apps are paid apps?*

Answers :

    * What proportion of apps are paid apps?
    - Free apps are predominant app types, less than 10% of apps are paid apps.

### How many apps are in each app content rating?

#### Frequency bar

``` r
apps_dataset %>%
  ggplot(aes(Content.Rating)) +
  geom_bar() +
  ggtitle("Count of Apps by Content Rating") +
  xlab("Content Ratings") + 
  ylab("Count")
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-14-1.png)

#### Horizontal frequency bar

``` r
apps_dataset %>%
  ggplot(aes(Content.Rating)) +
  geom_bar() +
  ggtitle ("Count of Apps by App Content Ratings") +
  xlab("Content Rating") +
  ylab("Count") +
  coord_flip()
```

![](GogglePlayApps-DataVisualization_files/figure-markdown_github/unnamed-chunk-15-1.png)

#### Cleveland plot

#### Pie chart

#### Answer

Question *How many apps are in each app rating?*

Answer

Quantitative Univariate Analysis
--------------------------------

**Questions to answer**

    * What is the app rating mean/max/min?
    * What is the app reviews mean/max/min?
    * What is the app size mean/max/min?
    * What is the app average price for an app?
    * What is the app installation distribution?
    * Are apps popularity in 2018 still high in comprarisson to previous years?

Qualitative Bivariate Analysis
------------------------------

**Questions to answer**

Quantitative Bivariate Analysis
-------------------------------

**Questions to answer**

Qualitative and Quantitative Bivariate Analysis
-----------------------------------------------

**Questions to answer**