Data Visualization - Quantitative Bivariate Analysis
================

Data Load
=========

Let's start by taking a look to our current dataset

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.2

    ## -- Attaching packages ---------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

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

    ## -- Conflicts ------------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dslabs)
```

    ## Warning: package 'dslabs' was built under R version 3.5.2

``` r
library(lattice)
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.5.2

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
load("rda/apps_dataset.rda")
load("rda/apps_categories_dataset.rda")
load("rda/apps_genres_dataset.rda")
load("rda/apps_versions_dataset.rda")
ds_theme_set()
```

Data Visualization
==================

Quantitative Bivariate Analysis
-------------------------------

Quantitative Features:

    * Ratings
    * Reviews
    * Size (Numeric)
    * Price
    * Last Updated
    * Installs (Numeric)

### Ratings vs Reviews

#### Scatterplot

``` r
title <- "Reviews vs Rating"
x_lab <- "Rating"
y_lab <- "Review"
z_lab <- "Density"
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x=Rating, y=Reviews)) +
  geom_point() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

with linear regression, even we already know there is no correlation between those features.

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating, y = Reviews)) +
  geom_point() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab) +
  geom_smooth(method = "lm")
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

#### Binned Frequency Heatmap

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x= Rating, y = Reviews)) +
  stat_bin2d() +
  ggtitle(title)+
  xlab(x_lab) +
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

#### Hexagonal Binned Frequency Heatmap

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x=Rating, y=Reviews)) +
  stat_binhex() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab)
```

    ## Warning: package 'hexbin' was built under R version 3.5.3

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

#### Countour Plot

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating, y = Reviews)) +
  geom_density2d() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-6-1.png)

#### Level Plot

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating, y =  Reviews)) +
  stat_density2d(aes(fill = ..level..), geom= "polygon") +
  ggtitle(title) +
  xlab(x_lab)+
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-7-1.png)

#### Mesh Plot

Creating the 2D kernel

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
temp_data <-
  apps_versions_dataset %>%
  filter(!is.na(Rating))
  

density2d <- kde2d(
  x = temp_data$Rating,
  y = temp_data$Reviews,
  n = 50)

grid <- expand.grid(
  x = density2d$x,
  y = density2d$y)

grid$z <- as.vector(density2d$z)

head(grid)
```

    ##          x y            z
    ## 1 1.000000 2 7.991171e-06
    ## 2 1.081633 2 3.468052e-06
    ## 3 1.163265 2 2.834726e-07
    ## 4 1.244898 2 4.364022e-09
    ## 5 1.326531 2 1.269770e-11
    ## 6 1.408163 2 2.829820e-11

``` r
grid %>%
  wireframe(
  x = z ~ x * y ,
  main = title,
  xlab= x_lab,
  ylab = y_lab,
  zlab = z_lab
)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-9-1.png)

#### Surface Plot

for the sake of practice, I am repeating the process to create the grid object

``` r
temp_data <- apps_versions_dataset%>%
  filter(!is.na(Rating)) 

density2d <- kde2d(
  x =  temp_data$Rating,
  y = temp_data$Reviews,
  n = 50)

grid <- expand.grid(
  x = density2d$x,
  y = density2d$y)

grid$z = as.vector(density2d$z)

head(grid)
```

    ##          x y            z
    ## 1 1.000000 2 7.991171e-06
    ## 2 1.081633 2 3.468052e-06
    ## 3 1.163265 2 2.834726e-07
    ## 4 1.244898 2 4.364022e-09
    ## 5 1.326531 2 1.269770e-11
    ## 6 1.408163 2 2.829820e-11

``` r
grid %>%
  wireframe(
    x = z ~ x * y,
    drape = TRUE,
    main = title,
    xlab = x_lab,
    ylab = y_lab,
    zlab = z_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-11-1.png)

### Ratings vs Size

#### Scatterplot

We can see there is no correlation between App Rating and size

``` r
title <- "Ratings vs Size"
y_lab <- "Size"

apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating, y = SizeNumeric)) +
  geom_point() +
  ggtitle(title)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating, y = SizeNumeric)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-13-1.png)

#### Binned Frequency Heatmap

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x=Rating, y = SizeNumeric)) +
  stat_bin2d() +
  ggtitle (title) +
  xlab(x_lab) +
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-14-1.png)

#### Hexagonal Binned Frequency Heatmap

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating, y = SizeNumeric)) +
  stat_binhex() +
  ggtitle(title)+
  xlab(x_lab)+
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-15-1.png)

#### Countour Plot

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating, y = SizeNumeric)) +
  geom_density2d() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-16-1.png)

#### Level Plot

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating, y = SizeNumeric)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon") +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-17-1.png)

#### Mesh Plot

``` r
temp_data <- apps_versions_dataset %>% filter(!is.na(Rating))

density2d <- kde2d(
  x = temp_data$Rating,
  y = temp_data$SizeNumeric,
  n = 50)

grid <- expand.grid(
  x = density2d$x,
  y = density2d$y)

grid$z <- as.vector(density2d$z)

grid %>%
  wireframe(
    x = z ~ x * y,
    main = title,
    xlab = x_lab,
    ylab = y_lab,
    zlab = z_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-18-1.png)

#### Surface Plot

``` r
temp_data <- apps_versions_dataset %>%
  filter(!is.na(Rating))
  
density2d <- kde2d(
  x = temp_data$Rating,
  y = temp_data$SizeNumeric,
  n = 50)  

grid <- expand.grid(
  x = density2d$x,
  y = density2d$y)

grid$z <- as.vector(density2d$z)

grid %>%
  wireframe(
    x = z ~ x * y, 
    drape = TRUE,
    main = title,
    xlab = x_lab,
    ylab = y_lab,
    zlab = z_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-19-1.png)

### Ratings vs Price

#### Scatterplot

``` r
title <- "Rating vs Price"
x_lab <- "Rating"
y_lab <- "Price"

apps_versions_dataset %>%
  filter(!is.na(Rating) & Type == "Paid") %>%
  ggplot(aes(x= Rating, y = PriceNumeric)) +
  geom_point() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
apps_versions_dataset %>% 
  filter(!is.na(Rating) & Type == "Paid") %>%
  ggplot(aes(x = Rating, y = PriceNumeric)) +
  geom_point() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab) +
  geom_smooth(method = "lm")
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-21-1.png) Taking a look at the graph above, one can think about some correlation between low price better rating... but this may be an example of correlation is not causation...

#### Binned Frequency Heatmap

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating) & Type == "Paid") %>%
  ggplot(aes(x = Rating, y = PriceNumeric)) +
  stat_bin2d() +
  ggtitle(title) +
  xlab (x_lab) +
  ylab (y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-22-1.png)

#### Hexagonal Binned Frequency Heatmap

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating) & Type == "Paid") %>%
  ggplot(aes(x = Rating, y = PriceNumeric)) +
  stat_binhex() +
  ggtitle(title) +
  xlab (x_lab) +
  ylab (y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-23-1.png)

#### Countour Plot

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating) & Type == "Paid")  %>%
  ggplot(aes(x = Rating, y = PriceNumeric)) +
  geom_density2d() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-24-1.png)

#### Level Plot

``` r
apps_versions_dataset %>%
  filter(!is.na(Rating) & Type == "Paid")  %>%
  ggplot(aes(x = Rating, y = PriceNumeric)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon") +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-25-1.png)

#### Mesh Plot

``` r
temp_data <- apps_versions_dataset %>% filter(!is.na(Rating) & Type == "Paid")

density2d <- kde2d(
  x = temp_data$Rating,
  y = temp_data$PriceNumeric,
  n = 50)

grid <- expand.grid(
  x = density2d$x,
  y = density2d$y)


grid$z <- as.vector(density2d$z)

grid %>% 
  wireframe(
  x = z ~ x * y,
  main = title,
  xlab = x_lab,
  ylab = y_lab,
  zlab = z_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-26-1.png)

#### Surface Plot

``` r
temp_data <- apps_versions_dataset %>% filter(!is.na(Rating) & Type == "Paid") 

density2d <- kde2d(
  x = temp_data$Rating,
  y = temp_data$PriceNumeric,
  n = 50)

grid <- expand.grid(
  x = density2d$x,
  y = density2d$y)

grid$z <- as.vector(density2d$z)

grid %>%
  wireframe(
    x = z ~ x * y,
    drape = TRUE,
    main = title,
    xlab = x_lab,
    ylab = y_lab,
    zlab = z_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-27-1.png)

### Size vs Price

#### Scatterplot

``` r
title <- "Size vs Price"
x_lab <- "Size"
y_lab <- "Price"

ggplot <- apps_versions_dataset %>%
  filter(Type == "Paid") %>%
  ggplot(aes(x = SizeNumeric, y = PriceNumeric)) +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab)
  
  ggplot +
  geom_point()
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-28-1.png)

#### Binned Frequency Heatmap

``` r
  ggplot +
  stat_bin2d()
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-29-1.png)

#### Hexagonal Binned Frequency Heatmap

``` r
  ggplot + 
  stat_binhex()
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-30-1.png)

#### Countour Plot

``` r
ggplot +
  geom_density2d()
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-31-1.png)

#### Level Plot

``` r
ggplot +
  stat_density2d(aes(fill = ..level..) , geom = "polygon")
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-32-1.png)

#### Mesh Plot

``` r
temp_data <- apps_versions_dataset %>% filter(Type == "Paid")

density2d <- kde2d(
  x = temp_data$SizeNumeric,
  y = temp_data$PriceNumeric,
  n = 50)

grid <- expand.grid(
  x = density2d$x,
  y = density2d$y)

grid$z <- as.vector(density2d$z)

grid %>%
  wireframe(
    x = z ~ x * y,
    main = title,
    xlab = x_lab,
    ylab = y_lab,
    zlab = z_lab)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-33-1.png)

#### Surface Plot

``` r
grid %>%
  wireframe(
    x = z ~ x * y,
    main = title,
    xlab = x_lab,
    ylab = y_lab,
    zlab = z_lab,
    drape = TRUE
  )
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-34-1.png)

### Ratings vs Last Updated

#### Step Chart

``` r
title <- "Ratings by Year"
x_lab <- "Year"
y_lab <- "Rating"

temp_data <- apps_versions_dataset %>% 
  filter(!is.na(Rating) & !is.nan(Rating)) %>%
  mutate(Year.Month = round_date(Last.UpdatedDate, unit = "month")) %>%
  group_by(Year.Month) %>%
  summarize(Date =max(Year.Month),Max = max(Rating), Min = min(Rating),Mean = mean(Rating), n = n())
  
  
temp_data %>% 
  ggplot(aes(x = Date, y= Mean)) +
  geom_step() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab) +
  expand_limits(y = 0)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-35-1.png)

#### Line Chart

``` r
temp_data %>%
  ggplot(aes(x = Date, y= Mean)) +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab) +
  geom_line() +
  expand_limits(y = 0)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-36-1.png)

#### Area chart

``` r
temp_data %>%
  ggplot(aes(x = Date, y= Mean)) +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab) +
  geom_area() +
  expand_limits(y = 0)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-37-1.png)

### Apps vs Last Updated

#### Step Chart

``` r
title <- "Apps by Year"
y_lab <- "Apps"

temp_data <- apps_versions_dataset %>%
  mutate(Year.Month = round_date(Last.UpdatedDate, unit = "month")) %>%
  group_by(Year.Month)  %>%
  summarize(Date = max(Year.Month), Count = n())


temp_data %>%
  ggplot(aes(x = Date, y = Count)) +
  geom_step() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab) +
  expand_limits(y = 0)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-38-1.png)

#### Line Chart

``` r
temp_data %>%
  ggplot(aes(x = Date, y = Count)) +
  geom_line() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab) +
  expand_limits(y = 0)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-39-1.png)

#### Area chart

``` r
temp_data %>%
  ggplot(aes(x = Date, y = Count)) +
  geom_area() +
  ggtitle(title) +
  xlab(x_lab) +
  ylab(y_lab) +
  expand_limits(y = 0)
```

![](DataVisualization-QuantitativeBivariateAnalysis_files/figure-markdown_github/unnamed-chunk-40-1.png)
