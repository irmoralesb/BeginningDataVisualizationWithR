Visualizing both a categorical and a numeric variable
================

We are requested to answer:

    1. How does average box-office revenue compare across raings?
    2. How similar are the spreads of box-office revenue across ratings?
    3. How do their shapes compare?

*Bivariate visualizations for bith qualitiative and quantitiative bivariate variable*

Setting up environment
----------------------

``` r
movies <- read.csv("../data/Movies.csv")
main_label <- "Average Box-Office Revenue by Rating"
rating_label <- "Rating"
box_office_label <- "Box Office ($M)"
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.5.2

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Plots
-----

Create table of average box office by rating

``` r
average  <- movies %>% 
  select(Rating, Box.Office) %>%
  group_by(Rating) %>%
  summarize(Box.Office = mean(Box.Office)) %>%
  as.data.frame()

print(average)
```

    ##   Rating Box.Office
    ## 1      G   55.47561
    ## 2     PG   56.40439
    ## 3  PG-13   54.56134
    ## 4      R   22.26118

### Bivariate bar chart

``` r
ggplot(
  data = average,
  aes(x =Rating, y = Box.Office)) +
  geom_bar(stat = "identity") +
  ggtitle(main_label) +
  xlab(rating_label) +
  ylab(box_office_label)
```

![](04-GGPlot_files/figure-markdown_github/unnamed-chunk-3-1.png)

### Boxplot

``` r
main_label <- "Box Office Revenue by Rating"
ggplot(
  data = movies,
  aes(x = Rating, y = Box.Office)) +
  geom_boxplot() +
  ggtitle(main_label) + 
  xlab(rating_label) +
  ylab(box_office_label)
```

![](04-GGPlot_files/figure-markdown_github/unnamed-chunk-4-1.png)

### Notched boxplot

``` r
main_label <- "Box Office Revenue by Rating"
ggplot(
  data = movies,
  aes(x = Rating, y = Box.Office)) +
  geom_boxplot(notch = TRUE) +
  ggtitle(main_label) + 
  xlab(rating_label) +
  ylab(box_office_label)
```

![](04-GGPlot_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Violin plot

``` r
ggplot(
  data = movies,
  aes(x= Rating, y = Box.Office)) +
  geom_violin() +
  ggtitle(main_label) +
  xlab(rating_label) +
  ylab(box_office_label)
```

![](04-GGPlot_files/figure-markdown_github/unnamed-chunk-6-1.png)
