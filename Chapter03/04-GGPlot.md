Visualizing One Numeric Variable
================

We are requested to answer these questions:

    1. What is the average movie runtime?
    2. Are there any outliers?
    3. How spread out are the movie runtimes?
    4. What is the shape of the runtime distribution?

Univariate visualizations of a quantitiative variable

Setup environment
-----------------

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.5.2

``` r
movies = read.csv("../data/Movies.csv")
main_title <- "Distribution of Movies Runtimes"
x_lab = "Runtime in minutes"
```

Plot
----

Create dot plot

``` r
ggplot(
  data = movies,
  aes(x=Runtime, stat= "Count")) +
  geom_dotplot(binwidth = 1) +
  ggtitle(main_title) + 
  xlab(x_lab)
```

![](04-GGPlot_files/figure-markdown_github/unnamed-chunk-2-1.png)

Create a Violin-Style dot plot

``` r
ggplot(
  data = movies,
  aes(x = Runtime, stat = "count")) +
  geom_dotplot(
    binwidth = 1,
    stackdir = "center") +
  ggtitle(main_title) +
  xlab(x_lab)
```

![](04-GGPlot_files/figure-markdown_github/unnamed-chunk-3-1.png)

Boxplot
-------

Create box plot of runtime

``` r
ggplot(
  data = movies,
  aes(x= Runtime, y=Runtime)) +
  geom_boxplot() + 
  coord_flip() +
  ggtitle(main_title) + 
  xlab("") +
  ylab(x_lab) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())
```

    ## Warning: Continuous x aesthetic -- did you forget aes(group=...)?

![](04-GGPlot_files/figure-markdown_github/unnamed-chunk-4-1.png)

Histogram
---------

Create histogram with ggplot

``` r
ggplot(
data = movies,
aes(x = Runtime)) +
geom_histogram(binwidth = 10) +
  ggtitle(main_title) + 
  xlab(x_lab)
```

![](04-GGPlot_files/figure-markdown_github/unnamed-chunk-5-1.png)

Density plot
------------

Create density plot with ggplot

``` r
ggplot(
  data = movies,
  aes(x=Runtime)) +
  geom_density() +
  ggtitle(main_title) +
  xlab(x_lab)
```

![](04-GGPlot_files/figure-markdown_github/unnamed-chunk-6-1.png)

All in one
----------

Plot small multiples of all four

``` r
dev.off()
```

    ## null device 
    ##           1

``` r
library(grid)

viewport <- viewport(
  layout = grid.layout(4,1))

pushViewport(viewport)

dot <-ggplot(
  data = movies,
  aes(x = Runtime, ..count..)) +
  geom_dotplot(
    binwidth = 0.25) +
  scale_x_continuous(
    limits = c(0,250)) +
  ggtitle(main_title) +
  xlab("")

print(
  x = dot,
  vp = viewport(
    layout.pos.row = 1,
    layout.pos.col = 1))

box <- ggplot(
  data = movies,
  aes(x = Runtime, y = Runtime)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(
    limits = c(0,250)) + 
  xlab("")

print(
  x = box,
  vp = viewport(
    layout.pos.row = 2,
    layout.pos.col = 1))
```

    ## Warning: Continuous x aesthetic -- did you forget aes(group=...)?

``` r
hist <- ggplot(
    data = movies, 
    aes(x = Runtime)) +
    geom_histogram(binwidth = 10) +
    scale_x_continuous(
        limits = c(0, 250)) +
    xlab("")

print(
  x = hist,
  vp = viewport(
    layout.pos.col = 1,
    layout.pos.row = 3)) 
```

    ## Warning: Removed 2 rows containing missing values (geom_bar).

``` r
density <- ggplot(
    data = movies, 
    aes(x = Runtime)) +
    geom_density() +
    scale_x_continuous(
        limits = c(0, 250)) +
    xlab(x_lab)

print(
  x = density,
  vp = viewport(
    layout.pos.col = 1,
    layout.pos.row = 4))
```
