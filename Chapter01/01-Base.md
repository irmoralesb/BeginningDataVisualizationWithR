Chapter 1 - Base Chart
================

Demo using Base R
-----------------

Create data frame

``` r
df <- data.frame(
  Name = c("a","b","c"),
  Value = c(1,2,3))
```

Display the data frame

``` r
print(df)
```

    ##   Name Value
    ## 1    a     1
    ## 2    b     2
    ## 3    c     3

*Note that when passing the data frame it displays the axis names, this is because it has where to get the data from*, if you pass the column as parameter it doest't have a way to determine those axis names automatically.

### Plot

Plot the data frame using default parameters

``` r
plot(df)
```

![](01-Base_files/figure-markdown_github/unnamed-chunk-3-1.png)

Plot using default parameter order

``` r
plot(df$Name, df$Value)
```

![](01-Base_files/figure-markdown_github/unnamed-chunk-4-1.png)

Plot using named parameters

``` r
plot(  
  x = df$Name,
  y = df$Value)
```

![](01-Base_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Bar chart

Create a bar chart

``` r
barplot(
  names= df$Name,
  height= df$Value,
  col= "skyblue",
  main= "Hello World",
  xlab="Name",
  ylab= "Value"
)
```

![](01-Base_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Help files

`?plot` and `?barplot` to see paramters help `?par`
