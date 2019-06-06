Functions Revisited
===================

We will source a file from online with our functions from last lecture (06).

``` r
source("http://tinyurl.com/rescale-R")
```

Try out the last day's rescale() function:

``` r
rescale(1:10)
```

    ##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
    ##  [8] 0.7777778 0.8888889 1.0000000

``` r
# rescale2(c(2,3,"String"))
```

This will give an error.

Find missing NA values in 2 vectors:
====================================

A function to count the number of missing value/"NA"s occuring in the same spot in two vectors

``` r
x <- c(1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)

# The answer would be 1 here, but we want R to figure that out for us.
```

The function is.na() will tell us whether a variable is an NA

``` r
is.na(x)
```

    ## [1] FALSE FALSE  TRUE FALSE  TRUE

``` r
is.na(y)
```

    ## [1]  TRUE FALSE  TRUE FALSE FALSE

Try putting these together with an AND statement, and we get a boolean vector

``` r
is.na(x) & is.na(y)
```

    ## [1] FALSE FALSE  TRUE FALSE FALSE

Take the sum() of the boolean vector to get the number of TRUEs (same as 1) which is the number of positions where the value is NA in both vectors.

``` r
sum(is.na(x) & is.na(y))
```

    ## [1] 1

How could this be adapted into a function?

``` r
both_na <- function(vec1, vec2) {
  sum(is.na(vec1) & is.na(vec2))
}
```

Here is the functional output with an example.

``` r
both_na(x,c(NA,3,NA,2,NA))
```

    ## [1] 2

``` r
xtest <- c(NA, NA, NA)
y1 <- c(1, NA, NA)
y2 <- c(1, NA, NA, NA)
```

``` r
both_na(xtest,y2)
```

    ## Warning in is.na(vec1) & is.na(vec2): longer object length is not a
    ## multiple of shorter object length

    ## [1] 3

How did this happen? Let's check a nother case

``` r
y3 <- c(1, NA, NA, NA, NA, NA, NA, NA)
both_na(xtest, y3)
```

    ## Warning in is.na(vec1) & is.na(vec2): longer object length is not a
    ## multiple of shorter object length

    ## [1] 7

The issue is that the function is recycling the shorter vector to match the longer vector.

``` r
3 != 2
```

    ## [1] TRUE

``` r
length(xtest)
```

    ## [1] 3

``` r
length(y2)
```

    ## [1] 4

Now lets build a both\_na2 function which will check for matching vector length

``` r
both_na2 <- function(vec1, vec2) {
  
  if(length(vec1) != length(vec2)){
    stop("Hey dungus, these vectors aren't the same length!")
  }
   
  sum(is.na(vec1) & is.na(vec2))
}
```

``` r
# both_na2(xtest,y3)
```

An error is generated

``` r
which(c(F,F,T,F,T))
```

    ## [1] 3 5

``` r
#which(is.na(c(1, 2, NA, 4)))
```

``` r
x <- c(1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)

both_na3(x,y)
```

    ## Found 1 NA's at position(s):3

    ## $number
    ## [1] 1
    ## 
    ## $which
    ## [1] 3

Intersect Function: Looking at genes
------------------------------------

``` r
df1
```

    ##     IDs exp
    ## 1 gene1   2
    ## 2 gene2   1
    ## 3 gene3   1

``` r
df2
```

    ##     IDs exp
    ## 1 gene2  -2
    ## 2 gene4  NA
    ## 3 gene3   1
    ## 4 gene5   2

Make things simpler

``` r
x <- df1$IDs
y <- df2$IDs
x
```

    ## [1] "gene1" "gene2" "gene3"

``` r
y
```

    ## [1] "gene2" "gene4" "gene3" "gene5"

``` r
intersect(x,y)
```

    ## [1] "gene2" "gene3"

``` r
x %in% y
```

    ## [1] FALSE  TRUE  TRUE

z

``` r
which(x %in% y)
```

    ## [1] 2 3

``` r
x[x %in% y]
```

    ## [1] "gene2" "gene3"

``` r
cbind(x[x %in% y],
      y[y %in% x] )
```

    ##      [,1]    [,2]   
    ## [1,] "gene2" "gene2"
    ## [2,] "gene3" "gene3"

Using Code -&gt; Extract Function shortcut in toolbar at top, we turned this code into a function

``` r
gene_intersect <- function(x, y) {
  cbind( x[ x %in% y ], 
         y[ y %in% x ] )
}
```

``` r
gene_intersect(df1$IDs, df2$IDs)
```

    ##      [,1]    [,2]   
    ## [1,] "gene2" "gene2"
    ## [2,] "gene3" "gene3"

``` r
gene_intersect2(df1,df2)
```

    ##     IDs exp df2[df2$IDs %in% df1$IDs, "exp"]
    ## 2 gene2   1                               -2
    ## 3 gene3   1                                1

\`

``` r
gene_intersect3(df1,df2)
```

    ##     IDs exp exp2
    ## 2 gene2   1   -2
    ## 3 gene3   1    1

``` r
gene_intersect4(df1,df2)
```

    ##     IDs exp exp2
    ## 2 gene2   1   -2
    ## 3 gene3   1    1

``` r
sessionInfo()
```

    ## R version 3.6.0 (2019-04-26)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Mojave 10.14.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_3.6.0  magrittr_1.5    tools_3.6.0     htmltools_0.3.6
    ##  [5] yaml_2.2.0      Rcpp_1.0.1      stringi_1.4.3   rmarkdown_1.12 
    ##  [9] knitr_1.23      stringr_1.4.0   xfun_0.7        digest_0.6.18  
    ## [13] evaluate_0.14
