About
-----

This is my BIMM 143 **Lecture 6** R markdown document (Rmd) with my *code* and notes for the day.

``` r
plot(1:10, typ = "l", col = "blue", main = "Test Plot")
```

![](lecture6_files/figure-markdown_github/unnamed-chunk-1-1.png)

**Practice Reading in Files**
-----------------------------

### Three .txt files were read into data.frames using read.table-based functions:

``` r
# Test 1
read.csv('test1.txt')
```

    ##   Col1 Col2 Col3
    ## 1    1    2    3
    ## 2    4    5    6
    ## 3    7    8    9
    ## 4    a    b    c

``` r
# Test 2
read.table('test2.txt', header = TRUE, sep = '$')
```

    ##   Col1 Col2 Col3
    ## 1    1    2    3
    ## 2    4    5    6
    ## 3    7    8    9
    ## 4    a    b    c

``` r
# Test 3
read.table('test3.txt')
```

    ##   V1 V2 V3
    ## 1  1  6  a
    ## 2  2  7  b
    ## 3  3  8  c
    ## 4  4  9  d
    ## 5  5 10  e

Alternatively, files can be read as URLs

``` r
# Test 1, URL
read.csv("https://bioboot.github.io/bimm143_S19/class-material/test1.txt")
```

    ##   Col1 Col2 Col3
    ## 1    1    2    3
    ## 2    4    5    6
    ## 3    7    8    9
    ## 4    a    b    c

**Practice with Writing Functions**
-----------------------------------

### Here is the first WACKY SILLY FUN example we use:

``` r
add <- function(x, y = 1){
  # The body of the function is contained within brackets
  # Sum the input x and y
  x + y
}
```

``` r
add(10)
```

    ## [1] 11

``` r
add(10, y = 10)
```

    ## [1] 20

We can also use vectors:

``` r
add(c(1,2,3))
```

    ## [1] 2 3 4

``` r
add(c(1,2,3), 4)
```

    ## [1] 5 6 7

The correct arguments and correct number of arguments must be used:

``` r
#add(2,2,2)
# Note that three arguments are input, but the function is specified for only two. This will output an error.
```

``` r
#add(1,"b")
# Note that the argument for y is a character, not a numerical. This will output an error.
```

### Another example

``` r
rescale <- function(x){
  rng <- range(x)
  (x-rng[1]) / (rng[2] - rng[1])
}
```

Note how much more simple this is than calculating min(x) and max(x): we do one calculation, and simply index into the resultant vector. *This seems a bit overkill for one input, but if that input is a 100,000 value matrix, the number of calculations performed becomes very important. This is the difference between "I can run this on my MacBook Air" and "I need to overclock my i7-28103032kkxlsx to 12.3 GHz".*

``` r
rescale2 <- function(x){
  # Modified to ignore NAs in the data.
 
   if(!is.numeric(x)){
    stop("ALERT! ALERT! We are NOT gucci! THIS IS NOT A DRILL! REMOVE NON-NUMERICS FROM INPUT! THIS IS NOT A DRILL")
    }
  
  rng <- range(x, na.rm = TRUE)
  (x-rng[1]) / (rng[2] - rng[1])
}
```

``` r
rescale(1:10)
```

    ##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
    ##  [8] 0.7777778 0.8888889 1.0000000

``` r
# How can the function be reworked to give a proper output?
rescale(c(1,2,NA,3,10))
```

    ## [1] NA NA NA NA NA

What is the range vector doing in this case?

``` r
x <- c(1,2,NA,3,10)
  rng <- range(x)
  rng
```

    ## [1] NA NA

``` r
 # (x-rng[1]) / (rng[2] - rng[1])
```

We want the range vector to behave like this instead:

``` r
x <- c(1,2,3,10)
  rng <- range(x)
  rng
```

    ## [1]  1 10

To do this, we add an argument to range(x):

``` r
x <- c(1,2,NA,3,10)
  rng <- range(x, na.rm = TRUE)
  # na.rm will omit "NA"s when set to TRUE
  # This argument exists to tell you if you missing data.
  rng
```

    ## [1]  1 10

``` r
   (x-rng[1]) / (rng[2] - rng[1])
```

    ## [1] 0.0000000 0.1111111        NA 0.2222222 1.0000000

Using the fixed function rescale2, we can now ignore NA for proper output

``` r
rescale2(c(1,2,NA,3,10))
```

    ## [1] 0.0000000 0.1111111        NA 0.2222222 1.0000000

``` r
# Can go back to original by adding a na.rm = FALSE argument after the input argument
```

But what if we had a string in the arguments?

``` r
# What will the function do here? Give an error, but not a specific one. If we modify rescale2 to tell us something more specific, it would be a lot more useful.

# rescale2(c(1,10,"string"))
```

If we actually ran this code, we'd get an error message that we specified in rescale2.

### Now for another example function

Here we start using "print statements" and some if statements:

``` r
rescale3 <- function(x, na.rm=TRUE, plot=FALSE) {
  
 rng <-range(x, na.rm=na.rm)
 print("Hello")
 
 answer <- (x - rng[1]) / (rng[2] - rng[1])
 print("is it me you are looking for?")
 
 if(plot) {
  plot(answer, typ="b", lwd=4)
  print("Please stop singing, no one wants this") 
 }
 
 print("I can see it in ...")
 return(answer) # Returns everything specified above it. If we added it after print("Hello"), all we would get is "Hello"
 
}
```

``` r
rescale3(c(1,3,NA,10))
```

    ## [1] "Hello"
    ## [1] "is it me you are looking for?"
    ## [1] "I can see it in ..."

    ## [1] 0.0000000 0.2222222        NA 1.0000000

``` r
rescale3(c(1,3,NA,10), plot = TRUE)
```

    ## [1] "Hello"
    ## [1] "is it me you are looking for?"

![](lecture6_files/figure-markdown_github/unnamed-chunk-22-1.png)

    ## [1] "Please stop singing, no one wants this"
    ## [1] "I can see it in ..."

    ## [1] 0.0000000 0.2222222        NA 1.0000000

Using print statements is a good way to see if things worked correctly!

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
