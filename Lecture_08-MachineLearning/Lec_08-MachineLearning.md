K-means clustering
------------------

Let's start off with an example run of the **kmeans()** function:

``` r
# Generate some example data for clustering
tmp <- c(rnorm(30,-3), rnorm(30,3)) # 30 points centered on -3, 30 points center on +3
x <- cbind(x=tmp, y=rev(tmp)) # binds the two together as a matrix

plot(x)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-1-1.png)

Use the kmeans() function setting k to 2 and nstart=20 Inspect/print the results Q. How many points are in each cluster? *30 in each* Q. What ‘component’ of your result object details - cluster size? *"size"* -- call x\_clus$size  - cluster assignment/membership?  \*"cluster"\* call x\_clus$cluster - cluster center? *"centers"* call x\_clus$centers Plot x colored by the kmeans cluster assignment and add cluster centers as blue points

``` r
x_clus <- kmeans(x, centers = 2, nstart = 20)
x_clus
```

    ## K-means clustering with 2 clusters of sizes 30, 30
    ## 
    ## Cluster means:
    ##           x         y
    ## 1  2.980069 -3.146035
    ## 2 -3.146035  2.980069
    ## 
    ## Clustering vector:
    ##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1
    ## [36] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 62.7904 62.7904
    ##  (between_SS / total_SS =  90.0 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"    
    ## [5] "tot.withinss" "betweenss"    "size"         "iter"        
    ## [9] "ifault"

``` r
x_clus$cluster
```

    ##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1
    ## [36] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

``` r
plot(x, col = x_clus$cluster)
points(x_clus$centers, pch = 18, col = "blue", cex = 3)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-4-1.png)

Hierarchical Clustering Example
-------------------------------

Must give the **hclust()** function a distance matrix, not raw data, as an input.

``` r
# Distance matrix calc.
d <- dist(x)

# Clustering
hc <- hclust(d)
plot(hc)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-5-1.png)

This dendrogram makes sense: it starts from individual points, and begins clustering from there. It also shows us the structure of the data: which points lie in which groups, and so on. It is easy to visualzie that the points converge into two main groups

``` r
plot(hc)
abline(h = 6, col = 2) # adds a height cutoff
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
cutree(hc, h = 6) # groups clusters that fall below a height cut off, and returns a vector of clusters
```

    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2
    ## [36] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
cutree(hc, k = 2) # groups data into k# of clusters, and gives a vector output as well
```

    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2
    ## [36] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

### Now with a more complicated dataset, with significant overlap.

``` r
# Step 1. Generate some example data for clustering
x <- rbind(
 matrix(rnorm(100, mean=0, sd = 0.3), ncol = 2), # c1
 matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2), # c2
 matrix(c(rnorm(50, mean = 1, sd = 0.3), # c3
 rnorm(50, mean = 0, sd = 0.3)), ncol = 2))
colnames(x) <- c("x", "y")
```

``` r
# Step 2. Plot the data without clustering
plot(x)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
# Step 3. Generate colors for known clusters
# (just so we can compare to hclust results)

col <- as.factor( rep(c("c1","c2","c3"), each=50) )

plot(x, col=col)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-11-1.png)

Q. Use the dist(), hclust(), plot() and cutree() functions to return 2 and 3 clusters Q. How does this compare to your known 'col' groups?

#### Now we use hclust to cluster the data hierarchically

``` r
# *STEP 1*

# Converting x to distance data
xdist <- dist(x)

# Clustering of xdist, hierarchical
hier_x <- hclust(xdist)
hier_x
```

    ## 
    ## Call:
    ## hclust(d = xdist)
    ## 
    ## Cluster method   : complete 
    ## Distance         : euclidean 
    ## Number of objects: 150

``` r
# *STEP 2*

plot(hier_x, main = "2 Clusters")
abline(h = 2.5, col = 2)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
plot(hier_x, main = "3 Clusters")
abline(h=1.75, col = 3)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-13-2.png)

``` r
# *STEP 3.1*
gp2 <- cutree(hier_x, k = 2) # 2 clusters
gp2
```

    ##   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [36] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 1 2 2 2 2 2 1 2 2 1 2 2 1 2 2 2
    ##  [71] 1 2 1 1 2 2 2 2 2 2 2 1 1 2 2 2 1 1 1 2 2 1 1 1 2 1 2 1 2 1 1 1 1 1 1
    ## [106] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [141] 1 1 1 1 1 1 1 1 1 1

``` r
# *STEP 3.2*
gp3 <- cutree(hier_x, k = 3) # 2 clusters
gp3
```

    ##   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1 1 1 2 1
    ##  [36] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 2 3 3 3 3 3 2 3 3 2 3 3 2 3 3 3
    ##  [71] 2 3 2 2 3 3 3 3 3 3 3 2 2 3 3 3 2 2 2 3 3 2 2 2 3 2 3 2 3 2 2 2 2 2 2
    ## [106] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2
    ## [141] 2 2 2 2 2 2 2 2 2 2

``` r
plot(x, col = gp2)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-16-1.png)

The two-way clustering seems to be insufficient for clustering the data, so how about a three-way cluster?

``` r
plot(x, col = gp3)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-17-1.png)

The three way clustering isn't perfect, but it's a lot better than it was before we used hclust().

Let's look at how many groups we have in each cluster:

``` r
table(gp2)
```

    ## gp2
    ##   1   2 
    ## 118  32

``` r
table(gp3)
```

    ## gp3
    ##  1  2  3 
    ## 47 71 32

What about a cross comparison?

``` r
table(gp2, gp3)
```

    ##    gp3
    ## gp2  1  2  3
    ##   1 47 71  0
    ##   2  0  0 32

The results seem confusing at first, but in comparing them to our earlier tables, we can see that what gp3 did was split gp2's cluster 1 (n = 89) into two separate clusters, while retaining cluster 2 as cluster 3. If we look at our dendograms, we can easily see where the clustering would occur

PCA: Principal Component Analysis
=================================

We will use **prcomp()** function for PCA

``` r
## You can also download this file from the class website!
mydata <- read.csv("https://tinyurl.com/expression-CSV",
 row.names=1)
head(mydata, 10)
```

    ##         wt1 wt2  wt3  wt4 wt5 ko1 ko2 ko3 ko4 ko5
    ## gene1   439 458  408  429 420  90  88  86  90  93
    ## gene2   219 200  204  210 187 427 423 434 433 426
    ## gene3  1006 989 1030 1017 973 252 237 238 226 210
    ## gene4   783 792  829  856 760 849 856 835 885 894
    ## gene5   181 249  204  244 225 277 305 272 270 279
    ## gene6   460 502  491  491 493 612 594 577 618 638
    ## gene7    27  30   37   29  34 304 304 285 311 285
    ## gene8   175 182  184  166 180 255 291 305 271 269
    ## gene9   658 669  653  633 657 628 627 603 635 620
    ## gene10  121 116  134  117 133 931 941 990 982 934

``` r
# How many genes do we have?

nrow(mydata)
```

    ## [1] 100

100 genes

``` r
# How many organisms are we looking at?

ncol(mydata)
```

    ## [1] 10

10 organisms.

prcomp() will expect the data to be samples to be in rows, and genes to be in columns, so we must transpose

``` r
t(head(mydata))
```

    ##     gene1 gene2 gene3 gene4 gene5 gene6
    ## wt1   439   219  1006   783   181   460
    ## wt2   458   200   989   792   249   502
    ## wt3   408   204  1030   829   204   491
    ## wt4   429   210  1017   856   244   491
    ## wt5   420   187   973   760   225   493
    ## ko1    90   427   252   849   277   612
    ## ko2    88   423   237   856   305   594
    ## ko3    86   434   238   835   272   577
    ## ko4    90   433   226   885   270   618
    ## ko5    93   426   210   894   279   638

``` r
colnames(mydata)
```

    ##  [1] "wt1" "wt2" "wt3" "wt4" "wt5" "ko1" "ko2" "ko3" "ko4" "ko5"

Running the PCA on our data (transposed)

``` r
pca <- prcomp(t(mydata), scale = T)
attributes(pca) # SO WE CAN LOOK AT WHAT IS IN OUR PCA
```

    ## $names
    ## [1] "sdev"     "rotation" "center"   "scale"    "x"       
    ## 
    ## $class
    ## [1] "prcomp"

Now we can plot our PCA.

``` r
plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab ="PC2") 
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
## Variance captured per PC
pca.var <-  pca$sdev^2

## Percent variance captured per PC
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
```

``` r
head(pca.var.per)
```

    ## [1] 92.6  2.3  1.1  1.1  0.8  0.7

Now we can see the percent variance captured by each PC. This gives us a visual representation of how effective each PC is.

Let's plot this

``` r
barplot(pca.var.per, main="Scree Plot",
 xlab="Principal Component", ylab="Percent Variation")
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-29-1.png)

Now we can make our PCA plot look all pretty, like something you'd see in a journal

``` r
## A vector of colors for wt and ko samples
colvec <- colnames(mydata)
colvec[grep("wt", colvec)] <- "red"
colvec[grep("ko", colvec)] <- "blue"
plot(pca$x[,1], pca$x[,2], col=colvec, pch=16,
 xlab=paste0("PC1 (", pca.var.per[1], "%)"),
 ylab=paste0("PC2 (", pca.var.per[2], "%)")) 
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-30-1.png)

Now to do some PCA for our UK Foods Dataset
-------------------------------------------

Let's start by reading the data into a data frame:

``` r
foods <- read.csv("UK_foods.csv", row.names = 1)
head(foods)
```

    ##                England Wales Scotland N.Ireland
    ## Cheese             105   103      103        66
    ## Carcass_meat       245   227      242       267
    ## Other_meat         685   803      750       586
    ## Fish               147   160      122        93
    ## Fats_and_oils      193   235      184       209
    ## Sugars             156   175      147       139

First we try looking at the data:

``` r
barplot(as.matrix(foods), beside=F, col=rainbow(nrow(foods)))
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-32-1.png)

Now we should try looking at pairwise plots of food data:

``` r
pairs(foods, col=rainbow(10), pch=16)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-33-1.png)

Now we run the PCA, to make some sense of this

``` r
tfoods <- t(foods)
foodpca <- prcomp(tfoods)
summary(foodpca)
```

    ## Importance of components:
    ##                             PC1      PC2      PC3       PC4
    ## Standard deviation     324.1502 212.7478 73.87622 4.189e-14
    ## Proportion of Variance   0.6744   0.2905  0.03503 0.000e+00
    ## Cumulative Proportion    0.6744   0.9650  1.00000 1.000e+00

Now we plot the PCA:

``` r
plot(foodpca$x[,1], foodpca$x[,2], xlab="PC1", ylab="PC2", xlim=c(-270,500))
text(foodpca$x[,1], foodpca$x[,2], colnames(foods))
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-35-1.png)

``` r
colnames(foods)
```

    ## [1] "England"   "Wales"     "Scotland"  "N.Ireland"

``` r
mycols <- c("red","orange","green","blue")
```

Now that we've done that, we can make our plot prettier

``` r
plot(foodpca$x[,1], foodpca$x[,2],main = "PCA: UK Foods", xlab="PC1", ylab="PC2", xlim=c(-270,500))
text(foodpca$x[,1], foodpca$x[,2], colnames(foods), col = mycols)
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-38-1.png)

``` r
## Lets focus on PC1 as it accounts for > 90% of variance 
par(mar=c(10, 3, 0.35, 0))
barplot( foodpca$rotation[,1], las=2 )
```

![](Lec_08-MachineLearning_files/figure-markdown_github/unnamed-chunk-39-1.png)

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
