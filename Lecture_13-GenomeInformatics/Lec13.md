Genotype data for a specific population
---------------------------------------

Here we focus on the ancestry

First we read our .csv of MXL asthma SNP data.

``` r
# Reading the .csv into a data.frame.

mxl <- read.csv("AsthmaSNP_MXL.csv")

head(mxl)
```

    ##   Sample..Male.Female.Unknown. Genotype..forward.strand. Population.s.
    ## 1                  NA19648 (F)                       A|A ALL, AMR, MXL
    ## 2                  NA19649 (M)                       G|G ALL, AMR, MXL
    ## 3                  NA19651 (F)                       A|A ALL, AMR, MXL
    ## 4                  NA19652 (M)                       G|G ALL, AMR, MXL
    ## 5                  NA19654 (F)                       G|G ALL, AMR, MXL
    ## 6                  NA19655 (M)                       A|G ALL, AMR, MXL
    ##   Father Mother
    ## 1      -      -
    ## 2      -      -
    ## 3      -      -
    ## 4      -      -
    ## 5      -      -
    ## 6      -      -

Let's see how many individuals have each genotype:

``` r
genos <- table(mxl$Genotype..forward.strand.)
genos
```

    ## 
    ## A|A A|G G|A G|G 
    ##  22  21  12   9

And now, the proportion of each genotype:

``` r
genos/nrow(mxl) * 100
```

    ## 
    ##     A|A     A|G     G|A     G|G 
    ## 34.3750 32.8125 18.7500 14.0625

Quality Scores in FASTQ files
-----------------------------

The 4th line of a FASTQ indicates the quality score of each base called in the second line as an ASCII encoded sequence, telling us the quality of the sequence at a given position (i.e. the likelihood of how correct it is based upon the instrument)

Now we look at the per base quality scores of an example sequence, using the ASCII encoding of a fastqsanger file.

``` r
library(seqinr)
library(gtools)
asc(s2c("DDDDCDEDCDDDDBBDDDCC@")) - 33
```

    ##  D  D  D  D  C  D  E  D  C  D  D  D  D  B  B  D  D  D  C  C  @ 
    ## 35 35 35 35 34 35 36 35 34 35 35 35 35 33 33 35 35 35 34 34 31

Population Scale Analysis
-------------------------

Now lets examine population-level genotype diversity in these SNPs

First let's read that data in:

``` r
pop <- read.table("https://bioboot.github.io/bimm143_S18/class-material/rs8067378_ENSG00000172057.6.txt")
head(pop)
```

    ##    sample geno      exp
    ## 1 HG00367  A/G 28.96038
    ## 2 NA20768  A/G 20.24449
    ## 3 HG00361  A/A 31.32628
    ## 4 HG00135  A/A 34.11169
    ## 5 NA18870  G/G 18.25141
    ## 6 NA11993  A/A 32.89721

``` r
summary(pop)
```

    ##      sample     geno          exp        
    ##  HG00096:  1   A/A:108   Min.   : 6.675  
    ##  HG00097:  1   A/G:233   1st Qu.:20.004  
    ##  HG00099:  1   G/G:121   Median :25.116  
    ##  HG00100:  1             Mean   :25.640  
    ##  HG00101:  1             3rd Qu.:30.779  
    ##  HG00102:  1             Max.   :51.518  
    ##  (Other):456

Now let's look into this a little bit more with the table() function:

``` r
# How many individuals have each genotype?
genopop <- table(pop$geno)
genopop
```

    ## 
    ## A/A A/G G/G 
    ## 108 233 121

``` r
# What is the proportion of each genotype in the population?
genopop * 100 / nrow(pop)
```

    ## 
    ##      A/A      A/G      G/G 
    ## 23.37662 50.43290 26.19048

Here is a barplot describing the distribution of genotypes.

``` r
barplot(genopop, main = "Distribution of Genotypes", col = c(4,5,3), ylim = c(0,300))
```

![](Lec13_files/figure-markdown_github/unnamed-chunk-8-1.png)

Now we organize the data by genotypes

``` r
pop_aa <- pop[pop$geno == "A/A",]
pop_ag <- pop[pop$geno == "A/G",]
pop_gg <- pop[pop$geno == "G/G",]
```

And we summarize each again to get a sense of their distribution.

``` r
print("A/A Genotype")
```

    ## [1] "A/A Genotype"

``` r
summary(pop_aa)
```

    ##      sample     geno          exp       
    ##  HG00096:  1   A/A:108   Min.   :11.40  
    ##  HG00100:  1   A/G:  0   1st Qu.:27.02  
    ##  HG00101:  1   G/G:  0   Median :31.25  
    ##  HG00102:  1             Mean   :31.82  
    ##  HG00104:  1             3rd Qu.:35.92  
    ##  HG00105:  1             Max.   :51.52  
    ##  (Other):102

``` r
print("A/G Genotype")
```

    ## [1] "A/G Genotype"

``` r
summary(pop_ag)
```

    ##      sample     geno          exp        
    ##  HG00097:  1   A/A:  0   Min.   : 7.075  
    ##  HG00103:  1   A/G:233   1st Qu.:20.626  
    ##  HG00106:  1   G/G:  0   Median :25.065  
    ##  HG00110:  1             Mean   :25.397  
    ##  HG00114:  1             3rd Qu.:30.552  
    ##  HG00115:  1             Max.   :48.034  
    ##  (Other):227

``` r
print("G/G Genotype")
```

    ## [1] "G/G Genotype"

``` r
summary(pop_gg)
```

    ##      sample     geno          exp        
    ##  HG00099:  1   A/A:  0   Min.   : 6.675  
    ##  HG00109:  1   A/G:  0   1st Qu.:16.903  
    ##  HG00112:  1   G/G:121   Median :20.074  
    ##  HG00116:  1             Mean   :20.594  
    ##  HG00118:  1             3rd Qu.:24.457  
    ##  HG00120:  1             Max.   :33.956  
    ##  (Other):115

Now we look at a boxplot distribution of our expression data for each genotype:

``` r
boxplot(exp ~ geno, data = pop, col = c(4,5,3), ylab = "Gene Expression")
```

![](Lec13_files/figure-markdown_github/unnamed-chunk-11-1.png)

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
    ## other attached packages:
    ## [1] gtools_3.8.1 seqinr_3.4-5
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] MASS_7.3-51.4   compiler_3.6.0  magrittr_1.5    tools_3.6.0    
    ##  [5] htmltools_0.3.6 yaml_2.2.0      Rcpp_1.0.1      stringi_1.4.3  
    ##  [9] rmarkdown_1.12  knitr_1.23      ade4_1.7-13     stringr_1.4.0  
    ## [13] xfun_0.7        digest_0.6.18   evaluate_0.14
