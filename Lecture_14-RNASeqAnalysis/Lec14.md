Importing Data for DESeq2
-------------------------

First, we import our RNA Seq data and metadata to data.frames:

``` r
# Read scale counts into data.frame.
counts <- read.csv("data/airway_scaledcounts.csv", 
                                 stringsAsFactors = F)

# Read meta data into data.frame.
metadata <- read.csv("data/airway_metadata.csv",
                            stringsAsFactors = F)
```

Now we examine each:

``` r
head(counts)
```

    ##           ensgene SRR1039508 SRR1039509 SRR1039512 SRR1039513 SRR1039516
    ## 1 ENSG00000000003        723        486        904        445       1170
    ## 2 ENSG00000000005          0          0          0          0          0
    ## 3 ENSG00000000419        467        523        616        371        582
    ## 4 ENSG00000000457        347        258        364        237        318
    ## 5 ENSG00000000460         96         81         73         66        118
    ## 6 ENSG00000000938          0          0          1          0          2
    ##   SRR1039517 SRR1039520 SRR1039521
    ## 1       1097        806        604
    ## 2          0          0          0
    ## 3        781        417        509
    ## 4        447        330        324
    ## 5         94        102         74
    ## 6          0          0          0

``` r
head(metadata)
```

    ##           id     dex celltype     geo_id
    ## 1 SRR1039508 control   N61311 GSM1275862
    ## 2 SRR1039509 treated   N61311 GSM1275863
    ## 3 SRR1039512 control  N052611 GSM1275866
    ## 4 SRR1039513 treated  N052611 GSM1275867
    ## 5 SRR1039516 control  N080611 GSM1275870
    ## 6 SRR1039517 treated  N080611 GSM1275871

Toy Differential Gene Expression
--------------------------------

**This is NOT how differental Gene Expression should be done. This exercise is purely for introducing the concept.**

Let's look at metadata again:

``` r
View(metadata)
```

    ## Warning in system2("/usr/bin/otool", c("-L", shQuote(DSO)), stdout = TRUE):
    ## running command ''/usr/bin/otool' -L '/Library/Frameworks/R.framework/
    ## Resources/modules/R_de.so'' had status 1

Now let's try a bit of code to analyze our RNA Seq data:

``` r
control <- metadata[metadata[,"dex"]=="control",]
control.mean <- rowSums( counts[ ,control$id] )/4
names(control.mean) <- counts$ensgene
```

Q1) The code above could be made more robust but not using specific values (i.e. change the division by 4 to reflect the total \# of controls). If more samples were added, this code would still return an output, but it would be wrong since the control.mean will only reflect a mean of n = 4, when n is actually higher.

Q2) Let's try this now for our treated data:

``` r
treated <- metadata[metadata[,"dex"]=="treated",]
treated.mean <- rowSums( counts[ ,treated$id] )/4 
names(treated.mean) <- counts$ensgene
```

Let's combine our mean data to make things easier, and take a (crude) look at counts:

``` r
meancounts <- data.frame(control.mean, treated.mean)
colSums(meancounts)
```

    ## control.mean treated.mean 
    ##     23005324     22196524

*For a real workflow, you should normalize the samples for sequencing depth. Otherwise, your analysis is biased (and basically useless).*

Q3) Let's show a scatterplot of our means comparing controls to treated samples:

``` r
# Axes are assigned to make our code a little less convoluted.
xax_toy <- meancounts$control.mean 
yax_toy <- meancounts$treated.mean

# Now we plot
plot(xax_toy,yax_toy, main = "Toy RNASeq Analysis: Treated vs. Controls", 
     xlab = "Controls, Mean Counts", ylab = "Treated, Mean Counts")
```

![](Lec14_files/figure-markdown_github/unnamed-chunk-8-1.png)

On this scale, we can't really see the full breadth of data (60k-ish data points). Let's put this on a more reasonable scale for analysis:

``` r
# First we put the data on a log scale.
# We do this once to save computing time, instead of for each axis.
meancounts2 <- log10(meancounts)


# Axes are assigned to make our code a little less convoluted.
xax_toy2 <- meancounts2$control.mean 
yax_toy2 <- meancounts2$treated.mean

# Now we plot
plot(xax_toy2,yax_toy2, main = "Toy RNASeq Analysis: Treated vs. Controls", 
     xlab = "Log10 of Controls, Mean Counts", 
     ylab = "Log10 of Treated, Mean Counts")
```

![](Lec14_files/figure-markdown_github/unnamed-chunk-9-1.png)

Now let's look for *strong candidates for differential expression* by taking the log2-fold change in expression between cotnrol and mean.

``` r
meancounts$log2fc <- log2(meancounts[,"treated.mean"]/
                            meancounts[,"control.mean"])
head(meancounts)
```

    ##                 control.mean treated.mean      log2fc
    ## ENSG00000000003       900.75       658.00 -0.45303916
    ## ENSG00000000005         0.00         0.00         NaN
    ## ENSG00000000419       520.50       546.00  0.06900279
    ## ENSG00000000457       339.75       316.50 -0.10226805
    ## ENSG00000000460        97.25        78.75 -0.30441833
    ## ENSG00000000938         0.75         0.00        -Inf

Unfortunately, this gives us NaN and -Inf, or division by zero and log(0) results, respectively. This is because some genes won't be expressed at all in the control condition (NaN), while others are completely unexpressed in the treatment conditions (-Inf).

So, let's filter that garbage out:

``` r
zero.vals <- which(meancounts[,1:2] == 0, arr.ind = T)

to.rm <- unique(zero.vals[,1])
mycounts <- meancounts[-to.rm,]
head(mycounts)
```

    ##                 control.mean treated.mean      log2fc
    ## ENSG00000000003       900.75       658.00 -0.45303916
    ## ENSG00000000419       520.50       546.00  0.06900279
    ## ENSG00000000457       339.75       316.50 -0.10226805
    ## ENSG00000000460        97.25        78.75 -0.30441833
    ## ENSG00000000971      5219.00      6687.50  0.35769358
    ## ENSG00000001036      2327.00      1785.75 -0.38194109

Q4) arrayInd is a logical argument that will specify whether the indices of the array should be returned or not. Since it's true, this has the effect of returning the indices that are equal to 0.

Now let's filter for the samples that are *differentially expressed*:

``` r
up.ind <- mycounts$log2fc > 2
down.ind <- mycounts$log2fc < (-2)
```

Q5) Number of upregulated and downregulated genes. This can be found by taking the length of each vector above:

``` r
print(paste("Up-regulated genes:", sum(up.ind)))
```

    ## [1] "Up-regulated genes: 250"

``` r
print(paste("Down-regulated genes:", sum(down.ind)))
```

    ## [1] "Down-regulated genes: 367"

Adding Annotation Data
----------------------

Let's put our annotation data into a data.frame

``` r
anno <- read.csv("https://bioboot.github.io/bimm143_W18/class-material/annotables_grch38.csv")
head(anno)
```

    ##           ensgene entrez   symbol chr     start       end strand
    ## 1 ENSG00000000003   7105   TSPAN6   X 100627109 100639991     -1
    ## 2 ENSG00000000005  64102     TNMD   X 100584802 100599885      1
    ## 3 ENSG00000000419   8813     DPM1  20  50934867  50958555     -1
    ## 4 ENSG00000000457  57147    SCYL3   1 169849631 169894267     -1
    ## 5 ENSG00000000460  55732 C1orf112   1 169662007 169854080      1
    ## 6 ENSG00000000938   2268      FGR   1  27612064  27635277     -1
    ##          biotype
    ## 1 protein_coding
    ## 2 protein_coding
    ## 3 protein_coding
    ## 4 protein_coding
    ## 5 protein_coding
    ## 6 protein_coding
    ##                                                                                                  description
    ## 1                                                          tetraspanin 6 [Source:HGNC Symbol;Acc:HGNC:11858]
    ## 2                                                            tenomodulin [Source:HGNC Symbol;Acc:HGNC:17757]
    ## 3 dolichyl-phosphate mannosyltransferase polypeptide 1, catalytic subunit [Source:HGNC Symbol;Acc:HGNC:3005]
    ## 4                                               SCY1-like, kinase-like 3 [Source:HGNC Symbol;Acc:HGNC:19285]
    ## 5                                    chromosome 1 open reading frame 112 [Source:HGNC Symbol;Acc:HGNC:25565]
    ## 6                          FGR proto-oncogene, Src family tyrosine kinase [Source:HGNC Symbol;Acc:HGNC:3697]

Q6) Merging data.frames:

``` r
annocounts <- merge(x = mycounts, y = anno, by.x = "row.names", by.y = "ensgene")
```

Alternatively, we can use bioconductor annotation. Let's load the packages

``` r
library("AnnotationDbi")
```

    ## Loading required package: stats4

    ## Loading required package: BiocGenerics

    ## Loading required package: parallel

    ## 
    ## Attaching package: 'BiocGenerics'

    ## The following objects are masked from 'package:parallel':
    ## 
    ##     clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
    ##     clusterExport, clusterMap, parApply, parCapply, parLapply,
    ##     parLapplyLB, parRapply, parSapply, parSapplyLB

    ## The following objects are masked from 'package:stats':
    ## 
    ##     IQR, mad, sd, var, xtabs

    ## The following objects are masked from 'package:base':
    ## 
    ##     anyDuplicated, append, as.data.frame, basename, cbind,
    ##     colMeans, colnames, colSums, dirname, do.call, duplicated,
    ##     eval, evalq, Filter, Find, get, grep, grepl, intersect,
    ##     is.unsorted, lapply, lengths, Map, mapply, match, mget, order,
    ##     paste, pmax, pmax.int, pmin, pmin.int, Position, rank, rbind,
    ##     Reduce, rowMeans, rownames, rowSums, sapply, setdiff, sort,
    ##     table, tapply, union, unique, unsplit, which, which.max,
    ##     which.min

    ## Loading required package: Biobase

    ## Welcome to Bioconductor
    ## 
    ##     Vignettes contain introductory material; view with
    ##     'browseVignettes()'. To cite Bioconductor, see
    ##     'citation("Biobase")', and for packages 'citation("pkgname")'.

    ## Loading required package: IRanges

    ## Loading required package: S4Vectors

    ## 
    ## Attaching package: 'S4Vectors'

    ## The following object is masked from 'package:base':
    ## 
    ##     expand.grid

``` r
library("org.Hs.eg.db")
```

    ## 

Let's look at the organism package

``` r
columns(org.Hs.eg.db)
```

    ##  [1] "ACCNUM"       "ALIAS"        "ENSEMBL"      "ENSEMBLPROT" 
    ##  [5] "ENSEMBLTRANS" "ENTREZID"     "ENZYME"       "EVIDENCE"    
    ##  [9] "EVIDENCEALL"  "GENENAME"     "GO"           "GOALL"       
    ## [13] "IPI"          "MAP"          "OMIM"         "ONTOLOGY"    
    ## [17] "ONTOLOGYALL"  "PATH"         "PFAM"         "PMID"        
    ## [21] "PROSITE"      "REFSEQ"       "SYMBOL"       "UCSCKG"      
    ## [25] "UNIGENE"      "UNIPROT"

Let's add some of this to our mycounts data.frame:

``` r
mycounts$symbol <- mapIds(org.Hs.eg.db,
                     keys=row.names(mycounts),
                     column="SYMBOL",
                     keytype="ENSEMBL",
                     multiVals="first")
```

    ## 'select()' returned 1:many mapping between keys and columns

Q7: Adding UniProt and Entrez data:

``` r
#Adding Entrez IDs
mycounts$entrez <- mapIds(org.Hs.eg.db,
                     keys=row.names(mycounts),
                     column="ENTREZID",
                     keytype="ENSEMBL",
                     multiVals="first")
```

    ## 'select()' returned 1:many mapping between keys and columns

``` r
# Adding UniProt
mycounts$uniprot <- mapIds(org.Hs.eg.db,
                     keys=row.names(mycounts),
                     column="UNIPROT",
                     keytype="ENSEMBL",
                     multiVals="first")
```

    ## 'select()' returned 1:many mapping between keys and columns

Q8) Differentially Expressed Genes

``` r
View(mycounts[mycounts$log2fc > 2 | mycounts$log2fc < -2, ])
```

    ## Warning in system2("/usr/bin/otool", c("-L", shQuote(DSO)), stdout = TRUE):
    ## running command ''/usr/bin/otool' -L '/Library/Frameworks/R.framework/
    ## Resources/modules/R_de.so'' had status 1

This seems reasonable to trust, since its the same way of obtaining our data as we did before.

Let's look at upregulated genes again:

``` r
head(mycounts[up.ind,])
```

    ##                 control.mean treated.mean   log2fc  symbol entrez
    ## ENSG00000004799       270.50      1429.25 2.401558    PDK4   5166
    ## ENSG00000006788         2.75        19.75 2.844349   MYH13   8735
    ## ENSG00000008438         0.50         2.75 2.459432 PGLYRP1   8993
    ## ENSG00000011677         0.50         2.25 2.169925  GABRA3   2556
    ## ENSG00000015413         0.50         3.00 2.584963   DPEP1   1800
    ## ENSG00000015592         0.50         2.25 2.169925   STMN4  81551
    ##                    uniprot
    ## ENSG00000004799     A4D1H4
    ## ENSG00000006788     Q9UKX3
    ## ENSG00000008438     O75594
    ## ENSG00000011677     P34903
    ## ENSG00000015413 A0A140VJI3
    ## ENSG00000015592     Q9H169

DESeq2 Analysis
---------------

Let's start off by loading the relevant package into RStudio

``` r
library(DESeq2)
```

    ## Loading required package: GenomicRanges

    ## Loading required package: GenomeInfoDb

    ## Loading required package: SummarizedExperiment

    ## Loading required package: DelayedArray

    ## Loading required package: matrixStats

    ## 
    ## Attaching package: 'matrixStats'

    ## The following objects are masked from 'package:Biobase':
    ## 
    ##     anyMissing, rowMedians

    ## Loading required package: BiocParallel

    ## 
    ## Attaching package: 'DelayedArray'

    ## The following objects are masked from 'package:matrixStats':
    ## 
    ##     colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges

    ## The following objects are masked from 'package:base':
    ## 
    ##     aperm, apply

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

``` r
citation("DESeq2")
```

    ## 
    ##   Love, M.I., Huber, W., Anders, S. Moderated estimation of fold
    ##   change and dispersion for RNA-seq data with DESeq2 Genome
    ##   Biology 15(12):550 (2014)
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2},
    ##     author = {Michael I. Love and Wolfgang Huber and Simon Anders},
    ##     year = {2014},
    ##     journal = {Genome Biology},
    ##     doi = {10.1186/s13059-014-0550-8},
    ##     volume = {15},
    ##     issue = {12},
    ##     pages = {550},
    ##   }

Now let's import our data using 'DESeqDataSetFromMatrix' and specify which parts of the dataset correspond to counts, metadata, and what the treatment column is. This will turn our RNASeq data into something DESeq2 can read:

``` r
dds <- DESeqDataSetFromMatrix(countData=counts, 
                              colData=metadata, 
                              design=~dex,
                              tidy=TRUE)
```

    ## converting counts to integer mode

    ## Warning in DESeqDataSet(se, design = design, ignoreRank): some variables in
    ## design formula are characters, converting to factors

``` r
dds
```

    ## class: DESeqDataSet 
    ## dim: 38694 8 
    ## metadata(1): version
    ## assays(1): counts
    ## rownames(38694): ENSG00000000003 ENSG00000000005 ...
    ##   ENSG00000283120 ENSG00000283123
    ## rowData names(0):
    ## colnames(8): SRR1039508 SRR1039509 ... SRR1039520 SRR1039521
    ## colData names(4): id dex celltype geo_id

Now let's start putting this data into the DESeq2 pipeline.

First let' try examining the objects we obtain before the data is in the pipeline:

``` r
sizeFactors(dds)
```

    ## NULL

``` r
dispersions(dds)
```

    ## NULL

``` r
# results(dds)
```

Notice that there is no output generated, since the objects don't exist. Running 'results' on an empty dataset generates an error.

What we need to do is actually run our DESeq analysis. We'll do this using 'DESeq', passing dds as an argument and assigning it back to dds

``` r
dds <- DESeq(dds)
```

    ## estimating size factors

    ## estimating dispersions

    ## gene-wise dispersion estimates

    ## mean-dispersion relationship

    ## final dispersion estimates

    ## fitting model and testing

Now let's look at our DESeq results:

``` r
res <- results(dds)

res
```

    ## log2 fold change (MLE): dex treated vs control 
    ## Wald test p-value: dex treated vs control 
    ## DataFrame with 38694 rows and 6 columns
    ##                          baseMean     log2FoldChange             lfcSE
    ##                         <numeric>          <numeric>         <numeric>
    ## ENSG00000000003  747.194195359907  -0.35070302068658 0.168245681332529
    ## ENSG00000000005                 0                 NA                NA
    ## ENSG00000000419  520.134160051965  0.206107766417862 0.101059218008052
    ## ENSG00000000457  322.664843927049 0.0245269479387466 0.145145067649248
    ## ENSG00000000460   87.682625164828  -0.14714204922212 0.257007253994673
    ## ...                           ...                ...               ...
    ## ENSG00000283115                 0                 NA                NA
    ## ENSG00000283116                 0                 NA                NA
    ## ENSG00000283119                 0                 NA                NA
    ## ENSG00000283120 0.974916032393564  -0.66825846051647  1.69456285241871
    ## ENSG00000283123                 0                 NA                NA
    ##                               stat             pvalue              padj
    ##                          <numeric>          <numeric>         <numeric>
    ## ENSG00000000003  -2.08446967499531 0.0371174658432818 0.163034808641677
    ## ENSG00000000005                 NA                 NA                NA
    ## ENSG00000000419   2.03947517584631 0.0414026263001157 0.176031664879167
    ## ENSG00000000457  0.168982303952742  0.865810560623564 0.961694238404392
    ## ENSG00000000460  -0.57252099672319  0.566969065257939 0.815848587637731
    ## ...                            ...                ...               ...
    ## ENSG00000283115                 NA                 NA                NA
    ## ENSG00000283116                 NA                 NA                NA
    ## ENSG00000283119                 NA                 NA                NA
    ## ENSG00000283120 -0.394354484734893  0.693319342566817                NA
    ## ENSG00000283123                 NA                 NA                NA

Let's summarize these results:

``` r
summary(res)
```

    ## 
    ## out of 25258 with nonzero total read count
    ## adjusted p-value < 0.1
    ## LFC > 0 (up)       : 1563, 6.2%
    ## LFC < 0 (down)     : 1188, 4.7%
    ## outliers [1]       : 142, 0.56%
    ## low counts [2]     : 9971, 39%
    ## (mean count < 10)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

Let's order our data by the smallest p-values:

``` r
resOrdered <- res[order(res$pvalue),]
```

Now let's compute our results with an alpha of 0.05:

``` r
res05 <- results(dds, alpha = 0.05)
summary(res05)
```

    ## 
    ## out of 25258 with nonzero total read count
    ## adjusted p-value < 0.05
    ## LFC > 0 (up)       : 1236, 4.9%
    ## LFC < 0 (down)     : 933, 3.7%
    ## outliers [1]       : 142, 0.56%
    ## low counts [2]     : 9033, 36%
    ## (mean count < 6)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

``` r
resSig05 <- subset(as.data.frame(res), padj < 0.05)
nrow(resSig05)
```

    ## [1] 2181

Q9) At 0.05, we have 2181 significant results. How many results are significant at an alpha of 0.01? At alpha = 0.01, we only have 1437 significant results.

``` r
res01 <- results(dds, alpha = 0.01)
summary(res01)
```

    ## 
    ## out of 25258 with nonzero total read count
    ## adjusted p-value < 0.01
    ## LFC > 0 (up)       : 850, 3.4%
    ## LFC < 0 (down)     : 581, 2.3%
    ## outliers [1]       : 142, 0.56%
    ## low counts [2]     : 9033, 36%
    ## (mean count < 6)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

``` r
resSig01 <- subset(as.data.frame(res), padj <0.01)
nrow(resSig01)
```

    ## [1] 1437

Q10) Now let's annotate res01:

``` r
# Adding protein/gene name symbols
res01$symbol <- mapIds(org.Hs.eg.db,
                     keys=row.names(res01),
                     column="SYMBOL",
                     keytype="ENSEMBL",
                     multiVals="first")
```

    ## 'select()' returned 1:many mapping between keys and columns

``` r
# Adding Entrez IDs
res01$entrez <- mapIds(org.Hs.eg.db,
                     keys=row.names(res01),
                     column="UNIPROT",
                     keytype="ENSEMBL",
                     multiVals="first")
```

    ## 'select()' returned 1:many mapping between keys and columns

``` r
# Adding UniProt
res01$uniprot <- mapIds(org.Hs.eg.db,
                     keys=row.names(res01),
                     column="UNIPROT",
                     keytype="ENSEMBL",
                     multiVals="first")
```

    ## 'select()' returned 1:many mapping between keys and columns

``` r
head(res01)
```

    ## log2 fold change (MLE): dex treated vs control 
    ## Wald test p-value: dex treated vs control 
    ## DataFrame with 6 rows and 9 columns
    ##                          baseMean     log2FoldChange             lfcSE
    ##                         <numeric>          <numeric>         <numeric>
    ## ENSG00000000003  747.194195359907  -0.35070302068658 0.168245681332529
    ## ENSG00000000005                 0                 NA                NA
    ## ENSG00000000419  520.134160051965  0.206107766417862 0.101059218008052
    ## ENSG00000000457  322.664843927049 0.0245269479387466 0.145145067649248
    ## ENSG00000000460   87.682625164828  -0.14714204922212 0.257007253994673
    ## ENSG00000000938 0.319166568913118  -1.73228897394308  3.49360097648095
    ##                               stat             pvalue              padj
    ##                          <numeric>          <numeric>         <numeric>
    ## ENSG00000000003  -2.08446967499531 0.0371174658432818  0.17154028826365
    ## ENSG00000000005                 NA                 NA                NA
    ## ENSG00000000419   2.03947517584631 0.0414026263001157 0.185218652390162
    ## ENSG00000000457  0.168982303952742  0.865810560623564 0.965995924142129
    ## ENSG00000000460  -0.57252099672319  0.566969065257939 0.830033829269779
    ## ENSG00000000938 -0.495846258804286  0.620002884826012                NA
    ##                      symbol      entrez     uniprot
    ##                 <character> <character> <character>
    ## ENSG00000000003      TSPAN6  A0A024RCI0  A0A024RCI0
    ## ENSG00000000005        TNMD      Q9H2S6      Q9H2S6
    ## ENSG00000000419        DPM1      O60762      O60762
    ## ENSG00000000457       SCYL3      Q8IZE3      Q8IZE3
    ## ENSG00000000460    C1orf112  A0A024R922  A0A024R922
    ## ENSG00000000938         FGR      P09769      P09769

``` r
resSig01 <- subset(as.data.frame(res), padj <0.01)
```

Now lets arrange and view the results by adjusted p-value:

``` r
ord <- order( resSig01$padj )
# View(res01[ord,]) # Remove comment pound to run View function
head(resSig01[ord,])
```

    ##                   baseMean log2FoldChange      lfcSE      stat
    ## ENSG00000152583   954.7709       4.368359 0.23712679  18.42204
    ## ENSG00000179094   743.2527       2.863889 0.17556931  16.31201
    ## ENSG00000116584  2277.9135      -1.034701 0.06509844 -15.89440
    ## ENSG00000189221  2383.7537       3.341544 0.21240579  15.73189
    ## ENSG00000120129  3440.7038       2.965211 0.20369513  14.55710
    ## ENSG00000148175 13493.9204       1.427168 0.10038904  14.21638
    ##                       pvalue         padj
    ## ENSG00000152583 8.744898e-76 1.324415e-71
    ## ENSG00000179094 8.107836e-60 6.139658e-56
    ## ENSG00000116584 6.928546e-57 3.497761e-53
    ## ENSG00000189221 9.144326e-56 3.462270e-52
    ## ENSG00000120129 5.264243e-48 1.594539e-44
    ## ENSG00000148175 7.251278e-46 1.830344e-42

Let's write these annotated, ordered, significant results to a .csv file:

``` r
write.csv(resSig01[ord,], "signif01_results.csv")
```

Data Visualization
------------------

Let's start off by looking at the gene 'CRISPLD2'. Let's extract that row using grep() for that keyword in the "symbol" column:

``` r
i <- grep("CRISPLD2", resSig01$symbol)
i
```

    ## integer(0)

``` r
resSig01[i, ]
```

    ## [1] baseMean       log2FoldChange lfcSE          stat          
    ## [5] pvalue         padj          
    ## <0 rows> (or 0-length row.names)

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
    ## [1] parallel  stats4    stats     graphics  grDevices utils     datasets 
    ## [8] methods   base     
    ## 
    ## other attached packages:
    ##  [1] DESeq2_1.22.2               SummarizedExperiment_1.12.0
    ##  [3] DelayedArray_0.8.0          BiocParallel_1.16.6        
    ##  [5] matrixStats_0.54.0          GenomicRanges_1.34.0       
    ##  [7] GenomeInfoDb_1.18.2         org.Hs.eg.db_3.7.0         
    ##  [9] AnnotationDbi_1.44.0        IRanges_2.16.0             
    ## [11] S4Vectors_0.20.1            Biobase_2.42.0             
    ## [13] BiocGenerics_0.28.0        
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] bit64_0.9-7            splines_3.6.0          Formula_1.2-3         
    ##  [4] assertthat_0.2.1       latticeExtra_0.6-28    blob_1.1.1            
    ##  [7] GenomeInfoDbData_1.2.0 yaml_2.2.0             pillar_1.3.1          
    ## [10] RSQLite_2.1.1          backports_1.1.4        lattice_0.20-38       
    ## [13] glue_1.3.1             digest_0.6.18          RColorBrewer_1.1-2    
    ## [16] XVector_0.22.0         checkmate_1.9.3        colorspace_1.4-1      
    ## [19] htmltools_0.3.6        Matrix_1.2-17          plyr_1.8.4            
    ## [22] XML_3.98-1.19          pkgconfig_2.0.2        genefilter_1.64.0     
    ## [25] zlibbioc_1.28.0        xtable_1.8-4           purrr_0.3.2           
    ## [28] scales_1.0.0           annotate_1.60.1        tibble_2.1.1          
    ## [31] htmlTable_1.13.1       ggplot2_3.1.1          nnet_7.3-12           
    ## [34] lazyeval_0.2.2         survival_2.44-1.1      magrittr_1.5          
    ## [37] crayon_1.3.4           memoise_1.1.0          evaluate_0.14         
    ## [40] foreign_0.8-71         tools_3.6.0            data.table_1.12.2     
    ## [43] stringr_1.4.0          locfit_1.5-9.1         munsell_0.5.0         
    ## [46] cluster_2.0.8          compiler_3.6.0         rlang_0.3.4           
    ## [49] grid_3.6.0             RCurl_1.95-4.12        rstudioapi_0.10       
    ## [52] htmlwidgets_1.3        bitops_1.0-6           base64enc_0.1-3       
    ## [55] rmarkdown_1.12         gtable_0.3.0           DBI_1.0.0             
    ## [58] R6_2.4.0               gridExtra_2.3          knitr_1.23            
    ## [61] dplyr_0.8.0.1          bit_1.1-14             Hmisc_4.2-0           
    ## [64] stringi_1.4.3          Rcpp_1.0.1             geneplotter_1.60.0    
    ## [67] rpart_4.1-15           acepack_1.4.1          tidyselect_0.2.5      
    ## [70] xfun_0.7
