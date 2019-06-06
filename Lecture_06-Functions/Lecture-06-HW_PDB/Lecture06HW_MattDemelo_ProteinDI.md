Function and Optimization Practice
----------------------------------

### Below is a function optimizing the operations performed in Q6 of the Lecture 06 hands-on activity:

``` r
# pdbdrug Function

# Generalized function for plotting B-factor of amino acid residues in specific protein structures, utilizing the bio3d package. This function takes a PDB number of a protein structure and uses that protein structure's structural data to generate a plot. Additional arguments are listed to specify chain, elety, and the title of the resultant plot. 

# To use this function, simply use an existing PDB number as a string argument. Other arguments can be left default or specified as desired. 

# The output of this function is two strings which verify 1) The PDB number is valid and its data can be extracted, and 2) That the data.frame of structural data has been trimmed to the region of interest, and that the B-factor data has been indexed properly; most importantly, a line plot of the B-factor vs. Residue in the protein structure.

# e.g. pdbdrug("4BAE", plottitle = "Test Run")
  
# ARGUMENTS
  # pdnum: PDB number of a given protein structure, a string input.
  # chn = The chain we would like to study, a character input.
  # elt = The elety we are interested in, a string input.
  # plottitle = The title of the B-factor vs. Residue Plot, a string input.

pdbdrug <- function(pdbnum, chn = "A", elt = "CA", plottitle = "B-factor vs. Residue Plot"){
  
  library("bio3d") 
  # Activates bio3d package
  
  pdbstruc= read.pdb(pdbnum)
  # Reads PDB number input and constructs a dataframe of structural data. 
  # See the read.pdb function documentation for more information.
  
  print("Valid PDB#. PDB Structure data imported.") 
  # Verifies that the PDB number is valid and import was successful. 
  
  trimstruc <- trim.pdb(pdbstruc, chain = chn, elety = elt) 
  # "Trims" the structural data data.frame to a region of interest
  
  bfac <- trimstruc$atom$b 
  # Indexes into atom matrix and indexes the bfactor column (b)
  
  print("Structure data trimmed, B-factor data indexed. Generating plot...") 
  # Checks that trimstruc and bfac have been constructed properly/without error.
  
  plotb3(bfac, sse = trimstruc, typ = "l", main = plottitle, ylab = "Bfactor") 
  # Plots the B-factor of a given residue for each residue in the trimmed structure.
  
}
```

Running the code, we see that the arguments we need to give are far less complicated. Further, due to the way the function is structured, different arguments can be given for chain and elety, depending on our interests.

``` r
pdbdrug("4AKE", plottitle = "Kinase w/ Drug (4AKE)")
```

    ##   Note: Accessing on-line PDB file
    ## [1] "Valid PDB#. PDB Structure data imported."
    ## [1] "Structure data trimmed, B-factor data indexed. Generating plot..."

![](Lecture06HW_MattDemelo_ProteinDI_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
pdbdrug("1AKE", plottitle = "Kinase w/ out Drug (1AKE)")
```

    ##   Note: Accessing on-line PDB file
    ##    PDB has ALT records, taking A only, rm.alt=TRUE
    ## [1] "Valid PDB#. PDB Structure data imported."
    ## [1] "Structure data trimmed, B-factor data indexed. Generating plot..."

![](Lecture06HW_MattDemelo_ProteinDI_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
pdbdrug("1E4Y", plottitle = "Kinase w/ Drug (1E4Y)")
```

    ##   Note: Accessing on-line PDB file
    ## [1] "Valid PDB#. PDB Structure data imported."
    ## [1] "Structure data trimmed, B-factor data indexed. Generating plot..."

![](Lecture06HW_MattDemelo_ProteinDI_files/figure-markdown_github/unnamed-chunk-2-3.png)

A unique plot is generated for each PDB\# input, with our custom plot title. Doing this, we've made this process reproducible and cut down on the opportunity for error from "copy and paste" coding of repetitive operations. This also makes it much clearer what the operation is actually doing, with proper comments and variable names.

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
    ## [1] bio3d_2.3-4
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_3.6.0  magrittr_1.5    parallel_3.6.0  tools_3.6.0    
    ##  [5] htmltools_0.3.6 yaml_2.2.0      Rcpp_1.0.1      stringi_1.4.3  
    ##  [9] rmarkdown_1.12  grid_3.6.0      knitr_1.23      stringr_1.4.0  
    ## [13] xfun_0.7        digest_0.6.18   evaluate_0.14
