Setup for Docking
-----------------

We will first repare our HIV-Pr system for drug docking by creating a protein only PDB format file (i.e. we will remove water, existing ligands, etc.)

``` r
# First we load in the bio3d package and use it to import the relevant structural
# data.
library(bio3d)
file <- get.pdb("1hsg")
```

    ## Warning in get.pdb("1hsg"): ./1hsg.pdb exists. Skipping download

``` r
# Now we read the .pdb file into a Data.Frame of structural data.

pdb <- read.pdb(file)
pdb
```

    ## 
    ##  Call:  read.pdb(file = file)
    ## 
    ##    Total Models#: 1
    ##      Total Atoms#: 1686,  XYZs#: 5058  Chains#: 2  (values: A B)
    ## 
    ##      Protein Atoms#: 1514  (residues/Calpha atoms#: 198)
    ##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
    ## 
    ##      Non-protein/nucleic Atoms#: 172  (residues: 128)
    ##      Non-protein/nucleic resid values: [ HOH (127), MK1 (1) ]
    ## 
    ##    Protein sequence:
    ##       PQITLWQRPLVTIKIGGQLKEALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYD
    ##       QILIEICGHKAIGTVLVGPTPVNIIGRNLLTQIGCTLNFPQITLWQRPLVTIKIGGQLKE
    ##       ALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYDQILIEICGHKAIGTVLVGPTP
    ##       VNIIGRNLLTQIGCTLNF
    ## 
    ## + attr: atom, xyz, seqres, helix, sheet,
    ##         calpha, remark, call

``` r
prot <- atom.select(pdb, "protein", value = T)
write.pdb(prot, file = "1hsg_protein.pdb")
prot
```

    ## 
    ##  Call:  trim.pdb(pdb = pdb, sele)
    ## 
    ##    Total Models#: 1
    ##      Total Atoms#: 1514,  XYZs#: 4542  Chains#: 2  (values: A B)
    ## 
    ##      Protein Atoms#: 1514  (residues/Calpha atoms#: 198)
    ##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
    ## 
    ##      Non-protein/nucleic Atoms#: 0  (residues: 0)
    ##      Non-protein/nucleic resid values: [ none ]
    ## 
    ##    Protein sequence:
    ##       PQITLWQRPLVTIKIGGQLKEALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYD
    ##       QILIEICGHKAIGTVLVGPTPVNIIGRNLLTQIGCTLNFPQITLWQRPLVTIKIGGQLKE
    ##       ALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYDQILIEICGHKAIGTVLVGPTP
    ##       VNIIGRNLLTQIGCTLNF
    ## 
    ## + attr: atom, helix, sheet, seqres, xyz,
    ##         calpha, call

``` r
lig <- atom.select(pdb, "ligand", value = T)
write.pdb(lig, file = "1hsg_ligand.pdb")
lig
```

    ## 
    ##  Call:  trim.pdb(pdb = pdb, sele)
    ## 
    ##    Total Models#: 1
    ##      Total Atoms#: 45,  XYZs#: 135  Chains#: 1  (values: B)
    ## 
    ##      Protein Atoms#: 0  (residues/Calpha atoms#: 0)
    ##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
    ## 
    ##      Non-protein/nucleic Atoms#: 45  (residues: 1)
    ##      Non-protein/nucleic resid values: [ MK1 (1) ]
    ## 
    ## + attr: atom, helix, sheet, seqres, xyz,
    ##         calpha, call

Q1: HOH and MK1, they can be seen in the output of these files.

Q2: Yes, it is a pocket in the center of the structure. It might not be necessary to visualize hydrogen atoms in a structure, since it can be assumed that they are there. (Or perhaps they cannot be visualized in a crystallograph, and must be added in after)

Q3: These charges makes sense, since amino acids are typically partially charged, and many of the atoms carry a charge close to 0.5 +/-.

Observing Results with VMD
--------------------------

Now we prepare our results from our autodock analysis

``` r
library(bio3d)

res <- read.pdb("all.pdbqt", multi=TRUE) # Data.frame of vina-derived data.
write.pdb(res, "results.pdb") # Writes results into new pdb file.
```

Q4: The docking appears to fit well with the model of docking scene in the crystal structure, as all of the autodock produced docks are within the same region of the protein. The positioning of the ligand is different however, and only a few of the docks fit with the same exact positioning and orientation as the crystal dock.

Now we run a quantitative analysis of our docking data:

``` r
ori <- read.pdb("ligand.pdbqt")
rmsd(ori, res) # root means squared differences of crystal docking vs. autdock.
```

    ##  [1]  0.590 11.163 10.531  4.364 11.040  3.682  5.741  3.864  5.442 10.920
    ## [11]  4.318  6.249 11.084  8.929

Q5) It is obvious now that the ligand binding is best in the 1st autodock model, since it has a very low RMSD relative to the others. The only autodock dock to produce a &lt;= 1 Angstrom RMSD from the crystal mode is the first. All others have larger RMSD's.

Q6) Using a conditional statement where we select for atoms that are not hydrogen e.g. atom.select()

NMA for Flexibility Prediction
------------------------------

Now we look at more complex situation using NMA.

``` r
pdb <- read.pdb("1hel")
```

    ##   Note: Accessing on-line PDB file

``` r
modes <- nma( pdb )
```

    ##  Building Hessian...     Done in 0.021 seconds.
    ##  Diagonalizing Hessian...    Done in 0.149 seconds.

``` r
m7 <- mktrj(modes, mode=7, file="mode_7.pdb")
```

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
