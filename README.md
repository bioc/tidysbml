# tidysbml

The package provides conversion from a SBML document to R dataframes. SBML components (i.e. listOfCompartments, listOfSpecies, listOfReactions) are translated into dataframes to access information and enable handily manipulations and subsequent analysis. For instance, these dataframes can be exploited for network's analysis within R, where networks can be easily build starting from the output tabular data, which describe the biological pathway represented by the SBML selected. 

# Installation 

To install the package from Bioconductor (when submission will be concluded), run 
```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("tidysbml")
```

To install the package from Github, run 

```
#install.packages("devtools")

library(devtools)
install_github("veronicapaparozzi/tidysbml", build_vignettes = TRUE)
```

## Contact 

For bug reports, please register an [issue](https://github.com/veronicapaparozzi/tidysbml/issues) here on Github page.
