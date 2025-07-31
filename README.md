![biomer logo](https://raw.githubusercontent.com/azizka/biomer/main/inst/extdata/biomer_logo.png)


# biomes
Distribute spatially explicit biome definitions and link species occurrences with biomes

# How to use
`biomer` contains global 10 x10 km rasters of the biome definitions digitized by Fischer et al (2022) plus some additional convenience function to classify records and species into biomes.

## 📦 Installation

To install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("azizka/biomer")
``` 

# Reference
Fischer J-C, Waltenowitz A, Beierkuhnlein C (2022) The biome inventory - Standardizing global biogeographical units. Global Ecology and Biogeography31(11):2172-2183. \url{https://doi.org/10.1111/geb.13574}

# Citation
citation("biomer")

# Remaining questions to discuss
 - add consensus biome definition
 - possible to make a selection function based on a trait matrix, to help people select the most suitable biome definition for their purpose
 
