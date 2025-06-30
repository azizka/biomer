This data set contains a RasterStack object with 31 layers of standardized global biome and land-cover classifications (Biome_Inventory_RasterStack.tif).
Legends of all maps are provided in a text file (Biome_Inventory_Legends.txt) in the same order like the layers in the RasterStack object.

### Biome Inventory R-Code
### Fischer, J. C., Walentowitz, A. & Beierkuhnlein, C.

### This R-code can be used to open and visualize the RasterStack of the biome inventory from file "Biome_Inventory_RasterStack.tif" with associated legend data from file "Biome_Inventory_Legends.txt"

# required packages
library(maptools)
library(rgdal)
library(viridis)
library(raster)
library(terra) 
library(stringr)

# Open and visualize the final RasterStack of our biome inventory
# from file "Biome_Inventory_RasterStack.tif"
# with associated legend data from file "Biome_Inventory_Legends.txt"

# load raster stack
Biome_Inventory_RasterStack <- stack("Biome_Inventory_RasterStack.tif")
# load legends text file
Biome_Inventory_Legends <- read.delim("Biome_Inventory_Legends.txt")

# write png-files of all classifications including legends
# loop over all layers in raster stack
for(i in 1:nlayers(Biome_Inventory_RasterStack)){
  # address individual raster
  Input_ras <- Biome_Inventory_RasterStack[[i]]
  # extract name
  Input_name <- names(Input_ras)
  
  ### change raster for plotting
  # identify azonal biomes
  ras_Inland_water    <- Input_ras==95
  ras_Oceanic_islands <- Input_ras==96
  ras_Mountains       <- Input_ras==97
  ras_Urban           <- Input_ras==98
  ras_Inland_water   [ras_Inland_water   ==0] <- NA
  ras_Oceanic_islands[ras_Oceanic_islands==0] <- NA
  ras_Mountains      [ras_Mountains      ==0] <- NA
  ras_Urban          [ras_Urban          ==0] <- NA
  
  # generate color palette for azonal biomes
  colors_azonal <- c()
  if(!is.na(minValue(ras_Inland_water   ))){colors_azonal <- append(colors_azonal, "deepskyblue")}
  if(!is.na(minValue(ras_Oceanic_islands))){colors_azonal <- append(colors_azonal, "gray50"     )}
  if(!is.na(minValue(ras_Mountains      ))){colors_azonal <- append(colors_azonal, "brown"      )}
  if(!is.na(minValue(ras_Urban          ))){colors_azonal <- append(colors_azonal, "darkorange" )}
  
  # separate zonal biomes
  Input_ras[Input_ras >= 95] <- NA
  
  # set path for png outputs
  path_png <- "~"
  
  ### arrange colors
  # zonal biomes
  colors <- rev(viridis(n=length(unique(na.omit(Input_ras)))))
  # combine colors of zonal and azonal biomes
  colors_legend <- c(colors, colors_azonal)
  
  ### plot map
  png(filename=file.path(path_png, paste0(Input_name, "_map.png")), width=2000, height=2000)
  par(mar=c(0, 0, 0, 0), xpd=T, bty="n")
  # zonal biomes
  plot(Input_ras, col=colors, xaxt="n", yaxt="n", bty="n", legend=F)
  # azonal biomes
  par(mar=c(0, 0, 0, 0), xpd=T, bty="n", new=T)
  plot(ras_Mountains      , legend=F, axes=F, bty="n", col="brown"      )
  par(mar=c(0, 0, 0, 0), xpd=T, bty="n", new=T)
  plot(ras_Inland_water   , legend=F, axes=F, bty="n", col="deepskyblue")
  par(mar=c(0, 0, 0, 0), xpd=T, bty="n", new=T)
  plot(ras_Oceanic_islands, legend=F, axes=F, bty="n", col="gray50"     )
  par(mar=c(0, 0, 0, 0), xpd=T, bty="n", new=T)
  plot(ras_Urban          , legend=F, axes=F, bty="n", col="darkorange" )
  dev.off()
  
  ### prepare legend
  # select legend
  Input_legend <- Biome_Inventory_Legends[i,]
  # remove layer number and source info (from the beginning of the string)
  Input_legend <- gsub(".*:", "", Biome_Inventory_Legends[i,])
  # split string by ";"
  Input_legend_split <- strsplit(Input_legend, ";")
  ### loop over all legend items
  for(j in 1:length(Input_legend_split[[1]])){
    # check
    Input_legend_split[[1]][j]
    # exclude all before ","
    Input_legend_split[[1]][j] <- gsub(".*,", "", Input_legend_split[[1]][j])
    # exclude space at the beginning
    Input_legend_split[[1]][j] <- str_sub(Input_legend_split[[1]][j], 2, nchar(Input_legend_split[[1]][j]))
  } ### end loop over all legend items
  
  ### plot legend
  png(filename=file.path(path_png, paste0(Input_name, "_legend.png")), width=1000, height=1000)
  par(mar=c(0, 0, 0, 0), xpd=T, bty="n")
  plot(NULL, xaxt="n", yaxt="n", bty="n", ylab="", xlab="", xlim=0:1, ylim=0:1)
  legend("topleft", legend=Input_legend_split[[1]], fill=colors_legend, cex=1.5, bty="n", xpd=T)
  dev.off()
  
  print(paste0("finished ", Input_name, " which is layer ", i, " out of ", nlayers(Biome_Inventory_RasterStack), " layers in total"))
} # end loop over all layers in raster stack
