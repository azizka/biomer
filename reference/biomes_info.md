# Print metadata for selected biome definitions

Prints a human-readable summary of the biome classifications shipped
with the package. For each requested classification the function prints
the publication, the criteria and methodology used to define the
classes, a short description, the number of biome classes, the biome
scheme number, and a list of biome-class names with their raster values.

## Usage

``` r
biomes_info(x = NULL)
```

## Arguments

- x:

  Integer vector of biome scheme numbers between 1 and 31. If `NULL`
  (the default), information for all 31 schemes is printed.

## Value

Invisibly returns the integer vector of biome scheme numbers that was
printed. The function is called for its side effect of printing to the
console.

## Details

This is the *interactive* sibling of the `biomes_information` data set:
use
[`biomes_information`](https://azizka.github.io/biomes/reference/biomes_information.md)
when you want the raw metadata table (e.g. to subset, filter, or join
programmatically), and `biomes_info()` when you want a quick read of the
most relevant fields for a specific biome scheme.

## See also

[`biomes_information`](https://azizka.github.io/biomes/reference/biomes_information.md)
for the underlying metadata table and
[`biomes_legend`](https://azizka.github.io/biomes/reference/biomes_legend.md)
for the mapping from raster values to biome names.

## Examples

``` r
# Print information for all biome definitions
biomes_info()
#> 
#> Name: Global vegetation patterns of the past 140,000 years (Allen et al., 2020)
#> 
#> Biome scheme number: 1
#> 
#> Criteria: Carbon mass, LAI, and plant functional types
#> 
#> Methodology: Modelling with the global dynamic vegetation Lund-Potsdam-Jena General Ecosystem Simulator
#> 
#> Description: Global biomes were simulated over the past 140,000 years. Input factors to the dynamic global vegetation model included reconstructed atmospheric CO2 concentrations, Earth's obliquity and paleo- as well as pre-industrial climate simulations by HadCM3. Biomes were assigned according to specified ranges of vegetation carbon mass and leaf area index (LAI) of functional plant types based on consistent rules.
#> 
#> Number of biome classes: 21 (21/0)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical evergreen forest
#>      2: Tropical raingreen forest
#>      3: Savanna
#>      4: Tropical grassland
#>      5: Warm temperate woodland
#>      6: Desert
#>      7: Temperate broadleaf evergreen forest
#>      8: Semidesert
#>      9: Temperate shrubland
#>     10: Temperate needleleaf evergreen forest
#>     11: Steppe
#>     12: Temperate parkland
#>     13: Temperate summergreen forest
#>     14: Temperate mixed forest
#>     15: Boreal parkland
#>     16: Tundra
#>     17: Boreal summergreen broadleaf forest
#>     18: Boreal evergreen needleleaf forest
#>     19: Boreal summergreen needleleaf forest
#>     20: Shrub tundra
#>     21: Boreal woodland
#> 
#> -----
#> 
#> Name: Dataset of the global component of the Copernicus Land Monitoring Service (Buchhorn et al., 2019)
#> 
#> Biome scheme number: 2
#> 
#> Criteria: Multi-spectral Earth surface reflectance on top of canopies
#> 
#> Methodology: Supervised classification of satellite imagery data based on external reference data sets and expert opinion
#> 
#> Description: Copernicus Global Land Service (CGLS) provide an annual dynamic product on global land cover at 100 m spatial resolution derived from classification of daily-synthesis surface reflectance from the PROBA-V sensor.
#> 
#> Number of biome classes: 20 (18/2)
#> 
#> Biome classes (raster value: name):
#>      1: Closed forest (evergreen broadleaf)
#>      2: Open forest (deciduous broadleaf)
#>      3: Open forest (unknown)
#>      4: Shrubs
#>      5: Bare soil/sparse vegetation
#>      6: Cultivated and managed vegetation/agriculture (cropland)
#>      7: Closed forest (deciduous broadleaf)
#>      8: Closed forest (unknown)
#>      9: Open forest (evergreen broadleaf)
#>     10: Herbaceous vegetation
#>     11: Open forest (evergreen needleleaf)
#>     12: Closed forest (mixed)
#>     13: Closed forest (evergreen needleleaf)
#>     14: Herbaceous wetland
#>     15: Closed forest (deciduous needleleaf)
#>     16: Open forest (deciduous needleleaf)
#>     17: Snow and ice
#>     18: Moss and lichen
#>     19: Inland water
#>     20: Urban
#> 
#> -----
#> 
#> Name: Present and future Köppen-Geiger climate classification maps at 1-km resolution (Beck et al., 2018)
#> 
#> Biome scheme number: 3
#> 
#> Criteria: Climate (temperature and precipitation)
#> 
#> Methodology: Application of a slightly adjusted Köppen-Geiger classification based on climatological thresholds following Peel et al. (2007)
#> 
#> Description: This present-day (1980–2016) Köppen-Geiger climate product at 1 km spatial resolution is based on ensemble data of multiple global climatic maps. The classification follows predefined temperature and precipitation thresholds as well as seasonality.
#> 
#> Number of biome classes: 30 (30/0)
#> 
#> Biome classes (raster value: name):
#>      1: Af - Tropical rainforest
#>      2: Am - Tropical monsoon
#>      3: Aw - Tropical savanna
#>      4: Cwc - Temperate dry winter cold summer
#>      5: BSh - Arid steppe hot
#>      6: Cwb - Temperate dry winter warm summer
#>      7: Cwa - Temperate dry winter hot summer
#>      8: BWh - Arid desert hot
#>      9: Cfa - Temperate no dry season hot summer
#>     10: Csb - Temperate dry summer warm summer
#>     11: Dsa - Cold dry summer hot summer
#>     12: Csc - Temperate dry summer cold summer
#>     13: Cfb - Temperate no dry season warm summer
#>     14: Csa - Temperate dry summer hot summer
#>     15: BWk - Arid desert cold
#>     16: Dsb - Cold dry summer warm summer
#>     17: BSk - Arid steppe cold
#>     18: Dwa - Cold dry winter hot summer
#>     19: Dfa - Cold no dry season hot summer
#>     20: Dwb - Cold dry winter warm summer
#>     21: Cfc - Temperate no dry season cold summer
#>     22: Dfb - Cold no dry season warm summer
#>     23: Dwc - Cold dry winter cold summer
#>     24: ET - Polar tundra
#>     25: Dfc - Cold no dry season cold summer
#>     26: Dsc - Cold dry summer cold summer
#>     27: Dwd - Cold dry winter very cold winter
#>     28: Dfd - Cold no dry season very cold winter
#>     29: Dsd - Cold dry summer very cold winter
#>     30: EF - Polar frost
#> 
#> -----
#> 
#> Name: Global mapping of potential natural vegetation: an assessment of machine learning algorithms for estimating land potential (Hengl et al., 2018)
#> 
#> Biome scheme number: 4
#> 
#> Criteria: Potential natural vegetation
#> 
#> Methodology: Modelling based on several machine learning techniques including neural networks, random forest, gradient boosting, K-nearest neighbour, and Cubist
#> 
#> Description: Biome mapping based on modern pollen reconstructions from the BIOME 6000 data set with 160 explanatory parameters including biophysical, atmospheric, climatic, topographic and lithologic geospatial characteristics.
#> 
#> Number of biome classes: 20 (20/0)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical evergreen broadleaf forest
#>      2: Tropical semi-evergreen broadleaf forest
#>      3: Tropical savanna
#>      4: Tropical deciduous broadleaf forest and woodland
#>      5: Xerophytic woodland scrub
#>      6: Warm temperate evergreen and mixed forest
#>      7: Steppe
#>      8: Temperate sclerophyll woodland and shrubland
#>      9: Desert
#>     10: Temperate evergreen needleleaf open woodland
#>     11: Temperate deciduous broadleaf forest
#>     12: Cool temperate rainforest
#>     13: Cool mixed forest
#>     14: Graminoid and forb tundra
#>     15: Cold evergreen needleleaf forest
#>     16: Cool evergreen needleleaf forest
#>     17: Cold deciduous forest
#>     18: Low and high shrub tundra
#>     19: Erect dwarf shrub tundra
#>     20: Prostrate dwarf shrub tundra
#> 
#> -----
#> 
#> Name: An ecoregion-based approach to protecting half the terrestrial realm (Dinerstein et al., 2017)
#> 
#> Biome scheme number: 5
#> 
#> Criteria: Biogeographic zonation and species distribution
#> 
#> Methodology: Revision of the terrestrial ecoregions of the world by Olson et al. (2001) based on technical advances and expert knowledge
#> 
#> Description: A total of 846 global ecoregions were nested in 14 terrestrial biomes.
#> 
#> Number of biome classes: 14 (14/0)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical and subtropical moist broadleaf forest
#>      2: Mangrove
#>      3: Tropical and subtropical grassland savanna and shrubland
#>      4: Tropical and subtropical dry broadleaf forest
#>      5: Flooded grassland and savanna
#>      6: Tropical and subtropical coniferous forest
#>      7: Deserts and xeric shrubland
#>      8: Montane grassland and shrubland
#>      9: Mediterranean forest woodland and scrub 
#>     10: Temperate grassland savanna and shrubland
#>     11: Temperate broadleaf and mixed forest
#>     12: Temperate conifer forest
#>     13: Boreal forest/taiga
#>     14: Tundra
#>     15: Rock and ice
#> 
#> -----
#> 
#> Name: A global classification of vegetation based on NDVI, rainfall and temperature (Zhang et al., 2017)
#> 
#> Biome scheme number: 6
#> 
#> Criteria: Climate (temperature and precipitation) and vegetation (NDVI)
#> 
#> Methodology: Non-hierarchical data clustering based on K-means distances in 14 classes, validation via Kappa statistics
#> 
#> Description: Reconstruction of global climatic vegetation types by K-means partitioning based on global monthly mean temperature and precipitation data interpolated from observation stations and monthly mean NDVI from the Global Inventory Modelling and Mapping Studies (GIMMS) data set based on satellite imagery from the Advanced Very-High-Resolution Radiometer (AVHRR) from 1982–2013. The number of classes was chosen to align with the 14 main global climate types.
#> 
#> Number of biome classes: 14 (14/0)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical forest
#>      2: Tropical monsoon forest
#>      3: Tropical grassland
#>      4: Tropical Sahel and semiarid grassland
#>      5: Temperate desert
#>      6: Tropical desert
#>      7: Temperate grassland
#>      8: Temperate continental climate with deciduous forest
#>      9: Temperate maritime climate with evergreen broadleaf forest
#>     10: Sub-frigid mixed forest
#>     11: Frigid evergreen coniferous forest
#>     12: Frigid deciduous coniferous forest
#>     13: Polar tundra
#>     14: Polar frost
#> 
#> -----
#> 
#> Name: On using a clustering approach for global climate classification (Netzel & Stepinski, 2016a)
#> 
#> Biome scheme number: 7
#> 
#> Criteria: Mean monthly climatic conditions including temperature, precipitation and temperature range
#> 
#> Methodology: Climate data clustering based on dynamic time warping as a measure for dissimilarity between climate types
#> 
#> Description: Out of a total of 32 different clustering-based classifications utilizing WorldClim data, two products were derived and compared to the rule-based Köppen-Geiger classification.
#> 
#> Number of biome classes: 13 (13/0)
#> 
#> Biome classes (raster value: name):
#>      1: Cluster 5
#>      2: Cluster 9
#>      3: Cluster 7
#>      4: Cluster 11
#>      5: Cluster 10
#>      6: Cluster 12
#>      7: Cluster 13
#>      8: Cluster 6
#>      9: Cluster 8
#>     10: Cluster 4
#>     11: Cluster 3
#>     12: Cluster 2
#>     13: Cluster 1
#> 
#> -----
#> 
#> Name: On uising a clustering approach for global climate classification (Netzel & Stepinski, 2016b)
#> 
#> Biome scheme number: 8
#> 
#> Criteria: Mean monthly climatic conditions including temperature, precipitation and temperature range
#> 
#> Methodology: Climate data clustering based on Euclidean distance as a measure for dissimilarity between climate types
#> 
#> Description: Out of a total of 32 different clustering-based classifications utilizing WorldClim data, two products were derived and compared to the rule-based Köppen-Geiger classification.
#> 
#> Number of biome classes: 13 (13/0)
#> 
#> Biome classes (raster value: name):
#>      1: Cluster 5
#>      2: Cluster 7
#>      3: Cluster 9
#>      4: Cluster 10
#>      5: Cluster 13
#>      6: Cluster 11
#>      7: Cluster 12
#>      8: Cluster 6
#>      9: Cluster 8
#>     10: Cluster 4
#>     11: Cluster 3
#>     12: Cluster 2
#>     13: Cluster 1
#> 
#> -----
#> 
#> Name: Defining functional biomes and monitoring their change globally (Higgins et al., 2016)
#> 
#> Biome scheme number: 9
#> 
#> Criteria: Vegetation parameters including a productivity index, timing of minimum vegetation activity, vegetation height; essential data for the definition of those factors are NDVI, soil moisture, solar radiation and temperature
#> 
#> Methodology: Classification of vegetation categories based on multiple predefined parameters of vegetation height, productivity and plant growth limitations
#> 
#> Description: The definition of global biomes is based on vegetation height, productivity and limitation factors (temperature and soil moisture) applied to bi-weekly NDVI data from 1981-2012 provided by the Advanced Very High- Resolution Radiometer (AVHRR) at 0.083° spatial resolution. Out of the 31-year time series, one biome classification of the dominant key vegetation types was created. This product was included in our catalogue. The spatial data is provided by Higgins et al. (2017).
#> 
#> Number of biome classes: 24 (24/0)
#> 
#> Biome classes (raster value: name):
#>      1: THN
#>      2: THD
#>      3: SHD
#>      4: SHN
#>      5: SMD
#>      6: TMD
#>      7: SLD
#>      8: SLN
#>      9: THB
#>     10: TLD
#>     11: SHB
#>     12: TLN
#>     13: SMN
#>     14: TMN
#>     15: SHC
#>     16: THC
#>     17: SMB
#>     18: TMB
#>     19: SLB
#>     20: SMC
#>     21: TMC
#>     22: SLC
#>     23: TLB
#>     24: TLC
#> 
#> -----
#> 
#> Name: Earth´s vegetation (Pfadenhauer & Klötzli, 2014)
#> 
#> Biome scheme number: 10
#> 
#> Criteria: Life form and distribution of potential natural dominant vegetation types as defined by local environmental habitat conditions (climate, soil, relief)
#> 
#> Methodology: Review and modification of global vegetation patterns by Schmithüsen (1976) informed by multiple regional sources
#> 
#> Description: This map of global vegetation patterns is an adaptation of the concept on global vegetation zonation presented in the “Atlas of biogeography” by Schmithüsen (1976). For exact sources of undertaken map modification of the original map (Schmithüsen, 1976) see Pfadenhauer & Klötzli (2014) page 73. “Earth´s Vegetation” is the translated title. The original title is “Vegetation der Erde” (German).
#> 
#> Number of biome classes: 34 (31/3)
#> 
#> Biome classes (raster value: name):
#>      1: Evergreen and seasonal tropical lowland rainforest
#>      2: Raingreen moist savanna
#>      3: Evergreen moist savanna
#>      4: Raingreen dry savanna
#>      5: Semievergreen and raingreen deciduous forest
#>      6: Tropical-subtropical dry forest
#>      7: Evergreen dry savanna
#>      8: Tropical-subtropical dwarf bush semidesert
#>      9: Tropical-subtropical grass semidesert
#>     10: Tropical-subtropical desert
#>     11: Evergreen subtropical laurel forest
#>     12: Tropical-subtropical succulent semidesert
#>     13: High mountian steppe semidesert
#>     14: Subtropical grassland
#>     15: Subtropical sclerophyll forest
#>     16: Nemoral dry forest
#>     17: Nemoral desert
#>     18: Nemoral dwarf bush semidesert
#>     19: Evergreen nemoral laural forest
#>     20: Summergreen nemoral deciduous forest
#>     21: Mixed and low-grass steppe
#>     22: Evergreen and summergreen forest, tall-grass steppe
#>     23: Evergreen nemoral Nothofagus forest
#>     24: Evergreen nemoral coniferous forest
#>     25: Hemiboreal deciduous coniferous mixed forest
#>     26: Evergreen boreal coniferous forest
#>     27: Summergreen boreal coniferous forest
#>     28: Summergreen boreal deciduous forest
#>     29: Polar gras and dwarf shrub tundra
#>     30: Polar desert
#>     31: Polar semidesert
#>     32: Inland water
#>     33: Oceanic islands
#>     34: Mountains
#> 
#> -----
#> 
#> Name: Spatiotemporal change in geographical distribution of global climate types in the context of climate warming (Zhang & Yan, 2014)
#> 
#> Biome scheme number: 11
#> 
#> Criteria: Climate (temperature and precipitation)
#> 
#> Methodology: Non-hierarchical data clustering based on K-means distances in 14 classes, validation via Kappa statistics
#> 
#> Description: Reconstruction of 14 global climate types by K-means partitioning based on global monthly mean temperature and precipitation data interpolated from observation stations from 1982–2013. The number of classes was chosen to reflect the 14 main classes in the original Köppen-Geiger climate classification.
#> 
#> Number of biome classes: 14 (14/0)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical forest
#>      2: Tropical monsoon forest
#>      3: Tropical grassland
#>      4: Tropical Sahel and semiarid grassland
#>      5: Tropical desert
#>      6: Temperate grassland
#>      7: Frigid deciduous coniferous forest
#>      8: Temperate desert
#>      9: Temperate maritime climate with evergreen broadleaf forest
#>     10: Temperate continental climate with deciduous forest
#>     11: Sub-frigid mixed forest
#>     12: Frigid evergreen coniferous forest
#>     13: Polar tundra
#>     14: Polar frost
#> 
#> -----
#> 
#> Name: A high-resolution bioclimate map of the world: a unifying framework for global biodiversity research and monitoring (Metzger et al., 2013)
#> 
#> Biome scheme number: 12
#> 
#> Criteria: 42 climatic and physical environmental parameters including temperature, precipitation, evapotranspiration, aridity and humidity indices, solar irradiance, and elevation
#> 
#> Methodology: Compilation of multiple bioclimatic parameters, collinearity reduction among input parameters based on Pearson correlation, statistical grouping by principal components analysis of the covariance matrix, data clustering by iterative self-organizing data analysis for classification of principal components into homogeneous environmental strata, similarity-based aggregation of strata into global environmental zones based on Euclidean distance, comparison of final classification with multiple global and regional ecosystem concepts by Kappa statistics
#> 
#> Description: For the Global Environmental Stratification (GEnS), a global bioclimatic classification is derived from statistic partitioning of global geographic space according to local bioclimatic conditions. The source of the spatial data set is Metzger (2018).
#> 
#> Number of biome classes: 16 (16/0)
#> 
#> Biome classes (raster value: name):
#>      1: Extremely hot and moist
#>      2: Hot and mesic
#>      3: Extremely hot and xeric
#>      4: Extremely hot and arid
#>      5: Hot and dry
#>      6: Hot and arid
#>      7: Warm temperate and xeric
#>      8: Warm temperate and mesic
#>      9: Cool temperate and moist
#>     10: Cool temperate and xeric
#>     11: Cool temperate and dry
#>     12: Cold and mesic
#>     13: Cold and wet
#>     14: Extremely cold and mesic
#>     15: Extremely cold and wet
#>     16: Arctic
#> 
#> -----
#> 
#> Name: Global ecological zones for FAO forest reporting: 2010 update (Food and Agriculture Organization of the United Nations, 2012)
#> 
#> Biome scheme number: 13
#> 
#> Criteria: Bioregionalization, biogeography, biodiversity, and macroecological patterns, vegetation
#> 
#> Methodology: Delineation of global ecological zones based on a compilation of global and regional ecological and vegetational source maps, revision according to remote sensing observational data and expert consultations for categorization of broad vegetation (forest) types
#> 
#> Description: Earlier product of the Global Ecological Zones (GEZ) classification (GEZ 2000) was updated by the Global Forest Resources Assessment (FRA) of the Food and Agriculture Organization of the United Nations (FAO) in 2011. The result is GEZ 2010 which accounts for more up to date Earth observation data (satellite imagery from 2008-2011) and improved spatial products on climate and land cover.
#> 
#> Number of biome classes: 21 (20/1)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical rainforest
#>      2: Tropical moist forest
#>      3: Tropical mountain system
#>      4: Tropical shrubland
#>      5: Tropical dry forest
#>      6: Tropical desert
#>      7: Subtropical desert
#>      8: Subtropical humid forest
#>      9: Subtropical mountain system
#>     10: Subtropical steppe
#>     11: Subtropical dry forest
#>     12: Temperate mountain system
#>     13: Temperate desert
#>     14: Temperate steppe
#>     15: Temperate continental forest
#>     16: Temperate oceanic forest
#>     17: Boreal mountain system
#>     18: Boreal coniferous forest
#>     19: Boreal tundra woodland
#>     20: Polar
#>     21: Inland water
#> 
#> -----
#> 
#> Name: Global Land Cover by national mapping organizations (Tateishi et al., 2011; Tateishi et al., 2014; Kobayashi et al., 2017)
#> 
#> Biome scheme number: 14
#> 
#> Criteria: Earth's spectral surface reflectance
#> 
#> Methodology: Supervised classification of satellite imagery by MODIS based on multiple remote sensing products for reference as well as specific regional maps and expert opinion, individual unsupervised classification for certain classes, validation with stratified random sampling
#> 
#> Description: Spectral surface reflectance data derived from sevens bands of MODIS Earth observation data from 2003/2008/2013 was classified by supervised (for 14 classes) and unsupervised approaches (for six classes). Training data originated from several remote sensors including Landsat, MODIS NDVI products, Google Earth and Virtual Earth. Satellite imagery is grouped according to the Land Cover Classification System (LCCS) by the Food and Agriculture Organization of the United Nations (FAO). Copyright information of the original data set: Global Land Cover by National Mapping Organizations: GLCNMO Version 1, Geospatial Information Authority of Japan, Chiba University and Collaborating Organizations.
#> 
#> Number of biome classes: 19 (18/1)
#> 
#> Biome classes (raster value: name):
#>      1: Mangrove
#>      2: Broadleaf evergreen forest
#>      3: Herbaceous with sparse tree/shrub
#>      4: Bare soil - unconsolidated (sand)
#>      5: Paddy field
#>      6: Cropland/other vegetation mosaic
#>      7: Shrub
#>      8: Broadleaf deciduous forest
#>      9: Bare soil - consolidated (gravel and rock)
#>     10: Tree open
#>     11: Wetland
#>     12: Sparse vegetation
#>     13: Cropland
#>     14: Herbaceous
#>     15: Needleleaf evergreen forest
#>     16: Mixed forest
#>     17: Needleleaf deciduous forest
#>     18: Snow and ice
#>     19: Urban
#> 
#> -----
#> 
#> Name: ISLSCP II University of Maryland global land cover classifications, 1992-1993 (Defries et al., 2010)
#> 
#> Biome scheme number: 15
#> 
#> Criteria: NDVI, vegetation cover and canopy height
#> 
#> Methodology: Resampling of land cover and derived NDVI data from AVHRR with hierarchical classification
#> 
#> Description: The underlying procedure of classifying AVHRR data from 1992-1993 according to certain thresholds of vegetation cover and canopy height into distinct classes to generate this product is documented by Hansen et al. (2000).
#> 
#> Number of biome classes: 13 (12/1)
#> 
#> Biome classes (raster value: name):
#>      1: Evergreen broadleaf forest
#>      2: Wooded grassland shrubland
#>      3: Woodland
#>      4: Deciduous broadleaf forest
#>      5: Grassland
#>      6: Cropland
#>      7: Open shrubland
#>      8: Closed bushland/shrubland
#>      9: Barren
#>     10: Mixed forest
#>     11: Evergreen needleleaf forest
#>     12: Deciduous needleleaf forest
#>     13: Urban
#> 
#> -----
#> 
#> Name: Anthropogenic transformation of the biomes, 1700 to 2000 (Ellis et al., 2010)
#> 
#> Biome scheme number: 16
#> 
#> Criteria: Human population density and land use
#> 
#> Methodology: Rule-based model classification according to standardized thresholds
#> 
#> Description: Anthromes were formed by rule-based classification of human population density and land use classes to allow comparison with potential natural vegetation (Ramankutty & Foley, 1999). Out of the defined anthropogenic biomes for the years 1700, 1800, 1900 and 2000, the latter was added to our catalogue as the most up to date representative.
#> 
#> Number of biome classes: 19 (18/1)
#> 
#> Biome classes (raster value: name):
#>      1: Residential rangeland
#>      2: Pastoral villages
#>      3: Residential woodland
#>      4: Rice villages
#>      5: Populated rangeland
#>      6: Remote woodland
#>      7: Populated woodland
#>      8: Rainfed villages
#>      9: Mixed settlements
#>     10: Inhabited treeless and barren land
#>     11: Remote rangeland
#>     12: Irrigated villages
#>     13: Residential rainfed cropland
#>     14: Residential irrigated cropland
#>     15: Populated cropland
#>     16: Remote cropland
#>     17: Wild treeless and barren land
#>     18: Wild woodland
#>     19: Urban
#> 
#> -----
#> 
#> Name: GlobCover (European Space Agency, 2010)
#> 
#> Biome scheme number: 17
#> 
#> Criteria: Earth surface reflectance of solar radiance in 15 spectral bands ranging from 412.5-900 nm in wavelength
#> 
#> Methodology: Regionally specified classification of high-resolution surface reflectance mosaics, validation informed by expert knowledge
#> 
#> Description: The global land cover map is based on automated and regionally specified classification of high resolution (300 m) surface reflectance mosaics. The input data is a time series of MERIS (Medium Resolution Imaging Spectrometer Instrument) observations from the full year 2009.
#> 
#> Number of biome classes: 21 (20/1)
#> 
#> Biome classes (raster value: name):
#>      1: Closed to open (>15%) broadleaf forest regularly flooded (fresh/brackish water)
#>      2: Closed to open (>15%) broadleaf evergreen or semi-deciduous forest (>5m)
#>      3: Open (15-40%) broadleaf deciduous forest/woodland (>5m)
#>      4: Closed (>40%) broadleaf forest or shrubland permanently flooded (saline/brackish water)
#>      5: Closed to open (>15%) (broadleaf or needleleaf evergreen or deciduous) shrubland (<5m)
#>      6: Mosaic vegetation (grassland/shrubland/forest) (50-70%)/cropland (20-50%)
#>      7: Mosaic cropland (50-70%)/vegetation (grassland/shrubland/forest) (20-50%)
#>      8: Post-flooding or irrigated cropland (or aquatic)
#>      9: Closed to open (>15%) herbaceous vegetation (grassland savanna or lichen/moss)
#>     10: Bare soil
#>     11: Rainfed cropland
#>     12: Mosaic forest or shrubland (50-70%)/grassland (20-50%)
#>     13: Closed (>40%) broadleaf deciduous forest (>5m)
#>     14: Closed (>40%) needleleaf evergreen forest (>5m)
#>     15: Mosaic grassland (50-70%)/forest or shrubland (20-50%)
#>     16: Closed to open (>15%) grassland or woody vegetation regularly flooded (fresh/brackish/saline water)
#>     17: Sparse (<15%) vegetation
#>     18: Closed to open (>15%) mixed broadleaf and needleleaf forest (>5m)
#>     19: Open (15-40%) needleleaf deciduous or evergreen forest (>5m)
#>     20: Snow and ice
#>     21: Urban
#> 
#> -----
#> 
#> Name: MODIS collection 5 global land cover: algorithm refinements and characterization of new datasets (Friedl et al., 2010)
#> 
#> Biome scheme number: 18
#> 
#> Criteria: Earth surface reflectance data derived from time series of seven spectral bands provided by MODIS, EVI, remotely sensed land surface temperature, surface albedo
#> 
#> Methodology: Nested classification of Earth observation data based on ensemble decision trees, cross-validation analysis
#> 
#> Description: Several maps were generated from classifications of spectro-temporal data provided by the Collection 5 MODIS Global Land Cover Type product at 500 m spatial resolution (MCD12Q1). One of the applied legends comprises the classes by the International Geosphere-Biosphere Programme (IGBP) legend. This product was included into our catalogue.
#> 
#> Number of biome classes: 16 (15/1)
#> 
#> Biome classes (raster value: name):
#>      1: Evergreen broadleaf forest
#>      2: Cropland/natural vegetation mosaic
#>      3: Closed shrubland
#>      4: Barren
#>      5: Savanna
#>      6: Grassland
#>      7: Cropland
#>      8: Deciduous broadleaf forest
#>      9: Woody savanna
#>     10: Open shrubland
#>     11: Permanent wetland
#>     12: Mixed forest
#>     13: Evergreen needleleaf forest
#>     14: Deciduous needleleaf forest
#>     15: Snow and ice
#>     16: Urban
#> 
#> -----
#> 
#> Name: Terrestrial ecoregions of the world (The Nature Conservancy, 2009)
#> 
#> Biome scheme number: 19
#> 
#> Criteria: Macro-biogeographic patterns
#> 
#> Methodology: Compilation of selected global and regional ecozones, alignment and expert informed modification
#> 
#> Description: Different global and regional geospatial resources including global ecoregions by Olson and Dinerstein (2002), ecoregions of the United States by Bailey (1995), terrestrial ecozones of Canada by Wiken (1986) and terrestrial assessment units of The Nature Conservancy were modified according to ecological, bio-physical and political criteria.
#> 
#> Number of biome classes: 16 (15/1)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical subtropical moist broadleaf forest
#>      2: Mangrove
#>      3: Tropical subtropical grassland savanna and shrub
#>      4: Tropical subtropical dry broadleaf forest
#>      5: Tropical subtropical coniferous forest
#>      6: Flooded grassland and savanna
#>      7: Desert and xeric shrub
#>      8: Montane grassland and shrub
#>      9: Mediterranean forest woodland and scrub
#>     10: Temperate broadleaf and mixed forest
#>     11: Temperate grassland savanna and shrub
#>     12: Temperate conifer forest
#>     13: Boreal forest/taiga
#>     14: Tundra
#>     15: Rock and ice
#>     16: Inland water
#> 
#> -----
#> 
#> Name: Updated world map of the Köppen-Geiger climate classification (Peel et al., 2007)
#> 
#> Biome scheme number: 20
#> 
#> Criteria: Climate (temperature and precipitation)
#> 
#> Methodology: Classification and spatial interpolation of observational climate records based on predefined thresholds
#> 
#> Description: Long-term monthly time series records on local temperature and precipitation from 4279 climate stations were classified according to the original Köppen-Geiger system (Köppen, 1936) with minor adjustments. Continuous maps were created by two-dimensional spatial interpolation with thin-plate spline.
#> 
#> Number of biome classes: 31 (31/0)
#> 
#> Biome classes (raster value: name):
#>      1: Af - Tropical rainforest
#>      2: Am - Tropical monsoon
#>      3: Aw - Tropical savanna
#>      4: Cwc - Temperate dry winter cold summer
#>      5: BSh - Arid steppe hot
#>      6: Cwb - Temperate dry winter warm summer
#>      7: Cwa - Temperate dry winter hot summer
#>      8: BWh - Arid desert hot
#>      9: Cfa - Temperate no dry season hot summer
#>     10: Csb - Temperate dry summer warm summer
#>     11: Csa - Temperate dry summer hot summer
#>     12: ETH - Polar tundra highland
#>     13: BWk - Arid desert cold
#>     14: Cfb - Temperate no dry season warm summer
#>     15: BSk - Arid steppe cold
#>     16: Dsa - Cold dry summer hot summer
#>     17: Dwa - Cold dry winter hot summer
#>     18: Dwb - Cold dry winter warm summer
#>     19: Dfa - Cold no dry season hot summer
#>     20: Dsb - Cold dry summer warm summer
#>     21: EFH - Polar frost highland
#>     22: Cfc - Temperate no dry season cold summer
#>     23: Dwc - Cold dry winter cold summer
#>     24: Dfb - Cold no dry season warm summer
#>     25: Dsc - Cold dry summer cold summer
#>     26: Dfc - Cold no dry season cold summer
#>     27: Dsd - Cold dry summer very cold winter
#>     28: Dwd - Cold dry winter very cold winter
#>     29: Dfd - Cold no dry season very cold winter
#>     30: ET - Polar tundra
#>     31: EF - Polar frost
#> 
#> -----
#> 
#> Name: GLC2000: a new approach to global land cover mapping from Earth observation data (Bartholomé & Belward, 2005)
#> 
#> Biome scheme number: 21
#> 
#> Criteria: Top-of-canopy surface reflectance
#> 
#> Methodology: Derivation of land cover maps from spectral surface reflectance at four wavelength ranges based on regionally optimized image classification procedure
#> 
#> Description: This land cover product is generated from global daily images of the year 2000 from the VEGETATION-1 sensors of the SPOT 4 satellite and other remote sensing instruments. Regionally specific continental maps were harmonized into one consistent global map.
#> 
#> Number of biome classes: 21 (20/1)
#> 
#> Biome classes (raster value: name):
#>      1: Tree cover (regularly flooded fresh water)
#>      2: Tree cover (broadleaf evergreen)
#>      3: Tree cover (regularly flooded saline water)
#>      4: Tree cover (broadleaf deciduous open)
#>      5: Mosaic cropland/tree cover/other natural vegetation
#>      6: Mosaic cropland/shrub/grass
#>      7: Bare soil
#>      8: Herbaceous cover (closed-open)
#>      9: Shrub cover (closed-open deciduous)
#>     10: Cultivated and managed area
#>     11: Tree cover (broadleaf deciduous closed)
#>     12: Shrub cover (closed-open evergreen)
#>     13: Sparse herbaceous/shrub cover
#>     14: Mosaic tree cover/other natural vegetation
#>     15: Regularly flooded shrub/herbaceous cover
#>     16: Tree cover (needleleaf evergreen)
#>     17: Tree cover (mixed leaf type)
#>     18: Tree cover (burnt)
#>     19: Tree cover (needleleaf deciduous)
#>     20: Snow and ice
#>     21: Urban
#> 
#> -----
#> 
#> Name: Climate change and arctic ecosystems: 2. modeling, paleodata-model comparisons, and future projections (Kaplan et al., 2003)
#> 
#> Biome scheme number: 22
#> 
#> Criteria: Potential natural vegetation and associated phenological, hydrological and biogeochemical characteristics
#> 
#> Methodology: Coupled biogeographic and biogeochemical distribution modelling of biomes of defined by main potential natural vegetation types
#> 
#> Description: Simulation of the distribution of major potential natural vegetation types to form biomes with the BIOME4 model. Input factors comprise solar radiation, atmospheric CO2, climatic parameters (precipitation, temperature, solar radiation) and soil characteristics. Competition among plant functional types is accounted for by consideration of net primary productivity (NPP) and maximum leaf area (LAI). Biomes are assigned to plant functional types by empirical classification (https://pmip2.lsce.ipsl.fr/synth/biome4.shtml).
#> 
#> Number of biome classes: 27 (27/0)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical evergreen forest
#>      2: Tropical semi-deciduous forest
#>      3: Tropical savanna
#>      4: Tropical deciduous forest/woodland
#>      5: Tropical xerophytic shrubland
#>      6: Tropical grassland
#>      7: Barren
#>      8: Desert
#>      9: Warm mixed forest
#>     10: Temperate conifer forest
#>     11: Temperate sclerophyll woodland
#>     12: Open conifer woodland
#>     13: Temperate xerophytic shrubland
#>     14: Steppe tundra
#>     15: Temperate broadleaf savanna
#>     16: Temperate deciduous forest
#>     17: Temperate grassland
#>     18: Cold mixed forest
#>     19: Cool mixed forest
#>     20: Cool conifer forest
#>     21: Evegreen taiga/montane forest
#>     22: Deciduous taiga/montane forest
#>     23: Cushion-forbs lichen and moss
#>     24: Shrub tundra
#>     25: Dwarf shrub tundra
#>     26: Snow and ice
#>     27: Prostrate shrub tundra
#> 
#> -----
#> 
#> Name: Terrestrial ecoregions of the world: a new map of life on Earth (Olson et al., 2001)
#> 
#> Biome scheme number: 23
#> 
#> Criteria: Distribution of distinct natural communities prior to human land use change, biogeographic zonation and species distribution
#> 
#> Methodology: Review of global and regional biogeographic provinces, hierarchical classification into ecoregions, refinement based on expert consultation, nesting of ecoregions into biomes and biogeographic realms
#> 
#> Description: Global biome concepts, biogeographic provinces and distribution maps of certain floristic and zoological groups as well as main vegetation types were reviewed. Realms were detected by hierarchical classification and adjusted according to expert opinion. Delineated ecoregions were refined based on regional maps. Finally, 825 terrestrial ecoregions were nested into biomes and biogeographic realms.
#> 
#> Number of biome classes: 16 (15/1)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical and subtropical moist broadleaf forest
#>      2: Mangrove
#>      3: Tropical and subtropical grassland savanna and shrubland
#>      4: Tropical and subtropical dry broadleaf forest
#>      5: Tropical and subtropical coniferous forest
#>      6: Flooded grassland and savanna
#>      7: Deserts and xeric shrubland
#>      8: Montane grassland and shrubland
#>      9: Mediterranean forest woodland and scrub
#>     10: Temperate broadleaf and mixed forest
#>     11: Temperate conifer forest
#>     12: Temperate grassland savanna and shrubland
#>     13: Boreal forest/taiga
#>     14: Tundra
#>     15: Snow and ice
#>     16: Inland water
#> 
#> -----
#> 
#> Name: Development of a global land cover characteristics database and IGBP DISCover from 1 km AVHRR data (Loveland et al., 2000)
#> 
#> Biome scheme number: 24
#> 
#> Criteria: NDVI
#> 
#> Methodology: Unsupervised classification and subsequent stratification of monthly NDVI composites provided by AVHRR from 1992–1993 at continental scale
#> 
#> Description: The IGBP DISCover global land cover data set served as the base for the derivation of seven distinct land cover products. Those include: Olson Global Ecosystems (Olson, 1994), IGBP DISCover (Belward, 1996), Biosphere–Atmosphere Transfer Scheme (BATS) (Dickinson et al., 1986), Simple Biosphere Model (SiB) (Sellers et al., 1986), Simple Biosphere Model 2 (SiB2) (Sellers et al., 1996), USGS Land Use/Land Cover System (Anderson et al., 1976), Global Remote Sensing Land Cover (Running et al., 1995).
#> 
#> Number of biome classes: 16 (15/1)
#> 
#> Biome classes (raster value: name):
#>      1: Evergreen broadleaf
#>      2: Savanna
#>      3: Crop/natural vegetation
#>      4: Bare surface
#>      5: Woody savanna
#>      6: Deciduous broadleaf
#>      7: Wetlands
#>      8: Closed shrub
#>      9: Grassland
#>     10: Cropland
#>     11: Open shrub
#>     12: Mixed forest
#>     13: Evergreen needleleaf
#>     14: Deciduous needleleaf
#>     15: Ice
#>     16: Urban
#> 
#> -----
#> 
#> Name: Estimating historical changes in global land cover: croplands from 1700 to 1992 (Ramankutty & Foley, 1999)
#> 
#> Biome scheme number: 25
#> 
#> Criteria: Potential natural vegetation
#> 
#> Methodology: Informed classification of remotely sensed land cover
#> 
#> Description: Potential natural vegetation is derived by classifying DISCover land cover data following the Olson Global Ecosystems framework (Olson, 1994).
#> 
#> Number of biome classes: 12 (12/0)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical evergreen woodland
#>      2: Tropical deciduous woodland
#>      3: Savanna
#>      4: Dense shrubland
#>      5: Desert and barren
#>      6: Open shrubland
#>      7: Grassland and steppe
#>      8: Temperate evergreen woodland
#>      9: Temperate deciduous woodland
#>     10: Mixed woodland
#>     11: Tundra
#>     12: Boreal woodland
#> 
#> -----
#> 
#> Name: Possible changes in natural vegetation patterns due to global warming (Leemans, 1990)
#> 
#> Biome scheme number: 26
#> 
#> Criteria: Vegetation determined by bio-climatic site conditions (biotemperature, precipitation, potential evapotranspiration ratio)
#> 
#> Methodology: Application of the Holdridge life zone classification (Holdridge 1947, 1967) based on spatially interpolated bio-climatic parameters from climate station records
#> 
#> Description: Climate observation records of from around 5500 stations around the globe from 1931-1960 were compiled to extract monthly mean temperature and precipitation data. Holdridge’s life zone classification (Holdridge 1947, 1967) was applied to derive a map of world life zones/large-scale vegetation patterns.
#> 
#> Number of biome classes: 39 (38/1)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical moist forest
#>      2: Subtropical wet forest
#>      3: Warm temperate rainforest
#>      4: Tropical wet forest
#>      5: Subtropical rainforest
#>      6: Tropical dry forest
#>      7: Subtropical moist forest
#>      8: Tropical very dry forest
#>      9: Tropical thorn steppe
#>     10: Subtropical dry forest
#>     11: Tropical desert bush
#>     12: Warm temperate wet forest
#>     13: Tropical desert
#>     14: Subtropical thorn steppe
#>     15: Subtropical desert
#>     16: Subtropical desert bush
#>     17: Warm temperate moist forest
#>     18: Warm temperate desert
#>     19: Warm temperate dry forest
#>     20: Cool temperate rainforest
#>     21: Warm temperate thorn steppe
#>     22: Warm temperate desert bush
#>     23: Cool temperate wet forest
#>     24: Cool temperate desert
#>     25: Cool temperate desert bush
#>     26: Cool temperate steppe
#>     27: Boreal rainforest
#>     28: Cool temperate moist forest
#>     29: Boreal desert
#>     30: Boreal dry bush
#>     31: Polar rain tundra
#>     32: Boreal wet forest
#>     33: Boreal moist forest
#>     34: Polar desert
#>     35: Polar dry tundra
#>     36: Polar wet tundra
#>     37: Polar moist tundra
#>     38: Ice
#>     39: Inland water
#> 
#> -----
#> 
#> Name: Ecozones of the Earth (Schultz, 1988, 1995, 2002, 2008, 2016)
#> 
#> Biome scheme number: 27
#> 
#> Criteria: Climate (temperature, precipitation, evapotranspiration), vegetation (community composition, phytomass distribution, primary production, growing season), radiation, pedosphere, lithosphere, fauna, human activities (settlement, land use)
#> 
#> Methodology: Review, evaluation of regional ecological studies, quantitative ecosystem analysis
#> 
#> Description: The Ecozones of the Earth were originally published in 1988 with four revised editions (1995, 2002, 2008, 2016). The spatial distribution of the ecozones is based on Troll and Paffen (1964) and was revised in respect to the subdivision of seasonal tropic and summer moist tropic ecozones. We digitized, the third edition (Schultz, 2002) because this map includes mountains (only shown in newer versions from 2002, 2008, and 2016) and large parts of Antarctica (only shown in older versions from 1988, 1995, and 2002). “Ecozones of the Earth” is the translated title. The original title is “Ökozonen der Erde” (German).
#> 
#> Number of biome classes: 15 (14/1)
#> 
#> Biome classes (raster value: name):
#>      1: Continuous moist tropics
#>      2: Moist savanna
#>      3: Dry savanna
#>      4: Summer moist xeric shrubland
#>      5: Tropical-subtropical desert
#>      6: Continuous moist subtropics
#>      7: Winter moist steppe
#>      8: Winter moist subtropics
#>      9: Temperate desert
#>     10: Temperate steppe
#>     11: Moist mid-latitudes
#>     12: Boreal
#>     13: Tundra
#>     14: Ice
#>     15: Inland water
#>     16: Oceanic islands
#>     17: Mountains
#> 
#> -----
#> 
#> Name: Landscape belts of the Earth (Müller-Hohenstein, 1981)
#> 
#> Biome scheme number: 28
#> 
#> Criteria: Climate, vegetation, soil
#> 
#> Methodology: Review and combination of thematic concepts
#> 
#> Description: The biogeographic landscape belts are adapted from Troll & Paffen (1964) considering climatic characteristics in combination with large-scale vegetation patterns from Schmithüsen (1976) and soil characteristics provided by Ganssen & Hädrich (1965). “Landscape belts of the Earth” is the translated title. The original title is “Die Landschaftsgürtel der Erde” (German). The map shown was reissued by Schulze et al. (2019).
#> 
#> Number of biome classes: 15 (13/2)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical rainforest
#>      2: Tropical pasture highland
#>      3: Moist savanna
#>      4: Dry savanna and tropical dry forest
#>      5: Tropical agriculture
#>      6: Desert and semidesert
#>      7: Dry zone oasis
#>      8: Dry steppe
#>      9: Tillage
#>     10: Temperate grassland and pasture
#>     11: Temperate forest and bush
#>     12: Tundra
#>     13: Rock and ice
#>     14: Inland water
#>     15: Oceanic islands
#> 
#> -----
#> 
#> Name: Atlas of biogeography (Schmithüsen, 1976)
#> 
#> Biome scheme number: 29
#> 
#> Criteria: Climate (temperature, potential evapotranspiration), potential natural vegetation, soil, topography, elevation
#> 
#> Methodology: Biogeographic analysis
#> 
#> Description: An underlying assumption of the Atlas of Biogeography is that vegetation patterns mirror climatic conditions and are furthermore influenced by regional geological effects and disturbances. Geographical units are defined by zonal natural vegetation with climax stages that cannot be distinguishing by sharp gradients. Anthropogenic effects are not considered in this theoretical concept. The map was adapted by Sitte et al. (2002) and by Pfadenhauer & Klötzli (2014). “Atlas of Biogeography” is the translated title. The original title is “Atlas zur Biogeographie” (German).
#> 
#> Number of biome classes: 31 (29/2)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical rainforest
#>      2: Paramo heath and wet Puna
#>      3: Moist savanna
#>      4: Tropical mountain rainforest
#>      5: Tropical semi-evergreen rainforest
#>      6: Dry savanna
#>      7: Thorn and succulent forest
#>      8: Thorn savanna
#>      9: Tropical dry forest
#>     10: Dry desert
#>     11: Laurel forest and subtropical rainforest
#>     12: Semidesert
#>     13: Thorn shrub and succulents
#>     14: Sclerophyll vegetation
#>     15: Coniferous dry shrub
#>     16: Dry steppe and hard cushion formations
#>     17: Transition steppe
#>     18: Summer green deciduous forest
#>     19: Mountain vegetation
#>     20: Subantarctic heath
#>     21: Summer green tree steppe
#>     22: Summer green deciduous forest with conifers
#>     23: Mountain coniferous forest
#>     24: Temperate rainforest
#>     25: Evergreen boreal coniferous forest
#>     26: Summer green coniferous forest
#>     27: Subpolar meadow and summer green shrub
#>     28: Tundra
#>     29: Cold desert
#>     30: Inland water
#>     31: Oceanic islands
#> 
#> -----
#> 
#> Name: Communities and ecosystems (Whittaker, 1975)
#> 
#> Biome scheme number: 30
#> 
#> Criteria: Climate (temperature, precipitation) and vegetation (plant community distribution)
#> 
#> Methodology: Biogeographic analysis
#> 
#> Description: The presented biome map outlines terrestrial macro-biogeographic patterns.
#> 
#> Number of biome classes: 14 (12/2)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical rainforest
#>      2: Tropical savanna
#>      3: Tropical thornwood
#>      4: Tropical seasonal forest
#>      5: Woodland
#>      6: Desert and semidesert
#>      7: Mediterranean
#>      8: Temperate forest
#>      9: Temperate grassland
#>     10: Boreal forest
#>     11: Tundra and alpine
#>     12: Ice
#>     13: Inland water
#>     14: Oceanic islands
#> 
#> -----
#> 
#> Name: Vegetation and climate (Walter, 1964, 1968; Walter & Breckle, 1970; Breckle & Rafiqpoor, 2019)
#> 
#> Biome scheme number: 31
#> 
#> Criteria: Bio-physical environmental parameters including temperature, precipitation, solar radiation, soil characteristics, flora and fauna, continentality and maritime influence, snow cover
#> 
#> Methodology: Classification according to defined thresholds on climatic data, vegetation proxies and surface cover indices
#> 
#> Description: For this classification, biomes are defined as consistent terrestrial ecological regions to form habitats which correspond to relatively uniform landscapes. Those fundamental global ecological units are characterized by similar vegetation and fauna. Important shaping factors are climate and soil. The concept includes ecotones which mark small-scale transition zones between distinct areas of certain exclusive classes. Zonobiomes are zonally connected and azonal relief units, like mountain systems, are defined as orobiomes.  The first version consisted of two volumes with the original title “Die Vegetation der Erde in öko-physiologischer Betrachtung“ (Walter, 1964, 1968). There have been several later English and German editions of this fundamental book series over the past decades. Those include among others Walter (1973, 1979, 1984, 1990) and Walter & Breckle (1985, 1991, 1999). The map consulted in this inventory was taken from the last edition by Breckle & Rafiqpoor (2019).
#> 
#> Number of biome classes: 34 (32/2)
#> 
#> Biome classes (raster value: name):
#>      1: ET Tropical rainforest - desert semidesert
#>      2: Tropical rainforest
#>      3: ET Tropical rainforest - tropical subtropical seasonal rainforest savanna
#>      4: Tropical subtropical seasonal rainforest savanna
#>      5: ET Desert semidesert - tropical subtropical seasonal rainforest savanna
#>      6: Desert semidesert
#>      7: ET Laurel forest - tropical subtropical seasonal rainforest savanna
#>      8: ET Laurel forest - tropical rainforest
#>      9: ET Laurel forest - desert semidesert
#>     10: ET Mediterranean - desert semidesert
#>     11: ET Winter cold steppe - tropical subtropical seasonal rainforest sava
#>     12: Laurel forest
#>     13: ET Laurel forest - winter cold steppe
#>     14: ET Desert semidesert - winter cold semidesert
#>     15: ET Mediterranean - winter cold steppe
#>     16: Mediterranean
#>     17: Winter cold desert
#>     18: ET Laurel forest - mediterranean
#>     19: Winter cold steppe
#>     20: Winter cold semidesert
#>     21: ET Mediterranean - nemoral
#>     22: Nemoral
#>     23: ET Laurel forest - nemoral
#>     24: ET Winter cold steppe - polar
#>     25: ET Nemoral - winter cold steppe
#>     26: ET Nemoral - boreal
#>     27: ET Nemoral - polar
#>     28: Boreal
#>     29: ET Winter cold steppe - boreal
#>     30: ET Boreal - polar
#>     31: Polar
#>     32: Ice
#>     33: Inland water
#>     34: Oceanic islands
#> 
#> -----

# Print information for the first three biomes
biomes_info(1:3)
#> 
#> Name: Global vegetation patterns of the past 140,000 years (Allen et al., 2020)
#> 
#> Biome scheme number: 1
#> 
#> Criteria: Carbon mass, LAI, and plant functional types
#> 
#> Methodology: Modelling with the global dynamic vegetation Lund-Potsdam-Jena General Ecosystem Simulator
#> 
#> Description: Global biomes were simulated over the past 140,000 years. Input factors to the dynamic global vegetation model included reconstructed atmospheric CO2 concentrations, Earth's obliquity and paleo- as well as pre-industrial climate simulations by HadCM3. Biomes were assigned according to specified ranges of vegetation carbon mass and leaf area index (LAI) of functional plant types based on consistent rules.
#> 
#> Number of biome classes: 21 (21/0)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical evergreen forest
#>      2: Tropical raingreen forest
#>      3: Savanna
#>      4: Tropical grassland
#>      5: Warm temperate woodland
#>      6: Desert
#>      7: Temperate broadleaf evergreen forest
#>      8: Semidesert
#>      9: Temperate shrubland
#>     10: Temperate needleleaf evergreen forest
#>     11: Steppe
#>     12: Temperate parkland
#>     13: Temperate summergreen forest
#>     14: Temperate mixed forest
#>     15: Boreal parkland
#>     16: Tundra
#>     17: Boreal summergreen broadleaf forest
#>     18: Boreal evergreen needleleaf forest
#>     19: Boreal summergreen needleleaf forest
#>     20: Shrub tundra
#>     21: Boreal woodland
#> 
#> -----
#> 
#> Name: Dataset of the global component of the Copernicus Land Monitoring Service (Buchhorn et al., 2019)
#> 
#> Biome scheme number: 2
#> 
#> Criteria: Multi-spectral Earth surface reflectance on top of canopies
#> 
#> Methodology: Supervised classification of satellite imagery data based on external reference data sets and expert opinion
#> 
#> Description: Copernicus Global Land Service (CGLS) provide an annual dynamic product on global land cover at 100 m spatial resolution derived from classification of daily-synthesis surface reflectance from the PROBA-V sensor.
#> 
#> Number of biome classes: 20 (18/2)
#> 
#> Biome classes (raster value: name):
#>      1: Closed forest (evergreen broadleaf)
#>      2: Open forest (deciduous broadleaf)
#>      3: Open forest (unknown)
#>      4: Shrubs
#>      5: Bare soil/sparse vegetation
#>      6: Cultivated and managed vegetation/agriculture (cropland)
#>      7: Closed forest (deciduous broadleaf)
#>      8: Closed forest (unknown)
#>      9: Open forest (evergreen broadleaf)
#>     10: Herbaceous vegetation
#>     11: Open forest (evergreen needleleaf)
#>     12: Closed forest (mixed)
#>     13: Closed forest (evergreen needleleaf)
#>     14: Herbaceous wetland
#>     15: Closed forest (deciduous needleleaf)
#>     16: Open forest (deciduous needleleaf)
#>     17: Snow and ice
#>     18: Moss and lichen
#>     19: Inland water
#>     20: Urban
#> 
#> -----
#> 
#> Name: Present and future Köppen-Geiger climate classification maps at 1-km resolution (Beck et al., 2018)
#> 
#> Biome scheme number: 3
#> 
#> Criteria: Climate (temperature and precipitation)
#> 
#> Methodology: Application of a slightly adjusted Köppen-Geiger classification based on climatological thresholds following Peel et al. (2007)
#> 
#> Description: This present-day (1980–2016) Köppen-Geiger climate product at 1 km spatial resolution is based on ensemble data of multiple global climatic maps. The classification follows predefined temperature and precipitation thresholds as well as seasonality.
#> 
#> Number of biome classes: 30 (30/0)
#> 
#> Biome classes (raster value: name):
#>      1: Af - Tropical rainforest
#>      2: Am - Tropical monsoon
#>      3: Aw - Tropical savanna
#>      4: Cwc - Temperate dry winter cold summer
#>      5: BSh - Arid steppe hot
#>      6: Cwb - Temperate dry winter warm summer
#>      7: Cwa - Temperate dry winter hot summer
#>      8: BWh - Arid desert hot
#>      9: Cfa - Temperate no dry season hot summer
#>     10: Csb - Temperate dry summer warm summer
#>     11: Dsa - Cold dry summer hot summer
#>     12: Csc - Temperate dry summer cold summer
#>     13: Cfb - Temperate no dry season warm summer
#>     14: Csa - Temperate dry summer hot summer
#>     15: BWk - Arid desert cold
#>     16: Dsb - Cold dry summer warm summer
#>     17: BSk - Arid steppe cold
#>     18: Dwa - Cold dry winter hot summer
#>     19: Dfa - Cold no dry season hot summer
#>     20: Dwb - Cold dry winter warm summer
#>     21: Cfc - Temperate no dry season cold summer
#>     22: Dfb - Cold no dry season warm summer
#>     23: Dwc - Cold dry winter cold summer
#>     24: ET - Polar tundra
#>     25: Dfc - Cold no dry season cold summer
#>     26: Dsc - Cold dry summer cold summer
#>     27: Dwd - Cold dry winter very cold winter
#>     28: Dfd - Cold no dry season very cold winter
#>     29: Dsd - Cold dry summer very cold winter
#>     30: EF - Polar frost
#> 
#> -----
```
