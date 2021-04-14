# variables couleurs
reach_rouge         <- "#EE5859"
reach_rose          <- "#f5a6a7"
reach_orange        <- "#F69E61"
reach_gris          <- "#58585A"
reach_lt_gris       <- "#D1D3D4"
reach_palette_rouge <- c('#FEF2F2','#EE5859', '#772B2C')
blanc               <-  "#FFFFFF"

# couches spatiales
bfa_admin2 <- st_read("donnees/spatiale/bfa_admin2_simple.geojson")

bfa_chemin_trunk <- st_read("donnees/spatiale/Trunk.shp")


# autre maniere de faire des lignes admin1 c'est avec un dissolve de admin2. il viens du package rmapshaper
bfa_admin1 <- st_read("donnees/spatiale/bfa_admin1_simple.geojson")

# convert gov boundaries to lines to display on map so they do not clash with district interactivity
bfa_admin1_lignes <- st_cast(bfa_admin1,"MULTILINESTRING")
bfa_admin1_lignes <- st_cast(bfa_admin1_lignes,"LINESTRING")

bfa_admin1_centroid <- st_centroid(bfa_admin1)


##############################################################################
### couche des villages
bfa_villages <- readxl::read_xlsx("donnees/tableaux/SETTLEMENTS_3F_BFA_20200505.xlsx", sheet = "SETTLEMENTS_3F")

# convert to sf object and define coordinate columns in order to plot in leaflet
bfa_villages <- bfa_villages %>%
  st_as_sf(
    coords = c("POINT_X", "POINT_Y"), # choose lat and long columns in table 
    crs = 4326)

region_a_filtrer <- "Sahel"

bfa_admin1_filtree <- bfa_admin1 %>% 
  filter(ADM1_FR == region_a_filtrer)

villages_filtree <-  st_intersection(bfa_villages, bfa_admin1_filtree)


###########################################################################

# population raster
pop_bfa_2018 <- raster::raster("donnees/spatiale/population_bfa_2018-10-01.tif")


# zonal stats with the pop raster
pop_2018 <- spatialEco::zonal.stats(bfa_admin2, pop_bfa_2018, stats = "sum")

# faire un bind pour mettre les statistiques dans les donnees admin2
bfa_admin2 <- cbind(bfa_admin2, pop_2018) %>% 
  dplyr::rename(pop_2018 = sum.population_bfa_2018.10.01)