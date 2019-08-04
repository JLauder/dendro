library(tidyverse)
library(sf)
library(raster)

setwd("~/UC Davis/Research Projects/Lauder drought metric comparison/dendro")


#### Load and prep non-species-specific data files ####

# Sierra Nevada boundary
sierra = st_read("data/distrib/sierra/sierra_boundary.geojson")
sierra = sierra %>% st_union() %>% st_buffer(0.01) %>% st_transform(3310)
sierra_sp = as(sierra,"Spatial") # make a copy in the older "sp" format for compatibility with `raster` package

# PRISM normal annual precip
prism_ppt = raster("data/climate/prism/normal-annual-ppt/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")
prism_ppt = prism_ppt %>% crop(sierra_sp) %>% mask(sierra_sp)

# Plot locations
trees = read.csv("data/tree/AlltreesFinal.csv")
unique_plots = !duplicated(trees$Plot)
plots = trees[unique_plots,]
plots = plots %>%
  dplyr::select(Plot,Latitude,Longitude)
plots = st_as_sf(plots,coords=c("Longitude","Latitude"),crs=4326) %>% st_transform(3310)


#### Get PRISM values at plot locations ####

plots$prism_ppt = extract(prism_ppt,plots,method="bilinear")



#### Species-specific ppt thresholding ####

species_thresholds = data.frame() # initialize data frame for storing species thresholds

species_opts = c("abieconc","abiemagn","pinulamb","pinupond","pinusabi","pseumenz")

for(species in species_opts) {
  
  # Load species distribution (Little's range maps downloaded from: https://github.com/wpetry/USTreeAtlas)
  sp_distrib_file = paste0("data/distrib/species/",species,"/",species,".shp")
  sp_distrib = st_read(sp_distrib_file) %>% st_union
  st_crs(sp_distrib) = 4326
  sp_distrib = st_transform(sp_distrib,3310)
  
  # Intersect species distrib with Sierra
  sp_sierra = st_intersection(sp_distrib,sierra)
  sp_sierra_geo = st_transform(sp_sierra,4326) # put it in geographic coordinates to match the prism layer for the cropping to follow
  sp_sierra_geo_sp= as(sp_sierra_geo,"Spatial") # make a copy in the older "sp" format for compatibility with `raster` package
  
  # Crop and mask PRISM to the Sierra-specific species distrib
  prism_ppt_sp = prism_ppt %>% crop(sp_sierra_geo_sp) %>% mask(sp_sierra_geo_sp)
  
  # Extract all PRISM values from the species- and sierra- cropped ppt layer
  ppt_vals = values(prism_ppt_sp)
  mean_ppt = mean(ppt_vals,na.rm=TRUE)
  
  species_threshold = data.frame(species = species, ppt_threshold = mean_ppt)
  species_thresholds = rbind(species_thresholds, species_threshold)
  
  
  # For each plot, determine whether it was above or below the species-specific ppt threshold
  newcol_name = paste0("ppt_",species) # what to call the spcies-specific threshold column
  plots[,newcol_name] = ifelse(plots$prism_ppt > mean_ppt,"high","low")
  

}



#### Write plot table and species threshold table ####

plots_nogeom = plots
st_geometry(plots_nogeom) = NULL

write.csv(species_thresholds,"data/plot/species_ppt_thresholds.csv",row.names=FALSE)
write.csv(plots_nogeom,"data/plot/plots_w_species_thresholds.csv",row.names=FALSE)


