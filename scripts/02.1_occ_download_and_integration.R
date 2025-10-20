## %######################################################%##
#                                                          #
####            Downloading, integrating, and           ####
####           cleaning occurrence databases            ####
#                                                          #
## %######################################################%##
# Written by: Santiago J.E. Velazco, M. Brooke Rose, & Janet Franklin

###### Packages
require(dplyr) # Manipulate data
require(readr) # Read and write data
require(ridigbio) # Download data from iDigBio
require(rgbif) # Download data from GBIF
require(BIEN) # Download data from BIEN
require(rinat) # Download data from inaturaList
require(Rocc) # Download data from speciesLink


# Despite bdc is available on CRAN for this class we advice to install the development version
# available on GitHub (installed above)
require(bdc) # Biodiversity data cleaning https://brunobrr.github.io/bdc/index.html
require(ggplot2) # Plot data
require(sf) # For handling spatial data
require(maps) # A spatial database of country boundaries

## %###########################################################################%##
##### It is strongly recommended to create a project in RStudio to               #
##### facilitate managing file paths and saving outputs. So create a project     #
##### with the name "SDM_practice" or another name that reminds you what         #
##### this project is used for. If you have never created a project before,      #
##### you can google it or watch the video below                                 #
##### https://www.youtube.com/watch?v=pyJMWlDptYw&t=12s                          #
## %###########################################################################%##



## %######################################################%##
#                                                          #
####                    Species list                    ####
#                                                          #
## %######################################################%##

# In this tutorial let's work with two beautiful species from South America

# Peltophorum dubium - Fabaceae
## vernacular name: caña fístola, yvyrá-Pytá, yellow poinciana tree)
## more information: https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:187026-2

# Ceiba chodatii - Malvaceae
## (vernacular name: palo borracho or yuchán)
## more information: https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:1005658-1

# We also will use non-existent species names ("Asdf sfd") to learn how to deal with situations
# where a species name is not found

# Vector with species names
spp1 <- read.csv("tablas/global_tree_search_trees_1_7.csv" , header = T)

###data de nativas exoticas 
###
library("dplyr")                                    # Load dplyr package
library("plyr")                                     # Load plyr package
library("readr") 


data_all <- list.files(path = "tablas/csv",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data_all                                            # Print data to RStudio console

data_all = unique(data_all$taxon)
data_all <- as.data.frame(data_all)
######Hago listas de especies
######

data_g <- spp1$TaxonName
spp<- data_all$data_all

####Obtener las especies no coincidentes

spp <- setdiff(data_g,spp)
spp <- unique(spp)

spp <- as.data.frame(spp)
##Cite the Information: Fuentes N, Pauchard A, Sánchez P, Esquivel J & Marticorena A. 2013. A new comprehensive database of alien plant species in Chile based on herbarium records. Biological Invasions. Volumen 15, Issue 4, pp 847-858


dirs <- file.path(getwd(),"puntos_tf")
dirs # This is the path were the codes will be stored
dir.create(dirs) # Function for creating the directory where each dataset will be saved

## %######################################################%##
#                                                          #
####            Downloading occurrences data            ####
####               from different sources               ####
#                                                          #
## %######################################################%##


# Little function to transform a vector of species names to a list object
sp_to_list <- function(x) {
  x2 <- as.list(rep(NA, length(x)))
  names(x2) <- x
  return(x2)
}

## %######################################################%##
#                                                          #
####                       GBIF                         ####
#                                                          #
## %######################################################%##
# https://www.gbif.org
# https://docs.ropensci.org/rgbif/index.html
# https://vimeo.com/127119010
# rgbif::gbif_issues() %>% tibble() %>% View()

# Read more about GBIF issues and gbif_issues() function for performing some extra data.cleaning

# In this tutorial will will use lots of "for loops", if you want
# to read more about this go to https://r4ds.had.co.nz/iteration.html


occ_list <- sp_to_list(spp) # an empty list


for (i in 16165 :length(spp)) {
  message(paste(i, Sys.time()))
  occ_list[[i]] <-
    rgbif::occ_data(
      scientificName = spp[i],
      country = c("AR", "BO", "PE", "CO", "EC", "VE", "CL", "PY", "UY", "BR"),
      hasCoordinate = TRUE,  # Without spatial issues
      #limit = 1000000
    )
  # Check if occ_list[[i]] is NA, NULL, or empty, and skip to the next iteration if it is
  # Let's extract the tibble (a kind of data.frame) object for each species
  occ_list[[i]] <- occ_list[[i]]$data
  names(occ_list)[i] <- spp[i]
  write.table(names(occ_list)[i], file.path(dirs, paste0(i, ".csv")))
}


                                                                                    
class(occ_list[[1]]) # it's a gbif_data not a data.frame or tibble


# Remove empty element of the list occ_list
filt <- sapply(occ_list, function(x) !is.null(nrow(x)))
# let's merge these tibbles
occ_list <- dplyr::bind_rows(occ_list[filt], .id = "search_name")
names(occ_list) # all this are the names of columns

occ_list <- occ_list %>% dplyr::select(-networkKeys)

# save this occurrence database as csv
readr::write_csv(occ_list, file.path(dirs, "GBIF.csv"))
# Where was it saved?
file.path(dirs, "GBIF.csv")
     
                                                                                                   
                                                      # 
####                      iDigBio                       ####
#                                                          #
## %######################################################%##
# https://www.idigbio.org/portal/search

occ_list <- sp_to_list(spp)
for (i in 35669:length(spp) ) {
  message(paste(i, Sys.time()))
  occ_list[[i]] <-
    ridigbio::idig_search_records(rq = list(scientificname = spp[i], country = c("argentina","paraguay", "brasil", "chile", "bolivia", "peru", "colombia", "ecuador", "venezuela")), limit = 100000) %>% as_tibble()
  names(occ_list)[i] <- spp[i]
  write_csv(occ_list[[i]], file.path(dirs, paste0(i, ".csv"))) 
  
}
occ_list1 = IDIGBIO
occ_list <- occ_list[sapply(occ_list, function(x) nrow((x))) > 0] # this line is only for removing
occ_list <- dplyr::bind_rows(occ_list, .id = "search_name")
occ_list

# Extract year from date collected
occ_list1$datecollected <- lubridate::ymd(occ_list1$datecollected) # 57 failed to parse
occ_list1$year <- lubridate::year(occ_list1$datecollected)

# save this occurrence database as csv
readr::write_csv(occ_list1, file.path(getwd(), "puntos_tf3/IDIGBIO.csv"))



## %######################################################%##
#                                                          #
####                    iNaturalist                     ####
#                                                          #
## %######################################################%##
# https://www.inaturalist.org

occ_list <- sp_to_list(spp)
for (i in 1:length(spp)) {
  message(paste(i, Sys.time()))
  try(res <- rinat::get_inat_obs(query = spp[i], quality = "research", geo = TRUE, maxresults = 10000))
  try(res <- res %>% dplyr::filter(iconic_taxon_name == "Plantae", captive_cultivated == "false") %>% as_tibble())
  try(occ_list[[i]] <- res)
  try(rm(res))
  names(occ_list)[i] <- spp[i]
} # Don't worry about these try() just catch the errors and continue the loop


# Checking rinat ouptut for valid species output and filtering out NA
occ_list <- occ_list[names(unlist(sapply(occ_list, nrow)))]

occ_list <- dplyr::bind_rows(occ_list, .id = "search_name") %>% as_tibble()
occ_list <-  plyr::rbind.fill(occ_list) %>% as_tibble()
# Extract year from date collected
occ_list$year <- lubridate::year(occ_list$datetime)

# Save this occurrence database as csv
readr::write_csv(occ_list, file.path(dirs, "INATURALIST.csv"))


## %######################################################%##
#                                                          #
####                       BIEN                         ####
#                                                          #
## %######################################################%##
# https://bien.nceas.ucsb.edu/bien/

occ_list <- sp_to_list(spp)
for (i in 37388:length(spp)) {
  message(paste(i, Sys.time()))
  occ_list[[i]] <- BIEN::BIEN_occurrence_species(
    species = spp[i],
    cultivated = TRUE,
    new.world = TRUE,
    all.taxonomy = TRUE,
  ) %>%
    dplyr::tibble()
  names(occ_list)[i] <- spp[i]
  write_csv(occ_list[[i]], file.path(dirs, paste0(i, ".csv"))) 
}

occ_list <- occ_list[sapply(occ_list, nrow) > 0]
occ_list <- dplyr::bind_rows(occ_list, .id = "search_name")

occ_list = BIEN
# Extract year from date collected
occ_list$date_colle <- lubridate::ymd(occ_list$date_colle)
occ_list$year <- lubridate::year(occ_list$date_colle)

# Save this occurrence database as csv
readr::write_csv(occ_list, file.path(dirs, "BIEN.csv")) # save as csv


names(occ_list)
View(occ_list)

## %######################################################%##
#                                                          #
####                    SpeciesLink                     ####
#                                                          #
## %######################################################%##
# https://specieslink.net
# More interesting for Brazil and other countries of South America
spp <- as.list(spp)

occ_list <- sp_to_list(spp)
for (i in 1:length(spp)) {
  occ_list[[i]] <- rspeciesLink(
    species = spp[i],
    basisOfRecord = NULL,
    Scope = "plants",
    save = FALSE,
    Coordinates = "Yes",
    CoordinatesQuality = "Good"
  ) %>%
    tibble()
  names(occ_list)[i] <- spp[i]
  write_csv(occ_list[[i]], file.path(dirs, paste0(i, ".csv")))
}

# IGNORE THIS MESSAGE: Output is empty. Check your request.
library(scales)
occ_list = SPECIESLINK
occ_list <- dplyr::bind_rows(occ_list, .id = "search_name") %>% discard( ~ nrow(.x) == 0)
occ_list <-  plyr::rbind.fill(occ_list) %>% as_tibble() %>% discard( ~ nrow(.x) == 0)
occ_list$datecollected <- paste(occ_list$year, occ_list$month, occ_list$day, sep = "-")
occ_list$datecollected <- lubridate::ymd(occ_list$datecollected)

# Save this occurrence database as csv
readr::write_csv(occ_list, file.path(dirs, "SPECIESLINK.csv")) # save as csv




## %######################################################%##
## %######################################################%##
## %######################################################%##
## %######################################################%##
## %######################################################%##
#                                                          #
####               Using bdc package for                ####
####   integrating and cleaning occurrences datasets    ####
#                                                          #
## %######################################################%##
## %######################################################%##
## %######################################################%##
## %######################################################%##
## %######################################################%##


## %######################################################%##
#                                                          #
####        bdc: Integrating different databases         ####
#                                                          #
## %######################################################%##

readr::write_csv(GBIF, file.path(GBIF, "GBIF.csv")) # save as csv

dirs <- file.path(getwd(), "raw_data")
dirs

# Extract year from date collected
BIEN$date_collected <- lubridate::ymd(BIEN$date_collected)
BIEN$year <- lubridate::year(BIEN$date_collected)

readr::write_csv(BIEN, file.path(dirs, "BIEN.csv")) # save as csv



BIEN = BIEN %>%select("scrubbed_species_binomial",	"latitude",	"longitude",	"datasource_id",		"date_collected",	"year",	"country",	"state_province",	"county",	"locality")

SPECIESLINK = occ_list %>%select("scientificName",	"decimalLatitude",	"decimalLongitude",	"catalogNumber",	"basisOfRecord",	"datecollected",	"year",	"country",	"stateProvince",	"county", "locality",	"identifiedBy",		"coordinatePrecision",	"recordedBy")

IDIGBIO <- occ_list1 %>%select("scientificname",	"geopoint.lat",	"geopoint.lon",	"uuid",		"datecollected", 	"year",	"country", 	"stateprovince",						"collector")

GBIF <- gbif  %>%select("scientificName",	"decimalLatitude",	"decimalLongitude",	"gbifID",	"basisOfRecord"	,"eventDate",	"year",	"countryCode", 	"stateProvince",		"locality", 	"identifiedBy",	"coordinateUncertaintyInMeters",		"recordedBy")



readr::write_csv(GBIF, file.path(dirs, "GBIF1.csv")) # save as csv
# WARNINGN!!!
### First copy and paste the "DatabaseInfo.txt" (our "configuration table")
### stored in "0_scripts_and_database/DatabaseInfo.txt" in the "./my_sdms/raw_data" folder


# Read the configuration table
metadata <- readr::read_tsv(file.path(dirs, "DatabaseInfo.txt"), show_col_types = FALSE)

# Let's list different database
list_db <- list.files(dirs, pattern = ".csv", full.names = TRUE)
nms <- list_db %>%
  basename() %>%
  gsub(".csv", "", .)
names(list_db) <- nms
list_db <- dplyr::tibble(datasetName = names(list_db), fileName = list_db)
list_db


# Correct the fileName (path file)
metadata$fileName <- NULL

# Merge databases
metadata <- dplyr::left_join(list_db, metadata, by = "datasetName")

# Standarize datasets
database <-
  bdc::bdc_standardize_datasets(
    metadata = metadata,
    format = "csv",
    overwrite = TRUE,
    save_database = TRUE
  )

# Note in the console bdc_standardize_datasets will inform the directory
# where the "Output" folder is stored
# In Output folder will be stored all the bdc package outputs
# e.g., C:/Users/my_computer/OneDrive/Output/Intermediate/00_merged_database.csv

# bdc_standardize_datasets created a system of folders in "Documents" folder if you are not working
# in an RStudio project.

# Let's view a portion of this integrated database (first 50 rows)
View(database[1:50, ])



## %######################################################%##
#                                                          #
####     Some data exploration to convince you why      ####
####     it is important to clean up your database      ####
#                                                          #
## %######################################################%##

# Let's read and explore the integrated raw database
dirs <- file.path(getwd(), "raw_data7")
dir2 <- file.path(getwd(), "Output/Intermediate/00_merged_database.csv")
database <- readr::read_csv(dir2, show_col_types = FALSE)
database

# Names used for searching occurrences
database$searchName %>% unique()

# Names returned by different databases
database$scientificName %>%
  unique() %>%
  sort()

# Are the names used for searching occurrences equal to the species names returned by different
# databes?

# Names of the countries. Are country names homogeneous and correct?
database$country %>%
  unique() %>%
  sort()

# Years
ggplot(database, aes(year)) +
  geom_histogram() +
  theme_bw()
table(!is.na(database$year)) # many data not have year and other are very old

# Patterns of occurrences
w <- sf::st_as_sf(map("world", plot = FALSE, fill = TRUE))
ggplot(w) +
  geom_sf() +
  geom_hex(data = database, aes(decimalLongitude, decimalLatitude), binwidth = c(10,10)) +
  scale_fill_viridis_c() +
  theme_bw() +
  theme(legend.position = "bottom")


# Keep 00_merged_database.csv file because it will be use in the 02_occ_data_cleaning.R
