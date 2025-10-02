## %######################################################%##
#                                                          #
####              Occurrence data cleaning              ####
#                                                          #
## %######################################################%##
# Written by: Santiago J.E. Velazco, M. Brooke Rose, & Janet Franklin
# Several comments in this script came from the bdc website (https://github.com/brunobrr/bdc)

# This script aims to use the 'bdc' package for prefiltering an occurrence database (i.e.,
# flagging and removing invalid or non-interpretable information) and performing geographical,
# taxonomical, and temporal corrections.

###### Packages
require(dplyr) # Manipulate data
require(readr) # Read and writ data
require(bdc) # Biodiversity data cleaning
require(ggplot2) # Plot data
require(sf) # For handling spatial data
require(maps) # A spatial database of country boundaries
require(rnaturalearth)
require(rnaturalearthdata)

# Let's remove all object of you R session
rm(list = ls()) # code for deleting all R object

## %######################################################%##
#                                                          #
####                     PRE-FILTER                     ####
#                                                          #
## %######################################################%##

# The Pre-filter module contains a series of tests to detect, remove, and,
# whenever, possible, correct such erroneous or suspect records

dir2 <- file.path(getwd(), "Output/Intermediate/01_check_pf_country.csv")
database <- readr::read_csv(dir2, show_col_types = FALSE)


## %#######################################################################%##
## %##      IMPORTANT: # The results of the VALIDATION test              ##%##
## %##         used to flag data quality are appended in                 ##%##
## %##          separate columns in this database and                    ##%##
## %##     retrieved as TRUE (ok) or FALSE (check carefully).            ##%##
## %#######################################################################%##



# 1 - Records with missing species names
check_pf <-
  bdc_scientificName_empty(
    data = database,
    sci_name = "scientificName"
  )


check_pf$.scientificName_empty %>% table() # this is the new column for missing species names (TRUE or FALSE)

# 2 - Records lacking information on geographic coordinates
# This is a VALIDATION. Flag records missing partial or complete information on geographic coordinates
# will be flagged as FALSE.
check_pf <- bdc_coordinates_empty(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude"
)

check_pf$.coordinates_empty %>% table() # this is the new column for flagging incomplete/missing coordinates (TRUE or FALSE)


# 3 - Records with out-of-range coordinates
# This is a VALIDATION. This test flags records with out-of-range coordinates:
# latitude > 90 or -90; longitude >180 or -180 (i.e. geographically impossible coordinates)
check_pf <- bdc_coordinates_outOfRange(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude"
)

check_pf$.coordinates_outOfRange %>% table()

# 4 - Records from doubtful sources
# This is a VALIDATION. This test flags records from doubtful source. For example, records from
# drawings, photographs, or multimedia objects, fossil records, among others.
check_pf <- bdc_basisOfRecords_notStandard(
  data = check_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all"
)

check_pf$.basisOfRecords_notStandard %>% table()

check_pf %>% # if we explore the frequency of different sources we get this
  dplyr::group_by(basisOfRecord) %>%
  dplyr::summarise(n = dplyr::n())


# 5 - Getting country names from valid coordinates
# This is an ENRICHMENT function because it improves the database by deriving country names
# based on coordinates for records with missing country names.

check_pf <- bdc_country_from_coordinates(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country"
)


check_pf$country
check_pf$country %>% unique()

# 6 - Standardizing country names and getting country code information
# This is an ENRICHMENT function because the country names are standardized against
# a list of country names in several languages retrieved from Wikipedia.
# 
check_pf =  check_pf[ , -which(names(check_pf) %in% c("country_suggested","countryCode"))]

check_pf <- bdc_country_standardized(
  data = check_pf,
  country = "country"
)

# original messy country names
check_pf$country %>%
  unique() %>%
  sort()
# corrected and standardized country names
check_pf$country_suggested %>%
  unique() %>%
  sort()
# It is good to check the names to be sure that the corrected country names were correctly matched
ctrn <- check_pf %>%
  dplyr::select(country_suggested, country) %>%
  unique() %>%
  arrange(country_suggested)
View(ctrn)

check_pf$country_suggested[check_pf$country == "BO"] <- "Bolivia"
check_pf$country_suggested[check_pf$country == "PE"] <- "Peru"
check_pf$country_suggested[check_pf$country == "CL"] <- "Chile"
check_pf$country_suggested[check_pf$country == "BR"] <- "Brasil"
check_pf$country_suggested[check_pf$country == "VE"] <- "Venezuela"
check_pf$country_suggested[check_pf$country == "V�n�zu�la"] <- "Venezuela"
check_pf$country_suggested[check_pf$country == "CO"] <- "Colombia"
check_pf$country_suggested[check_pf$country == "PY"] <- "Paraguay"
check_pf$country_suggested[check_pf$country == "EC"] <- "Ecuador"
check_pf$country_suggested[check_pf$country == "AR"] <- "Argentina"
check_pf$country_suggested[check_pf$country == "Guyane française"] <- "Guyana"

# 7 - Correcting latitude and longitude that have been "transposed"
# The mismatch between the country listed and coordinates can be the result of transposed
# signs (+ -) or coordinates. Once a mismatch is detected, different coordinate transformations are made
# to correct the country and coordinates mismatch. Verbatim coordinates are then replaced by the
# rectified ones in the returned database (a database containing verbatim and corrected
# coordinates can be created in the Output folder if save_outputs = TRUE). Records near
# countries coastline are not tested to avoid false positives.
check_pf %>%
  readr::write_csv(
    .,
    file.path(getwd(), "Output", "Intermediate", "01_check_pf_country.csv")
  )
dir2 <- file.path(getwd(), "Output/Intermediate/01_check_pf_country.csv")
check_pf <- readr::read_csv(dir2, show_col_types = FALSE)


check_pf <-
  bdc_coordinates_transposed(
    data = output,
    id = "database_id",
    sci_names = "scientificName",
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    country = "country_suggested",
    countryCode = "countryCode",
    border_buffer = 0.1, # in decimal degrees (~11 km at the equator)
    save_outputs = FALSE
  )
check_pf$coordinates_transposed %>% table()


# 8 - Records outside a region of interest
# This is a VALIDATION function because records outside one or multiple reference countries
# (i.e., records in other countries) or at an odd distance from the coast
# (e.g., in the ocean). This last step avoids flagging records close to country
# limits (e.g., records of coast or marshland species) as invalid.

# bdc_coordinates_country_inconsistent needs a vector with countries to be tested.
# In this case all countries; however, we can test only for countries where our species are native
# for instance, using Argentina, Brazil, Paraguay, and Bolivia.
cntr <- check_pf$country_suggested %>%
  unique() %>%
  na.omit() %>%
  c()
cntr

# Just for this example lets test only those countries were both species used here naturally distribute
cntr <- c(
  "Argentina",
  "Paraguay",
  "Brasil",
  "Brazil",
  "Peru",
  "Chile",
  "Bolivia",
  "Colombia",
  "Ecuador",
  "Venezuela",
  "Paraguay",
  "Uruguay"
)

check_pf <- subset(check_pf, country_suggested %in% cntr )

# Note that in this step we can test
check_pf <- bdc_coordinates_country_inconsistent(
  data = check_pf,
  country = "country_suggested",
  country_name = cntr,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  dist = 0.1 # in decimal degrees (~5 km at the equator)
)

# 9 - Identifying records not geo-referenced but containing locality information
# ENRICHMENT. Coordinates can be derived from a detailed description of the locality associated with
# records in a process called retrospective geo-referencing.
xyFromLocality <- bdc_coordinates_from_locality(
  data = check_pf,
  locality = "locality",
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  save_outputs = TRUE # This is for saving the output
)
xyFromLocality[, c("country", "stateProvince", "county", "locality")]
# Note that this create a new database with coordinates for records that were geo-referenced


##### Report #####
# Here we create a column named '.summary' summing up the results of all VALIDATION tests
# (those starting with '.'). This column is FALSE when a record is flagged as FALSE in any
# data quality test (i.e. potentially invalid or suspect record).

check_pf <- bdc_summary_col(data = check_pf)
?bdc_summary_col

# Creating a report summarizing the results of all tests.
report <-
  bdc_create_report(
    data = check_pf,
    database_id = "database_id",
    workflow_step = "prefilter",
    save_report = TRUE # For saving report
  )

report

length(unique(check_pf$scientificName))
# Figures
# Here we create figures (bar plots and maps) to make the interpretation of the results of
# data quality tests easier. See some examples below.

figures <-
  bdc_create_figures(
    data = check_pf,
    database_id = "database_id",
    workflow_step = "prefilter",
    save_figures = TRUE # For saving figures
  )

# Check figures using
figures


# Filtering the database
# We can remove records flagged as erroneous or suspect to obtain a cleaner database.
# We can use the column .summary to filter valid records
# passing in all tests (i.e., flagged as TRUE). Next, we use
# the bdc_filter_out_flags function to remove all tests columns (that is, those starting with '.').

output <-
  check_pf %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all")

# Saving the database
# You can use qs::qread() instead of write_csv to save a large database in a compressed format

output %>%
  readr::write_csv(
    .,
    file.path(getwd(), "01_prefilter_database.csv")
  )


## %######################################################%##
#                                                          #
####                      TAXONOMY                      ####
#                                                          #
## %######################################################%##

# Little function for extracting genus and species from cleaned species names
get_gs <- function(x) {
  x <- stringr::str_split_fixed(x, " ", 3)[, 1:2]
  x <- paste(x[, 1], x[, 2])
  return(x)
}

# Read the prefiltered database
db <-
  readr::read_csv(file.path(getwd(), ("Output/Intermediate/01_prefilter_database.csv")))


db$scientificName %>%
  unique() %>%
  sort() # Many different names even other species


##### 1) Clean and parse species names


# In case a large database is being used with many species could be more efficient
# use a vector with unique elements (i.e., species names)
# and then merge it with the original database.
# This step is important for helping update specie names (see next function)
db <- subset(db, taxonRank != "FAMILY")



parse_names <-
  bdc_clean_names(
    sci_names = unique(db$scientificName), # unique of species names
    save_outputs = TRUE
  )

parse_names # note that we have names_clean column


# scientificName: original names supplied
# .uncer_terms: indicates the presence of taxonomic uncertainty terms (e.g., sp., aff., affin.)
# .infraesp_names: indicates the presence of infraspecific terms
# name_clean: scientific names resulting from the cleaning and parsing processes
# quality: an index indicating the quality of parsing process. It ranges from 0 to 4,
# being 1 no problem detected, 4 serious problems detected; a value of
# 0 indicates no interpretable name that was not parsed).

check_taxonomy <- dplyr::left_join(db, parse_names, by = "scientificName")
check_taxonomy

check_taxonomy$scientificName %>% unique() # raw names
check_taxonomy$names_clean %>% unique() # now the names are cleaned and prepare to be checked
# and updated in a taxonomic authority database
# Note that despite species names being corrected, several names could be or not
# synonyms


##### 2) Names standardization
# See the taxonomic database available until now in bdc in:
# https://brunobrr.github.io/bdc/articles/taxonomy.html
# In this case we will use GBIF as taxonomic authority
?bdc_query_names_taxadb


# Función para contar palabras en una celda
library(stringr)
count_words <- function(x) {
  str_count(x, "\\w+")
}
check_taxonomy <- check_taxonomy %>%
  filter(count_words(names_clean) >= 2)

spl <- unique(check_taxonomy$names_clean) %>% sort() # list of raw species names


# If you have trouble with "bdc_query_names_taxadb" function see this issue about "duckbd"
# https://github.com/brunobrr/bdc/issues/233
# Run fs::dir_delete(taxadb:::taxadb_dir())  and the try use the function again

query_names <- bdc_query_names_taxadb(
  sci_name            = spl,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "ott", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = FALSE, # should parallel processing be used?
  ncores              = 2, # number of cores to be used in the palatalization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)

View(query_names)


#####estandarizar por liepzig catalogo de plantas
df_filtered <- check_taxonomy %>%
  filter(count_words(names_clea) >= 2)

devtools::install_github("idiv-biodiversity/lcvplants")
library(lcvplants)
query_names <- lcvp_fuzzy_search(spl, 
                  max_distance = 0.9,keep_closest = T,  status = c("accepted", "synonym"),
                  bind_result = T)

#REMUEVO VALORES DE CELDA NO BINOMIALES CON LA LISTA



parse_names1 <- parse_names[!(parse_names$names_clean %in% nom), ]


# This step is crucial, so it is recommended to check all names
# and update it by hand in case you need it. For instance, we can save the
# query_names database and manipulate it in a spreadsheet. For example, you could
# update the following columns by hand, if necessary:
# scientificName, taxonomicStatus, family, genus, specificEpithet, and infraspecificEpithet

# Additionally, there are three important worldwide taxonomic databases for plants that
# were not implemented in bdc

#### LCVP -Leipzig catalogue of vascular plants-
# data available at: https://idata.idiv.de/ddm/Data/ShowData/1806
# R package: https://idiv-biodiversity.github.io/lcvplants/index.html

#### WFO -World Flora Online-
# Website: http://www.worldfloraonline.org
# R package: https://pubmed.ncbi.nlm.nih.gov/33014632/
# WFO database: http://www.worldfloraonline.org/downloadData

#### POWO -Plants of the World Online-
# Website: https://powo.science.kew.org

# Defining a taxonomic authority is essential because it will determine the names
# of your species and consequently, the number of species (e.g., in case you
# are working with an entire family), and the analysis derived of these points
# (e.g., points used for SDM or for calculating Extent of Occurrence). In case
# you are working only with occurrences and species within a given country,
# you can decide to use the official taxonomic authority of this country
# (for instance, Flora do Brazil 2020 - http://floradobrasil.jbrj.gov.br/ - or United States Department of Agriculture -https://plants.usda.gov/home -).
# And remember to be explicit in your manuscript which taxonomic authority you used!


# In this example we will add the accepted names for the three names not found in GBIF
# Another way is to save the "query_names" database and and edit it in an Excel,
# read it in R, and integrate with your original database.

# We can see that for two names ("Chorisia chodatii", "Peltophorum adnatum",
# "Peltophorum berteroanum") GBIF database did not find their accepted names.
# However, if we check those names in Plant of The World Online we see that they
# are existing species names
# https://powo.science.kew.org/results?q=Chorisia%20chodatii
# https://powo.science.kew.org/results?q=Peltophorum%20adnatum
# https://powo.science.kew.org/results?q=%20Peltophorum%20berteroanum


query_names[query_names$original_search == "Jatropha curcas coriacea", "scientificName"] <- "Jatropha curcas"
query_names[query_names$original_search == "Eucalyptus crebra populnea", "scientificName"] <- "Eucalyptus crebra"
query_names[query_names$original_search == "Eucalyptus saligna e. robusta", "scientificName"] <- "Eucalyptus saligna"



##### 3) Merging results of accepted names with the original database
# Merging results of the taxonomy standardization process with the original database.
# See bdc package tutorial how merging from bdc_query_names_taxadb output

# A columns will be added with the final accepted names
# accepted_scientificName_2: only genus and species (Peltophorum dubium)

# Lets select some columns to reduce this database
query_names2 <- query_names %>% select(Input.Genus, Input.Epitheton, Status,
  accepted_scientificName_2 = Output.Taxon
)

# Our get_gs function will be used for extracting genus and species
query_names2$accepted_scientificName <-
  get_gs(query_names2$accepted_scientificName_2)

# rearranging columns
query_names2 <- query_names2 %>% dplyr::relocate( accepted_scientificName, accepted_scientificName_2)

query_names2
# View(query_names2)

# Merge the original database with updated names database
check_taxonomy_2 <-
  check_taxonomy %>%
  dplyr::left_join(., query_names2,
    by = c("names_clean" = "accepted_scientificName")
  )

# Now the occurrence database has the information about taxonomy of original names
check_taxonomy_2 %>%
  dplyr::select(database_id, scientificName, accepted_scientificName_2)
# Remember "searchName" is the cleaned original name of records
# accepted_scientificName has corrected and updated names only
# accepted_scientificName_2 has corrected and updated names with genus, species and subspecies


check_taxonomy_2 %>%
  dplyr::select(scientificName, accepted_scientificName_2) %>%
  unique()

check_taxonomy_2 %>%
  dplyr::select(scientificName, accepted_scientificName) %>%
  unique()


# Report
# The report is based on the column notes containing the results of the name
# standardization process. The notes can be grouped into two categories: accepted
# names and those with a taxonomic issue or warning, needing further inspections.
# can be automatically saved if save_report = TRUE.

report <-
  bdc_create_report(
    data = check_taxonomy_2,
    database_id = "database_id",
    workflow_step = "taxonomy",
    save_report = TRUE
  )

report

# Unresolved names
# It is also possible to filter out records with a taxonomic status that is different
# from "accepted". Such records may be potentially resolved manually.
unresolved_names <-
  bdc_filter_out_names(
    data = check_taxonomy_2,
    col_name = "notes",
    taxonomic_status = c("accepted",  "homotypic synonym", "accepted | replaceSynonym"),
    opposite = TRUE
  )

unresolved_names # In this case no names were unresolved

# Save the table containing unresolved names
unresolved_names %>%
  readr::write_csv(., file.path(getwd(),"Output/Check/02_unresolved_names.csv"))


# Filtering the database
# It is possible to remove records with unresolved or invalid names to get a 'clean' database.
# However, to ensure that all records will be evaluated in all the data quality tests
# (i.e., tests of the taxonomic, spatial, and temporal modules of the package),
# potentially erroneous or suspect records will be removed in the final module of the package.

check_taxonomy_final <-
  bdc_filter_out_names(
    data = check_taxonomy_2,
    col_name = "notes",
    taxonomic_status = c("accepted", "homotypic synonym", "accepted | replaceSynonym"), # here we assume that the accepted names of synonym original names were correctly updated.
    opposite = FALSE
  )



# But as we can see in our database, we have have points for other species
# and we only want to work with "Peltophorum dubium" and "Ceiba chodatii"
check_taxonomy_final$accepted_scientificName %>% unique()

nrow(check_taxonomy_2)
nrow(check_taxonomy_final) # how many records were removed?


# Remove columns .uncer_terms and .infraesp_names for starting SPATIAL
# cleaning without any flag and taxonomical cleaned
check_taxonomy_final <- check_taxonomy_final %>% dplyr::select(-starts_with("."))

check_taxonomy_2 %>%
  readr::write_csv(
    .,
    file.path(getwd(),"02_taxonomy_database.csv")
  )

rm(list = ls()) # code for deleting all R object



## %######################################################%##
#                                                          #
####                       SPACE                        ####
#                                                          #
## %######################################################%##


db <- readr::read_csv(file.path(getwd(), "Output/Intermediate/02_taxonomy_database.csv"))

# 1) Flagging common spatial issues
# This function identifies records with a coordinate precision below a specified number
# of decimal places. For example, the precision of a coordinate with 1 decimal place is
# 11.132 km at the equator, i.e., the scale of a large city.

# The precision depends on the use of the data, for instance, in the
# context of species distribution models a higher precision is needed if
# you are using high resolution environmental variables

check_space <-
  bdc_coordinates_precision(
    data = db,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    ndec = 3 # number of decimals to be tested
  )

table(check_space$.rou) # 740 records have < 3 decimals

check_space %>%
  dplyr::filter(!.rou) %>%
  dplyr::select(starts_with("decimal"))


# 2) flag common spatial issues using functions of the package CoordinateCleaner.
?clean_coordinates

# the process of spatial cleaning is processed by species, therefor it is important
# the database has a column with cleaned and updated species names
# and is important define the taxonomic level desired to work (i.e. species or subspecies)
check_space <-
  CoordinateCleaner::clean_coordinates(
    x = db,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName", # Species names with genus and species
    countries = ,
    tests = c(
      "duplicates", # duplicated records
      "zeros" # records with coordinates 0,0
      # "urban"         # records within urban areas
    ),
    zeros_rad = 0.5,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>%
  dplyr::tibble()


check_space <- CoordinateCleaner::cc_dupl(
  x = db,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "accepted_scientificName_2", # Species names with genus and species
  additions = c("recordedBy", "verbatimEv"),
  value = "clean",
  verbose = TRUE
)%>%
  dplyr::tibble()



check_space%>% readr::write_csv(., file.path("space_database.csv"))

# The '.summary' column summing up the results of all tests. Here, we update it
# to integrate results of bdc_coordinates_precision and clean_coordinates
check_space$.summary %>% table()
check_space <- bdc_summary_col(data = check_space)
check_space$.summary %>% table()

colSums(!dplyr::select(check_space, starts_with(".")))

# Explore all error patterns
check_space %>%
  # dplyr::filter(.summary == FALSE) %>% # map only records flagged as FALSE
  bdc_quickmap(
    data = .,
    lon = "decimalLon",
    lat = "decimalLat",
    col_to_map = ".summary",
    size = 0.9
  ) +
  facet_grid(.summary ~ .)


# Georeferenced on institutions
check_space %>%
  dplyr::filter(.inst == FALSE) %>% # map only records flagged as FALSE
  bdc_quickmap(
    data = .,
    lon = "decimalLon",
    lat = "decimalLat",
    col_to_map = ".inst",
    size = 0.9
  )

# Records with duplicated coordinates
check_space %>%
  dplyr::filter(.dpl == FALSE) %>% # map only records flagged as FALSE
  bdc_quickmap(
    data = .,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    col_to_map = ".dpl",
    size = 0.9
  )


# 3) Figures
figures <-
  bdc_create_figures(
    data = check_space,
    database_id = "database_id",
    workflow_step = "space",
    save_figures = TRUE
  )

# Check figures using
figures$.rou
figures$.cen
figures$.inst
figures$.dpl
figures$.summary


# 4) Filtering database
check_space_final <-
  check_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all")

nrow(check_space)
nrow(check_space_final)


check_space %>%
  readr::write_csv(
    .,
    file.path(getwd(), "space_database.csv")
  )

# rm(list=ls()) # code for deleting all R object


## %######################################################%##
#                                                          #
####                        TIME                        ####
#                                                          #
## %######################################################%##

# time cleaning will depend of the use of the database. For instance,
# in species distribution model is not recommended use data from all time periods (i.e., <1950)
# because they could have low geographical precision

db <- readr::read_csv(file.path(getwd(), "Output/Intermediate/03_space_database.csv"))

db$year %>% hist() # seems to be very old records in the database
db$year %>% range(., na.rm = T)


# 1) Records lacking event date information
# VALIDATION. This  function flags records lacking event date information (e.g., empty or NA).

check_time <-
  bdc_eventDate_empty(data = db, eventDate = "verbatimEventDate") %>% tibble()

# data without a year or date may or may not be removed, depending on what the
# database is used for.
table((check_time$.eventDate_empty))

# In this tutorial records without year will be retained
check_time$.eventDate_empty <- NULL

# 2) Records with out-of-range collecting year
# VALIDATION. This function identifies records with illegitimate or potentially imprecise # collecting years. The year provided can be out-of-range (e.g., in the future) or
# collected before a specified year supplied by the user (e.g., 1950).
check_time <-
  bdc_year_outOfRange(
    data = check_time,
    eventDate = "year",
    year_threshold = 1950
  )



# Report
check_time <- bdc_summary_col(data = check_time)
report <-
  bdc_create_report(
    data = check_time,
    database_id = "database_id",
    workflow_step = "time",
    save_report = FALSE
  )

report

# Figures
figures <-
  bdc_create_figures(
    data = check_time,
    database_id = "database_id",
    workflow_step = "time",
    save_figures = FALSE
  )

# Check figures using
figures

# 3) Saving a "raw" database
check_time %>%
  readr::write_csv(
    .,
    file.path(getwd(), "Output", "Intermediate", "04_time_raw_database.csv")
  )


cleaned_database <-
  check_time %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all") %>%
  dplyr::tibble()



## %######################################################%##
#                                                          #
####          FINALLY we have a database that           ####
####         is taxonomically, geographically,          ####
####               and temporally cleaned               ####
#                                                          #
## %######################################################%##

cleaned_database %>%
  readr::write_csv(
    .,
    file.path(getwd(), "Output", "Intermediate", "05_cleaned_database.csv")
  )


rm(list=ls()) # code for deleting all R object

# Keep 05_cleaned_database.csv file because it will be use in the 03_occ_outliers_figures_shapefiles.R
