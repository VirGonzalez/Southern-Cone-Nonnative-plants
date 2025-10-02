##%######################################################%##
#                                                          #
####         Package installation ENM and SDM           ####
#                                                          #
##%######################################################%##

# by: Santiago J.E. Velazco <sjevelazco@gmail.com>

# Download and install Rtools 4.2 from 
# https://cran.r-project.org/bin/windows/Rtools/ or 
# https://www.r-project.org/nosvn/winutf8/ucrt3/.


# Codes for installing package from CRAN:
install.packages("BIEN")
install.packages("remotes")
install.packages("maps")
install.packages("readr")
install.packages("rgbif")
install.packages("ridigbio")
install.packages("rinat")
install.packages("rnaturalearth")
install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
install.packages("rworldmap")
install.packages("sf")
install.packages("terra")
install.packages("tidyverse")


# Codes for installing package from GitHub:
install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
remotes::install_github("ropensci/rnaturalearthdata")
remotes::install_github("ropensci/rgnparser", force = TRUE)
rgnparser::install_gnparser() 
# If you have troubles usin install_gnparser() use the load and run the function 
# install_gnparser2() stored in "0_scripts_dataset/install_gnparser2.R"
paste0(Sys.getenv("APPDATA"), "\\gnparser") #open this directory to check if gnparser was installed.
# There should be a folder named gnparser and the file gnparser.exe inside it
# The line below are for windows users ;)
paste0(Sys.getenv("APPDATA"), "\\gnparser") %>% shell.exec() 

# It is very likely that if you are using Mac OS operating system, you will have trouble installing the 
# “gnparser” package. In case you have this trouble see the help at 
# https://github.com/gnames/gnparser#install-with-homebrew

remotes::install_github("brunobrr/bdc", force = TRUE)
remotes::install_github("liibre/rocc")
remotes::install_github("sjevelazco/flexsdm")
remotes::install_github("andrefaa/ENMTML")
