##########################################################################
# EMPODAT_Pharmaceuticals_Surface_Waters_Database_Builder
#
# In this script the the NORMAN EMPODAT monitoring database file for surface water 
#   is imported, cleaned, filtered with pharmaceuticals databases and a list of European countries.
#
# FILTERS ARE APPLIED:
# 
#
#
# - INPUT FILES REQUIRED:
#
# 1. EMPODAT monitoring data files, obtained from the EMPODAT website at:
# https://www.norman-network.com/nds/empodat/downloadDCT.php 
#
#There are 10 input files in total, one for each of the following compartments:
# - air, biota, ground water, outdoors air, sediments, sewage sludge, soil, surface water, suspended matter and waste water.
#
#
# 2. Pharma_identifiers_drugbank_pubchem_V10_Rev52_17112022.csv
# (the output from the R Script "Article_57_pharma_import_and_clean_v10.R")
#
#
# - OUTPUTS:
# A cleaned database of monitoring data for surface waters. 
# Exported a tab-delimited files.
#
#
##########################################################################


#######Housekeeping#######
rm(list=ls()) #remove ALL objects 
cat("\014") # clear console window prior to new run
Sys.setenv(LANG = "en") #Let's keep stuff in English
Sys.setlocale("LC_ALL","English")
options(scipen=999) #avoid scientific annotation
##########################################################################
# Original author: Patrik Svedberg
# Edits: Francis Spilsbury


#Set version number:
vers <- 6


#Timestamp version
Revision <- "2022_06_22"


##########################
# Set working directory:
##########################

inwd <- "E:/GU/AA ARTICLE PHARMACEUTICALS IN EU/R/GitHub/R Input"
outwd <- "E:/GU/AA ARTICLE PHARMACEUTICALS IN EU/R/GitHub/R Output"
setwd(inwd)


##########################
# packages, imports, functions
##########################

packages<-c("tidyverse", "readxl", "webchem", "rvest", "xml2", "data.table", "urltools", "stringi")
lapply(packages, library, character.only=TRUE)




CAS_String_Convert = function(x) {
  # NB requires stringi package
  
  stri_sub(str = x, 
           from = (nchar(x) -2), 
           to = (nchar(x) -3)) = "-" # puts in he first dash
  stri_sub(str = x, 
           from = (nchar(x)), 
           to = 0) = "-" # puts in the second dash
  x
}


##########################
# Import data files
##########################

#The pharamceuticals from the EMA's Article 57 List:
art57 <- read.csv(paste0(inwd,"/Pharma_identifiers_drugbank_pubchem_V10_Rev52_17112022.csv"), 
                  sep = "\t", header = TRUE)


EMPODAT_surface_water <- as.data.frame(fread(file = paste0(inwd, "/NORMAN EMPODAT Raw Data/Surface_water_2022-06-20-125058.csv"), 
                                             header = FALSE))



##########################
# Tidy up:
##########################

#The column names are spread over the first 3 rows of the dataframe.
#Manual correction seems the easiest workaround:

colnames(EMPODAT_surface_water) <-   c('ID',                        #1
                                       'country',                   #2
                                       'sampling_site',             #3
                                       'national_code',             #4
                                       'relevant_EC_code_WISE',     #5
                                       'relevant_EC_code_other',    #6
                                       'other_code',                #7
                                       'longitude_east_west',       #8
                                       'longitude_deg',             #9
                                       'longitude_min',             #10
                                       'longitude_sec',             #11
                                       'longitude_decimal',         #12
                                       'latitude_north_south',      #13
                                       'latitude_deg',              #14
                                       'latitude_min',              #15
                                       'latitude_sec',              #16
                                       'latitude_decimal',          #17
                                       'precision_of_coordinates',  #18
                                       'altitude_meters',           #19
                                       'ecosystem_sample_matrix_surface_water', #20
                                       'ecosystem_sample_matrix_other', #21
                                       'chemical',                  #22
                                       'norman_susdat_ID',          #23
                                       'CAS',                       #24
                                       'InChiKey',                  #25
                                       'concentration_data',        #26
                                       'individual_concentration',  #27
                                       'concentration_value',       #28
                                       'concentration_unit',        #29
                                       'sampling_date_day',         #30
                                       'sampling_date_month',       #31
                                       'sampling_date_year',        #32
                                       'sampling_date_hour',        #33
                                       'sampling_date_minute',      #34
                                       'sampling_surface_water_fraction', #35
                                       'sampling_name_of_water_body', #36
                                       'sampling_river_basin_name', #37
                                       'sampling_river_km',         #38
                                       'sampling_proxy_pressures',  #39
                                       'sampling_type_of_depth_sampling', #40
                                       'sampling_depth_meters',     #41
                                       'sampling_surface_of_the_area_km2', #42
                                       'sampling_salinity_min_psu', #43
                                       'sampling_salinity_mean_psu',#44
                                       'sampling_salinity_max_psu', #45
                                       'sampling_spm_conc_mgl',     #46
                                       'sampling_pH',               #47
                                       'sampling_temperature_c',    #48
                                       'sampling_disolved_organic_carbon_mgl', #49
                                       'sampling_hardness_mgl_caco3', #50
                                       'sampling_remark',           #51
                                       'data_source_type',          #52
                                       'data_source_monitoring',    #53
                                       'data_source_other',         #54
                                       'data_source_title',         #55
                                       'data_source_organisation',  #56
                                       'data_source_email',         #57
                                       'data_source_laboratory',    #58
                                       'data_source_laboratory_ID', #59
                                       'data_source_references1',   #60
                                       'data_source_references2',   #61
                                       'data_source_author',        #62
                                       'QA_LoD_microgram_per_liter',#63
                                       'QA_LoQ_microgram_per_liter',#64
                                       'QA_LoQ_uncertainty_percent',#65
                                       'QA_coverage_factor',        #66
                                       'QA_sample_prep_method',     #67
                                       'QA_sample_prep_method_other',# 68
                                       'QA_analytical_method',      #69
                                       'QA_analytical_method_other',#70
                                       'QA_standard_analytical_method_code', #71
                                       'QA_standard_analytical_method_other', #72
                                       'QA_standard_analytical_method_number', #73
                                       'QA_method_validation_protocols', #74
                                       'QA_results_corrected_for_extraction_recovery', #75
                                       'QA_field_blank_checked',    #76
                                       'QA_lab_iso_17025_acc',      #77
                                       'QA_lab_analyte_acc',        #78
                                       'QA_lab_determinant_interlab_participation', #79
                                       'QA_lab_determinant_interlab_summary_of_performance', #80
                                       'QA_determinant_control_charts', #81
                                       'QA_data_controlled_by_competent_authority', #82
                                       'QA_remark')                 #83

  

#Delete first 3 rows that contain column name information
EMPODAT_surface_water <- EMPODAT_surface_water[4:nrow(EMPODAT_surface_water) , ]


##########################
# Filter for Article 57 pharmaceuticals
##########################

#CAS number contains unnecessary strings in the format (e.g.for ibuprofen) "CAS_RN: 15687-27-1"
EMPODAT_surface_water$CAS <- gsub("CAS_RN: ", "", EMPODAT_surface_water$CAS)


#Remove records not in the Article 57 CAS list
EMPODAT_surface_water <- subset(EMPODAT_surface_water, EMPODAT_surface_water$CAS %in% art57$CAS_number)



##########################
#EMPODAT Filter for EU countries
##########################

as.data.frame(EMPODAT_surface_water %>% group_by(country) %>% tally())

# - no non-EU countries in the dataset.

#Tidy up the country names
EMPODAT_surface_water$country <- gsub(", Republic Of", "", EMPODAT_surface_water$country)



###################
#EMPODAT DELETIONS
###################

as.data.frame(EMPODAT_surface_water %>% group_by(CAS)%>% tally())

#Remove anything with no CAS number:
EMPODAT_surface_water <- subset(EMPODAT_surface_water, !is.na(EMPODAT_surface_water$CAS))



#Remove unnecessary columns:
colnames(EMPODAT_surface_water)

Cols_to_Keep <- c("CAS",  
                  "chemical",
                  "InChiKey",
                  "ID",
                  "country",
                  "sampling_site",
                  "national_code",
                  "longitude_east_west",
                  "longitude_deg",
                  "longitude_min",                                     
                  "longitude_sec",
                  "longitude_decimal",                                 
                  "latitude_north_south",
                  "latitude_deg" ,                                     
                  "latitude_min",
                  "latitude_sec",                                      
                  "latitude_decimal",
                  "norman_susdat_ID",
                  "concentration_data",
                  "individual_concentration" ,                         
                  "concentration_value",
                  "concentration_unit",                                
                  "sampling_date_day",
                  "sampling_date_month" ,                              
                  "sampling_date_year",  
                  "sampling_name_of_water_body", 
                  "sampling_river_basin_name",
                  "QA_LoD_microgram_per_liter",
                  "QA_LoQ_microgram_per_liter")



#Remove unnecessary columns:
EMPODAT_surface_water <- EMPODAT_surface_water[ , c(colnames(EMPODAT_surface_water) %in% Cols_to_Keep)]


###################
#EMPODAT TIDY UP
###################

#Make everything that's a number into class numeric
EMPODAT_surface_water$latitude_decimal <- as.numeric(EMPODAT_surface_water$latitude_decimal)
EMPODAT_surface_water$latitude_deg <- as.numeric(EMPODAT_surface_water$latitude_deg)
EMPODAT_surface_water$latitude_min <- as.numeric(EMPODAT_surface_water$latitude_min)
EMPODAT_surface_water$latitude_sec <- as.numeric(EMPODAT_surface_water$latitude_sec)

EMPODAT_surface_water$longitude_decimal <- as.numeric(EMPODAT_surface_water$longitude_decimal)
EMPODAT_surface_water$longitude_deg <- as.numeric(EMPODAT_surface_water$longitude_deg)
EMPODAT_surface_water$longitude_min <- as.numeric(EMPODAT_surface_water$longitude_min)
EMPODAT_surface_water$longitude_sec <- as.numeric(EMPODAT_surface_water$longitude_sec)

EMPODAT_surface_water$QA_LoD_microgram_per_liter <- as.numeric(EMPODAT_surface_water$QA_LoD_microgram_per_liter)
EMPODAT_surface_water$QA_LoQ_microgram_per_liter <- as.numeric(EMPODAT_surface_water$QA_LoQ_microgram_per_liter)


###################
# EMPODAT MANUAL FIXES
###################

#Fix the artifacts in the river basin names by confirming that everything in is the latin alphabet:
EMPODAT_surface_water$sampling_river_basin_name <- iconv(EMPODAT_surface_water$sampling_river_basin_name, to = "latin1")
EMPODAT_surface_water$sampling_site <- iconv(EMPODAT_surface_water$sampling_site, to = "latin1")
EMPODAT_surface_water$concentration_unit <- iconv(EMPODAT_surface_water$concentration_unit, to = "latin1")


#Remove an artifact in the units column:
EMPODAT_surface_water$concentration_unit <- gsub("?", "", EMPODAT_surface_water$concentration_unit)
EMPODAT_surface_water$concentration_unit <- gsub("Â", "", EMPODAT_surface_water$concentration_unit)


###################
# EXPORT FILES
###################

write_delim(x = EMPODAT_surface_water, file = paste0(outwd, "/EMPODAT_pharma_surface_water_v", vers, "_rev", Revision, ".csv"),
            delim = "\t", escape = "none", quote = "none")

