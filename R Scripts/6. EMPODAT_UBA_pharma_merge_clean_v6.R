##########################################################################
# EMPODAT and UBA Pharmaceutical Databases Merge and Clean
#

# - INPUT FILES REQUIRED:
#
# 1. UBA_pharma_surface_water_v6_rev3_2021.csv
#   Output from the Script "UBA_pharma_database_builder_v6.R"
# 
#   A tab-delimited file that contains cleaned and filtered pharmaceutical
#   surface water monitoring data (EU only) from UBA (German Environmenatal Agency)
#
#
# 2.  EMPODAT_pharma_surface_water_v6_rev2022_06_22.csv
#   Output from the Script "EMPODAT_pharma_database_builder_v6.R"
#
#   A tab-delimited file that contains cleaned and filtered pharmaceutical
#   surface water monitoring data (EU only) from NORMAN-EMPODAT
#
#
#
# - OUTPUTS:
#  "Monitoring_pharma_EU_surface_water_v6_rev2022_06_22.csv"
#
#   A tab-delimited file. A clean, amalgamated database of UBA and EMPODAT monitoring data 
#   for pharmaceuticals is the EU.
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


##########################
# Import data files
##########################

#The surface water pharmaceutical monitoring data from UBA (German Environmental Agency)
UBA_Data <- read_delim(file = paste0(outwd, "/UBA_pharma_surface_water_v6_rev3_2021.csv"))


#The surface water pharmaceutical monitoring data from EMPODAT-NORMAN
EMPODAT_Data <- read_delim(file = paste0(outwd, "/EMPODAT_pharma_surface_water_v6_rev2022_06_22.csv"))


##########################
# Tidy up:
##########################

###########################################################
### Filter to remove any non-EU records
###########################################################

EU_Countries <- c("Austria",
                  "Belgium",
                  "Bulgaria",
                  "Croatia",
                  "Cyprus",
                  "Czech Republic",
                  "Denmark",
                  "Estonia",
                  "Faroe Islands",
                  "Finland",
                  "France",
                  "Germany", 
                  "Greece",
                  "Greenland",
                  "Hungary",
                  "Iceland",
                  "Ireland",
                  "Italy",
                  "Latvia",
                  "Luxembourg",
                  "Malta",
                  "Moldova",
                  "Netherlands",
                  "Norway",
                  "Poland",
                  "Portugal",
                  "Romania" ,
                  "Serbia" ,
                  "Slovakia"  ,
                  "Slovenia",
                  "Spain",
                  "Sweden",
                  "Switzerland"  )


data.frame(EMPODAT_Data %>% group_by(country) %>% tally())
EMPODAT_Data <- subset(EMPODAT_Data, EMPODAT_Data$country %in% EU_Countries)


data.frame(UBA_Data %>% group_by(Sampling_Country) %>% tally())
UBA_Data <- subset(UBA_Data, UBA_Data$Sampling_Country %in% EU_Countries)



###########################################################
### Filter to remove UBA Records already in NORMAN - EMPODAT
###########################################################

as.data.frame(UBA_Data %>% group_by(UBA_Data$Literature_Citation) %>% tally())

#remove all Monitoring data values for compounds with the literature source cited as NORMAN (16917 entries)
UBA_Data <- subset(UBA_Data, UBA_Data$Literature_Citation != "NORMAN (2012)")



###########################################################
### MERGE UBA and EMPODAT
###########################################################

colnames(UBA_Data)
colnames(EMPODAT_Data)

#Fields that are common to both databases holding equivalent information:
#UBA                                  #EMPODAT
#[1] "Name_of_Analyte"                [15]  "chemical"  
#[2] "ID"                             [1]   "ID" 
#[3] "Therapeutic_Group"             
#[4] "Target_group"                  
#[5] "Type_of_Analyte"               
#[6] "Matrix"                        
#[7] "Sampling_Location"              [3]   "sampling_site"         
#[8] "Sampling_Description"           [16]  "norman_susdat_ID"
#[9] "Sampling_Period_Start"         
#[10] "Sampling_Period_End"           [31]  "sampling_year"
#[11] "Sampling_Country"              [2]   "country"
#[12] "Sampling_Province"             [27]  "sampling_river_basin_name"
#[13] "UN_Region"                     
#[14] "Statistics"                    
#[15] "Number_of_Samples_Analysed"    
#[16] "Detection"                     [20] "individual_concentration"      
#[17] "MEC_original"                                  
#[18] "Unit_original"                  
#[19] "MEC_standardized"              [21] "concentration_value"
#[20] "Unit_standard"                 [22] "concentration_unit"
#[21] "LoD_Limit_of_Detection"        
#[22] "Unit_LoD"                      
#[23] "LoD_standardized"              [28] "QA_LoD_microgram_per_liter"       
#[24] "Unit_LoD_standardized"         
#[25] "Emission_Source"               
#[26] "Literature_Citation"           
#[27] "Literature_Author/Organisation"
#[28] "Literature_Language"           
#[29] "Literature_Type"               
#[30] "Literature_Availability"       
#[31] "Literature_Credibility"        
#[32] "Data_Entry_By"                 
#[33] "Name_of_Analyte_Original"      
#[34] "Analyte_Name_German"           
#[35] "Therapeutic_Group_English"     
#[36] "Therapeutic_Group_German"      
#[37] "Target_Group"                  
#[38] "Pharma_Parent_Transformation"  
#[39] "PharmaATC1"                    
#[40] "PharmaATC2"                    
#[41] "PharmaATC3"                    
#[42] "PharmaATC4"                    
#[43] "PharmaATC5"                    
#[44] "PharmaATC6"                    
#[45] "PharmaATC7"                    
#[46] "PharmaATC8"                    
#[47] "PharmaATC9"                    
#[48] "PharmaATC10"                   
#[49] "PharmaATC11"                   
#[50] "Alt_CAS_Number"                
#[51] "CAS_number"                    [1]   "CAS"



#Re-name columns from EMPODAT with equivalent data in UBA:
EMPODAT_Data <- rename(EMPODAT_Data, CAS_number = CAS)
EMPODAT_Data <- rename(EMPODAT_Data, Name_of_Analyte = chemical)
EMPODAT_Data <- rename(EMPODAT_Data, Sampling_Location = sampling_site)
EMPODAT_Data <- rename(EMPODAT_Data, Sampling_Description = norman_susdat_ID)
EMPODAT_Data <- rename(EMPODAT_Data, Sampling_Period_End = sampling_date_year)
EMPODAT_Data <- rename(EMPODAT_Data, Sampling_Country = country)
EMPODAT_Data <- rename(EMPODAT_Data, Sampling_Province = sampling_river_basin_name)
EMPODAT_Data <- rename(EMPODAT_Data, Detection = individual_concentration)
EMPODAT_Data <- rename(EMPODAT_Data, MEC_standardized = concentration_value)
EMPODAT_Data <- rename(EMPODAT_Data, Unit_standard = concentration_unit)
EMPODAT_Data <- rename(EMPODAT_Data, LoD_standardized = QA_LoD_microgram_per_liter)



#Add some missing information to EMPODAT that we'd like to retain in UBA:
EMPODAT_Data$Therapeutic_Group_English <- NA
EMPODAT_Data$Target_Group <- NA


#Add a "U" to the ID for UBA data:
UBA_Data$ID <- paste0("U", UBA_Data$ID)

#Add an "E" to the ID for EMPODAT data:
EMPODAT_Data$ID <- paste0("E", EMPODAT_Data$ID)

        
#Delete non-matching columns from both databases:   

Cols_Keep <- c("CAS_number",
               "Name_of_Analyte",
               "ID",
               "Sampling_Location",
               "Sampling_Description",
               "Sampling_Period_End",
               "Sampling_Country",
               "Sampling_Province",
               "Detection",
               "MEC_standardized" ,
               "Unit_standard",
               "LoD_standardized",
               "Therapeutic_Group_English",
               "Target_Group")

EMPODAT_Data <- EMPODAT_Data[ , Cols_Keep]
UBA_Data <- UBA_Data[ , Cols_Keep]


###########################################################
### MERGE
###########################################################

#Put the datasets together, using a simple rbind:
Monitoring_data <- rbind(EMPODAT_Data, UBA_Data)



#######################################
# CHECK FOR DUPLICATES
#######################################

dummy <- unique(Monitoring_data)
#dataframes are the same size.



###########################################################
### EXPORT
###########################################################

write_delim(x = Monitoring_data, file = paste0(outwd, "/Monitoring_pharma_EU_surface_water v", vers, "_Rev", Revision, ".csv"),
            delim = "\t", escape = "none", quote = "none")
