###################################
# 
# ECOTOX PHARMA DATABASE BUILDER
#
###################################

# This script takes an amalgamated ECOTOX.Rda file  (output from AquireConverter_8.R) and
# - adds supplementary information
# - cleans up the data
# - converts units 
# - removes duplicates
#
# Adds supplementary information:
# - Adds Molecular weight data for all chemicals
# - Adds common name data for all species
# - Adds a marker for Reliability for each toxicity data point
# - Concatenates all warnings into a single field ("Comments")
#
# Data cleanup
# - Removes any data included on the EMA's Article 57 List
# - Removes any data not in the aquatic compartment
#
# Data conversion
# - Where possible, time durations converted to days
# - Where possible, concentrations converted to umol
#
#
# ---- INPUT FILES ----
# - "ECOTOX.Rda": flat file of the Aquire database
# (output from ECOTOX_Aquire_Converter_v8.R)
# 
# - "Mol_Wts_for_ECOTOX_v8_Rev_15.csv": 
# (molecular weights for all compounds sourced from PubChem and ChemSpider, as far as available.)
#
#
# ---- OUTPUT FILES ----
# "ECOTOX_pharma_clean_v14_Rev13_12_15_2022.csv" 
# (Tab-delimited Excel File)
#
###################################


##############################################
# Standard Tasks at the beginning
##############################################

rm(list=setdiff(ls(envir=.GlobalEnv), c("ECOTOX")), envir = .GlobalEnv)
Sys.setenv(LANG = "en") #Let's keep stuff in English
Sys.setlocale("LC_ALL","English")
cat("\014") # clear console window prior to new run

##############################################
# Version Number
##############################################
vers<-14

##############################################
# timestamp of the ECOTOX database
##############################################
database_timestamp<-"12_15_2022"
timestamp_version <- "13"

##############################################
# Set constants
##############################################


##########################
# Set working directory:
inwd <- "E:/GU/AA ARTICLE PHARMACEUTICALS IN EU/R/GitHub/R Input"
outwd <- "E:/GU/AA ARTICLE PHARMACEUTICALS IN EU/R/GitHub/R Output"
setwd(inwd)


################
# packages, imports, functions
################

require(tidyverse)
require(stringr)
require(readxl)
require(writexl)
require(webchem)
require(dplyr)


CAS_String_Convert = function(x) {
  stri_sub(str = x, 
           from = (nchar(x) -2), 
           to = (nchar(x) -3)) = "-" # puts in he first dash
  stri_sub(str = x, 
           from = (nchar(x)), 
           to = 0) = "-" # puts in the second dash
  x
}



Records_Removed <- function(descriptor_string, Before_Count, After_Count) {
  
  Message_String <- paste0(descriptor_string, ": ",(Before_Count-After_Count), " Records removed (", round((100*(Before_Count-After_Count)/Before_Count), digits = 2), "%)")
  
  return(Message_String)
}


###############
# Import Files
###############

# Molecular weights (missings marked as -7777)
Chemical_MW <- as.data.frame(read.csv(paste0(inwd,"/Mol_Wts_for_ECOTOX_v8_Rev_15.csv"), 
                                      sep = "\t", 
                                      header = TRUE, 
                                      strip.white = TRUE, 
                                      #na.strings = "0",
                                      stringsAsFactors = FALSE,
                                      check.names = FALSE))


# Chemical Identifiers for Article 57
Pharma_identifiers <- as.data.frame(read.csv(paste0(inwd,"/Pharma_identifiers_drugbank_pubchem_V10_Rev52_17112022.csv"), 
                                             sep = "\t", 
                                             header = TRUE, 
                                             strip.white = TRUE, 
                                             #na.strings = "0",
                                             stringsAsFactors = FALSE,
                                             check.names = FALSE))


#Load the entire ecotox database into dataframe ECOTOX
if (!exists("ECOTOX")) {
  load(paste0(inwd,"/ECOTOX ",database_timestamp,"_Rev", timestamp_version, " v8.Rda"))  
}



###########################################################################################
# BEGIN MAIN PROGRAM
###########################################################################################


########
# Deletions
########

# Remove anything not on the EMA's Article 57 Pharmaceuticals list:
ECOTOX <- subset(ECOTOX, ECOTOX$cas_number %in% Pharma_identifiers$CAS_number)


# Keep only certain columns
Cols_Keep <- c("cas_number",
               "chemical_name",
               "chemical_group",
               "test_id",                      
               "result_id",   
               "obs_duration_mean_op",         
               "obs_duration_mean",         
               "obs_duration_unit",
               "duration_unit_description_obs",
               "exposure_duration_mean_op",         
               "exposure_duration_mean",         
               "exposure_duration_unit",
               "duration_unit_description_exposure", 
               "media_type",
               "endpoint",       
               "effect",
               "measurement",
               "measurement_description",  
               "conc1_mean_op",               
               "conc1_mean",                   
               "conc1_unit",
               "conc1_type",
               "species_number", 
               "latin_name",
               "species",
               "common_name",
               "phylum_division",
               "class",
               "species_group",
               "organism_habitat",                        
               "organism_lifestage",                    
               "subhabitat",                   
               "substrate",                        
               "published_date",              
               "author",                       
               "title",                       
               "source",                      
               "publication_year",
               "reference_number",
               colnames(ECOTOX)[grepl("comments", colnames(ECOTOX))]) #includes all "comments" fields


# ECOTOX is used in the following, ECOTOX is kept untouched
# for reference purposes
ECOTOX <- ECOTOX[ , Cols_Keep]


# remove ECOTOX from the workspace to free up some space, if required
#rm(ECOTOX)


#The reasons for exclusion are:
#  1   delete all tests with non-reported test concentrations
#  2   delete all tests with negative or zero test concentrations
#  3   delete all tests with non reported endpoints
#  4   delete all tests that do not have a cas_nr
#  5   delete all tests without phyla information (e.g. communities etc)


##############################################
# Keep copies of original variables before conversions etc
##############################################

#Make copies of concentration data before any filtering or recalculation steps
ECOTOX$conc1_mean_op_original <- ECOTOX$conc1_mean_op
ECOTOX$conc1_mean_original <- ECOTOX$conc1_mean
ECOTOX$conc1_unit_original <- ECOTOX$conc1_unit
ECOTOX$conc1_type_original <- ECOTOX$conc1_type


#Make copies of exposure duration data before any filtering or recalculation steps
ECOTOX$obs_duration_mean_op_original <- ECOTOX$obs_duration_mean_op
ECOTOX$obs_duration_mean_original <- ECOTOX$obs_duration_mean
ECOTOX$obs_duration_unit_original <- ECOTOX$obs_duration_unit


# Keep old conc, chemical name and CAS 
ECOTOX$chemical_name_orginal <- ECOTOX$chemical_name
ECOTOX$cas_number_orginal <- ECOTOX$cas_number



##############################################
# 1.  delete all tests with non-reported test concentrations

dummy <- ECOTOX
ECOTOX <- subset(ECOTOX,ECOTOX$conc1_mean!="NR")  
ECOTOX <- subset(ECOTOX,ECOTOX$conc1_mean!="None")


#Removes any commas used as thousand indicators in concentration figure
ECOTOX$conc1_mean <- gsub(",", "", ECOTOX$conc1_mean)  


##############################################
# 2.  delete all tests with negative or zero test concentrations

dummy <- ECOTOX
ECOTOX <- subset(ECOTOX,ECOTOX$conc1_mean > 0)      



##############################################
# 3.  delete all data with non reported endpoints

dummy <- ECOTOX
ECOTOX <- subset(ECOTOX,!grepl("NR",ECOTOX$endpoint, ignore.case=FALSE))   


##############################################
# 4.    delete all chemicals that do not have a cas_nr

dummy <- ECOTOX
ECOTOX<-subset(ECOTOX,ECOTOX$cas_number !="")   
ECOTOX<-subset(ECOTOX,!is.na(ECOTOX$cas_number)) 



##############################################
# Add molecular weights from Chemical_MW

ECOTOX <-  merge(x = ECOTOX, y = Chemical_MW[c("cas_number", "mol_wt")], by = "cas_number", all.x=TRUE)

ECOTOX$cas_number <- ifelse(is.na(ECOTOX$cas_number), "-7777", ECOTOX$cas_number )

#Check for missing molecular weights:
dummy <- subset(ECOTOX, is.nan(ECOTOX$mol_wt))

if(nrow(dummy) > 0) {
  print("New Chemicals in the ECOTOX database are missing molecular weights")
  print("Manually amend the file: ")
  print("CAS_Lookup_MW_for_ECOTOX_Output_vX.csv")
  
}


#remove "*" from conc1_mean column and confirm as numeric
ECOTOX$conc1_mean <- gsub("\\*","",ECOTOX$conc1_mean)

#Manual substitution:
ECOTOX$conc1_mean <- gsub("ca", "", ECOTOX$conc1_mean )


ECOTOX$conc1_mean_num<-as.numeric(ECOTOX$conc1_mean)


# Check whether conversion is complete or whether there were any non-convertible entries
dummy<-ECOTOX[is.na(ECOTOX$conc1_mean_num),]
if (nrow(dummy) != 0){
  print("UNCONVERTED CONCENTRATION ENTRIES!")
  print(levels(as.factor(dummy$conc1_mean)))
  
  winDialog("ok", "UNCONVERTED CONCENTRATION ENTRIES!... Quitting (press cancel in next Dialog unless you want to cancel the whole R session)")
  quit("ask")
  
  #stop("see error message above")
  
  
} else {
  ECOTOX$conc1_mean<-ECOTOX$conc1_mean_num
  ECOTOX$conc1_mean_num<-NULL
}



########
# Mark commented data as potentially unreliable
#
# Retain the comments field in the variable "reliable". Where there are more than one comment, 
# add them all together with paste and out them in a new column called "comments"
#
########

ECOTOX$reliable <- TRUE
 
ECOTOX$comment <- ""

# Recode FW/ -> FW and SW/ to SW 
# Add any flagged information to the "comment" variable
ECOTOX$reliable <- ifelse(grepl("/",ECOTOX$media_type),FALSE,ECOTOX$reliable)
ECOTOX$comment <- ifelse(grepl("/",ECOTOX$media_type),paste0(ECOTOX$comment, "media_type: ", ECOTOX$media_type_comments) , ECOTOX$comment)
ECOTOX$media_type <-gsub("/","",ECOTOX$media_type)


# Delete the "/" from the effect field
# Add any flagged information to the "comment" variable
ECOTOX$reliable <- ifelse(grepl("/",ECOTOX$effect),FALSE,ECOTOX$reliable)
ECOTOX$comment <- ifelse(grepl("/",ECOTOX$effect),paste0(ECOTOX$comment, "effect: ", ECOTOX$effect_comments) , ECOTOX$comment)
ECOTOX$effect <- gsub("/","",ECOTOX$effect)


# Delete the "/" from the endpoint field
# Add any flagged information to the "comment" variable
ECOTOX$reliable <- ifelse(grepl("/",ECOTOX$endpoint),FALSE,ECOTOX$reliable)
ECOTOX$comment <- ifelse(grepl("/",ECOTOX$endpoint),paste0(ECOTOX$comment, "endpoint: ", ECOTOX$endpoint_comments) , ECOTOX$comment)
ECOTOX$endpoint <- gsub("/","",ECOTOX$endpoint)


# Delete the "*" from the endpoint field, 
# Add any flagged information to the "comment" variable
ECOTOX$reliable <- ifelse(grepl("\\*",ECOTOX$endpoint),FALSE,ECOTOX$reliable)
ECOTOX$comment <- ifelse(grepl("\\*",ECOTOX$endpoint),paste0(ECOTOX$comment, "endpoint: ", ECOTOX$endpoint_comments) , ECOTOX$comment)
ECOTOX$endpoint <- gsub("[*]","",ECOTOX$endpoint)


# Delete the "/" from the measurement field,
# Add any flagged information to the "comment" variable
ECOTOX$reliable <- ifelse(grepl("/",ECOTOX$measurement),FALSE,ECOTOX$reliable)
ECOTOX$comment <- ifelse(grepl("/",ECOTOX$measurement),paste0(ECOTOX$comment, "measurement: ", ECOTOX$measurement_comments) , ECOTOX$comment)
ECOTOX$measurement <- gsub("/","",ECOTOX$measurement)


# Delete the "<" , ">", "=" from the obs_duration_mean field,
# Add any flagged information to the "comment" variable
ECOTOX$reliable <- ifelse(grepl(c("/"), ECOTOX$obs_duration_mean),FALSE,ECOTOX$reliable)
ECOTOX$comment <- ifelse(grepl(c("/"),ECOTOX$obs_duration_mean),paste0(ECOTOX$comment, "obs_duration_mean: / ", ECOTOX$obs_duration_comments) , ECOTOX$comment)
ECOTOX$obs_duration_mean <- gsub(c("/"),"",ECOTOX$obs_duration_mean)

ECOTOX$reliable <- ifelse(grepl(c("<" ), ECOTOX$obs_duration_mean),FALSE,ECOTOX$reliable)
ECOTOX$comment <- ifelse(grepl(c("<"),ECOTOX$obs_duration_mean),paste0(ECOTOX$comment, "obs_duration_mean: < ", ECOTOX$obs_duration_comments) , ECOTOX$comment)
ECOTOX$obs_duration_mean <- gsub(c("<" ),"",ECOTOX$obs_duration_mean)

ECOTOX$reliable <- ifelse(grepl(c( ">"), ECOTOX$obs_duration_mean),FALSE,ECOTOX$reliable)
ECOTOX$comment <- ifelse(grepl(c(">"),ECOTOX$obs_duration_mean),paste0(ECOTOX$comment, "obs_duration_mean: > ", ECOTOX$obs_duration_comments) , ECOTOX$comment)
ECOTOX$obs_duration_mean <- gsub(c( ">"),"",ECOTOX$obs_duration_mean)

ECOTOX$reliable <- ifelse(grepl(c("="), ECOTOX$obs_duration_mean),FALSE,ECOTOX$reliable)
ECOTOX$comment <- ifelse(grepl(c("="),ECOTOX$obs_duration_mean),paste0(ECOTOX$comment, "obs_duration_mean: = ", ECOTOX$obs_duration_comments) , ECOTOX$comment)
ECOTOX$obs_duration_mean <- gsub(c("="),"",ECOTOX$obs_duration_mean)





########
# Conversions
########


########
# Convert entries in the species_group
# i.e. get rid of things like "standard test species" or "nuisance species"
#######

print("species_group prior to consolidation")
levels(as.factor(ECOTOX$species_group))

ECOTOX$species_group<-ifelse(grepl("fish",ECOTOX$species_group,ignore.case = TRUE),"Fish",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("crustaceans",ECOTOX$species_group,ignore.case = TRUE),"Crustaceans",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("molluscs",ECOTOX$species_group,ignore.case = TRUE),"Molluscs",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("invertebrates",ECOTOX$species_group,ignore.case = TRUE),"Invertebrates",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("amphibians",ECOTOX$species_group,ignore.case = TRUE),"Amphibians",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("insects/spiders",ECOTOX$species_group,ignore.case = TRUE),"Insects/Spiders",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("flowers, trees, shrubs, ferns",ECOTOX$species_group,ignore.case = TRUE),"Flowers, Trees, Shrubs, Ferns",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("worms",ECOTOX$species_group,ignore.case = TRUE),"Worms",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("algae",ECOTOX$species_group,ignore.case = TRUE),"Algae",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("birds",ECOTOX$species_group,ignore.case = TRUE),"Birds",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("mammals",ECOTOX$species_group,ignore.case = TRUE),"Mammals",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("reptiles",ECOTOX$species_group,ignore.case = TRUE),"Reptiles",ECOTOX$species_group)
ECOTOX$species_group<-ifelse(grepl("fungi",ECOTOX$species_group,ignore.case = TRUE),"Fungi",ECOTOX$species_group)

print("species_group after consolidation")
print(levels(as.factor(ECOTOX$species_group)))



##########
# Convert exposure (=observation) to days
##########
# Convert exposure duration to numbers
ECOTOX$obs_duration_mean<-as.numeric(ECOTOX$obs_duration_mean)

#Convert exposure durations that are not numeric to -7777
ECOTOX$obs_duration_mean <- ifelse(test = is.na(ECOTOX$obs_duration_mean), 
                                      yes = -7777, no = ECOTOX$obs_duration_mean)


ECOTOX$obs_duration_mean_new <- -7777

print("Observation duration units prior to conversion")
levels(as.factor(ECOTOX$obs_duration_unit))
levels(as.factor(ECOTOX$duration_unit_description_obs))


dummy <- subset(ECOTOX, ECOTOX$duration_unit_description_obs == "Day(s)")

# The values in exposure_duration_unit are not all directly usable, as there are values such as hpf (hours post fertilization). 
# Details are in the duration_unit_description_obs field
# the following lines extract the keywords "Sec*", "Min*" etc from that field, and collect the corresponding
# values from the "exposure_duration_unit field into the vector "SecAbbr", "MinAbbr" etc.
# the levels(as.factor) makes sure that only one entry is kept

SecAbbr <- levels(as.factor(ECOTOX[grepl("Sec*",ECOTOX$duration_unit_description_obs), "obs_duration_unit"]))
MinAbbr <- levels(as.factor(ECOTOX[grepl("Min*",ECOTOX$duration_unit_description_obs), "obs_duration_unit"]))
HourAbbr <- levels(as.factor(ECOTOX[grepl("Hour*",ECOTOX$duration_unit_description_obs), "obs_duration_unit"]))
DaysAbbr <- levels(as.factor(ECOTOX[grepl("Day*",ECOTOX$duration_unit_description_obs), "obs_duration_unit"]))
WeekAbbr <- levels(as.factor(ECOTOX[grepl("Week*",ECOTOX$duration_unit_description_obs), "obs_duration_unit"]))
MonthAbbr <- levels(as.factor(ECOTOX[grepl("Month*",ECOTOX$duration_unit_description_obs), "obs_duration_unit"]))
YearAbbr <- levels(as.factor(ECOTOX[grepl("Year*",ECOTOX$duration_unit_description_obs), "obs_duration_unit"]))


# convert to days
ECOTOX$obs_duration_mean_new <- ifelse(ECOTOX$obs_duration_unit %in% SecAbbr,   ECOTOX$obs_duration_mean/86400,ECOTOX$obs_duration_mean_new)
ECOTOX$obs_duration_mean_new <- ifelse(ECOTOX$obs_duration_unit %in% MinAbbr,   ECOTOX$obs_duration_mean/1440, ECOTOX$obs_duration_mean_new)
ECOTOX$obs_duration_mean_new <- ifelse(ECOTOX$obs_duration_unit %in% HourAbbr,  ECOTOX$obs_duration_mean/24,ECOTOX$obs_duration_mean_new)
ECOTOX$obs_duration_mean_new <- ifelse(ECOTOX$obs_duration_unit %in% DaysAbbr,  ECOTOX$obs_duration_mean*1,ECOTOX$obs_duration_mean_new)
ECOTOX$obs_duration_mean_new <- ifelse(ECOTOX$obs_duration_unit %in% WeekAbbr,  ECOTOX$obs_duration_mean*7,ECOTOX$obs_duration_mean_new)
ECOTOX$obs_duration_mean_new <- ifelse(ECOTOX$obs_duration_unit %in% MonthAbbr, ECOTOX$obs_duration_mean*30,ECOTOX$obs_duration_mean_new)
ECOTOX$obs_duration_mean_new <- ifelse(ECOTOX$obs_duration_unit %in% YearAbbr,  ECOTOX$obs_duration_mean*365, ECOTOX$obs_duration_mean_new)


# check what has not converted
dummy <- subset(ECOTOX, ECOTOX$obs_duration_mean_new == -7777)
print(levels(as.factor(dummy$duration_unit_description_obs)))
print(levels(as.factor(dummy$obs_duration_unit_original)))


# clean-up
ECOTOX$obs_duration_mean<-ECOTOX$obs_duration_mean_new
ECOTOX$obs_duration_mean_new<-NULL
ECOTOX$obs_duration_unit<-"d"
ECOTOX$duration_unit_description_obs<-"Day(s)"


#########
# Sort into acute and chronic
# Algorithm from Francis' vers 13
# UPDATED for version 4!
#########

#Manual additions of Class Data that are missing:

ECOTOX[ECOTOX$latin_name == "Limulus polyphemus", "class" ] <- "Merostomata"    #Horshoe Crab
ECOTOX[ECOTOX$latin_name == "Branchiostoma belcheri", "class" ] <- "Leptocardii"  #Lancelet
ECOTOX[ECOTOX$latin_name == "Branchiostoma caribaeum", "class" ] <- "Leptocardii"  #Caribbean Lancelet

ECOTOX[ECOTOX$latin_name == "Limulus polyphemus", "species_group" ] <- "Insects/Spiders"    #Horseshoe Crab - not actually a crustacean, its an arthropod most closely related to scoprions and spiders
ECOTOX[ECOTOX$latin_name == "Branchiostoma belcheri", "species_group" ] <- "Fish"  #Lancelet
ECOTOX[ECOTOX$latin_name == "Branchiostoma caribaeum", "species_group" ] <- "Fish"  #Caribbean Lancelet




#########
# Conversion to ?umol/L
# 
# Proceeds under the assumption that MW is present for all chemicals!
# 
# !! check whether there are more units that can be converted !!
#########
UnitsToKeep <- c( "%", 
                  "%w/v",
                  "mg/ml",
                  "g/dm3" ,
                  "g/L", 
                  "M", 
                  "mg/dm3",
                  "mg/L" ,
                  "mg/ml",
                  "mM" ,
                  "mmol",
                  "mol/L", 
                  "mol" ,
                  "mmol/L",
                  "mmol/m3",
                  "mmol/dm3", 
                  "ng/L" ,
                  "nmol",
                  "nmol/L", 
                  "nM/L",
                  "nmol/ml",
                  "ng/ml",
                  "nM",
                  "pmol",
                  "pM" ,
                  "ug/dm3",
                  "ug/L" ,
                  "ug/ml" ,
                  "um",
                  "uM" ,
                  "uM/L",
                  "umol",
                  "umol/dm3",
                  "umol/L",
                  "ppm", "ppb", "ppt", "% w/v",
                  "ai g/L", "AI mg/L", "ai mg/ml", 
                  "AI mM", "AI mmol/L", "AI ng/L", 
                  "AI ng/mL", "AI ppb", "AI ppm", 
                  "AI ppt", "AI ug/L", "AI ug/ml", 
                  "AI uM", "pg/ml",
                  
                  "% AE",                                               #New units added by Francis 10/05/2021
                  "ae g/200 L", "ae g/L" ,"ae mg/L", 
                  "ae ppb" ,"ae ppm" ,
                  "ae ug/L" , "ae ug/ml" ,"ae uM", "mm","umol/ml",
                  "AI %","AI % w/v", 
                  "AI ug/ul","ug/ml H2O", "pmol/L", "ppb H2O",
                  "ng/nl","ng eq/ml" ,"ng/ul","ppm/L", 
                  "umoles/l agar",
                  "g/3.75 L" ,"g/3.78 L", "g/3.79 L",
                  "AI g/378 L" ,"AI g/378.5 L" ,"AI g/379 L",
                  "g/dl","AI g/100 ml","g/10 L" , "g/10.2 L" , "g/189.25 L" ,"g/2 L",
                  "g/200 L","g/200 ml", "g/50 ml", "g/ml", 
                  "mg/10 L", "mg/100 ml" , "mg/200 ml", "mg/25 mL", "mg/100 ul" ,"mg/150 ml",
                  "ug/100 ml",  "ug/25 mL" ,"ug/50 ml" , "ug/ul",
                  "ng/0.5 ml",
                  "kg/hL",
                  "kg/2000 L", "g/hL", "lb/100 gal" ,"lb/gal", "ae lb/100 gal",
                  "AI g/100 gal", "AI g/100 L", "AI g/1100 L", 
                  "AI oz/100 gal","AI oz/gal",
                                                                            #New units added by Francis 20/06/2022
                  "AI g/10 L", "AI g/1000 L", "AI g/200 L", "AI g/300 L",  
                  "AI g/ml", 
                  "AI kg/93.5 L", "ai kg/379 l",  "AI kg/378.5 L", "AI kg/378 L",
                  "AI lb/100 gal", "AI lb/ga", "ae lb/gal", "lb/40 gal",
                  "oz/2.5 gal", "oz/3 gal",
                  "AI mg/50 ml H2O", "AI mg/ml",
                  "g/1000 L", "g/13.5 L", "g/16 L", "g/250 L", "g/379 L", "g/5 L", "g/L H2O", "g/100 L",
                  "g/500 ml", "g/800 ml", "g/946 ml", "g/1000 ml", "g/100 ml", "g/170 ml",
                  "AI g/hl",
                  "kg/3.78 L", "kg/L", 
                  "mg/60 ml", "mg/500 ml", "mg/gal",
                  "mol/ml", "ae M", "AI M",
                  "ng/25 mL",
                  "pg/L", "pg/ul",
                  "ug/3.5 L", "ug/10 L", "ug/5 ml",
                  "uM/ml", "pmol/ml")   

# Print non-converted units
dummy<-subset(ECOTOX,!ECOTOX$conc1_unit %in% UnitsToKeep)
print (paste0("Number of data with unusable units:", nrow(dummy), " of a total of ", nrow(ECOTOX),"."))
dummy <- (levels(as.factor(dummy$conc1_unit)))

# keep only units that can be converted
#ECOTOX<-subset(ECOTOX,ECOTOX$conc1_unit %in% UnitsToKeep)


UnitCat1 <- c("um",
              "uM" ,"AI uM","ae uM",
              "uM/L",
              "umol",
              "uMol",
              "umol/dm3",
              "umol/L","umoles/l agar",
              "mmol/m3",
              "nmol/ml")
UnitCat2 <- c("nM",
              "nmol",
              "nmol/L",
              "nM/L",
              "pmol/ml")
UnitCat3 <- c("mmol/L",
              #"mmol/m3",
              "mmol/dm3",
              "mM","AI mM","mm",
              "AI mmol/L",
              "mmol",
              "umol/ml", "uM/ml")
UnitCat4 <- c("pmol","pmol/L",
              "pM")
UnitCat5 <- c("M", "ae M", "AI M",
              "M/L","mol","mol/L","Mol/L")
UnitCat6 <- c("mg/ml","ai mg/ml", "AI mg/ml",
              "ng/nl",
              "g/dm3" ,
              "g/L", "ai g/L", "ae g/L", "g/L H2O", "g/1000 ml", 
              "AI ug/ul")
UnitCat7 <- c("%",
              "% AE","AI %","AI % w/v",
              "%w/v","% w/v")
UnitCat8 <- c("mg/dm3",
              "mg/L", "ae mg/L", "AI mg/L",
              "ug/ml", "ae ug/ml", "AI ug/ml","ug/ml H2O",
              "ng/ul",
              "ppm","ae ppm","AI ppm", "ppm/L")
UnitCat9 <- c("ug/L","AI ug/L","ae ug/L",
              "ppb","AI ppb","ae ppb","ppb H2O",
              "ng/ml","AI ng/mL", "ng eq/ml",
              "ug/dm3")
UnitCat10 <- c ("ng/L","AI ng/L",
                "ppt","AI ppt",
                "pg/ml")
UnitCat11 <- c ("g/3.75 L" ,"g/3.78 L", "g/3.79 L")
UnitCat12 <- c ("AI g/378 L" ,"AI g/378.5 L" ,"AI g/379 L")
UnitCat13 <- c ("g/dl",
                "AI g/100 ml", "g/100 ml", "g/100 ml",
                "kg/hL")


#Conversion rates used for imperial to metric:
#1gallon = 3.79L
#1oz = 28.35g
#1hl = 100L  
#1lb = 453.6g

ECOTOX$conc_uM<- -7777

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat1, ECOTOX$conc1_mean,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat2, ECOTOX$conc1_mean/1E3,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat3, ECOTOX$conc1_mean*1E3,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat4, ECOTOX$conc1_mean/1E6,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat5, ECOTOX$conc1_mean*1E6,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat6, ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat7, ECOTOX$conc1_mean*10*1E6/ECOTOX$mol_wt,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat8, ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat9, ECOTOX$conc1_mean/ECOTOX$mol_wt,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat10,ECOTOX$conc1_mean/1E3/ECOTOX$mol_wt,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat11,ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/3.79,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat12,ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/379,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit %in% UnitCat13,ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*10,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/10 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/10,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI g/10 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/10,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/10.2 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/10.2,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/189.25 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/189.25,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/2 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/2,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/100 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/100,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI g/hl" ,ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/100,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/200 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/200,ECOTOX$conc_uM)


ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI g/100 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/100,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI g/1000 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/1000,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI g/200 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/200,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI g/300 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/300,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/1000 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/1000,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/13.5 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/13.5,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/16 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/16,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/250 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/250,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/379 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/379,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/5 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/5,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI g/ml",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*1000,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/ml",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*1000,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/200 ml",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*5,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/50 ml",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*20,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/500 ml",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*2,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/800 ml",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*1.25,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/946 ml",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*1.057,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/170 ml",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*5.882,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "mg/10 L",ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt/10,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "mg/100 ml",ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt*10,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "mg/200 ml",ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt*5,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "mg/25 ml",ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt*40,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "mg/150 ml",ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt*6.667,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "mg/100 ul",ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt*10000,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI mg/50 ml H2O",ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt*20,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "mg/60 ml",ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt*16.666,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "mg/500 ml",ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt*2,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ug/100 ml",ECOTOX$conc1_mean/ECOTOX$mol_wt*10,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ug/25 ml",ECOTOX$conc1_mean/ECOTOX$mol_wt*40,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ug/50 ml",ECOTOX$conc1_mean/ECOTOX$mol_wt*20,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ug/5 ml",ECOTOX$conc1_mean/ECOTOX$mol_wt*200,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ug/ul",ECOTOX$conc1_mean/ECOTOX$mol_wt*1E6,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ug/3.5 L",ECOTOX$conc1_mean/ECOTOX$mol_wt/3.5,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ug/10 L",ECOTOX$conc1_mean/ECOTOX$mol_wt/10,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ng/0.5 ml",ECOTOX$conc1_mean/1E3/ECOTOX$mol_wt*2000,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ng/25 mL",ECOTOX$conc1_mean/1E3/ECOTOX$mol_wt*40,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "pg/L",ECOTOX$conc1_mean/1E6/ECOTOX$mol_wt,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "pg/ul",ECOTOX$conc1_mean/1E6/ECOTOX$mol_wt*1E6,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "g/hL",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/100,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "kg/2000 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/(2000/1000),ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI kg/93.5 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/(93.5/1000),ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ai kg/379 l",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/(379/1000),ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI kg/378.5 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/(378.5/1000),ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI kg/378 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/(378/1000),ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI kg/3.78 L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/(3.78/1000),ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "kg/L",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/0.001,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "mol/ml",ECOTOX$conc1_mean*1E6*1000,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "uM/ml",ECOTOX$conc1_mean*1*1000,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "lb/100 gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*453.6/379,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit ==  "ae lb/100 gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*453.6/379,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "lb/gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*453.6/3.79,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "ae lb/gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*453.6/3.79,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI lb/100 gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*453.6/379,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI lb/ga",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*453.6/3.79,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI oz/100 gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*28.35/379,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI oz/gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*28.35/3.79,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "oz/gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*28.35/3.79,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "oz/2.5 gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*28.35/(3.79*2.5),ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "oz/3 gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*28.35/(3.79*3),ECOTOX$conc_uM)


ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "mg/gal",ECOTOX$conc1_mean*1E3/ECOTOX$mol_wt/3.79,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI g/100 gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt/379,ECOTOX$conc_uM)

ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "lb/100 gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*453.6/379,ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "lb/40 gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*453.6/(3.79*40),ECOTOX$conc_uM)
ECOTOX$conc_uM <- ifelse(ECOTOX$conc1_unit == "AI lb/10 gal",ECOTOX$conc1_mean*1E6/ECOTOX$mol_wt*453.6/(3.79*10),ECOTOX$conc_uM)


#Clean Up:
ECOTOX$conc1_mean<-ECOTOX$conc_uM
ECOTOX$conc_uM<-NULL
ECOTOX$conc1_unit<-"umol/L"



#################
# Delete duplicates
# A duplicate is defined as a row with identical cas_number, measurement, latin_name, reference_number, AC_Class
# BUT: test_id and duration can be different. If so, the last observation is retained (=longest exposure time)
#      If several entries with longest exposure time: selected highest rank of EndpointHierarchy
#      
# Proceeds separately for acute and chronic data because of different endpoint hierarchies
################# 

# Sort. 
ECOTOX <- ECOTOX[order(ECOTOX$cas_number,
                             ECOTOX$measurement,
                             ECOTOX$endpoint,
                             ECOTOX$latin_name,
                             ECOTOX$reference_number,
                           -ECOTOX$obs_duration_mean,
                           ECOTOX$conc1_mean),]


ECOTOX <- subset(ECOTOX, !duplicated(ECOTOX))



#Make an alterative CAS number column, that has no dashes ("-")
ECOTOX$cas_number_alt <- gsub("-", "",ECOTOX$cas_number)


#rename the column containing the concentrations converted to ug/L:
ECOTOX <-  rename(ECOTOX, conc_uM = conc1_mean)


# -7777 is used as a place-holder for any values unable to be converted.
#Replace any non-converted values with NA
ECOTOX$conc_uM <- gsub("-7777", NA, ECOTOX$conc_uM)






##################################################
#Tidying Up for export
###################################################


#Remove any columns that are not needed for export (e.g. all of the various Comment fields)
Final_Short <- ECOTOX[ , c("cas_number","cas_number_alt", "chemical_name", "result_id",
                           "latin_name", "common_name", "species_group", "organism_habitat",
                           "endpoint", "measurement", "measurement_description",  
                           "obs_duration_mean", "obs_duration_unit", 
                           "conc1_mean_op", "conc_uM",
                           "conc1_mean_op_original", "conc1_mean_original", "conc1_unit_original",
                           "mol_wt", "media_type", 
                           "author", "title",  "source",  "publication_year",
                           "reliable", "comment")]



Final_Short$conc_uM <- as.numeric(Final_Short$conc_uM)


#Make sure conc1_mean_original is a numeric value, and remove any that are zero.
Final_Short$conc1_mean_original <- as.numeric(Final_Short$conc1_mean_original)
Final_Short <- subset(Final_Short, Final_Short$conc1_mean_original >0)


#Remove characters that interfere with delimiting the exports: ","
Final_Short$author <- gsub("\\,", " ", Final_Short$author)
Final_Short$title <- gsub("\\,", " ", Final_Short$title)
Final_Short$source <- gsub("\\,", " ", Final_Short$source)
Final_Short$measurement_description <- gsub("\\,", " ", Final_Short$measurement_description)


#Replace all NAs with either "missing" in the case of strings, or -7777 in the case of numeric values
Final_Short$cas_number <- ifelse(test = is.na(Final_Short$cas_number), yes = "missing", no = Final_Short$cas_number)
Final_Short$cas_number_alt <- ifelse(test = is.na(Final_Short$cas_number_alt), yes = "missing", no = Final_Short$cas_number_alt)
Final_Short$chemical_name <- ifelse(test = is.na(Final_Short$chemical_name), yes = "missing", no = Final_Short$chemical_name)
Final_Short$result_id <- ifelse(test = is.na(Final_Short$result_id), yes = "missing", no = Final_Short$result_id)
Final_Short$latin_name <- ifelse(test = is.na(Final_Short$latin_name), yes = "missing", no = Final_Short$latin_name)
Final_Short$common_name <- ifelse(test = is.na(Final_Short$common_name), yes = "missing", no = Final_Short$common_name)
Final_Short$species_group <- ifelse(test = is.na(Final_Short$species_group), yes = "missing", no = Final_Short$species_group)
Final_Short$organism_habitat <- ifelse(test = is.na(Final_Short$organism_habitat), yes = "missing", no = Final_Short$organism_habitat)
Final_Short$endpoint <- ifelse(test = is.na(Final_Short$endpoint), yes = "missing", no = Final_Short$endpoint)
Final_Short$measurement <- ifelse(test = is.na(Final_Short$measurement), yes = "missing", no = Final_Short$measurement)
Final_Short$measurement_description <- ifelse(test = is.na(Final_Short$measurement_description), yes = "missing", no = Final_Short$measurement_description)
Final_Short$obs_duration_mean <- ifelse(test = is.na(Final_Short$obs_duration_mean), yes = -7777, no = Final_Short$obs_duration_mean)
Final_Short$obs_duration_unit <- ifelse(test = is.na(Final_Short$obs_duration_unit), yes = "missing", no = Final_Short$obs_duration_unit)
Final_Short$conc1_mean_op <- ifelse(test = is.na(Final_Short$conc1_mean_op), yes = "missing", no = Final_Short$conc1_mean_op)
Final_Short$conc_uM <- ifelse(test = is.na(Final_Short$conc_uM), yes = -7777, no = Final_Short$conc_uM)
Final_Short$conc1_mean_op_original <- ifelse(test = is.na(Final_Short$conc1_mean_op_original), yes = "missing", no = Final_Short$conc1_mean_op_original)
Final_Short$conc1_mean_original <- ifelse(test = is.na(Final_Short$conc1_mean_original), yes = -7777, no = Final_Short$conc1_mean_original)
Final_Short$conc1_unit_original <- ifelse(test = is.na(Final_Short$conc1_unit_original), yes = "missing", no = Final_Short$conc1_unit_original)
Final_Short$mol_wt <- ifelse(test = is.na(Final_Short$mol_wt), yes = -7777, no = Final_Short$mol_wt)
Final_Short$author <- ifelse(test = is.na(Final_Short$author), yes = "missing", no = Final_Short$author)
Final_Short$title <- ifelse(test = is.na(Final_Short$title), yes = "missing", no = Final_Short$title)
Final_Short$source <- ifelse(test = is.na(Final_Short$source), yes = "missing", no = Final_Short$source)
Final_Short$publication_year <- ifelse(test = is.na(Final_Short$publication_year), yes = "missing", no = Final_Short$publication_year)
Final_Short$reliable <- ifelse(test = is.na(Final_Short$reliable), yes = FALSE, no = Final_Short$reliable)
Final_Short$comment <- ifelse(test = Final_Short$comment == "", yes = "missing", no = Final_Short$comment)




#Export a file that contains only the Aquatic Species:
Final_Short <- subset (Final_Short, Final_Short$organism_habitat == "Water")



#Export main data summary file as .csv
write_delim(Final_Short, paste0(outwd, "/ECOTOX_pharma_clean_v", vers,"_Rev", timestamp_version, "_",database_timestamp, ".csv"),
            delim = "\t", escape = "none", quote = "none")

