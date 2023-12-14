################
# 
#   ECOTOX AQUIRE CONVERTER
#
# This function takes the components of a full ASCII download of the US EPA ECOTOX database and
#   assembles it into a single file.
# 
# See: https://cfpub.epa.gov/ecotox/ 
#
############################################################
#
# INPUT:
#
# Download the full ASCII ECOTOX database, and un-zip:
#
# - species.txt
# - results.txt
# - tests.txt
# - references.txt
# - effect_codes.txt
# - measurement_codes.txt
# - duration_unit_codes.txt
# - chemicals.txt
# - lifestage_codes.txt
# - endpoint_codes.txt
#
#
# OUTPUT:
#
# An .Rda file named "ECOTOX_
#


############
# Housekeeping
############

rm(list=ls()) #remove ALL objects 
Sys.setenv(LANG = "en") #Let's keep stuff in English
dummy<-Sys.setlocale("LC_ALL","English")
cat("\014") # clear console window prior to new run

############
# Version Number
############
vers<-8

############
# timestamp of the ECOTOX database
############
database_timestamp<-"12_15_2022"
timestamp_version <- 13

############
# Set the current workdirectories
############
inwd <- "E:/GU/EPA ECOTOX/R Inputs"
outwd <- "E:/GU/EPA ECOTOX/R Outputs"

setwd(inwd)

################
# packages, imports, functions
################

library(readxl)
library(writexl)
library(tidyverse)
library(stringr)
library(data.table)


###############
# Import ECOTOX Files
###############

SpeciesLookup =  read.table("species.txt", 
                            comment.char = "", 
                            quote = "", 
                            sep="|", 
                            stringsAsFactors=FALSE ,
                            header = TRUE,
                            strip.white = TRUE, 
                            check.names = FALSE,
                            fill = TRUE)

ResultsLookup =  read.table("results.txt", 
                            comment.char = "", 
                            quote = "",
                            sep="|", 
                            stringsAsFactors=FALSE ,
                            header = TRUE,
                            strip.white = TRUE, 
                            check.names = FALSE,
                            fill = TRUE)

TestsLookup =  read.table("tests.txt", 
                          comment.char = "", 
                          quote = "",
                          sep="|", 
                          stringsAsFactors=FALSE ,
                          header = TRUE,
                          strip.white = TRUE, 
                          check.names = FALSE,
                          fill = TRUE,
                          colClasses = c("test_cas"="character")) # forces to import "test_cas" as character (not int)

ReferencesLookup =  read.table("references.txt", 
                               comment.char = "", 
                               quote = "", 
                               sep="|", 
                               stringsAsFactors=FALSE ,
                               header = TRUE,
                               strip.white = TRUE, 
                               check.names = FALSE,
                               fill = TRUE,
                               na.strings = c("","NA"))

EffectLookup =  read.table("effect_codes.txt", 
                           comment.char = "", 
                           quote = "",
                           sep="|", 
                           stringsAsFactors=FALSE ,
                           header = TRUE,
                           strip.white = TRUE, 
                           check.names = FALSE,
                           fill = TRUE)

MeasurementLookup =  read.table("measurement_codes.txt", 
                                comment.char = "", 
                                quote = "", 
                                sep="|", 
                                stringsAsFactors=FALSE ,
                                header = TRUE,
                                strip.white = TRUE, 
                                check.names = FALSE,
                                fill = TRUE)

# Timeunits werden an 4 Stellen ben?tigt
TimeUnitLookup_1 = read.table("duration_unit_codes.txt", 
                            comment.char = "", 
                            quote = "",
                            sep="|", 
                            stringsAsFactors=FALSE ,
                            header = TRUE,
                            strip.white = TRUE, 
                            check.names = FALSE,
                            fill = TRUE)

TimeUnitLookup_2 = read.table("duration_unit_codes.txt", 
                              comment.char = "", 
                              quote = "",
                              sep="|", 
                              stringsAsFactors=FALSE ,
                              header = TRUE,
                              strip.white = TRUE, 
                              check.names = FALSE,
                              fill = TRUE)

TimeUnitLookup_3 = read.table("duration_unit_codes.txt", 
                              comment.char = "", 
                              quote = "",
                              sep="|", 
                              stringsAsFactors=FALSE ,
                              header = TRUE,
                              strip.white = TRUE, 
                              check.names = FALSE,
                              fill = TRUE)

TimeUnitLookup_4 = read.table("duration_unit_codes.txt", 
                              comment.char = "", 
                              quote = "",
                              sep="|", 
                              stringsAsFactors=FALSE ,
                              header = TRUE,
                              strip.white = TRUE, 
                              check.names = FALSE,
                              fill = TRUE)    


CASLookup = read.table("chemicals.txt", 
                       comment.char = "", 
                       quote = "",
                       sep="|", 
                       stringsAsFactors=FALSE ,
                       header = TRUE,
                       strip.white = TRUE, 
                       check.names = FALSE,
                       fill = TRUE,
                       colClasses = c("character","character","character")) # forces to import everything as character

LifeStageLookup = read.table("lifestage_codes.txt", 
                             comment.char = "", 
                             quote = "", 
                             sep="|", 
                             stringsAsFactors=FALSE ,
                             header = TRUE,
                             strip.white = TRUE, 
                             check.names = FALSE,
                             fill = TRUE)

EndpointLookup = read.table("endpoint_codes.txt", 
                             comment.char = "", 
                             quote = "", 
                             sep="|", 
                             stringsAsFactors=FALSE ,
                             header = TRUE,
                             strip.white = TRUE, 
                             check.names = FALSE,
                             fill = TRUE)  


########
# Convert CAS number to Form with Hyphens
########

# in CASLookup
CASLookup$dummy<-str_sub(CASLookup$cas_number,nchar(CASLookup$cas_number),nchar(CASLookup$cas_number))
CASLookup$dummy2<-str_sub(CASLookup$cas_number,nchar(CASLookup$cas_number)-2,nchar(CASLookup$cas_number)-1)
CASLookup$dummy3<-str_sub(CASLookup$cas_number,1,nchar(CASLookup$cas_number)-3)

CASLookup$original_cas_number<-CASLookup$cas_number
CASLookup$cas_number<-paste0(CASLookup$dummy3,"-",CASLookup$dummy2,"-",CASLookup$dummy)

CASLookup$dummy<-NULL    
CASLookup$dummy2<-NULL
CASLookup$dummy3<-NULL
CASLookup$dummy4<-NULL

# in TestsLookup
TestsLookup$dummy<-str_sub(TestsLookup$test_cas,nchar(TestsLookup$test_cas),nchar(TestsLookup$test_cas))
TestsLookup$dummy2<-str_sub(TestsLookup$test_cas,nchar(TestsLookup$test_cas)-2,nchar(TestsLookup$test_cas)-1)
TestsLookup$dummy3<-str_sub(TestsLookup$test_cas,1,nchar(TestsLookup$test_cas)-3)

TestsLookup$original_test_cas<-TestsLookup$test_cas
TestsLookup$test_cas<-paste0(TestsLookup$dummy3,"-",TestsLookup$dummy2,"-",TestsLookup$dummy)

TestsLookup$dummy<-NULL    
TestsLookup$dummy2<-NULL
TestsLookup$dummy3<-NULL
TestsLookup$dummy4<-NULL

########
# Rename Variables that occur under the same name in various importfiles
# although they mean different things
########
setnames(ResultsLookup, "additional_comments", "results_additional_comments")
setnames(TestsLookup, "additional_comments", "tests_additional_comments")
setnames(ResultsLookup, "created_date", "results_created_date")
setnames(TestsLookup, "created_date", "tests_created_date")
setnames(ResultsLookup, "modified_date", "results_modified_date")
setnames(TestsLookup, "modified_date", "tests_modified_date")

setnames(SpeciesLookup, "ecotox_group", "species_group")
setnames(CASLookup, "ecotox_group", "chemical_group")

setnames(EffectLookup, "description", "effect_description")
setnames(EndpointLookup, "description", "endpoint_description")
setnames(MeasurementLookup, "description", "measurement_description")

setnames(TimeUnitLookup_1, "description", "duration_unit_description_organism")
setnames(TimeUnitLookup_2, "description", "duration_unit_description_study")
setnames(TimeUnitLookup_3, "description", "duration_unit_description_exposure")
setnames(TimeUnitLookup_4, "description", "duration_unit_description_obs")


########
# Rename Variables that have different names but are in fact keys needed for
# merging
########
setnames(TestsLookup, "test_cas", "cas_number")
setnames(EffectLookup, "code", "effect_code")
setnames(EndpointLookup, "code", "endpoint_code")
setnames(MeasurementLookup, "code", "measurement_code")
setnames(TimeUnitLookup_1, "code", "organism_age_unit")
setnames(TimeUnitLookup_2, "code", "study_duration_unit")
setnames(TimeUnitLookup_3, "code", "exposure_duration_unit")
setnames(TimeUnitLookup_4, "code", "obs_duration_unit")

Backup1<-ResultsLookup
# ResultsLookup<-Backup1
  
#######
# Generate keys from existing fields that are polluted 
# with / ~ *
# keep the original effect, measurement and endpoint code
# will be used later.
#######
ResultsLookup$effect_code<-gsub("/","",ResultsLookup$effect)
ResultsLookup$effect_code<-gsub("~","",ResultsLookup$effect_code)
ResultsLookup$effect_code<-gsub("\\*","",ResultsLookup$effect_code)

ResultsLookup$measurement_code<-gsub("/","",ResultsLookup$measurement)
ResultsLookup$measurement_code<-gsub("~","",ResultsLookup$measurement_code)
ResultsLookup$measurement_code<-gsub("\\*","",ResultsLookup$measurement_code)

ResultsLookup$endpoint_code<-gsub("/","",ResultsLookup$endpoint)
ResultsLookup$endpoint_code<-gsub("~","",ResultsLookup$endpoint_code)
ResultsLookup$endpoint_code<-gsub("\\*","",ResultsLookup$endpoint_code)  


########
# Mergers
########

# merge with results table
ECOTOX <- merge(ResultsLookup,TestsLookup, by="test_id")

# merge with References
ECOTOX <- merge(ECOTOX, ReferencesLookup, by="reference_number")

# merge with Chemical Names
ECOTOX <- merge(ECOTOX, CASLookup, by= "cas_number")

# merge with Species Names
ECOTOX <- merge(ECOTOX, SpeciesLookup, by="species_number")

# merge with effect codes
ECOTOX <- merge(ECOTOX, EffectLookup, by="effect_code")

# merge with endpoint codes
ECOTOX <- merge(ECOTOX,EndpointLookup,by="endpoint_code")

# by measurement
ECOTOX<-merge(ECOTOX,MeasurementLookup, by="measurement_code", all.x=TRUE)

# by organism_age_unit
ECOTOX<-merge(ECOTOX,TimeUnitLookup_1, by="organism_age_unit", all.x=TRUE)

# by study_duration_unit
ECOTOX<-merge(ECOTOX,TimeUnitLookup_2, by="study_duration_unit", all.x=TRUE)

# by exposure_duration_unit
ECOTOX<-merge(ECOTOX,TimeUnitLookup_3, by="exposure_duration_unit", all.x=TRUE)

# by obs_duration_unit
ECOTOX<-merge(ECOTOX,TimeUnitLookup_4, by="obs_duration_unit", all.x=TRUE)


########
# Export files:
########

#save dataframe
save(ECOTOX,file=paste0(outwd,"/ECOTOX ", database_timestamp, "_Rev", timestamp_version, " v", vers,".Rda"))

  
