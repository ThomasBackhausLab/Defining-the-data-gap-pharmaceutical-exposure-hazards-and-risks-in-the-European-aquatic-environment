##################################################################################################################
#
#UBA Pharamceuticals Database Builder
#
#
# In this script monitoring data from UBA (German federal environment agency) is imported, quality checked, 
#cross referenced with Drugbank and exported
#
# 1. A SERIES OF FILTERS ARE APPLIED
# - Only pharmaceuticals from the EMA Article 57 list are retained.
# - Only aquatic records are retained.
# - Chemicals without a CAS number are deleted
# - Records with no valid MEC are deleted
# - Records without a valid date are deleted.
# - Records with no sampling location or site description are deleted.
# - Only European records are retained
#
#
# 2. MANUAL CORRECTIONS OF WRONG CAS NUMBERS ARE APPLIED
#
#
# - INPUT:
# 1. pharms-uba_v3_2021_0-20220704.xlsx
#       An Excel .xlsx file downloaded from the UBA website at:
#       https://www.umweltbundesamt.de/dokument/database-pharmaceuticals-in-the-environment-excel
#
# This has two tabs in the excel spreadsheet which are import separately and merged in the script.
#
#
# 2. Article_57_drugbank_v9_Rev52_17112022.csv
#   A list of pharmaceuticals listed in the EMAs Article 57. 
#    (the output from the R Script "Article_57_pharma_import_and_clean_v10.R")
#     (Used to filter out non-pharamceutical records.)
#
#
# - OUPUT
# 1.  UBA_pharma_surface_water_v5_rev3_2021.csv
#
##################################################################################################################
# Original author: Pedro Inostroza
# Edits: Patrik Svedberg, Francis Spilsbury


#######Housekeeping#######
rm(list=ls()) #remove ALL objects 
cat("\014") # clear console window prior to new run
Sys.setenv(LANG = "en") #Let's keep stuff in English
Sys.setlocale("LC_ALL","English")
options(scipen=999) #avoid scientific annotation
##########################################################################

##############################################
# Set constants
##############################################

# Version
vers <- 6


#UBA Database version
UBA_vers <- 3
UBA_Timestamp <- "2021"


############
# Set the current working directory, i.e. where all files are kept
############
inwd <- "E:/GU/AA ARTICLE PHARMACEUTICALS IN EU/R/GitHub/R Input"
outwd <- "E:/GU/AA ARTICLE PHARMACEUTICALS IN EU/R/GitHub/R Output"
setwd(inwd)


################
# packages, functions
################
library(tidyverse)
library(readxl)
library(writexl)
library(webchem)
library(rvest)
library(xml2)
library(data.table)
library(stringi)
library(rematch)



CAS_String_Convert = function(x) {
  # x = "98106173"    #for testing pruposes only
  # NB requires stringi package
  
  stri_sub(str = x, 
           from = (nchar(x) -2), 
           to = (nchar(x) -3)) = "-" # puts in he first dash
  stri_sub(str = x, 
           from = (nchar(x)), 
           to = 0) = "-" # puts in the second dash
  x
}



##############################################
# IMPORTS
##############################################

#Import the envionrmental monitoring data (First tab)
UBA_Monitoring <- as.data.frame(read_xlsx(path = paste0(inwd, "/pharms-uba_v3_2021_0-20220704.xlsx"), sheet =1))

#Import the information about the chemicals (Second tab)
UBA_Chemicals <- as.data.frame(read_xlsx(path = paste0(inwd, "/pharms-uba_v3_2021_0-20220704.xlsx"), sheet =2))

#Import Downloaded data from Article 57 Website and Drugbank info:
Art_57_Data <- read.csv(paste0(inwd, "/Article_57_drugbank_v9_Rev52_17112022.csv"),
                        sep = "\t", header = TRUE, stringsAsFactors = FALSE)


##################################################################################################################
# Tidying Up
##################################################################################################################

str(UBA_Monitoring)
str(UBA_Chemicals)
#For the environmental monitoring data, the
#Variable names are actually listed in the second row.
colnames(UBA_Monitoring) <- UBA_Monitoring[2,]


#remove all spaces:
colnames(UBA_Monitoring) <- gsub(" ", "_", colnames(UBA_Monitoring))
colnames(UBA_Chemicals) <- gsub(" ", "_", colnames(UBA_Chemicals))


#Trim off the top 2 rows that contain repeats of the variable names
UBA_Monitoring <- UBA_Monitoring[3:nrow(UBA_Monitoring),]


#The following variables in UBA_Monitoring are converted to numeric format:
UBA_Monitoring$Number_of_Samples_Analysed <- as.numeric(UBA_Monitoring$Number_of_Samples_Analysed)
UBA_Monitoring$MEC_original <- as.numeric(UBA_Monitoring$MEC_original)
UBA_Monitoring$MEC_standardized<- as.numeric(UBA_Monitoring$MEC_standardized)
UBA_Monitoring$LoD_Limit_of_Detection<- as.numeric(UBA_Monitoring$LoD_Limit_of_Detection)
UBA_Monitoring$LoD_standardized <- as.numeric(UBA_Monitoring$LoD_standardized)


#Make all names of analytes lower case:
#(for matching purposes later)
UBA_Monitoring$Name_of_Analyte_Original <- UBA_Monitoring$Name_of_Analyte

UBA_Monitoring$Name_of_Analyte <- tolower(UBA_Monitoring$Name_of_Analyte)
UBA_Chemicals$Name_of_Analyte <- tolower(UBA_Chemicals$Name_of_Analyte)



##################################################################################################################
# DELETIONS
##################################################################################################################


#Delete any results that are not a single value.
UBA_Monitoring %>% group_by(Statistics) %>% tally()

UBA_Monitoring <- subset(UBA_Monitoring, grepl("single value", UBA_Monitoring$Statistics, ignore.case = TRUE))


#Delete any results that dont have a valid date.
as.data.frame(UBA_Monitoring %>% group_by(Sampling_Period_Start) %>% tally())

UBA_Monitoring <- subset(UBA_Monitoring, !grepl("-9999", UBA_Monitoring$Sampling_Period_Start, ignore.case = TRUE))


#Delete any results that dont have a credibility rating of "good". Delete "questionable"and "unknown"
as.data.frame(UBA_Monitoring %>% group_by(Literature_Credibility) %>% tally())

UBA_Monitoring <- subset(UBA_Monitoring, grepl("good", UBA_Monitoring$Literature_Credibility, ignore.case = TRUE))


#Delete any results that dont have a MEC value.
UBA_Monitoring <- subset(UBA_Monitoring, !grepl("-9999", UBA_Monitoring$MEC_standardized, ignore.case = TRUE))


#Delete results that have neither sampling location NOR sampling description
UBA_Monitoring <- subset(UBA_Monitoring, !(is.na(UBA_Monitoring$Sampling_Location) & is.na(UBA_Monitoring$Sampling_Description)))



##################################################################################################################
# Merging
##################################################################################################################

#Check that Name_of_Analyte is a match in both dataframes:

Name_Mismatches <- unique(UBA_Monitoring$Name_of_Analyte)[!unique(UBA_Monitoring$Name_of_Analyte) %in% UBA_Chemicals$Name_of_Analyte]


#If there's no chemicals in the monitoring data that are lacking chemical info, then merge:
if (length(Name_Mismatches) == 0) {
  
UBA_Data <- merge(UBA_Monitoring, UBA_Chemicals, by = "Name_of_Analyte")

} else {
  
  print("Names mismatching: Merge incomplete")
  stop()
  
}

str(UBA_Data)


##################################################################################################################

# CAS number checking

##################################################################################################################

#Check for mismatches between UBA_Chemicals and UBA_Monitoring:

unique(UBA_Data$Name_of_Analyte[UBA_Data$CAS_number.x != UBA_Data$CAS_number.y])

# 5 chemicals have mismatching CAS numbers
#[1] NA                       "cyproterone"            "megestrol acetate"      "n4-acetylsulfathiazole"
#[5] "phenobarbital"          "simvastatin" 

filter(UBA_Data[, c("Name_of_Analyte", "CAS_number.x", "CAS_number.y")], Name_of_Analyte == "cyproterone")
filter(UBA_Data[, c("Name_of_Analyte", "CAS_number.x", "CAS_number.y")], Name_of_Analyte == "megestrol acetate")
filter(UBA_Data[, c("Name_of_Analyte", "CAS_number.x", "CAS_number.y")], Name_of_Analyte == "n4-acetylsulfathiazole")
filter(UBA_Data[, c("Name_of_Analyte", "CAS_number.x", "CAS_number.y")], Name_of_Analyte == "phenobarbital"  )
filter(UBA_Data[, c("Name_of_Analyte", "CAS_number.x", "CAS_number.y")], Name_of_Analyte == "simvastatin")
filter(UBA_Data, is.na(Name_of_Analyte))


#List the chemicals that have no valid CAS number (that are NA):
unique(UBA_Monitoring[is.na(UBA_Monitoring$CAS_number), c("Name_of_Analyte", "CAS_number")])

#[1] "10-hydroxy-carbamazepine"                                                 29331-92-8                                                     
#[2] "17-alpha-estradiol-3-sulfate"                                                    
#[3] "21-alpha-hydroxyprogesterone"                                             64-85-7     
#[4] "4-(2-methylethyl)-1,5-dimethyl-1,2-dehydro-3-pyrazolone"                         
#[5] "4,6-cl-triclosan"                                                                
#[6] "4-cl-triclosan"                                                                  
#[7] "4-hydroxydiclofenac-dehydrate"                                            64118-84-9       
#[8] "6-cl-triclosan"                                                                  
#[9] "alpha-apo-oxytetracycline"                                                18695-01-7                                                       
#[10] "amdoph (1-acetyl-1-methyl-2-dimethyl-oxamoyl-2-phenylhydrazide)"         519-65-3      
#[11] "amino-valsartan"                                                         147225-68-1      
#[12] "beta-apo-oxytetracycline"                                                18751-99-0
#[13] "carbamazepine-10oh"                                                      29331-92-8      
#[14] "carboxyacyclovir"                                                        80685-22-9       
#[15] "dealkylated valsartan"                                                   914465-68-2                                                       
#[16] "epi-iso-chlorotetracycline"                                              14297-93-9                                                    
#[17] "erythrohydrobupropion"                                                   99102-04-2                                                       
#[18] "erythromycin-h2o"                                                        67733-56-6     
#[19] "estrogene"                                                               362-07-2        
#[20] "fluorquinolone"                                                          124467-22-7        
#[21] "guanylurea"                                                              141-83-3                                                               
#[22] "iohexol phase i"                                                         66108-95-0        
#[23] "iohexol phase ii"                                                        66108-95-0        
#[24] "iomeprol phase i"                                                        78649-41-9        
#[25] "iomeprol phase ii"                                                       78649-41-9        
#[26] "iopamidol phase i"                                                       60166-93-0        
#[27] "iopromide phase i"                                                       73334-07-3        
#[28] "iopromide phase ii"                                                      73334-07-3        
#[29] "iopromide phase iii"                                                     73334-07-3        
#[30] "na873 (6,8 dibrom-3-(trans-4-hydroxy-cyclohexyl)-1,2,3,4-tetrahydro-chinazoline)"
#[31] "penicilline"                                                                     
#[32] "sulfonamide"                                                                     
#[33] "triclosan-ome" 


#list the chemicals that have the wrong CAS number format:
unique(UBA_Data[ !grepl("-", UBA_Data$"CAS_number.y") , c("Name_of_Analyte", "CAS_number.x", "CAS_number.y")])

#as above, plus:
#
# "erythromycin oxime"                                                            134931-01-4
# "phenobarbital"                                                                 50-06-6         
# "simvastatin"                                                                   79902-63-9
# "n4-acetylsulfathiazole"                                                        127-76-4


#################
#INSERT WEBCHEM LOOP FOR CAS NUMBER LOOKUP HERE
################
#i = 2000
#for (i in 1:nrow(UBA_Data)){
#  
#  if (is.na(UBA_Data[i, "CAS_number.x"]) | is.na(UBA_Data[i, "CAS_number.y"])){
#  
#  Chem_Name <- UBA_Data[i, "Name_of_Analyte_Original"]#  
#
#  Chem_InChiKey <- unlist(cir_query(identifier = Chem_Name, representation = "stdinchikey", match = "first", verbose = TRUE))
#  Chem_InChiKey <- gsub("InChIKey=", "", Chem_InChiKey)
#  
#  Chem_CAS <- unlist(cir_query(Chem_Name, resolver = "name_pattern", representation = "cas", match = "first", verbose = TRUE))
#  
#  Sys.sleep(1.1)
#
#  }
#}

#As of 07/07/2022:
# Manual adjustments of missing CAS numbers:
# (CAS numbers were obtained via a manual Google and pubchem search (https://pubchem.ncbi.nlm.nih.gov/)

#Manual CAS number corrections:
UBA_Data[UBA_Data$Name_of_Analyte == "10-hydroxy-carbamazepine", "CAS_number.x"] <- "29331-92-8"
UBA_Data[UBA_Data$Name_of_Analyte == "alpha-apo-oxytetracycline", "CAS_number.x"] <- "18695-01-7"
UBA_Data[UBA_Data$Name_of_Analyte == "beta-apo-oxytetracycline", "CAS_number.x"] <- "18751-99-0"
UBA_Data[UBA_Data$Name_of_Analyte == "erythrohydrobupropion", "CAS_number.x"] <- "99102-04-2"
UBA_Data[UBA_Data$Name_of_Analyte == "guanylurea", "CAS_number.x"] <- "141-83-3"
UBA_Data[UBA_Data$Name_of_Analyte == "erythromycin oxime", "CAS_number.x"] <- "134931-01-4"
UBA_Data[UBA_Data$Name_of_Analyte == "phenobarbital", "CAS_number.x"] <- "50-06-6"
UBA_Data[UBA_Data$Name_of_Analyte == "simvastatin", "CAS_number.x"] <- "79902-63-9"
UBA_Data[UBA_Data$Name_of_Analyte == "n4-acetylsulfathiazole", "CAS_number.x"] <- "127-76-4"
UBA_Data[UBA_Data$Name_of_Analyte == "cyproterone", "CAS_number.x"] <- "2098-66-0"
UBA_Data[UBA_Data$Name_of_Analyte == "megestrol acetate", "CAS_number.x"] <- "595-33-5"
UBA_Data[UBA_Data$Name_of_Analyte == "21-alpha-hydroxyprogesterone", "CAS_number.x"] <- "64-85-7"
UBA_Data[UBA_Data$Name_of_Analyte == "4-hydroxydiclofenac-dehydrate", "CAS_number.x"] <- "64118-84-9"
UBA_Data[UBA_Data$Name_of_Analyte == "amdoph (1-acetyl-1-methyl-2-dimethyl-oxamoyl-2-phenylhydrazide)", "CAS_number.x"] <- "519-65-3"
UBA_Data[UBA_Data$Name_of_Analyte == "amino-valsartan", "CAS_number.x"] <- "147225-68-1"
UBA_Data[UBA_Data$Name_of_Analyte == "carbamazepine-10oh", "CAS_number.x"] <- "29331-92-8"
UBA_Data[UBA_Data$Name_of_Analyte == "carboxyacyclovir", "CAS_number.x"] <- "80685-22-9"
UBA_Data[UBA_Data$Name_of_Analyte == "dealkylated valsartan", "CAS_number.x"] <- "914465-68-2"
UBA_Data[UBA_Data$Name_of_Analyte == "epi-iso-chlorotetracycline", "CAS_number.x"] <- "14297-93-9"
UBA_Data[UBA_Data$Name_of_Analyte == "erythromycin-h2o", "CAS_number.x"] <- "67733-56-6"
UBA_Data[UBA_Data$Name_of_Analyte == "estrogene", "CAS_number.x"] <- "362-07-2"
UBA_Data[UBA_Data$Name_of_Analyte == "fluorquinolone", "CAS_number.x"] <- "124467-22-7"
UBA_Data[UBA_Data$Name_of_Analyte == "iohexol phase i", "CAS_number.x"] <- "66108-95-0"
UBA_Data[UBA_Data$Name_of_Analyte == "iohexol phase ii", "CAS_number.x"] <- "66108-95-0"
UBA_Data[UBA_Data$Name_of_Analyte == "iomeprol phase i", "CAS_number.x"] <- "78649-41-9"
UBA_Data[UBA_Data$Name_of_Analyte == "iomeprol phase ii", "CAS_number.x"] <- "78649-41-9"
UBA_Data[UBA_Data$Name_of_Analyte == "iopamidol phase i", "CAS_number.x"] <- "60166-93-0"
UBA_Data[UBA_Data$Name_of_Analyte == "iopromide phase i", "CAS_number.x"] <- "73334-07-3"
UBA_Data[UBA_Data$Name_of_Analyte == "iopromide phase ii", "CAS_number.x"] <- "73334-07-3"
UBA_Data[UBA_Data$Name_of_Analyte == "iopromide phase iii", "CAS_number.x"] <- "73334-07-3"



#Make Alt_CAS_Number column (no dashes)
UBA_Data$Alt_CAS_Number <- gsub("-", "", UBA_Data$CAS_number.x)

#Strip whitespace from end of CAS strings
UBA_Data$Alt_CAS_Number <- UBA_Data$Alt_CAS_Number %>% trimws(whitespace = "[\\h\\v]")

#Run all Alt_CAS_Number through the CAS string function
# (makes sure there are no other chemicals with erroneous CAS number formats)
UBA_Data$CAS_number <- CAS_String_Convert(UBA_Data$Alt_CAS_Number)



### filter out chemicals without CAS number
UBA_Data_No_CAS <-filter(UBA_Data, !grepl('^[0-9]+-[0-9]{2}-[0-9]$', CAS_number))          #entries with empty cells plus wrong numbers and NAs, in total 1400 chemicals

UBA_Data_No_CAS <- subset(UBA_Data_No_CAS, !duplicated(UBA_Data_No_CAS$Name_of_Analyte))

#######################################

# Tidying Up

#######################################

UBA_Data$CAS_number.x <- NULL
UBA_Data$CAS_number.y <- NULL

rm(UBA_Chemicals)
rm(UBA_Monitoring)


#Make a copy of all deleted records, then delete those with no CAS number
No_CAS_Deletions <- subset(UBA_Data, is.na(UBA_Data$CAS_number))

UBA_Data <- subset(UBA_Data, !is.na(UBA_Data$CAS_number))

### filter out chemicals without CAS number
uba_all_nope_cas<-filter(UBA_Data, !grepl('^[0-9]+-[0-9]{2}-[0-9]$', CAS_number))          #entries with empty cells plus wrong numbers and NAs, in total 1400 chemicals

unique(uba_all_nope_cas$Name_of_Analyte)

##################################################################################################################

# Checking Type of Analyte

##################################################################################################################

dummy <- subset(UBA_Data, UBA_Data$Type_of_Analyte.x != UBA_Data$Type_of_Analyte.y)
unique(dummy[ , c("Name_of_Analyte", "CAS_number", "Type_of_Analyte.x", "Type_of_Analyte.y")])

# There are two compounds that have a mismatched Type_of_Analyte:
#                  Name_of_Analyte      CAS_number      Type_of_Analyte.x             Type_of_Analyte.y
#     17-beta-estradiol-17-glucuronide  1806-98-0     Transformation Product                 Parent
#     beclomethasone 17,21-dipropionate 5534-09-8           Parent                 Transformation product
#
# 17-beta-estradiol-17-glucuronide is a metabolite (transformation product)  #Manual Google check
# beclomethasone 17,21-dipropionate is a parent compound.                    #Manual Google check


#Manual correction
UBA_Data[UBA_Data$Name_of_Analyte == "17-beta-estradiol-17-glucuronide", "Type_of_Analyte.x"] <- "Transformation Product"
UBA_Data[UBA_Data$Name_of_Analyte == "beclomethasone 17,21-dipropionate", "Type_of_Analyte.x"] <- "Parent"


#Tidying Up, and remove duplicate column
UBA_Data <- rename(UBA_Data, Type_of_Analyte = Type_of_Analyte.x)
UBA_Data$Type_of_Analyte.y <- NULL

rm(dummy)


##################################################################################################################

# Name Checking

##################################################################################################################

dummy <- UBA_Data %>% group_by(Name_of_Analyte, CAS_number)%>% 
  summarise(count = n())

duplicated_CAS_entries <- dummy$CAS_number[duplicated(dummy$CAS_number)]

duplicated_Name_entries <- dummy$Name_of_Analyte[duplicated(dummy$Name_of_Analyte)]

CAS_Duplicates <- subset(dummy, CAS_number %in% duplicated_CAS_entries)
CAS_Duplicates <- as.data.frame(CAS_Duplicates[order(CAS_Duplicates$CAS_number, -CAS_Duplicates$count) ,])


#Name_of_Analyte  CAS_number count
#1                  n4-acetyl sulfamethazine    100-90-3    35
#2                   n4-acetylsulfamethazine    100-90-3    15
#3                  sulfamethazine-n4-acetyl    100-90-3    23
#4                  n,o-desmethylvenlafaxine 135308-74-6    54
#5                n,o didesmethylvenlafaxine 135308-74-6    64
#6                   4-formylaminoantipyrine   1672-58-8   143
#7                     formylaminoantipyrine   1672-58-8    12
#8                n-formyl-4-aminoantipyrine   1672-58-8   128
#9                     o-hydroxyatorvastatin 214217-86-6    22
#10                    p-hydroxyatorvastatin 214217-86-6    22
#11                               clobetasol  25122-46-7    29
#12                     clobetasol propinate  25122-46-7     4
#13                alpha-apo-oxytetracycline   2869-27-4    15
#14                 beta-apo-oxytetracycline   2869-27-4     8
#15                 10-hydroxy-carbamazepine  29331-92-8    35
#16    10,11-dihydro-10-hydroxycarbamazepine  29331-92-8   126
#17   10,11-dihydro-10,11-epoxycarbamazepine  36507-30-9   150
#18              carbamazepine-10,11-epoxide  36507-30-9   216
#19              17-beta-estradiol-3-sulfate    481-96-9    55
#20                      estradiol-3-sulfate    481-96-9    34
#21                    hydroxy-metronidazole   4812-40-2   143
#22                         metronidazole-oh   4812-40-2    30
#23                        17-beta-estradiol     50-28-2  2625
#24                                estradiol     50-28-2   342
#25                            amitriptyline     50-48-6   381
#26                            amitryptiline     50-48-6   437
#27                              d-equilenin    517-09-9    18
#28                                equilenin    517-09-9    35
#29                      1-hydroxy ibuprofen  53949-53-4    78
#30                         hydroxyibuprofen  53949-53-4    60
#31         betamethasone 17,21-dipropionate   5593-20-4     8
#32               betamethasone dipropionate   5593-20-4    19
#33                            atenolol acid  56392-14-4    92
#34                          metoprolol acid  56392-14-4    81
#35                                meclizine    569-65-3    17
#36                                meclozine    569-65-3   109
#37                            sulfadimidine     57-68-1   970
#38                           sulfamethazine     57-68-1  3050
#39                4-dimethylaminoantipyrine     58-15-1    44
#40                   dimethylaminophenazone     58-15-1    56
#41                           sulfamethoxine    651-06-9   118
#42                       sulfametoxydiazine    651-06-9   192
#43                   2-hydroxycarbamazepine  68011-66-5    56
#44                        carbamazepine-2oh  68011-66-5    53
#45       6-alpha-methyl-hydroxyprogesterone     71-58-9    14
#46              medroxyprogesterone acetate     71-58-9    67
#47 17-alpha-ethinylestradiol 3-methyl ether     72-33-3     3
#48                                mestranol     72-33-3   248
#49               2-ethyl-2-phenylmalonamide   7206-76-0    45
#50                    phenylethylmalonamide   7206-76-0    32
#51                 monoethylglycinexylidide   7728-40-7     7
#52                             norlidocaine   7728-40-7    54
#53                fluticasone 17-propionate  80474-14-2     8
#54                   fluticasone propionate  80474-14-2    32
#55                    4-acetaminoantipyrine     83-15-8   340
#56                     acetoaminoantipyrine     83-15-8    23
#57                           desvenlafaxine  93413-62-8   281
#58                   o-desmethylvenlafaxine  93413-62-8   237
#59                            diflofloxacin  98106-17-3     6
#60                               difloxacin  98106-17-3   209



#Manual Corrections:
#Choose the name which has the highest number of occurances in the monitoring data
#>>>is this valid?? 
#>>>probably doesn't make a difference as matching is done by CAS, not by name
#
#cas = unique(CAS_Duplicates$CAS_number)[2]    #For testing purposes

for (cas in unique(CAS_Duplicates$CAS_number)) {
  
  #Select the Most commonly listed name for the compound
  dummy <- subset(CAS_Duplicates, CAS_number == cas)
  dummy <- as.data.frame(dummy[order( -dummy$count) ,])
  
  
  Chem_Name <- dummy$Name_of_Analyte[1]
  
  #Insert correct name into UBA_Data:
  UBA_Data[UBA_Data$CAS_number == cas, "Name_of_Analyte"] <- Chem_Name
  
}


#Re-Check that all substitutions have been made:

dummy <- UBA_Data %>% group_by(Name_of_Analyte, CAS_number)%>% 
  summarise(count = n())

duplicated_CAS_entries <- dummy$CAS_number[duplicated(dummy$CAS_number)]
duplicated_Name_entries <- dummy$Name_of_Analyte[duplicated(dummy$Name_of_Analyte)]

if (length(duplicated_CAS_entries) > 0 | length(duplicated_Name_entries) > 0) {
  
  print("Problem with Duplicated Names")
  
  stop()
  
}


length(levels(as.factor(UBA_Data$CAS_number)))



Backup <- UBA_Data

###########################################################
### Filter for Surface Water Data Only
###########################################################
levels(as.factor(UBA_Data$Matrix))

UBA_Data <- subset(UBA_Data, UBA_Data$Matrix %in% c("Surface Water - Aquaculture",
                                                    "Surface Water - Estuary",
                                                    "Surface Water - Lake",
                                                    "Surface Water - Pond",                        
                                                    "Surface Water - River/Stream",
                                                    "Surface Water - Sea or Ocean",                
                                                    "Surface Water - unspecific"))


###########################################################
### Filter for EU Countries
###########################################################
levels(as.factor(UBA_Data$Sampling_Country))

#Greenland is part of Denmark:
UBA_Data$Sampling_Country <- ifelse(test = grepl("Greenland", UBA_Data$Sampling_Country, ignore.case = TRUE),
                                    yes = "Denmark", no = UBA_Data$Sampling_Country)

#Faroe Islands are part of Denmark:
UBA_Data$Sampling_Country <- ifelse(test = grepl("Faroe", UBA_Data$Sampling_Country, ignore.case = TRUE),
                                    yes = "Denmark", no = UBA_Data$Sampling_Country)


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
                  #"Moldova",
                  "Netherlands",
                  "Norway",
                  "Poland",
                  "Portugal",
                  "Romania" ,
                  #"Serbia" ,
                  "Slovakia"  ,
                  "Slovenia",
                  "Spain",
                  "Sweden",
                  "Switzerland"  )
                  #"Turkey" ,
                  #"Ukraine",
                  #"United Kingdom" )

UBA_Data <- subset(UBA_Data, UBA_Data$Sampling_Country %in% EU_Countries)


length(levels(as.factor(UBA_Data$CAS_number)))
length(levels(as.factor(UBA_Data$Sampling_Country)))
length(levels(as.factor(UBA_Data$Sampling_Location)))
length(levels(as.factor(UBA_Data$Sampling_Description)))
length(levels(as.factor(UBA_Data$Matrix)))


###########################################################
### Filter for Article 57 Pharmaceuticals
###########################################################

#remove all Monitoring data values for compounds not in the Article 57 List of pharmaceuticals:
UBA_Data <- subset(UBA_Data, UBA_Data$CAS_number %in% levels(as.factor(Art_57_Data$CAS_number)))

as.data.frame(UBA_Data %>% group_by(UBA_Data$CAS_number) %>% tally())
as.data.frame(UBA_Data %>% group_by(UBA_Data$Sampling_Country) %>% tally())
as.data.frame(UBA_Data %>% group_by(UBA_Data$Sampling_Location) %>% tally())
as.data.frame(UBA_Data %>% group_by(UBA_Data$Matrix) %>% tally())



###########################################################
### Export
###########################################################

write_delim(x = UBA_Data, file = paste0(outwd, "/UBA_pharma_surface_water_v", 
                                        vers, "_rev", UBA_vers, "_", UBA_Timestamp, ".csv"),
            delim = "\t", escape = "none", quote = "none")
