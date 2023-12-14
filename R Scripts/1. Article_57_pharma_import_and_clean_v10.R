##########################################################################
#
# ARTICLE 57 PHARMA IMPORT AND CLEAN
#
##########################################################################
#
## Original author: Pedro Inostroza
# Edits: Patrik Svedberg
# Extensive re-write: Francis Spilsbury, 14th July 2022
#
# This script takes a download of the publicly available Article 57 database
# (available at: https://www.ema.europa.eu/en/human-regulatory/post-authorisation/data-medicines-iso-idmp-standards/public-data-article-57-database)
#
# Latest Article 57 download from EMA:
#   Revision 52
#   17th November 2022
#
#######################################
# Cleans the input data to produce a list of searchable/matchable chemical names in the Article 57 database
#######################################
#
# This list of pharmaceutical active substances is cleaned:
# - All herbal compounds are removed (uses EMA's registered herbal substances list to do this)
#
# - Notations about dosage are removed (a series of REGEX expressions)
# 
# - Punctuation artifacts are removed  (a series of REGEX expressions)
#
# - Duplicates are removed.
#
#
# This cleaned list of names is exported as both a .csv tab delimited file 
#
#
#
#INPUTS
# - article-57-product-data_en_12072022.csv     (download from EMA)
#
# - Medicines_output_herbal_medicines.xlsx      (download from EMA)
#
#
# OUTPUTS
# - Article_57_Clean_List_VV__RevRR_DDDDDDDD.csv (Article 57 chemicals with cleaned list of active substance names: just a list of API names)
#
#
############################################################
#
##########################
#
# Housekeeping
rm(list=ls()) #remove ALL objects 
cat("\014") # clear console window prior to new run
Sys.setenv(LANG = "en") #Let's keep stuff in English
Sys.setlocale("LC_ALL","English")
options(scipen=999) #avoid scientific annotation
##########################


##########################
# Set script version number
vers <- 10


##########################
#Article 57 Download Version:
#Revision 47, downloaded 16th June 2022
vers_art57 <- "Rev52_17112022"


##########################
# Set working directory:
inwd <- "E:/GU/AA ARTICLE PHARMACEUTICALS IN EU/R/GitHub/R Input"
outwd <- "E:/GU/AA ARTICLE PHARMACEUTICALS IN EU/R/GitHub/R Output"
setwd(inwd)


##########################
# Get packages
packages<-c("tidyverse", "readxl", "writexl", "reshape2", "webchem", "rvest", "xml2", "data.table", "urltools")
lapply(packages, library, character.only=TRUE)


##########################
# Define functions:
Extract_Search_Term <- function(term, n){
  
  search_term <- unlist(str_split(term, pattern = " "))[n]
  
  return(search_term)
  
}


##########################
# Import files
##########################

#Import Downloaded data from Article 57 Website.
#
#Available as an Excel workbook from:
#https://www.ema.europa.eu/en/human-regulatory/post-authorisation/data-medicines-iso-idmp-standards/public-data-article-57-database

Art_57_Data <- read_excel(paste0(inwd, "/article-57-product-data_en_17112022.xlsx"))


#Import Downloaded Data for Herbal and homeopathic medicines from EMA Website
#
#Available as an Excel workbook from:
#https://www.ema.europa.eu/en/medicines/download-medicine-data#herbal-medicines-section

EMA_PAR_Herbal <-  read_excel(paste0(inwd, "/medicines_output_herbal_medicines_en.xlsx"))


##########################
# Tidy up
##########################
#
# For the Article 57 list:
# - Column names are contained in row #15,
# - Rows 1:14 are not used

colnames(Art_57_Data) <- Art_57_Data[15,]

colnames(Art_57_Data) <- c("Product_name",
                           "Active_substance",
                           "Route_of_administration",
                           "Product_authorisation_country",
                           "Marketing_authorisation_holder",
                           "Pharmacovigilance system master file location",                                                                 
                           "Pharmacovigilance enquiries email address",                                                                     
                           "Pharmacovigilance enquiries telephone number" )

Art_57_Data <- Art_57_Data[16:nrow(Art_57_Data),]

#Remove unnecessary columns

Art_57_Data <- Art_57_Data[c("Product_name",
                             "Active_substance",
                             "Route_of_administration",
                             "Product_authorisation_country",
                             "Marketing_authorisation_holder")]        



# For the EMA list of herbal products.
#Column name information is held in ROW 8, and Rows 1:7 are blank.
colnames(EMA_PAR_Herbal) <- EMA_PAR_Herbal[8,]
EMA_PAR_Herbal <- EMA_PAR_Herbal[9:nrow(EMA_PAR_Herbal),]


#Adjust colnames : remove  " " and replace with "_"
colnames(EMA_PAR_Herbal) <- gsub(" ", "_", colnames(EMA_PAR_Herbal))

##########################
# Deletions
##########################

#Remove any NA Active Substances:
Art_57_Data <- subset(Art_57_Data, !is.na(Art_57_Data$Active_substance))


#Remove any herbal extracts:
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Extract", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Extract", x = Art_57_Data$Product_name, ignore.case = TRUE))

Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Essential", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Essential", x = Art_57_Data$Product_name, ignore.case = TRUE))

Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Oil", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Oil", x = Art_57_Data$Product_name, ignore.case = TRUE))

Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Dried", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Dried", x = Art_57_Data$Product_name, ignore.case = TRUE))

Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Pressed", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Pressed", x = Art_57_Data$Product_name, ignore.case = TRUE))

Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Herb", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Herb", x = Art_57_Data$Product_name, ignore.case = TRUE))

Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Tincture", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Tincture", x = Art_57_Data$Product_name, ignore.case = TRUE))

Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Leaf", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Leaf", x = Art_57_Data$Product_name, ignore.case = TRUE))

Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Flower", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Flower", x = Art_57_Data$Product_name, ignore.case = TRUE))

Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Plant", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Plant", x = Art_57_Data$Product_name, ignore.case = TRUE))

Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Balsam", x = Art_57_Data$Active_substance, ignore.case = TRUE))
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "Balsam", x = Art_57_Data$Product_name, ignore.case = TRUE))


#Remove obvious duplicates:
Art_57_Data <- subset(Art_57_Data, !duplicated(Art_57_Data$Active_substance))
Art_57_Data <- subset(Art_57_Data, !duplicated(Art_57_Data$Product_name))


#Make a list of names of plants used for herbal medicines
#Taken from the EMA herbal medicine register

#Make additional columns that contain each search term for herbal products:
for (i in 1:nrow(EMA_PAR_Herbal)){
  
  EMA_PAR_Herbal[i, "Latin_search_term"]  <- Extract_Search_Term(EMA_PAR_Herbal[i, "Latin_name_of_herbal_substance"],1)
  EMA_PAR_Herbal[i, "Latin_search_term_2"]  <- Extract_Search_Term(EMA_PAR_Herbal[i, "Latin_name_of_herbal_substance"],2)
  
  EMA_PAR_Herbal[i, "Botanical_search_term"] <- Extract_Search_Term(EMA_PAR_Herbal[i, "Botanical_name_of_plant"],1)
  EMA_PAR_Herbal[i, "Botanical_search_term_2"] <- Extract_Search_Term(EMA_PAR_Herbal[i, "Botanical_name_of_plant"],2)
  
}


#Put all search terms into a vector:
Herbal_search_list <- as.character(unlist(c(EMA_PAR_Herbal[,c("Latin_search_term",
                                                              "Latin_search_term_2",
                                                              "Botanical_search_term",                  
                                                              "Botanical_search_term_2")])))

#Remove artifacts like "[" and "()"
Herbal_search_list <- gsub("\\[", "", Herbal_search_list)
Herbal_search_list <- gsub("L\\.", "", Herbal_search_list)
Herbal_search_list <- gsub("\\(\\)", "", Herbal_search_list)

#Remove duplicates
Herbal_search_list <- Herbal_search_list[!is.na(Herbal_search_list)]
Herbal_search_list <- Herbal_search_list[!duplicated(Herbal_search_list)]


#Remove any remaining herbal remedies
for (i in 1:length(Herbal_search_list)){
  
  Art_57_Data<- subset(Art_57_Data, !grepl(pattern = Herbal_search_list[i], x = Art_57_Data$Active_substance, ignore.case = TRUE))
  Art_57_Data<- subset(Art_57_Data, !grepl(pattern = Herbal_search_list[i], x = Art_57_Data$Product_name, ignore.case = TRUE))
  
}


##########################
# Tidy up:
##########################

#Convert everything to the latin alphabet
#Solves problems from importing special characters such as "???" and "â¬"
#e.g. Converts "???" to  "?~"
Art_57_Data$Product_name <-iconv(Art_57_Data$Product_name, to = "latin1")
Art_57_Data$Active_substance <-iconv(Art_57_Data$Active_substance, to = "latin1")


#Removes the "?~" and "??"artifact that gets produced by the above iconv
Art_57_Data$Product_name <-gsub(pattern = "?~", replacement = "", x = Art_57_Data$Product_name)
Art_57_Data$Active_substance <-gsub(pattern = "?~", replacement = "", x = Art_57_Data$Active_substance)

Art_57_Data$Product_name <-gsub(pattern = "??", replacement = "", x = Art_57_Data$Product_name)
Art_57_Data$Active_substance <-gsub(pattern = "??", replacement = "", x = Art_57_Data$Active_substance)

Art_57_Data$Product_name <-gsub(pattern = "?", replacement = "", x = Art_57_Data$Product_name)
Art_57_Data$Active_substance <-gsub(pattern = "?", replacement = "", x = Art_57_Data$Active_substance)



##########################
# List each active ingredient separately, if there's more than one
##########################

# Where there are more than one active ingredients, 
# these are separated by commas or by "|" (i.e. ", " , "|" ) in the "Active_substance" column.

# List each active ingredient separately:
Separated_APIs <- as.data.frame(str_split(string = Art_57_Data$Active_substance, pattern = "(, |\\|)", n=Inf, simplify = TRUE))


#Fix the column names in the new dataframe:
colnames(Separated_APIs) <- c(paste0("Active_substance_", c(1:ncol(Separated_APIs))))


#Add the separated active substances to Art_57_Data:
Art_57_Data <- cbind(Art_57_Data, Separated_APIs)


#Remove unnecessary variables
Art_57_Data$Active_substance <- NULL
Art_57_Data$Route_of_administration <- NULL
Art_57_Data$Product_authorisation_country <- NULL
Art_57_Data$Marketing_authorisation_holder <- NULL


#Melt the data frame to long format:
Art_57_Data <- melt(Art_57_Data, id.vars = c("Product_name"))
Art_57_Data <- rename(Art_57_Data, AS_number = variable)
Art_57_Data <- rename(Art_57_Data, Active_substance = value)



##########################
# Deletions
##########################

#Remove any empty fields:
Art_57_Data <- subset(Art_57_Data, Art_57_Data$Active_substance != "")
Art_57_Data <- subset(Art_57_Data, Art_57_Data$Product_name != "")


#Remove any product names that contain "??"
Art_57_Data <- subset(Art_57_Data, !grepl(pattern = "[??]", x = Art_57_Data$Product_name))



##########################
# Analyte names tidy up 
##########################
#Make a copy of the original name
Art_57_Data$Active_substance_original <- Art_57_Data$Active_substance


#There are multiple entries for different dilutions of the same active substance
# either "Dil."or "Dx" or "D x"

#Remove all "Dil." or "Dil" from Active substance names
Art_57_Data$Active_substance <- str_replace_all(pattern = "Dil\\.?\\s?\\d?", replacement = "", string = Art_57_Data$Active_substance)


#Remove all variations of D4, D10, D300 etc
Art_57_Data$Active_substance <- str_replace_all(pattern = "D\\s?+\\d{1,}", replacement = "", string = Art_57_Data$Active_substance)


#Remove all variations of C5, C10, C100 etc
Art_57_Data$Active_substance <- str_replace_all(pattern = "C\\s?+\\d{1,}", replacement = "", string = Art_57_Data$Active_substance)


#Ph. Eur. refers to a reference standard from the European Pharmacopiea
#This is not needed for name recognition and matching, so delete
Art_57_Data$Active_substance <-gsub(pattern = "Ph. Eur.", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "Ph. Eur", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "Ph Eur", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "Ph.Eur.", replacement = "", x = Art_57_Data$Active_substance)


#"Trit."refers to a serial dilution of a substance. 
#Not needed for identifier of the active substance, so delete:
Art_57_Data$Active_substance <-gsub(pattern = "Trit.", replacement = "", x = Art_57_Data$Active_substance)


#Remove all indications of percentage concentration 
#(e.g. 5%, 20%, 10%, (3 per cent) "W 5%")
Art_57_Data$Active_substance <- str_replace_all(pattern = "W\\s\\d\\d?\\%", replacement = "", string = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <- str_replace_all(pattern = "\\d{1,2}\\.?\\d{1,2}?\\s?\\%", replacement = "", string = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <- str_replace_all(pattern = "\\d{1,2}\\%", replacement = "", string = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <- str_replace_all(pattern = "\\((\\d{1,2})\\sPer Cent\\)", replacement = "", string = Art_57_Data$Active_substance)


#Remove other indicators of volume or concentration that remain in the active substance name column:
#(), (Hab, 33c Bp
Art_57_Data$Active_substance <-gsub(pattern = "\\(\\)", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "\\(Hab\\s\\d{1,2}", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "\\(Hab", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "33c", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "Bp", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "\\(V\\/\\V)", replacement = "", x = Art_57_Data$Active_substance)


#Remove other import artifacts:
Art_57_Data$Active_substance <-gsub(pattern = "[345]{2}[abcde]", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "Aquos\\.?", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "\\(\\d{3}\\)", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "Sulphate", replacement = "Sulfate", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "Solution", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "(Powder Form)", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "(Water-Dispersible Form)", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "Distillate", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "Solution", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "\\s\\,", replacement = "", x = Art_57_Data$Active_substance)
Art_57_Data$Active_substance <-gsub(pattern = "\\s\\-", replacement = "", x = Art_57_Data$Active_substance)


##########################
# Remove duplicates
##########################

#first, trim the whitespace:
Art_57_Data$Active_substance <- trimws(Art_57_Data$Active_substance)
Art_57_Data$Product_name <- trimws(Art_57_Data$Product_name)


#Remove duplicates
Art_57_Data <- subset(Art_57_Data, !duplicated(Art_57_Data$Product_name))
Art_57_Data <- subset(Art_57_Data, !duplicated(Art_57_Data$Active_substance))


##########################
#Export cleaned list to outwd file as a tab-delimited file

write.table(Art_57_Data, file = paste0(outwd, "/Article_57_pharma_clean_list_V", vers, "_", vers_art57, ".csv"), 
            sep = "\t", row.names = FALSE)

