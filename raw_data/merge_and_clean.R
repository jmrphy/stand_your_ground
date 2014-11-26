setwd("~/Dropbox/gh_projects/florida_syg") # Change this to your working directory
require(reshape2)
require(memisc)

# Import the spreadsheet scraped from Import.IO (you have to manually delete first empty column)
import<-read.csv("raw_data/import_io.csv", na.strings=c(""))

# Make case ID for merging with Tampa Bay Times spreadsheet available at http://www.tampabay.com/stand-your-ground-law/data
import$casenumber<-gsub("http://www.tampabay.com/stand-your-ground-law/cases/case_", "", import$X_pageUrl)

# Split race and gender into different variables
import<-cbind(import, colsplit(import$victim_race_gender, " ", names=c("victim_race","victim_gender")))

import$victim_race<-factor(recode(import$victim_race,
                             "Non-white victim" <- "Black",
                             "Non-white victim" <- "Hispanic",
                             "White victim" <- "White",
                             otherwise=NA))

import$victim_gender<-factor(recode(import$victim_gender,
                                  "Female victim" <- "female",
                                  "Male victim" <- "male",
                                  otherwise=NA))

import<-cbind(import, colsplit(import$accused_race_gender, " ", names=c("accused_race","accused_gender")))
import$accused_race<-factor(recode(import$accused_race,
                                  "Non-white defendant" <- "Black",
                                  "Non-white defendant" <- "Hispanic",
                                  "Non-white defendant" <- "Other",
                                  "White defendant" <- "White",
                                  otherwise=NA))

import$accused_gender<-factor(recode(import$accused_gender,
                                    "Female defendant" <- "female",
                                    "Male defendant" <- "male",
                                    "Male defendant" <- "Indian male",
                                    otherwise=NA))

import$accused_weapon<-factor(recode(import$accused_weapon,
                                     "Defendant clearly had a gun" <- "Weapon: gun",
                                     "Defendant clearly had a gun" <- "Weapon: gun and concrete block",
                                     "Defendant clearly had a gun" <- "Weapon: g",
                                     otherwise = "Defendant did not clearly have a gun"))

import$victim_unarmed<-factor(recode(import$victim_weapon,
                                     "Victim clearly unarmed" <- "Weapon: unarmed",
                                     "Victim clearly unarmed" <- "Weapon: none",
                                     otherwise = "Victim not clearly unarmed"))

# Clean variables from check-images

import$victim_crime<-as.factor(ifelse(!is.na(import$victim_crime_yes), "Victim was committing a crime", "Victim was not clearly committing a crime"))
import$defendant_pursued<-as.factor(ifelse(!is.na(import$defendant_pursue), "Defendant pursued", "Defendant did not clearly pursue"))
import$could_retreat<-as.factor(ifelse(!is.na(import$could_retreat_yes), "Defendant could have retreated", "Defendant could not clearly have retreated"))
import$physical_evidence<-as.factor(ifelse(!is.na(import$physical_evidence_yes), "Physical evidence", "No clear physical evidence"))
import$defendant_property<-as.factor(ifelse(!is.na(import$defendant_property_yes), "On property of the defendant", "Not clearly on property of the defendant"))
import$victim_initiated<-as.factor(ifelse(!is.na(import$victim_initiated), "Victim initiated", "Victim did not clearly initiate"))



# Clean names and subset to select variables
import$url<-import$X_pageUrl

import$agency<-import$investigating_agency
import$agency<-gsub("Case decision", NA, import$agency)
import$decision_maker<-gsub("Investigating", NA, import$decision_maker)
import$decision_maker<-gsub("Trayvon", NA, import$decision_maker)


import<-subset(import, select=c("casenumber", "witnesses", "location", "initiator", "victim_age",
                                "victim_gender", "victim_race", "victim_unarmed", "url", "victim_outcome",
                                "accused_age", "accused_gender", "accused_race", "accused_image",
                                "victim_image", "location_details", "victim_name",
                                "legal_details", "agency", "decision_maker", "accused_weapon",
                                "victim_crime", "defendant_pursued", "could_retreat", "physical_evidence",
                                "defendant_property", "victim_initiated"))

# Import Tampa Bay Times spreadsheet available at http://www.tampabay.com/stand-your-ground-law/data
# Note: This now appears to have been removed from their website
syg<-read.csv("raw_data/syg.csv", na.strings=c(""))

# Clean names
syg$casenumber<-syg$what_happenedclick_for_details_link
syg$event_type<-syg$what_happenedclick_for_details_link._text
syg$accused_weapon<-syg$weapon
syg$deaths<-syg$deaths_number
syg$accused<-syg$the_accused
syg$year<-syg$year_number

# Identify number of victims, because we'll limit analyses to cases of one victim
syg$victim_name_count<-sapply(strsplit(as.character(syg$victims), " "), function(x) length(x))
syg$accused_name_count<-sapply(strsplit(as.character(syg$accused), " "), function(x) length(x))


syg$type<-syg$what_happenedclick_for_details_link._text
syg$type<-factor(recode(syg$type, 
                         "Domestic" <- "Domestic argument",
                         "Domestic" <- "domestic dispute",
                         "Domestic" <- "Domestic dispute",
                         "Domestic" <- "Roommate dispute",
                          NA <- "Unknown",
                             otherwise="Non-domestic"))
syg$type<-factor(df$type, levels=c("Non-domestic", "Domestic"))


# Make a subset of select variables
syg<-subset(syg, select=c("casenumber", "year", "city", "county", "accused", "accused_name_count", "victims",
                          "victim_name_count", "deaths", "outcome", "circumstances",
                          "type"))

# Merge
df<-merge(import, syg, by=c("casenumber"))

# Summary conviction / no conviction variable (pending cases are considered NA)

df$conviction<-factor(recode(df$outcome,
                                "No Conviction" <- "acquitted",
                                "No Conviction" <- "dismissed",
                                "No Conviction" <- "granted immunity",
                                "No Conviction" <- "not charged",
                                "Conviction" <- "guilty",
                                "Conviction" <- "plea",
                                otherwise=NA))
df$conviction<-factor(df$conviction, levels=c("No Conviction", "Conviction"))


# Matches victim_intiated perfectly
# df$initiation<-factor(recode(df$initiator,
#                                "Not clearly victim" <- "Initiator not known",
#                                "Not clearly victim" <- "Initiator: defendant",
#                                "Not clearly victim" <- "Initiator: Defendant",
#                                "Not clearly victim" <- "Initiator: Disputed",
#                                "Not clearly victim" <- "Initiator: Unclear",
#                                "Clearly victim" <- "Initiator: victim",
#                                "Clearly victim" <- "Initiator: Victim",
#                                otherwise=NA))


df$witness<-factor(recode(df$witnesses,
                               "No clear witness(es)" <- "Not known",
                               "No clear witness(es)" <- "Witnesses: no",
                               "No clear witness(es)" <- "Witnesses: No",
                               "No clear witness(es)" <- "Witnesses: Unclear",
                               "Clear witness(es)" <- "Witnesses: yes",
                               "Clear witness(es)" <- "Witnesses: Yes",
                               otherwise=NA))


write.csv(df, file="cleaned_data/florida_syg_master_data.csv")


convict.vars<-subset(df, victim_name_count<4, select=c("accused", "victims",
                                                       "conviction", "county", "year", "type",
                                                       "witness", "victim_age", "accused_age",
                                                       "deaths", "accused_weapon", "victim_unarmed",
                                                       "victim_crime", "defendant_pursued",
                                                       "could_retreat", "physical_evidence",
                                                       "defendant_property",
                                                       "victim_race", "victim_gender",
                                                       "accused_race", "accused_gender","victim_initiated"))

convict.vars<-subset(convict.vars, accused!="Earl Jackson Dervaunta Vaughn" &
                       accused!="Xavious McCray Lavelton Williams" &
                       accused!="Matthew Alfonso Garcia Daniel Martinez Joshua Ott" &
                       accused!="Zavrina Jones Zandra Dawn Jones" &
                       accused!="Jeffrey Brown Jamal Taylor Andrae Tyler" &
                       accused!="Anthony Julien Peter Julien" &
                       accused!="Damon \"Red Rock\" Darling Leroy \"Yellowman\" Larose")


write.csv(convict.vars[complete.cases(convict.vars),], file="cleaned_data/convict_model_data.csv")


