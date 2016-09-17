# File-Name:       02_create_CF_dataset.R
# Date:            2013-10-22
# Author:          Ken Benoit
# Email:           kbenoit@lse.ac.uk                                      
# Purpose:         
# Data Used:       
# Packages Used:   
# Output File:     
# Data Output:     
# Machine:         Macs


if (Sys.getenv("USER")=="kbenoit") {
  setwd("~/Dropbox/Papers/CMP_recoding/newanalysis/02 Create a New CF job")
}

rm(list = ls())  # clear workspace

library(foreign)

###########################################################
##### SET THESE PARAMETERS FOR EACH JOB              ######
###########################################################
MINCODESNEEDED <- 20  # constant for the Phase 1 res design
###########################################################
TOTALSENTENCESTOCODE <- 129
GOLDPERCENT          <-  15
fillinthreshold      <-   2
latest.master.coding.data <- "coding_all_long_2013-11-09.dta"
###########################################################

# add one third again of gold sentences, evenly distributed none/econ/social
gold.size.none <- floor(TOTALSENTENCESTOCODE * GOLDPERCENT/100 * 1/3)
gold.size.econ <- floor(TOTALSENTENCESTOCODE * GOLDPERCENT/100 * 1/3)
gold.size.soc  <- floor(TOTALSENTENCESTOCODE * GOLDPERCENT/100 * 1/3)

# read in master sentence set
load("../Data - Created/master.sentences.Rdata")

### determine which sentences need sampling

# load the existing codes from production run data
library(foreign)
d <- read.dta(paste("../Data - Created/", latest.master.coding.data, sep=""))
codedalready <- subset(d, unclass(stage)>4 & file_name!="f99419", select=c("sentenceid","code", "scale", "stage"))
rm(d)
codedsofar <- data.frame(table(codedalready$sentenceid))
names(codedsofar) <- c("sentenceid", "Ncodes")
# merge this with sentence dataset
sentences <- merge(sentences, codedsofar, by="sentenceid", all=TRUE)
sentences$Ncodes[is.na(sentences$Ncodes)] <- 0

## only deal with core 6 for now
sentences <- sentences[unclass(sentences$manifestoid)<=7 & unclass(sentences$manifestoid)>1,]
table(sentences$manifestoid)

sentences$codesneeded <- ifelse((MINCODESNEEDED - sentences$Ncodes)<=0, 0, 
                                (MINCODESNEEDED - sentences$Ncodes))


## select the sample for CF
sample.df <- function(df, n) df[sample(nrow(df), n), , drop = FALSE]
# randomly sample the non-gold above the fill in threshold minimum
nongold <-
    sample.df(subset(sentences, is.na(X_gold) & codesneeded>=fillinthreshold, select=c(1,17:24,26)), 
              TOTALSENTENCESTOCODE - gold.size.none - gold.size.econ - gold.size.soc)
# get non-gold by sorting method
nongold <- subset(sentences, is.na(X_gold), select=c(1,17:24,26))
# sort by codes needed
nongold <- nongold[order(nongold$codesneeded, decreasing = TRUE),]
# sample the top gold.size.none or the number that exist, whichever is smaller
nongold <- nongold[1:TOTALSENTENCESTOCODE,]

# get gold neither
gold.none <- subset(sentences, policy_area_gold==1, select=c(1,17:24,26))
# sort by codes needed
gold.none <- gold.none[order(gold.none$codesneeded, decreasing = TRUE),]
# sample the top gold.size.none or the number that exist, whichever is smaller
gold.none <- gold.none[1:min(gold.size.none, nrow(gold.none)),]

# get gold econ
gold.econ <- subset(sentences, policy_area_gold==2, select=c(1,17:24,26))
# sort by codes needed
gold.econ <- gold.econ[order(gold.econ$codesneeded, decreasing = TRUE),]
# sample the top gold.size.none or the number that exist, whichever is smaller
gold.econ <- gold.econ[1:min(gold.size.econ, nrow(gold.econ)),]

# get gold social
gold.soc <- subset(sentences, policy_area_gold==3, select=c(1,17:24,26))
# sort by codes needed
gold.soc <- gold.soc[order(gold.soc$codesneeded, decreasing = TRUE),]
# sample the top gold.size.none or the number that exist, whichever is smaller
gold.soc <- gold.soc[1:min(gold.size.soc, nrow(gold.soc)),]

# concatenate the data
data.for.CF <- rbind(nongold, gold.none, gold.econ, gold.soc)
# remove the codesneeded column
# data.for.CF <- data.for.CF[,-ncol(data.for.CF)]

# create the range of gold for econ
data.for.CF$econ_scale_gold_max <- data.for.CF$econ_scale_gold_min <- NA
data.for.CF$econ_scale_gold_min[which(data.for.CF$econ_scale_gold == -1)] <- -2
data.for.CF$econ_scale_gold_min[which(data.for.CF$econ_scale_gold == 1)] <- 1
data.for.CF$econ_scale_gold_max[which(data.for.CF$econ_scale_gold == -1)] <- -1
data.for.CF$econ_scale_gold_max[which(data.for.CF$econ_scale_gold == 1)] <- 2

# create the range of gold for social
data.for.CF$soc_scale_gold_max <- data.for.CF$soc_scale_gold_min <- NA
data.for.CF$soc_scale_gold_min[which(data.for.CF$soc_scale_gold == -1)] <- -2
data.for.CF$soc_scale_gold_min[which(data.for.CF$soc_scale_gold == 1)] <- 1
data.for.CF$soc_scale_gold_max[which(data.for.CF$soc_scale_gold == -1)] <- -1
data.for.CF$soc_scale_gold_max[which(data.for.CF$soc_scale_gold == 1)] <- 2

# matcher=range
# Specifies a numerical range within a contributor's response must fall. You must define a minimum and maximum value,
# which can be set in the gold digging interface or with a spreadsheet The headers should be formatted as column_name_min
# and column_name_max). A number validator is recommended. 

# rename the column for gold to _golden
names(data.for.CF)[which(names(data.for.CF)=="X_gold")] <- "_golden"

# write the output dataset
require(lubridate)
today <- date()
write.csv(data.for.CF, paste("data_for_CF_", today(), ".csv", sep=""), row.names=FALSE, na="",
          fileEncoding="UTF-8")

# THEN:
# 1) Find the new job on CF
# 2) Copy the instructions, CML, and similar title from previous jobs.  These are through the Edit tab.
# 3) Upload the data_for_CF_<today>.csv file
# 4) Choose Data -> Manage Data -> Convert uploaded gold


################################################
###### STOP ####################################
################################################

              

## Now output the gold set for inspection
#area <- factor(sentences$policy_area_gold[which(sentences$X_gold==TRUE)],
#               labels = c("None", "Econ", "Social"))
## combine econ and social scale gold into one measure
#scale.temp <- apply(sentences[which(sentences$X_gold==TRUE), 19:20], 1, sum, na.rm=TRUE)
#scale <- factor(scale.temp, levels=-2:2,
#                labels = c("Very Left", "Somewhat Left", "Neutral", "Somewhat Right", "Very Right"))
#write.csv(cbind(subset(sentences, X_gold==TRUE, select=c(1:2, 17)), area, scale), 
#          file="data_gold.csv", row.names=FALSE, na="", fileEncoding="UTF-8")
#
#dg <- read.csv("~/Downloads/data_gold - Sheet 1.csv")
#dg$sequence <- 1:nrow(dg)
#es <- read.dta("~/Dropbox/Papers/CMP_recoding/analysis/data_created/sentence_level_data.dta")
#dg2 <- merge(dg, es[,c(2,6:26,34:36)], by="sentenceid", all.x=TRUE, all.y=FALSE, sort=FALSE)
#write.csv(dg2, "temp_expertcodinginfo.csv")


