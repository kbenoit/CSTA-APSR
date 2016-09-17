# File-Name:       02a_create_CF_dataset_noncore.R
# Date:            2013-12-09
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
MINCODESNEEDED <- 5  # constant for the Phase 1 res design
###########################################################
latest.master.coding.data <- "coding_all_long_2013-11-09.dta"
###########################################################

# read in master sentence set
load("../Data - Created/master.sentences.Rdata")

### determine which sentences need sampling

## exclude core 6
data.for.CF <- subset(sentences,
                       ((unclass(sentences$manifestoid)>7 & unclass(sentences$manifestoid)>1)) | sentences$X_gold==TRUE)
                      
table(data.for.CF$manifestoid)

sentences$codesneeded <- MINCODESNEEDED


# just get them all
data.for.CF <- data.for.CF[, c(1,17:ncol(data.for.CF))]


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


###
### CREATE SCREENERS
###
sample.df <- function(df, n) df[sample(nrow(df), n), , drop = FALSE]
totalscreeners <- 5
screeners <- sample.df(data.for.CF, 11*totalscreeners)
screeners$policy_area_gold <- c(1, rep(2,5), rep(3,5))
screeners$econ_scale_gold <- screeners$econ_scale_gold_min <- screeners$econ_scale_gold_max <- c(NA, -2:2, rep(NA,5))
screeners$soc_scale_gold <- screeners$soc_scale_gold_min <- screeners$soc_scale_gold_max <- c(NA, rep(NA,5), -2:2)
screeners$sentence_text <- rep(
    c("Code this sentence as Neither Economic nor Social policy.",
      "Code this sentence as Economic policy, Very left.",
      "Code this sentence as Economic policy, Somewhat left",
      "Code this sentence as Economic policy, Neither left nor Right",
      "Code this sentence as Economic policy, Somewhat Right",
      "Code this sentence as Economic policy, Very right.",
      "Code this sentence as Social policy, Very liberal.",
      "Code this sentence as Social policy, Somewhat liberal",
      "Code this sentence as Social policy, Neither liberal nor conservative",
      "Code this sentence as Social policy, Somewhat conservative",
      "Code this sentence as Social policy, Very conservative."),
    5)
rownames(screeners) <- paste("scr", 1:(11*totalscreeners), sep="")
screeners$X_gold <- TRUE
screeners$sentenceid <- 999000000 +1:nrow(screeners)
data.for.CF <- rbind(data.for.CF, screeners)
                              

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


