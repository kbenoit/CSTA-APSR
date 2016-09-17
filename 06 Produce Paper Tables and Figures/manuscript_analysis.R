# File-Name:       manuscript_analysis.R
# Date:            2014-12-12
# Author:          Ken Benoit
# Email:           kbenoit@lse.ac.uk                                      
# Purpose:         
# Data Used:       
# Packages Used:   
# Output File:     
# Data Output:     
# Machine:         Macs


if (Sys.getenv("USER")=="kbenoit") {
  setwd("~/Dropbox/Papers/CMP_recoding/newanalysis/06 Produce Paper Tables and Figures")
}
rm(list = ls())  # clear workspace
LONGDATA <- "../Data - Created/coding_all_long_2014-03-14.dta"


###################
##### FIGURES #####
###################

###
### FIGURE 1
### n/a: this was a diagram from Illustrator
###


###
### FIGURE 3
### --- to bring in from Ben's work ---
###


###
### FIGURE 3
### --- to bring in from Ben's work ---
###


###
### FIGURE 4
### PLOT MEANS OF CODES CROWD V EXPERT
###

require(foreign)
data.long.entire <- read.dta(LONGDATA)
data.long <- subset(data.long.entire, source != "SemiExperts" & scale != "None")
require(reshape)
data.melted <- melt(data.long[, c("source", "sentenceid", "code", "scale")], measure="code")
data.cast <- cast(data.melted, sentenceid + variable ~ source + scale, mean, na.rm=TRUE)

pdf(file="Figure4_CrowdVExpertMeanCodes.pdf", height=6, width=12)
par(mfrow=c(1,2))
# economic
with(data.cast, 
     plot(jitter(Crowd_Economic, 30), jitter(Experts_Economic, 10), pch=19, col="grey50", cex=.3,
          xlab="Crowd Mean Code", ylab="Expert Mean Code", main="Economic Domain"))
lmfit <- lm(Experts_Economic ~ Crowd_Economic, data=data.cast)
require(mcr)
tlsfit <- mcreg(data.cast$Crowd_Economic, data.cast$Experts_Economic, error.ratio=1, method.reg="Deming",
                method.ci="bootstrap", na.rm=TRUE)
lmfit$coefficients <- getCoefficients(tlsfit)[,1]
bands <- as.data.frame(predict.lm(lmfit, 
                                  newdata=data.frame(Crowd_Economic=seq(-2, 2, 0.2)), 
                                  interval="confidence"))
xpts <- seq(-2, 2, 0.2)
polygon(x=c(xpts, rev(xpts)),
        y=c(bands$lwr, rev(bands$upr)), col=rgb(0,0,0,0.1), border=NA)
lines(xpts, bands$fit, col=rgb(0,0,0,0.5))
abline(h=0, v=0, lty="dashed", col="red")

# social
with(data.cast, 
     plot(jitter(Crowd_Social, 30), jitter(Experts_Social, 10), pch=19, col="grey50", cex=.3,
          xlab="Crowd Mean Code", ylab="Expert Mean Code", main="Social Domain"))
abline(h=0, v=0, lty="dashed", col="red")
lmfit <- lm(Experts_Social ~ Crowd_Social, data=data.cast)
tlsfit <- mcreg(data.cast$Crowd_Social, data.cast$Experts_Social, error.ratio=1, method.reg="Deming",
                method.ci="bootstrap", na.rm=TRUE)
lmfit$coefficients <- getCoefficients(tlsfit)[,1]
bands <- as.data.frame(predict.lm(lmfit, 
                                  newdata=data.frame(Crowd_Social=seq(-2, 2, 0.2)), 
                                  interval="confidence"))
xpts <- seq(-2, 2, 0.2)
polygon(x=c(xpts, rev(xpts)),
        y=c(bands$lwr, rev(bands$upr)), col=rgb(0,0,0,0.1), border=NA)
lines(xpts, bands$fit, col=rgb(0,0,0,0.5))
dev.off()


###
### FIGURE 5
### Collapse as ncoders increases
### 

## Mean of means version
require(foreign)
require(reshape)
data.long.entire <- read.dta(LONGDATA)
data.long <- subset(data.long.entire, source != "SemiExperts" & (manifestoid %in% 1:6) & scale!="None")

# initialize results matrix
results <- data.frame(n=NA, reps=NA, manifestoid=NA, Economic=NA, Social=NA)
t <- data.long[, c("sentenceid", "manifestoid", "code", "scale", "source")]
data.melted <- melt(t, measure="code")
data.cast <- cast(data.melted, variable ~ source + scale + manifestoid, subset=(scale!="None"), mean, na.rm=TRUE)
results <- data.frame(n=NA, reps=NA, data.cast)

# loop through subsample sequence, 1000 samples each
cat("Drawing subset ")
for (n in 1:20) {
    cat(n, " ")
    for (reps in 1:100) {
        t <- data.long[, c("sentenceid", "manifestoid", "code", "scale", "source")]
        # shuffle randomly
        t <- t[order(runif(nrow(t))), ]
        # generate a sequence
        t$seq <- ave(rep(1, nrow(t)), t$sentenceid, t$source, FUN=seq_along)
        # sample n per sentenceid 
        t <- t[which(t$seq <= n), ]
        data.melted <- melt(t, measure="code")
        # get sentence means for each meanifesto across sentenceid, by scale
        # (same as 2-stage mean of means since sentenceid samples are balanced)
        data.cast <- cast(data.melted, variable ~ source + scale + manifestoid, subset=(scale!="None"), mean, na.rm=TRUE)
        # save results to output dataframe
        results <- rbind(results,data.frame(n=n, reps=reps, data.cast))
    }
}
results <- results[-1,]
varsim <- aggregate(results[, 3:ncol(results)], by=list(results$n), sd)[, -2]
names(varsim)[1] <- "n"

## build the figure
pdf(file="Figure6_VarianceCollapse.pdf", height=6, width=12)
par(mfrow=c(1,2))
# economic
plot(varsim$n, varsim[, 14], pch=19, #type="l", 
     xlab="Crowd codes per sentence", ylab="Std. error of bootstrapped manifesto estimates",
     col=rgb(0,0,0,0.5),
     main="Economic", ylim=c(0,.05)) # log="y") #, 
for (i in 15:19) {
    points(varsim$n, varsim[, i], pch=19, col=rgb(0,0,0,0.5)) 
    points(varsim$n[1:6], varsim[1:6, i-12], col=rgb(1,0,0,0.5), pch=17)
}
abline(v=5, lty="dotted", col="grey60")
# social
plot(varsim$n, varsim[, 20], pch=19, #type="l", 
     xlab="Crowd codes per sentence", ylab="Std error of bootstrapped manifesto estimates",
     main="Social", col=rgb(0,0,0,0.5), ylim=c(0, .05)) # log="y") #, 
for (i in 21:25) {
    points(varsim$n, varsim[, i], pch=19, col=rgb(0,0,0,0.5))
    points(varsim$n[1:6], varsim[1:6, i-12], col=rgb(1,0,0,0.5), pch=17)
}
abline(v=5, lty="dotted", col="grey60")
legend(15, .05, c("Expert", "Crowd"), col=c("red", "grey60"), pch=c(17, 19), bty="n")
dev.off()



###
### FIGURE 6
### Immigration 2010 measure v. Benoit expert survey
### --- to bring in from Ben's work ---



###
### FIGURE 7
### EP DEBATE IN 6 LANGUAGES
###

## read in the speech variables
if (!require(xlsx)) {
    install.packages("xlsx")
    require(xlsx)
}
docv <- read.xlsx("../Data - External/coal-debate.xlsx", 1)[, 1:12]  # read the second sheet
names(docv) <- c("sequence", "nameFirst", "nameLast", "title", "speechlanguage", "country", "party", 
                 "EPG", "X1134", "X1135", "X1136", "X1137")
# docv <- docv[c(-29, -30), ]  # remove duplicated Rapkay and Almunia

## aggregate results data frame
meanResults <- docv[, -which(names(docv) %in% c("X1134", "X1135", "X1136", "X1137"))]
meanResults$vote <- factor(docv$X1137, levels=c(1,2,5), labels=c("For", "Against", "Abstain"))
meanResults$voteNoAbstain <- meanResults$vote
meanResults$voteNoAbstain[which(meanResults$vote == "Abstain")] <- NA

### get CF results
# combine two english into one
joblist <- c("f650814.csv", "f655764.csv", "fEnglishCombined.csv", "f655316.csv", "f656054.csv",
             "f656640.csv", "f658174.csv", "f658191.csv")
jobnames <- c("EN1", "EN2", "EN", "DE", "ES", "IT", "GR", "PL")
write.csv(rbind(read.csv("../Data - CF Jobs/f650814.csv", stringsAsFactors=FALSE),
                read.csv("../Data - CF Jobs/f655764.csv", stringsAsFactors=FALSE)),
          file = "../Data - CF Jobs/fEnglishCombined.csv")
for (i in 1:length(joblist)) {
    # read in job results
    results <- read.csv(paste0("../Data - CF Jobs/", joblist[i]), stringsAsFactors=FALSE)
    # exclude gold questions
    results <- results[which(results$X_golden == "false"), ]
    cat("Total sentences in", jobnames[i], nrow(subset(results, speechno != 14)), "sentences.\n")
    # aggregate score of (pos - neg)/N
    meansubsidy <- aggregate(results$subsidy_scale, by=list(results$speechno), mean)[, 2]
    # aggregate score of (pos - neg)/(pos + neg)
    meansubsidyNonZero <- aggregate(results$subsidy_scale, 
                                    by=list(results$speechno), 
                                    function(x) mean(x[which(x != 0)]))[, 2]
    # length of text in sentences
    textLength <- aggregate(results$sentenceid, by=list(results$speechno), function (x) length(unique(x)))[, 2]
    # temporary data frame for results
    tempdf <- data.frame(meansubsidy, meansubsidyNonZero, textLength)
    # rename with language prefix
    names(tempdf) <- outer(jobnames, names(tempdf), paste, sep="_")[i, ] 
    # append to results data frame
    meanResults <- cbind(meanResults, tempdf)
}

# drop the case with 3 sentences only
meanResults <- subset(meanResults, EN_textLength > 3)
# get column indexes (exclude EN1 and EN2)
varIndexSNZ <- grep("meansubsidyNonZero", names(meanResults))[3:length(jobnames)]
varIndexTL  <- grep("textLength", names(meanResults))[3:length(jobnames)]
# make "long" data frame
temp <- as.data.frame(apply(meanResults[, varIndexSNZ], 2, function(x) scale(x)[,1]))
meanResultsStacked <- 
    data.frame(sequence=rep(meanResults$sequence, length(jobnames)-2),
               meansubsidyNonZero = unlist(temp, use.names=FALSE),
               textLength = unlist(meanResults[, varIndexTL], use.names=FALSE),
               Language = rep(jobnames[3:length(jobnames)], each = nrow(meanResults)),
               voteNoAbstain = rep(meanResults$voteNoAbstain, length(jobnames)-2))
levels(meanResultsStacked$Language) <- 
    c("German", "English", "Spanish", "Greek", "Italian", "Polish")
quartz(width=6, height=4)
require(ggplot2)
ggplot(aes(y = meansubsidyNonZero, x = voteNoAbstain, fill = Language), 
       data = subset(meanResultsStacked, voteNoAbstain %in% c("For", "Against"))) +
    scale_x_discrete(labels = c("For (n=25", "Against (n=6)", "Abstain")) +
    geom_boxplot() + #, varwidth = TRUE) + 
    xlab("Vote") + ylab("Mean Crowd Score") + 
    theme_bw() + scale_fill_grey(start=0.45, end=1)



###
### TABLE 5
###
## overall correlations
round(cor(meanResults[meanResults$voteNoAbstain %in% c("For", "Against"), 
                      grep("meansubsidy$", names(meanResults))]), 2)
round(cor(meanResults[meanResults$voteNoAbstain %in% c("For", "Against"), 
                      grep("meansubsidyNonZero", names(meanResults))]), 2)





##################
##### TABLES #####
##################

### 
### TABLE 1: Texts and sentences coded
###


###
### TABLE A1b: Crowd model estimates table
###
load("../05 Scale Coders and Documents/Estimates/SavedPartyEstimates.Rdata")
crowd.point.estimates <- data.frame(thetabar.crowd.est)
cis <- data.frame(thetabar.crowd.ci)
table2 <- data.frame(Economic = crowd.point.estimates$Economic.Scale,
                     t(cis)[37:54,],
                     Social = crowd.point.estimates$Social.Scale,
                     t(cis)[55:72,])
names(table2)[2:3] <- c("Economic.95lo", "Economic.95hi")
names(table2)[5:6] <- c("Social.95lo", "Social.95hi")
table2

###  Crowd model estimates table
# last two are position
expert.point.estimates <- data.frame(thetabar.expert.est)
cis <- data.frame(thetabar.expert.ci)
table3 <- data.frame(Economic = expert.point.estimates$Economic.Scale,
                     t(cis)[37:54,],
                     Social = expert.point.estimates$Social.Scale,
                     t(cis)[55:72,])
names(table3)[2:3] <- c("Economic.95lo", "Economic.95hi")
names(table3)[5:6] <- c("Social.95lo", "Social.95hi")
table3

##
## NEED SEPARATE EXPERT SEQUENTIAL AND RANDOM ESTIMATES
##

round(diag(cor(expert.point.estimates, crowd.point.estimates)), 2)
require(epiR)
# Lin's cc: Economic
round(epi.ccc(expert.point.estimates$Economic.Scale, 
              crowd.point.estimates$Economic.Scale)$rho.c$est, 2)
## 0.95
# Lin's cc: Social
round(epi.ccc(expert.point.estimates$Social.Scale, 
              crowd.point.estimates$Social.Scale)$rho.c$est, 2)
## 0.84


### 
### TABLE 3: Cronbach's alpha
###          [IN STATA FILE]
###

## Correlation of Econ and Social scores with expert survey results
# expert survey data
require(foreign)
benoitscores <- read.dta("../Data - External/GBexpertscores2.dta")
benoitscores$Party <- gsub("GPEW", "Greens", benoitscores$party_abbrev)
benoitscores <- subset(benoitscores, party_abbrev %in% c("Con", "Lab", "LD"))
benoitscores$manifesto <- paste(benoitscores$party_abbrev, benoitscores$year)
# make 1992 equivalent to 1987 scores
temp1992 <- benoitscores[which(benoitscores$year==1987),]
temp1992$year <- 1992
temp1992$manifesto <- gsub("1987", "1992", temp1992$manifesto)
benoitscores <- rbind(benoitscores, temp1992)
# just take what we need
benoitscores <- subset(benoitscores, scale %in% c("Importance", "Position") & dimension!="Immigration",
                       select=c("mean", "dimension", "scale", "manifesto"))
require(reshape)
benoitscoresm <- melt(benoitscores, measure="mean")
benoitscoresm$dimension <- gsub("Taxes v. Spending", "SurveyEconomic", benoitscoresm$dimension)
benoitscoresm$dimension <- gsub("Social", "SurveySocial", benoitscoresm$dimension)
benoitscoresc <- cast(benoitscoresm, manifesto + variable ~ dimension + scale)

## correlations with mean of means
# exp surveys with crowd estimates
cor(as.data.frame(benoitscoresc[,3:6]), crowd.point.estimates)
# expert MOM with expert model
####
#### WHERE IS meanOfMeans???
####
cor(as.data.frame(meanOfMeans[,c(2:3)]), expert.point.estimates)
# crowd MOM with crowd model
cor(as.data.frame(meanOfMeans[,c(6:7)]), crowd.point.estimates)

load("../05 Scale Coders and Documents/Estimates/SavedPartyEstimates.Rdata")
crowd.point.estimates <- data.frame(thetabar.crowd.est)
crowd.point.estimates$manifesto <- row.names(crowd.point.estimates)
crowd.point.estimates2 <- merge(crowd.point.estimates, benoitscoresc[, -2], by="manifesto")
round(cor(crowd.point.estimates2[, -1]), 2)

# not sure this looks right...
with(crowd.point.estimates2, plot(Economic_Position, Economic.Code))
with(crowd.point.estimates2, text(Economic_Position, Economic.Code, manifesto))



###
### TABLE 3: Immigration estimates table
###          (note: "classified as immigration" from .do file)
###
# load (combined) model scores for immigration dimension
immscores <- read.csv("../05 Scale Coders and Documents/Estimates/ImmigrationEstimates.csv")
names(immscores) <- c("Party", "ImpCrowd", "ImpCrowd95lo", "ImpCrowd95hi", "PosCrowd", "PosCrowd95lo", "PosCrowd95hi")
immscores$Party <- gsub(" 2010", "", immscores$Party)
immscores$Party <- gsub("PC", "PCy", immscores$Party)

## merge in Benoit expert survey scores
library(foreign)
benoitscores <- read.dta("../Data - External/GBexpertscores2.dta")
benoitscores$Party <- gsub("GPEW", "Greens", benoitscores$party_abbrev)
immscores <- merge(immscores, 
                   subset(benoitscores, year==2010 & dimension=="Immigration" & scale=="Position", select=c("mean", "SE", "Party")),
                   by="Party", all.x=TRUE)
names(immscores)[(ncol(immscores)-1):ncol(immscores)] <- c("PosBenoitMean", "PosBenoitSE")
immscores <- merge(immscores, 
                   subset(benoitscores, year==2010 & dimension=="Immigration" & scale=="Importance", select=c("mean", "SE", "Party")),
                   by="Party", all.x=TRUE)
names(immscores)[(ncol(immscores)-1):ncol(immscores)] <- c("ImpBenoitMean", "ImpBenoitSE")

## merge in chapel hill 2010 expert survey estimates
CHscores <- read.dta("../Data - External/CHexpertsurveyscores.dta")
names(CHscores)[1] <- "Party"
CHscores$Party <- gsub("PLAID", "PCy", CHscores$Party)
CHscores$Party <- gsub("LAB", "Lab", CHscores$Party)
CHscores$Party <- gsub("CON", "Con", CHscores$Party)
CHscores$Party <- gsub("GREEN", "Greens", CHscores$Party)
CHscores$Party <- gsub("LIB", "LD", CHscores$Party)

immscores <- merge(immscores, 
                   subset(CHscores, dimension=="Immigration" & scale=="Position", select=c("Mean", "SE", "Party")),
                   by="Party", all.x=TRUE)
names(immscores)[(ncol(immscores)-1):ncol(immscores)] <- c("PosCHImmMean", "PosCHImmSE")
immscores <- merge(immscores, 
                   subset(CHscores, dimension=="Immigration" & scale=="Importance", select=c("Mean", "SE", "Party")),
                   by="Party", all.x=TRUE)
names(immscores)[(ncol(immscores)-1):ncol(immscores)] <- c("ImpCHImmMean", "ImpCHImmSE")
immscores <- merge(immscores, 
                   subset(CHscores, dimension=="Asylum" & scale=="Position", select=c("Mean", "SE", "Party")),
                   by="Party", all.x=TRUE)
names(immscores)[(ncol(immscores)-1):ncol(immscores)] <- c("PosCHAsylMean", "PosAsylCHSE")
immscores <- merge(immscores, 
                   subset(CHscores, dimension=="Asylum" & scale=="Importance", select=c("Mean", "SE", "Party")),
                   by="Party", all.x=TRUE)
names(immscores)[(ncol(immscores)-1):ncol(immscores)] <- c("ImpCHAsylMean", "ImpAsylCHSE")

# table
immscores[order(immscores$PosCrowd, decreasing=TRUE), ]

## for round 1 and round 2 comparisons
load("../05 Scale Coders and Documents/Estimates/SavedImmigrationEstimates.Rdata")
immscores <- cbind(immscores, thetabar.s1.est, t(thetabar.s1.ci[,,2]), thetabar.s2.est, t(thetabar.s2.ci[,,2]))
names(immscores)
names(immscores)[(ncol(immscores)-8+1):ncol(immscores)] <- 
    c("Domain1", "Position1", "Position1.95lo", "Position1.95hi",
      "Domain2", "Position2", "Position2.95lo", "Position2.95hi")
with(immscores, cor(Position1, Position2)) # correlation of two expert surveys
round(with(immscores, cor(Position1, PosBenoitMean, use="complete.obs")), 2) # correlation of 1st wave w/Benoit 2010
round(with(immscores, cor(Position1, (ImpCHImmMean + PosCHAsylMean)/2, use="complete.obs")), 2) # correlation of 1st wave w/CHES
round(with(immscores, cor(Position2, PosBenoitMean, use="complete.obs")), 2) # correlation of 2nd wave w/Benoit 2010
round(with(immscores, cor(Position2, (ImpCHImmMean + PosCHAsylMean)/2, use="complete.obs")), 2) # correlation of 2nd wave w/CHES
round(with(immscores, cor(PosCrowd, PosBenoitMean, use="complete.obs")), 2) # correlation of combined w/CHES
round(with(immscores, cor(PosCrowd, (ImpCHImmMean + PosCHAsylMean)/2, use="complete.obs")), 2) # correlation of combined w/CHES





###################
##### TESTING #####
###################


###
### EXPERTS
### Means of means
###
data.long <- read.dta(LONGDATA)
data.long <- subset(data.long, 
                    manifestoid!=0 & manifestoid!=99 & source=="Experts")
data.long$manifestoid <- factor(data.long$manifestoid,
                                labels=c("Con 1987", "LD 1987", "Lab 1987", "Con 1997", "LD 1997", "Lab 1997", "Con 1992", 
                                         "LD 1992", "Lab 1992", "Con 2001", 
                                         "LD 2001", "Lab 2001", "Con 2005", "LD 2005", "Lab 2005", "Con 2010", 
                                         "LD 2010", "Lab 2010"))
econ.pos <- subset(data.long, scale=="Economic")
econ.pos <- aggregate(econ.pos$code, 
                      by=list(econ.pos$manifestoid, econ.pos$sentenceid), 
                      mean, na.rm=TRUE)
econ.pos <- aggregate(econ.pos$x, list(econ.pos$Group.1), mean, na.rm=TRUE)
soc.pos <- subset(data.long, scale=="Social")
soc.pos <- aggregate(soc.pos$code, 
                     by=list(soc.pos$manifestoid, soc.pos$sentenceid), 
                     mean, na.rm=TRUE)
soc.pos <- aggregate(soc.pos$x, list(soc.pos$Group.1), mean, na.rm=TRUE)
meanOfMeans <- data.frame(manifesto=econ.pos$Group.1,
                          expmom.econ.pos=econ.pos$x,
                          expmom.soc.pos=soc.pos$x)
meanOfMeans$manifestoChar <- as.character(meanOfMeans$manifesto)

### CROWD Mean of Means
data.long <- read.dta(LONGDATA)
data.long <- subset(data.long, 
                    manifestoid!=0 & manifestoid!=99 & source=="Crowd")
data.long$manifestoid <- factor(data.long$manifestoid,
                                labels=c("Con 1987", "LD 1987", "Lab 1987", "Con 1997", "LD 1997", "Lab 1997", "Con 1992", 
                                         "LD 1992", "Lab 1992", "Con 2001", 
                                         "LD 2001", "Lab 2001", "Con 2005", "LD 2005", "Lab 2005", "Con 2010", 
                                         "LD 2010", "Lab 2010"))
econ.pos <- subset(data.long, scale=="Economic")
econ.pos <- aggregate(econ.pos$code, 
                      by=list(econ.pos$manifestoid, econ.pos$sentenceid), 
                      mean, na.rm=TRUE)
econ.pos <- aggregate(econ.pos$x, list(econ.pos$Group.1), mean, na.rm=TRUE)
soc.pos <- subset(data.long, scale=="Social")
soc.pos <- aggregate(soc.pos$code, 
                     by=list(soc.pos$manifestoid, soc.pos$sentenceid), 
                     mean, na.rm=TRUE)
soc.pos <- aggregate(soc.pos$x, list(soc.pos$Group.1), mean, na.rm=TRUE)
meanOfMeans <- cbind(meanOfMeans, 
                     data.frame(manifesto2=econ.pos$Group.1,
                                crwdmom.econ.pos=econ.pos$x,
                                crwdmom.soc.pos=soc.pos$x))
meanOfMeans$manifestoChar <- as.character(meanOfMeans$manifesto)

# mean of mean correlation of crowd with experts
round(cor(meanOfMeans$expmom.econ.pos, meanOfMeans$crwdmom.econ.pos), 2)
round(cor(meanOfMeans$expmom.soc.pos, meanOfMeans$crwdmom.soc.pos), 2)

## merge in Laver/Benoit et al expert survey scores
benoitscores <- read.dta("../Data - External/GBexpertscores2.dta")
benoitscores$Party <- gsub("GPEW", "Greens", benoitscores$party_abbrev)
benoitscores <- subset(benoitscores, party_abbrev %in% c("Con", "Lab", "LD"))
benoitscores$manifestoChar <- paste(benoitscores$party_abbrev, benoitscores$year)
# make 1992 equivalent to 1987 scores
temp1992 <- benoitscores[which(benoitscores$year==1987),]
temp1992$year <- 1992
temp1992$manifestoChar <- gsub("1987", "1992", temp1992$manifestoChar)
benoitscores <- rbind(benoitscores, temp1992)
# just take what we need
benoitscores <- subset(benoitscores, scale %in% c("Importance", "Position") & dimension!="Immigration",
                       select=c("mean", "dimension", "scale", "manifestoChar"))
require(reshape)
benoitscoresm <- melt(benoitscores, measure="mean")
benoitscoresm$dimension <- gsub("Taxes v. Spending", "SurveyEconomic", benoitscoresm$dimension)
benoitscoresm$dimension <- gsub("Social", "SurveySocial", benoitscoresm$dimension)
benoitscoresc <- cast(benoitscoresm, manifestoChar + variable ~ dimension + scale)

manifestimates <- merge(meanOfMeans, benoitscoresc[, -2], by="manifestoChar")

plot(manifestimates$expmom.econ.pos ~ manifestimates$SurveyEconomic_Position,
     ylab="Mean of Means Economic Position",
     xlab="Expert Survey Economic Position",
     pch=19, cex=.7)
text(manifestimates$SurveyEconomic_Position, manifestimates$mom.econ.pos, 
     manifestimates$manifestoChar, pos=4, cex=.8)
abline(lm(manifestimates$expmom.econ.pos ~ manifestimates$SurveyEconomic_Position))
text(6, .4, round(cor(manifestimates$expmom.econ.pos, manifestimates$SurveyEconomic_Position),2))

## load in saved expert?? estimates
load("../05 Scale Coders and Documents/Estimates/SavedPartyEstimates.Rdata")
point.estimates <- data.frame(thetabar.crowd.est)
point.estimates$manifestoChar <- rownames(point.estimates)
manifestimates <- merge(meanOfMeans, point.estimates, by="manifestoChar")

# mean of mean correlation of experts with external expert surveys
cor(manifestimates$expmom.econ.pos, manifestimates$Economic.Scale)
cor(manifestimates$expmom.soc.pos, manifestimates$Social.Scale)



###
### BASIC CF STATS ON CODERS
###
library(foreign)
data.long <- read.dta("../Data - Created/coding_all_long_2014-03-14.dta")
data.long <- subset(data.long, 
                    manifestoid!=0 & manifestoid!=99 & substr(stage,1,1)=="C")

# total unique coders
with(data.long, length(unique(coderid)))

# from which and how many countries
table.countries <- table(data.long$country, useNA="ifany")
table.countries <- table.countries[table.countries>0]
length(table.countries)
table.countries <- cbind(table.countries,
                         round(prop.table(table.countries)*100, 2))
(table.countries <- table.countries[order(table.countries[,1], decreasing=TRUE), ])

# how many codes per coder
table.coders <- table(data.long$coderid, useNA="ifany")
length(table.coders)
plot(density(log10(table.coders)), 
     xlab="Log(10) of Sentences Coded",
     main="")
summary(as.numeric(table.coders))
sort(table.coders, decreasing=TRUE)[1:50]
sum(table.coders > 1000)
## one crazy SOB coded 10,180 sentences!
crazy.coder <- names(table.coders[which(table.coders==10180)])
crazy.coder.data <- data.long[which(data.long$coderid==crazy.coder),]
crazy.coder.data[1,]
crazy.coder.data$manifestotitle <- paste(crazy.coder.data$party, crazy.coder.data$year) 
with(crazy.coder.data, table(manifestotitle))

# channels
table.channels <- table(data.long$channel, useNA="ifany")
require(Rmisc)
table.channels <- cbind(table.channels,
                        round(prop.table(table.channels)*100, 2))
table.channels <- cbind(table.channels, group.CI(trust ~ channel, data.long)[,2:4])
names(table.channels)[1:2] <- c("N", "Pct")
(table.channels <- table.channels[order(table.channels[,1], decreasing=TRUE), ])
nrow(table.channels)


### 
### Plot the codes over time 
###

###
### TABLE: Platform, total codes, total coders, median trust, IQR trust
###






###
### COMPUTE RELIABILITY STATISTICS FOR EXPERTS (SEQUENTIAL)
###
require(foreign)
data.long.entire <- read.dta("../Data - Created/coding_all_long_2014-03-14.dta")
data.long <- subset(data.long.entire, 
                    manifestoid!=0 & manifestoid!=99 & stage %in% c("ES", "ES2"))
library(reshape)
expert.melted <- melt(data.long[, c("sentenceid", "coderid", "scale", "manifestoid")], 
                      id.vars=c("sentenceid", "coderid", "manifestoid"))
expert.cast <- cast(expert.melted, sentenceid + variable + manifestoid ~ coderid)
library(irr)
manifesto.split <- split(expert.cast[, 4:ncol(expert.cast)], expert.cast$manifestoid)
(kappabymanifesto <- sapply(manifesto.split,
                            function(x) kappam.fleiss(x)$value))
(agreementbymanifesto <- sapply(manifesto.split,
                                function(x) agree(x)$value))
(alphabymanifesto <- sapply(manifesto.split,
                            function(x) kripp.alpha(t(x), "nominal")$value))
data.long$manifestoid <- factor(data.long$manifestoid,
                                labels=c("Con 1987", "LD 1987", "Lab 1987", "Con 1997", "LD 1997", "Lab 1997", "Con 1992", 
                                         "LD 1992", "Lab 1992", "Con 2001", 
                                         "LD 2001", "Lab 2001", "Con 2005", "LD 2005", "Lab 2005", "Con 2010", 
                                         "LD 2010", "Lab 2010"))
reliability <- data.frame(manifesto = names(table(data.long$manifestoid)),
                          flkappa = kappabymanifesto, 
                          agreement = agreementbymanifesto,
                          kripp = alphabymanifesto)
reliability
expert.cast <- cast(expert.melted, sentenceid + variable ~ coderid)
k1 <- kappam.fleiss(expert.cast[which(as.numeric(expert.cast$sentenceid)<110000000), 3:(ncol(expert.cast)-2)])
k2 <- kappam.fleiss(expert.cast[which(as.numeric(expert.cast$sentenceid)>=110000000), c(4, 6, 7, 9, 10)])
mean(k1$value, k2$value)
kripp.alpha(t(expert.cast[,4:ncol(expert.cast)]))


###
### FIGURE unusued
### Consensus and agreement stats on expert coders
###

library(foreign)
data.long <- read.dta(LONGDATA)
data.long <- subset(data.long, 
                    manifestoid!=0 & manifestoid!=99 & stage=="ES")
# agreement per sentence
expert.agreement <- as.data.frame.matrix(with(data.long, table(sentenceid, scale)))
expert.agreement$Consensus.Economic <- (abs(expert.agreement$Economic - 3)+3)/6
expert.agreement$Consensus.Social   <- (abs(expert.agreement$Social - 3)+3)/6
expert.agreement$Consensus.None  <- (abs(expert.agreement$None - 3)+3)/6


pdf(file="Figure3_EScoderagreement.pdf", height=4, width=6)
par(mar=c(3,5,1,1))
t <- apply(expert.agreement[,c(6,5,4)], 2, table)
bp <- barplot(t, 
              beside=TRUE,
              legend.text=TRUE,
              args.legend=list(x=16, y=4000, bty="n"),
              xlab="Experts Consensus on Domain",
              horiz=TRUE,
              names.arg=c("Neither", "Social", "Economic"),
              las=1)
text(rep(30, 8), as.numeric(bp), 
     rev(c("6 Experts", "5", "4", "3")),
     pos=4)
dev.off()
round(prop.table(t,2), 2)


