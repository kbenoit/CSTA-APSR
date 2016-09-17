
### ANALYZE COAL DEBATE CROWD-SOURCED CODING RESULTS

rm(list=ls())
library(quanteda)

setwd("~/Dropbox/Papers/CMP_recoding/newanalysis/07 Additional Texts/")

## read in the speech variables
if (!require(xlsx)) {
    install.packages("xlsx")
    require(xlsx)
}
docv <- read.xlsx("coal - input files/coal-debate.xlsx", 1)[, 1:12]  # read the second sheet
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
write.csv(rbind(read.csv("coal - files from CF/f650814.csv", stringsAsFactors=FALSE),
                read.csv("coal - files from CF/f655764.csv", stringsAsFactors=FALSE)),
          file = "coal - files from CF/fEnglishCombined.csv")

for (i in 1:length(joblist)) {
    # read in job results
    results <- read.csv(paste0("coal - files from CF/", joblist[i]), stringsAsFactors=FALSE)
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

# drop the cases with < 5 sentences
meanResults <- subset(meanResults, EN_textLength > 3)

## overall correlations
round(cor(meanResults[meanResults$voteNoAbstain %in% c("For", "Against"), 
                      grep("meansubsidy$", names(meanResults))]), 2)
round(cor(meanResults[meanResults$voteNoAbstain %in% c("For", "Against"), 
                      grep("meansubsidyNonZero", names(meanResults))]), 2)

### compute Lin's ccc
require(epiR)
forLins <- meanResults[meanResults$voteNoAbstain %in% c("For", "Against"), 
                       grep("meansubsidyNonZero", names(meanResults))]
forLins <- forLins[, 3:ncol(forLins)]
cccMatrix <- matrix(NA, ncol=5, nrow=5)
diag(cccMatrix) <- 1
for (i in 1:(ncol(forLins)-1))
    for (j in (i+1):ncol(forLins))
        cccMatrix[i, j] <- epi.ccc(forLins[,i], forLins[,j])$rho.c$est
        #cat(i, "v.", j, epi.ccc(forLins[,i], forLins[,j])$rho.c$est, "\n")
colnames(cccMatrix) <- rownames(cccMatrix) <- colnames(forLins)
round(t(cccMatrix), 2)

## PLOTS: ENGLISH ##

library(ggplot2)
quartz()
ggplot(aes(y = IT_meansubsidyNonZero, x = voteNoAbstain), 
       data = subset(meanResults, voteNoAbstain %in% c("For", "Against"))) +
    geom_hline(yintercept=0, colour="darkred", linetype="dashed") +
    geom_boxplot(outlier.shape = NA, varwidth = TRUE) + 
    geom_jitter(position=position_jitter(width=.1), aes(size=EN_textLength), shape=21, bg="grey50") + 
    ggtitle("English Crowd Scores by Vote Type") +
    xlab("Vote") + ylab("Mean English Crowd Score") + theme_bw()

# list if votes were For but speech was against
cat("Speakers who voted For but spoke Against:")
meanResults[which(meanResults$EN_meansubsidyNonZero < 0.3 & meanResults$vote=="For"), 
            c(1:3, 5:6, 8, 9, 12, 13)]

ggplot(aes(y = EN2_meansubsidyNonZero, x = voteNoAbstain), 
       data = subset(meanResults, voteNoAbstain %in% c("For", "Against"))) +
    geom_hline(yintercept=0, colour="darkred", linetype="dashed") +
    geom_boxplot(outlier.shape = NA, varwidth = TRUE) + 
#    geom_jitter(position=position_jitter(width=.1), size=2.5) + 
    geom_jitter(position=position_jitter(width=.1), aes(size=EN_textLength), shape=21, bg="grey50") + 
    ggtitle("English 1 Crowd Scores by Vote Type") +
    xlab("Vote") + ylab("Mean English Crowd Score")
# list if votes were For but speech was against
cat("Speakers who voted For but spoke Against:")
meanResults[which(meanResults$EN2_meansubsidyNonZero < 0 & meanResults$vote=="For"), 
            c(1:3, 5:6, 8, 9, 12)]


#### GERMAN ####
quartz()
ggplot(aes(y = DE_meansubsidyNonZero, x = voteNoAbstain), 
       data = subset(meanResults, voteNoAbstain %in% c("For", "Against"))) +
    geom_hline(yintercept=0, colour="darkred", linetype="dashed") +
    geom_boxplot(outlier.shape = NA, varwidth = TRUE) + 
    geom_jitter(position=position_jitter(width=.1), aes(size=EN_textLength), shape=21, bg="grey50") + 
    #geom_jitter(position=position_jitter(width=.1), shape=21, bg="grey50") + 
    # ggtitle("German Language Crowd Scores by Vote") +
    xlab("Vote") + ylab("Mean German Crowd Score") + theme(legend.position="none")
    # + theme_bw()

#### ALL LANGUAGES ####
# get column indexes (exclude EN1 and EN2)
varIndexSNZ <- grep("meansubsidyNonZero", names(meanResults))[3:length(jobnames)]
#varIndexSNZ <- grep("meansubsidy", names(meanResults))[3:length(jobnames)]
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
ggplot(aes(y = meansubsidyNonZero, x = voteNoAbstain, fill = Language), 
       data = subset(meanResultsStacked, voteNoAbstain %in% c("For", "Against"))) +
    scale_x_discrete(labels = c("For (n=25", "Against (n=6)", "Abstain")) +
    # geom_hline(yintercept = 0), colour="darkred", linetype="dashed") +
    geom_boxplot() + #, varwidth = TRUE) + 
    # geom_jitter(position=position_jitter(width=.1), aes(size=textLength, , fill = Language), shape=21, bg="grey50") + 
    #geom_jitter(position=position_jitter(width=.1), shape=21, bg="grey50") + 
    # ggtitle("German Language Crowd Scores by Vote") +
    xlab("Vote") + ylab("Mean Crowd Score") + theme_bw() + scale_fill_grey(start=0.45, end=1)
# + theme_bw()



ggplot(aes(y = DE_meansubsidy, x = voteNoAbstain), 
       data = subset(meanResults, voteNoAbstain %in% c("For", "Against"))) +
    geom_hline(yintercept=0, colour="darkred", linetype="dashed") +
    geom_boxplot(outlier.shape = NA, varwidth = TRUE) + 
    geom_jitter(position=position_jitter(width=.1), aes(size=EN_textLength), shape=21, bg="grey50") + 
    #geom_jitter(position=position_jitter(width=.1), shape=21, bg="grey50") + 
    # ggtitle("German Language Crowd Scores by Vote") +
    xlab("Vote") + ylab("Mean German Crowd Score") + theme(legend.position="none")
# + theme_bw()



#### SPANISH ####

ggplot(aes(y = ES_meansubsidyNonZero, x = voteNoAbstain), 
       data = subset(meanResults, voteNoAbstain %in% c("For", "Against"))) +
    geom_hline(yintercept=0, colour="darkred", linetype="dashed") +
    geom_boxplot(outlier.shape = NA, varwidth = TRUE) + 
    geom_jitter(position=position_jitter(width=.1), size=2.5) + 
    ggtitle("Spanish Crowd Scores by Vote Type") +
    xlab("Vote") + ylab("Mean Spanish Crowd Score")


## plot any pair
quartz()
voteColour <- ifelse(subset(meanResults, voteNoAbstain %in% c("For", "Against"), voteNoAbstain) =="For", "red", "blue")
ggplot(aes(y = EN2_meansubsidy, x = ES_meansubsidy),
       data = subset(meanResults, voteNoAbstain %in% c("For", "Against"))) +
    geom_point(colour = voteColour) +  
    geom_smooth(method=lm) +
    ggtitle("Spanish v. English") +
    xlab("Mean English Crowd Score") + ylab("Mean Spanish Crowd Score")


colSums(meanResults[, grep("textLength", names(meanResults), value=TRUE)])


## SCATTERPLOT MATRIX

quartz()
dataToPlot <- subset(meanResults, voteNoAbstain %in% c("For", "Against"),
                     select = grep("meansubsidy$", names(meanResults), value=TRUE))
names(dataToPlot) <- gsub("_", "", substr(names(dataToPlot), 1, 3))
voteColour <- ifelse(subset(meanResults, voteNoAbstain %in% c("For", "Against"), voteNoAbstain) =="For", "red", "blue")
require(psych)
pairs.panels(dataToPlot, col=voteColour,
             cex.cor=3, lm=TRUE, loess=FALSE, main="Subsidy Mean Scores")

dataToPlot2 <- data.frame(vote = factor(subset(meanResults, voteNoAbstain %in% c("For", "Against"), voteNoAbstain)[,1]),
                     dataToPlot)
GGally::ggpairs(dataToPlot2, 2:ncol(dataToPlot2), colour = "vote")







