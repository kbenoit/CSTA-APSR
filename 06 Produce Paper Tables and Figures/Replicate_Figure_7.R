###
### FIGURE 7
### EP DEBATE IN 6 LANGUAGES
###

rm(list=ls())
setwd("~/Dropbox/Papers/CMP_recoding/newanalysis/06 Produce Paper Tables and Figures")
  

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
# combine two english into one ###########################################
joblist <- c("f650814.csv", "f655764.csv", "fEnglishCombined.csv",
             "f655316.csv", "f656054.csv",
             "f656640.csv", "f658174.csv", "f658191.csv", "f660453.csv")
jobnames <- c("ENA", "ENB", "EN", "DE", "ES", "IT", "GR", "PL", "EN2")
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
#levels(meanResultsStacked$Language) <- 
#    c("German", "English", "Spanish", "Greek", "Italian", "Polish", "English2")
quartz(width=6, height=4)
require(ggplot2)
ggplot(aes(y = meansubsidyNonZero, x = voteNoAbstain, fill = Language), 
       data = subset(meanResultsStacked, voteNoAbstain %in% c("For", "Against"))) +
    scale_x_discrete(labels = c("For (n=25)", "Against (n=6)", "Abstain")) +
    geom_boxplot() + #, varwidth = TRUE) + 
    xlab("Vote") + ylab("Mean Crowd Score") + 
    theme_bw() + scale_fill_grey(start=0.45, end=1)
