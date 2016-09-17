## segment the text of the R1 APSR REVIEWS

rm(list=ls())
library(quanteda)

setwd("~/Dropbox/Papers/CMP_recoding/newanalysis/07 Additional/reviews/")

# read in the text file
tf <- file("reviewer_texts.txt", "rt")
txt <- readChar(tf, 100000)
close(tf)

# split into separate texts on #TEXT
txt <- unlist(strsplit(txt, "\\s*#TEXT\\s"))
# remove blank speeches
txt <- txt[which(txt != "")]
names(txt) <- c("Editor", paste("R", 1:5, sep="_"))

# create a sentence-level corpus
reviewsCorp <- corpus(txt, source="APSR Round 1 Reviews")
docvars(reviewsCorp, "sequence") <- 1:6
reviewsCorpSent <- changeunits(reviewsCorp, to="sentences", delimiter="[.?!]+")
thisCorpus <- reviewsCorpSent
sentenceid <- docvars(thisCorpus, "sequence")*100 + metadoc(thisCorpus, "serialno")
sentenceid <- sentenceid[,1]
sentences <- data.frame(sentenceid = sentenceid,
                        reviewno = docvars(thisCorpus, "sequence"),
                        "gold" = "",
                        pub_scale = "",
                        pub_scale_gold = "",
                        pub_scale_gold_min = "",
                        pub_scale_gold_max = "",
                        pub_scale_gold_reason = "",
                        screener=FALSE,
                        sentence_text=texts(thisCorpus),
                        pre_sentence = "",
                        post_sentence = "",
                        stringsAsFactors=FALSE)
# rename the column for gold to _golden
names(sentences)[which(names(sentences)=="gold")] <- "_golden"
    
for (i in 1:nrow(sentences)) {
    if (i>2 && sentences$reviewno[i-2]==sentences$reviewno[i]) {
        sentences$pre_sentence[i] <- paste(sentences$sentence_text[i-2], sentences$sentence_text[i-1])
    } else if (i>1 && sentences$reviewno[i-1]==sentences$reviewno[i]) {
        sentences$pre_sentence[i] <- sentences$sentence_text[i-1]
    } else sentences$pre_sentence[i] <- ""
    if (i<=(nrow(sentences)-2) && sentences$reviewno[i+2]==sentences$reviewno[i]) {
        sentences$post_sentence[i] <- paste(sentences$sentence_text[i+1], sentences$sentence_text[i+2])
    } else if (i<=(nrow(sentences)-1) && sentences$reviewno[i+1]==sentences$reviewno[i]) {
        sentences$post_sentence[i] <- sentences$sentence_text[i+1]
    } else sentences$post_sentence[i] <- ""
}

write.csv(sentences, file = "reviewer_texts_CF.csv", 
              fileEncoding="UTF-8", na="", row.names=FALSE)
cat("\nWrote reviewer sentence file to \'reviewer_texts_CF.csv\'.\n")


### ANALYZE RESULTS
results <- read.csv("f653428.csv", stringsAsFactors=FALSE)
results <- read.csv("f655779.csv", stringsAsFactors=FALSE)

results <- results[which(results$X_golden == "false" & (results$sentenceid<620 | results$sentenceid>638)), ]

senttable <- table(results$reviewno)
names(senttable) <- c("Editor", paste("R", 1:5, sep=""))
print(senttable)
sents <- unique(results[, c("sentenceid", "reviewno")])
sents <- table(sents$reviewno)
names(sents) <- c("Editor", paste("R", 1:5, sep=""))
print(rbind(sents, senttable))

# include zero in denominator
meanresults <- aggregate(results$pub_scale, by=list(results$sentenceid), mean)
names(meanresults) <- c("sentenceid", "sentencemean")
meanresults$reviewno <- floor(meanresults$sentenceid / 100)
meanresults2 <- aggregate(meanresults$sentencemean, by=list(meanresults$reviewno), 
                          function(x) mean(x[x != 0]))
sdresults <- aggregate(meanresults$sentencemean, by=list(meanresults$reviewno), 
                       function(x) sd(x[x != 0]))
Nresults <- aggregate(meanresults$sentencemean, by=list(meanresults$reviewno), 
                      function(x) sum(x != 0))
mytable <- table(results$pub_scale, results$reviewno)
R <- mytable[5,] + mytable[4,]
L <- mytable[1,] + mytable[2,]
#R <- mytable[3,]
#L <- mytable[1,]

a <- 0.5
logsigma2 <- (L + a)^(-1) + (R + a)^(-1)
meanResults <- data.frame(review = factor(c("Editor", paste("R", 1:5, sep="")), 
                                          ordered=TRUE),
                     meanresults = meanresults2[,2],
                     meanresultsSE = sdresults[,2] / sqrt(Nresults[, 2]),
                     logresults = log((R + a) / (L + a)),
                     logresultsSE = sqrt(logsigma2))
rm(meanresults)
save(meanResults, file="meanResults.RData")

library(ggplot2)
reviewColors <- c("gray60", rep("darkgreen", 2), rep("darkred", 2), "darkgreen")
meanresults <- meanResults
limits <- aes(ymax = meanresults + 1.96*meanresultsSE, 
              ymin = meanresults - 1.96*meanresultsSE)
p <- ggplot(meanResults, aes(y = meanresults, x = review))
p + geom_pointrange(limits, col = reviewColors) + coord_flip() + 
    xlab("") + ylab("Mean Score of Anti-/Pro-Publication") + 
    geom_hline(yintercept=0, colour="grey30", linetype="dashed") +
    theme(axis.text.y = element_text(colour = reviewColors, size=14))

quartz()
limits <- aes(ymax = logresults + 1.96*logresultsSE, 
              ymin = logresults - 1.96*logresultsSE)
p <- ggplot(meanResults, aes(y = logresults, x = review))
p + geom_pointrange(limits, col = reviewColors) + 
    coord_flip() + 
    xlab("") + ylab("Logit Score of Anti-/Pro-Publication") + 
    geom_hline(yintercept=0, colour="grey40", linetype="dashed") +
    theme(axis.text.y = element_text(colour = reviewColors, size=10))



