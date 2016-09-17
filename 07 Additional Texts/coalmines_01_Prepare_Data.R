## original speech data from
## http://www.europarl.europa.eu/sides/getDoc.do?pubRef=-//EP//TEXT+CRE+20101123+ITEM-005+DOC+XML+V0//EN&language=EN
## legislation from 
## http://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:52010AP0424&from=EN

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

## read in the speeches
textList <- list()
for (speech in c("EN", "ES", "DE", "IT", "HU", "PL", "GR")) {
    # read in each file
    tf <- file(paste("coal - input files/", speech, ".txt", sep=""), "rt")
    txt <- readChar(tf, 100000)
    close(tf)
    
    # split into separate texts on #TEXT
    txt <- unlist(strsplit(txt, "\\s*#TEXT\\s"))
    # remove blank speeches
    txt <- txt[which(txt != "")]
    # make sure all the dashes are the same across texts
    #txt <- gsub("–|-|-", "-", txt)
    # remove the speaker tags, e.g. "  Bernhard Rapkay, rapporteur. – (DE) "
    txt <-  gsub("^([^\\-]+)-\\s(\\([A-Z]{2}\\)\\s)*", "", txt)
    names(txt) <- paste(speech, 1:length(txt), "_", sep="")
    textList[[speech]] <- txt
    # append(textList, list(speech=txt))
}



# start with English
coalspeechCorp <- corpus(textList[[1]], 
                         source="http://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:52010AP0424&from=EN")
docvars(coalspeechCorp) <- docv
language(coalspeechCorp) <- "EN"
# read in the other 4 languages
for (lang in names(textList)[2:length(names(textList))]) {
    tempCorp <- corpus(textList[[lang]])
    docvars(tempCorp) <- docv
    language(tempCorp) <- lang
    coalspeechCorp <- coalspeechCorp + tempCorp
}

encoding(coalspeechCorp) <- "UTF-8"

coalspeechCorpSent <- changeunits(coalspeechCorp, to="sentences", delimiter="[.?]|!_")


## are sentence numbers the same?
senttab <- table(docvars(coalspeechCorpSent, "sequence"), as.vector(language(coalspeechCorpSent)))
uneven <- as.numeric(which(apply(senttab, 1, var) > 0))

cat("\nTotal sentences by speech and language:\n")
print(senttab[uneven, ])

append <- FALSE # for first iteration
# cycle through the cases where sentences are not perfectly matched
for (seqno in uneven) {

    thisspeech <- subset(coalspeechCorpSent, sequence==seqno & `_language`=="EN")
    maxlength <-  max(senttab[seqno,])
    ENtext <- texts(thisspeech)
    ENtext <- paste(1:length(ENtext), ENtext)
    length(ENtext) <- maxlength
    sequence <- docvars(thisspeech, "sequence")
    length(sequence) <- maxlength
    nameLast <- docvars(thisspeech, "nameLast")
    length(nameLast) <- maxlength
    tempdf <- data.frame(sequence, nameLast, EN=ENtext)

    # cycle through non-English texts 
    for (c in c(1, 3:length(names(textList)))) {
        if (senttab[seqno, c] == senttab[seqno, "EN"]) next
        thislang <- colnames(senttab)[c]
        nonENtext <- texts(subset(coalspeechCorpSent, sequence==seqno & `_language`==thislang))
        nonENtext <- paste(1:length(nonENtext), nonENtext)
        length(nonENtext) <- maxlength
        tempdf <- cbind(tempdf, nonENtext)
        names(tempdf)[ncol(tempdf)] <- thislang
    }
    
    write.xlsx(tempdf, "sentencesToMatch.xlsx", 
               sheetName=paste("Speech", seqno),
               append=append)
    append <- TRUE  # second and subsequent iterations
}



cat("\n\nTotal sentences by language:\n")
print(table(language(coalspeechCorpSent)))

###### TO DO LIST ######
# output the following, by language:
#     sequenceno * 1000 + sentenceno
#     nameLast + nameFirst
#     sentence
#     twobefore
#     twoafter
# create a set of gold questions in each language

for (lang in names(table(language(coalspeechCorpSent)))) {
    thisCorpus <- subset(coalspeechCorpSent, `_language`==lang)
    sentenceid <- docvars(thisCorpus, "sequence")*1000 + metadoc(thisCorpus, "serialno")
    sentenceid <- sentenceid[,1]
    sentences <- data.frame(sentenceid = sentenceid,
                            speechno = docvars(thisCorpus, "sequence"),
                            language=lang,
                            "gold" = "",
                            subsidy_scale = "",
                            subsidy_scale_gold = "",
                            subsidy_scale_gold_reason = "",
                            screener=FALSE,
                            sentence_text=texts(thisCorpus),
                            pre_sentence = "",
                            post_sentence = "",
                            stringsAsFactors=FALSE)
    # rename the column for gold to _golden
    names(sentences)[which(names(sentences)=="gold")] <- "_golden"
        
    for (i in 1:nrow(sentences)) {
        if (i>2 && sentences$speechno[i-2]==sentences$speechno[i]) {
            sentences$pre_sentence[i] <- paste(sentences$sentence_text[i-2], sentences$sentence_text[i-1])
        } else if (i>1 && sentences$speechno[i-1]==sentences$speechno[i]) {
            sentences$pre_sentence[i] <- sentences$sentence_text[i-1]
        } else sentences$pre_sentence[i] <- ""
        if (i<=(nrow(sentences)-2) && sentences$speechno[i+2]==sentences$speechno[i]) {
            sentences$post_sentence[i] <- paste(sentences$sentence_text[i+1], sentences$sentence_text[i+2])
        } else if (i<=(nrow(sentences)-1) && sentences$speechno[i+1]==sentences$speechno[i]) {
            sentences$post_sentence[i] <- sentences$sentence_text[i+1]
        } else sentences$post_sentence[i] <- ""
    }
    
    write.csv(sentences, 
              file=paste("coal - files prepared for CF/coalsentences_", lang, ".csv", sep=""), 
              fileEncoding="UTF-8", na="", row.names=FALSE)
}

cat("\nWrote", length(names(table(language(coalspeechCorpSent)))), "sentence files to .csv.\n")


