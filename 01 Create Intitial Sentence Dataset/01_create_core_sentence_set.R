# File-Name:       create_core_sentence_set.R
# Date:            2013-09-20
# Author:          Ken Benoit
# Email:           kbenoit@lse.ac.uk                               
# Purpose:         create the core master sentence list from the manifesto data
#                  add some sentence-level variables
# 
#                  ##### RUN THIS ONLY ONCE, TO CREATE THE MASTER SENTENCE SET ######
# 
# Data Used:       a series of segmented manifestos in /sentence_segmented_manifesto_folder/
#                  ../Data - External/sentence_level_data.dta
#                  ../Data - External/goldset.504.csv
# Data Created:    master_sentence_list.csv
#                  master.sentences.Rdata
# Packages Used:   foreign, NLP, OpenNLP
# Machine:         Macs

myfolderlocation <- "~/Dropbox/Papers/CMP_recoding/newanalysis/1_create_sentence_set/"
myfolderlocation <- getwd()
sentence_segmented_manifesto_folder <- "Post-script cleaning manifestos/"

##
## load the older master list of sentences
##
require(foreign)
master.sentences <- read.dta("../Data - External/sentence_level_data.dta")
master.sentences <- subset(master.sentences,
                           select=c("manifestoid", 
                                    "party", "year", 
                                    names(master.sentences)[grep("sentence", names(master.sentences))],
                                    "total_syllables", "n_complex_words",
                                    "nouns", "verbs", "adjs",
                                    "advs", "propnouns"))
master.sentences$sentence_text <- gsub("\xc2", "", master.sentences$sentence_text)

master.sentences$truncated <- NA
master.sentences$sentence_text[13953] <- 
  "We want every pupil to be stretched, including the brightest, so we will develop extended projects at A-level, harder A-level questions to challenge the most able, and give universities the individual module marks as well as overall grades."
master.sentences$sentence_text[12652] <- "We will break the link between drugs and crime by massively expanding treatment programmes, including 25,000 residential rehab places (compared with fewer than 2,500 places today), and by giving all young users of hard drugs a straight choice."
master.sentences$truncated[which(nchar(master.sentences$sentence_text)==244)] <- 1

table(master.sentences$manifestoid)
sum(table(master.sentences$manifestoid)[1:7])
summary(nchar(master.sentences$sentence_text)) # verify truncation
# rename sentence_text to sentence_text_old
names(master.sentences)[which(names(master.sentences)=="sentence_text")] <- "sentence_text_old"

##
## process and append the new sentences
##
# get list of filenames - note these are cleaned and verified source files
filelist <- list.files(paste(myfolderlocation, sentence_segmented_manifesto_folder, sep=""))
# reorder to conform to the levels of manifestoid
filelist <- filelist[c(1,3,2,7,9,8,4,6,5,10,12,11,13,15,14,16,18,17)]

sentence_text <- ""  # initialize a container for reprocessed sentences
for (f in filelist) {
  cat(paste("Processing: ", myfolderlocation, "data_temp/", f, "...", sep=""))
  filename <- paste(myfolderlocation, sentence_segmented_manifesto_folder, f, sep="")
  toclean <- read.csv(filename, stringsAsFactors=FALSE, header=FALSE, sep="^")
  names(toclean) <- "sentence_text_new"
  for (i in 1:(nrow(toclean)-1)) {
    if (substr(toclean$sentence_text_new[i],1,1) == "#") {
      if (substr(toclean$sentence_text_new[i+1],1,1) == "#") {
        toclean$sentence_text_new[i+1] <- 
          paste("#", substr(toclean$sentence_text_new[i],2,nchar(toclean$sentence_text_new[i])), " ",
                substr(toclean$sentence_text_new[i+1],2,nchar(toclean$sentence_text_new[i+1])), sep="")
      } else {
        toclean$sentence_text_new[i+1] <- 
          paste(substr(toclean$sentence_text_new[i],2,nchar(toclean$sentence_text_new[i])), " ",
                substr(toclean$sentence_text_new[i+1],1,nchar(toclean$sentence_text_new[i+1])), sep="")
      }
    }
  }
  toclean <- subset(toclean, substr(sentence_text_new,1,1)!="#")
  write.csv(toclean, file=gsub(".txt", ".csv", paste(myfolderlocation, "data_temp/", f, sep="")))
  cat(paste(nrow(toclean), "lines.\n"))
  sentence_text <- c(sentence_text, toclean$sentence_text_new)
}
master.sentences$sentence_text <- sentence_text[-1]

# declare the encoding like this or else nchar fails("invalid multibyte string)
Encoding(master.sentences$sentence_text) <- "UTF-8"

##
## Count the syllables in each sentence, and calculate n complex words
##
require(quanteda)
countSyllables <- function(sourceText) {
  # load the RData file but only if not already loaded!
  # note that data() defaults to .Globalenv
  if (!exists(as.character(substitute(counts)))) {
    data(syllableCounts)
    print("loaded: syllableCounts")
  }
  # clean the string, change to uppercase for syllable dictionary match
  string <- gsub("[[:punct:][:digit:]]", "", sourceText)
  string <- gsub("\n", "", string)
  string <- toupper(string)
  words <- unlist(strsplit(string, " "))
  # lookup the syllables in the words found in the dictionary
  # uses vectorization and named vector indexing - not looping!
  n.syllables <- counts[words]
  # name the syllable count vector with the words
  names(n.syllables) <- words
  # count the syllables in each word?
  vowel.count.lookup <- sapply(words, function(l) sum((attr(gregexpr("[AEIOUY]*", l)[[1]], "match.length"))!=0))
  # replace the un-looked-up words with vowel formula words
  n.syllables[is.na(n.syllables)] <- 
    vowel.count.lookup[is.na(n.syllables)]
  return(list(total=sum(n.syllables), complex=sum(n.syllables>2)))
}
rm(counts)
system.time(syllables <- t(sapply(master.sentences$sentence_text, countSyllables, USE.NAMES=FALSE)))
master.sentences$total_syllables <- as.numeric(syllables[,1])
master.sentences$n_complex_words <- as.numeric(syllables[,2])

##
## count the number of words
##
nword <- function(stringVector) {
  sapply(strsplit(gsub("-", "", stringVector), " +", perl=TRUE), length)
}

master.sentences$sentence_nchars <- nchar(master.sentences$sentence_text)
master.sentences$sentence_nwords <- nword(master.sentences$sentence_text)
master.sentences$sentence_mean_word_length <- 
  (master.sentences$sentence_nchars - master.sentences$sentence_nwords + 1) / master.sentences$sentence_nwords 

##
## figure out the parts of speech in each sentence
##
require(openNLP)
require(NLP)
determine.pos <- function(sentence) {
  # clean sentence of punctuation and numbers
  sentence <- gsub("[[:punct:][:digit:]]", "", sentence)
  # print(sentence)
  # tage sentence parts of speech
  tagged.sentence <- tagPOS(sentence)$POStagged
  gc() # garbage collection - seems to prevent Heap Memory errors for Java call
  return()
  if (tagged.sentence=="") tagged.sentence<-"DeleteMe"
  # tokenize
  tagged.sentence.pos.char.vector <- scan(what="char", text=tagged.sentence, quiet=TRUE)
  # create a list of splits on the / character that precedes POS tags
  tagged.sentence.pos.parsedlist <- strsplit(tagged.sentence.pos.char.vector, "/")
  # put the second element of the list into a (factor) vector
  tagged.sentence.pos.factor.vector <-
    (sapply(tagged.sentence.pos.parsedlist, function(x) x[2]))
  # return as a factor vector of same length as text
  return(tagged.sentence.pos.factor.vector)
} 

tagPOS <-  function(text.var, pos_tag_annotator, ...) {
  gc()
  s <- as.String(text.var)
  
  ## Set up the POS annotator if missing (for parallel)
  if (missing(pos_tag_annotator)) {
    PTA <- Maxent_POS_Tag_Annotator()
  }
  
  ## Need sentence and word token annotations.
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, PTA, a2)
  
  ## Determine the distribution of POS tags for word tokens.
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, "[[", "POS"))
  
  ## Extract token/POS pairs (all of them): easy.
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  # list(POStagged = POStagged, POStags = POStags)
  return(POStags)
}

#
# have to loop because of Java heap space error - TAKES > 4 hours!!!
#
time.start <- proc.time()
pos.df <- data.frame(table(tagPOS(master.sentences$sentence_text[1])))
#pos.df <- data.frame(table(tagPOS(master.sentences$sentence_text[1])$POStags))
#pos.df <- cbind(master.sentences$sentenceid, pos.df)
names(pos.df) <- c("pos", master.sentences$sentenceid[1])
cat("Processing sentence: 1 ")
for (i in 2:nrow(master.sentences)) {
  #for (i in 20:30) {
  cat(i, " ")         
  this.sentence <- data.frame(table(tagPOS(master.sentences$sentence_text[i])))
  # got to have this to catch any zero-length POS vectors
  if (nrow(this.sentence)==0) { this.sentence <- data.frame(Var1="DT", Freq=0) }
  pos.df <- merge(pos.df, this.sentence, by.x=1, by.y=1, all=TRUE)
  names(pos.df)[ncol(pos.df)] <- master.sentences$sentenceid[i]
}
row.names(pos.df) <- pos.df$pos
pos.df <- t(pos.df[,-1])
print(proc.time()-time.start)
save(pos.df, file="data_temp/pos.df.Rdata")
pos.df[is.na(pos.df)] <- 0


sentences.pos <- data.frame(nouns=apply(pos.df[,c("NN", "NNS", "NNP", "NNPS")], 1, sum),
                            verbs=apply(pos.df[,c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ")], 1, sum),
                            adjs=apply(pos.df[,c("JJR", "JJ", "JJS")], 1, sum),
                            advs=apply(pos.df[,c("RB", "RBR", "RBS")], 1, sum),
                            propnouns=apply(pos.df[,c("NNP", "NNPS")], 1, sum))

master.sentences[,11:15] <- sentences.pos # replace existing data with new

#
# merge in the gold sentences
#
gold504 <- read.csv("../Data - External/goldset.504.csv")
master.sentences <- merge(master.sentences, gold504[,-c(2:4)], by.x="sentenceid", by.y="text_unit_id", all=TRUE)

#
# merge in screener questions
#
###
###  CODE HERE - IF WE WANT TO USE SCREENERS
###

# order the master sentences
master.sentences <- master.sentences[order(master.sentences$sentenceid),]

##
## create pre_sentence and post_sentence - slow but straightforward!
##
sentences <- master.sentences
for (i in 1:(nrow(sentences)-0)) {
  if (i>2 && sentences$manifestoid[i-2]==sentences$manifestoid[i]) {
    sentences$pre_sentence[i] <- paste(sentences$sentence_text[i-2], sentences$sentence_text[i-1])
  } else if (i>1 && sentences$manifestoid[i-1]==sentences$manifestoid[i]) {
    sentences$pre_sentence[i] <- sentences$sentence_text[i-1]
  } else sentences$pre_sentence[i] <- ""
  if (i<=(nrow(sentences)-2) && sentences$manifestoid[i+2]==sentences$manifestoid[i]) {
    sentences$post_sentence[i] <- paste(sentences$sentence_text[i+1], sentences$sentence_text[i+2])
  } else if (i<=(nrow(sentences)-1) && sentences$manifestoid[i+1]==sentences$manifestoid[i]) {
    sentences$post_sentence[i] <- sentences$sentence_text[i+1]
  } else sentences$post_sentence[i] <- ""
}

sentences <- sentences[,-which(names(sentence)=="sentence_text_old")] # drop this column

# merge in the policy area gold reasons
sentences.unmerged <- sentences
save(sentences.unmerged, file="../Data - Temp/sentences.unmerged.Rdata")
sentences <- merge(sentences.unmerged,
                   subset(read.csv("../Data - External/data_gold_reasons.csv", stringsAsFactors=FALSE), 
                          select=c("sentenceid", "policy_area_gold_reason")),
                   by="sentenceid", all.x=TRUE)

##
## Save the master sentence file
##
save(master.sentences, file="../Data - Temp/master.sentences.orig.Rdata")
save(sentences, file="../Data - Created/master.sentences.Rdata")
write.csv(sentences, file="../Data - Created/master.sentences.csv", fileEncoding="UTF-8", na="")


#2  Con 1987 1015  OK
#3   LD 1987  878  OK
#4  Lab 1987  455  OK
#5  Con 1997 1171  OK
#6   LD 1997  873  OK
#7  Lab 1997 1052  OK
#8  Con 1992 1731  OK
#9   LD 1992  884  OK
#10 Lab 1992  661  OK
#11 Con 2001  748  OK
#12  LD 2001 1178  OK
#13 Lab 2001 1752  OK
#14 Con 2005  414  OK
#15  LD 2005  821  OK
#16 Lab 2005 1186  OK
#17 Con 2010 1240  OK
#18  LD 2010  855  OK
#19 Lab 2010 1349  OK
