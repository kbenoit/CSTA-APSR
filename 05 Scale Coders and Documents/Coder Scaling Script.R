
if (Sys.getenv("USER")=="kbenoit") {
  setwd("~/Dropbox/Papers/CMP_recoding/newanalysis/02 Create a New CF job")
} else {
  setwd("~/Dropbox/Research/Project - CMP Recoding/newanalysis/05 Scale Coders and Documents/")
}

##################################################
##################################################
##                   WARNING                    ##
##                                              ##
##        RUNNING THE MCMC IN THIS SCRIPT       ##
##         REQUIRES 7.2GB OF FREE MEMORY        ##
##            FOR 200 SAVED ITERATIONS          ##
##                                              ##
##                   WARNING                    ##
##################################################
##################################################

new.mcmc.run <- FALSE
new.crowd.sims <- FALSE
new.crowd.sim.size <- rep(1:20,each=10)
new.expert.sims <- FALSE
new.expert.sim.size <- rep(1:5,each=10)

iter.mcmc <- 500
iter.burn <- 500

##################################################
##               LOAD LIBRARIES                 ##
##################################################

library(rjags)
library(MASS)

##################################################
##                 SUBSET DATA                  ##
##################################################

print("Loading Data...")

Data <- read.csv("../Data - Created/coding_all_long_2013-12-19.csv")

# Remove screener items and semi-expert data 
is.screener <- Data$manifestoid == "screener" | Data$manifestoid == "99" 
is.semiexpert <- Data$source == "SemiExperts"
Data <- subset(Data,!is.screener & !is.semiexpert)

is.expert <- Data$source == "Experts"

##################################################
##                 SET UP DATA                  ##
##################################################

Stage <- factor(Data$stage)
is.cfstage <- Stage == "CF_kb" | Stage == "CF12_kb"

Coder <- factor(Data$coderid)
Sentence <- factor(Data$sentenceid)
Manifesto <- factor(Data$manifestoid)

Ncodings <- dim(Data)[1]
Ncoders <- length(levels(Coder))
Nsentences <- length(levels(Sentence))
Nmanifestos <- length(levels(Manifesto))

Scale <- relevel(Data$scale,"None")
Code <- Data$code

Y3 <- Scale   # 1=None, 2=Economic, 3=Social
Y11 <- 1 + (as.numeric(Y3) == 3)*5 + replace(Code + 3, is.na(Code), 0)
Y11 <- factor(Y11, labels=c("None","E:-2","E:-1","E:0","E:1","E:2","S:-2","S:-1","S:0","S:1","S:2"))
# check that there are no None's with position codes, and no Econ/Social with NA position codes
print(table(Scale, Y11))

which.crowd <- grep("[0-9]+",levels(Coder))  # numeric coder ids are crowd
which.experts <- setdiff(1:Ncoders,which.crowd)  # non-numeric coder ids are experts

manifesto.order <- c("Con 1987","LD 1987","Lab 1987","Con 1997","LD 1997","Lab 1997","Con 1992","LD 1992","Lab 1992","Con 2001","LD 2001","Lab 2001","Con 2005","LD 2005","Lab 2005","Con 2010","LD 2010","Lab 2010")

manifesto.years <- c(1987,1992,1997,2001,2005,2010)

manifesto.data <- matrix(unlist(strsplit(manifesto.order," ")),ncol=2,byrow=TRUE)
colnames(manifesto.data) <- c("party","year")
manifesto.data <- as.data.frame(manifesto.data)
manifesto.data$year <- as.numeric(as.character(manifesto.data$year))
manifesto.data$colour <- rep(" ",18)
manifesto.data$order <- rep(0,18)

for (l in 18:1) Manifesto <- relevel(Manifesto, ref=manifesto.order[l])

all.party.colours <- matrix(c(
"#0087DC","Con",
"#FDBB30","LD",
"#DC241F","Lab"
),ncol=2,byrow=TRUE)

fade.colour <- function(col,alpha){
	components <- col2rgb(col)
	return(rgb(components[1,]/255,components[2,]/255,components[3,]/255,alpha=alpha))
	}

for (l in 1:18){
	manifesto.data$colour[l] <- all.party.colours[which(manifesto.data$party[l] == all.party.colours[,2]),1]
	manifesto.data$order[l] <- which(manifesto.data$year[l] == manifesto.years)
	}

# following function plots a posterior ellipse based on a bivariate posterior sample
add.ellipse <- function(bivariate.sample,col,level=0.95,independent=FALSE){
	require(ellipse)	
	covar <- var(bivariate.sample)
	if (independent) covar <- covar * diag(1,2,2)
	ellipse.temp <- ellipse(x=covar,centre=colMeans(bivariate.sample),level=level)
	polygon(ellipse.temp[,1],ellipse.temp[,2],col=col,border=NA)
	}
	
# following function plots a posterior ellipse based on mean and covariance matrix
add.ellipse.mat <- function(mu,Sigma,col,level=0.95){
	require(ellipse)	
	ellipse.temp <- ellipse(x=Sigma,centre=mu,level=level)
	polygon(ellipse.temp[,1],ellipse.temp[,2],col=col,border=NA)
	}


CoderID <- as.numeric(Coder)
SentenceID <- as.numeric(Sentence)
ManifestoID <- as.numeric(Manifesto)

ManifestoIDforSentence <- round(as.numeric(levels(Sentence))/10000000,0)

# hacky line below makes the manifesto IDs 1-18 rather than 1-6 and 107-118
# this is necessary as the variable is used for indexing purposes in the JAGS code
ManifestoIDforSentence <- as.numeric(as.factor(ManifestoIDforSentence))

core.manifesto.names <- c("Con 1987","Lab 1987","LD 1987","Con 1997","Lab 1997","LD 1997") 
core.manifesto <- which(levels(Manifesto) %in% core.manifesto.names)

in.core <- ManifestoID %in% core.manifesto  

is.expert.sequential <- Data$stage == "ES" | Data$stage == "ES2" 
is.expert.random <- Data$stage == "ER"

##################################################
##              DEFINE JAGS MODEL               ##
##################################################

jagscode <- '
model {

  	for (q in 1:Ncodings){
  		
  		# Define latent response for code/scale in econ/social 
  		
  		mucode[q,1] <- (theta[SentenceID[q],1,1] + psi[CoderID[q],1,1])*chi[CoderID[q],1,1];
  		mucode[q,2] <- (theta[SentenceID[q],2,1] + psi[CoderID[q],2,1])*chi[CoderID[q],2,1]; 
  		muscale[q,1] <- (theta[SentenceID[q],1,2] + psi[CoderID[q],1,2])*chi[CoderID[q],1,2]; 
  		muscale[q,2] <- (theta[SentenceID[q],2,2] + psi[CoderID[q],2,2])*chi[CoderID[q],2,2]; 	
  		
  		# Translate latent responses into 11 category probabilities (up to normalization)	
  		
  		mu[q,1] <- 1;
  		mu[q,2] <- exp(mucode[q,1])*(ilogit(-1*cut[2] - muscale[q,1]));
  		mu[q,3] <- exp(mucode[q,1])*(ilogit(-1*cut[1] - muscale[q,1])-ilogit(-1*cut[2] - muscale[q,1]));
  		mu[q,4] <- exp(mucode[q,1])*(ilogit(1*cut[1] - muscale[q,1])-ilogit(-1*cut[1] - muscale[q,1]));
  		mu[q,5] <- exp(mucode[q,1])*(ilogit(1*cut[2] - muscale[q,1])-ilogit(1*cut[1] - muscale[q,1]));
  		mu[q,6] <- exp(mucode[q,1])*(1-ilogit(1*cut[2] - muscale[q,1]));
  		mu[q,7] <- exp(mucode[q,2])*(ilogit(-1*cut[2] - muscale[q,2]));
  		mu[q,8] <- exp(mucode[q,2])*(ilogit(-1*cut[1] - muscale[q,2])-ilogit(-1*cut[2] - muscale[q,2]));
  		mu[q,9] <- exp(mucode[q,2])*(ilogit(1*cut[1] - muscale[q,2])-ilogit(-1*cut[1] - muscale[q,2]));
  		mu[q,10] <- exp(mucode[q,2])*(ilogit(1*cut[2] - muscale[q,2])-ilogit(1*cut[1] - muscale[q,2]));
  		mu[q,11] <- exp(mucode[q,2])*(1-ilogit(1*cut[2] - muscale[q,2]));
  		
  		# 11 category multinomial
  		
  		Y[q] ~ dcat(mu[q,1:11]);
  		
  	}	
  	
  	# Specify uniform priors for ordinal threshholds (assumes left-right symmetry)
  	
  	cut[1] ~ dunif(0,5);
  	cut[2] ~ dunif(cut[1],10);
  	
  	# Priors for coder bias parameters
  	
  	for (i in 1:Ncoders) {
  		psi[i,1,1] ~ dnorm(0,taupsi[1,1]);
  		psi[i,2,1] ~ dnorm(0,taupsi[2,1]);
  		psi[i,1,2] ~ dnorm(0,taupsi[1,2]);
  		psi[i,2,2] ~ dnorm(0,taupsi[2,2]);  		
  	}	
  	
  	# Priors for coder sensitivity parameters
  	
  	for (i in 1:Ncoders) {
  		chi[i,1,1] ~ dnorm(0,1)T(0,);
  		chi[i,2,1] ~ dnorm(0,1)T(0,);
  		chi[i,1,2] ~ dnorm(0,1)T(0,);
  		chi[i,2,2] ~ dnorm(0,1)T(0,);  		
  	}	  	  	
  	
  	# Priors for sentence latent parameters 	
  	
  	for (j in 1:Nsentences) {
  		theta[j,1,1] ~ dnorm(thetabar[ManifestoIDforSentence[j],1,1],tautheta[1,1]);
  		theta[j,2,1] ~ dnorm(thetabar[ManifestoIDforSentence[j],2,1],tautheta[2,1]);
  		theta[j,1,2] ~ dnorm(thetabar[ManifestoIDforSentence[j],1,2],tautheta[1,2]);
  		theta[j,2,2] ~ dnorm(thetabar[ManifestoIDforSentence[j],2,2],tautheta[2,2]);  		
  	}	
  	
  	# Priors for manifesto latent parameters
  	
  	for (k in 1:Nmanifestos) {
  		thetabar[k,1,1] ~ dnorm(0,1);
  		thetabar[k,2,1] ~ dnorm(0,1);
   		thetabar[k,1,2] ~ dnorm(0,1);
  		thetabar[k,2,2] ~ dnorm(0,1); 		
  	}
  	
  	# Variance parameters
  	
  	taupsi[1,1] ~ dgamma(1,1);
   	taupsi[2,1] ~ dgamma(1,1);
   	taupsi[1,2] ~ dgamma(1,1);
   	taupsi[2,2] ~ dgamma(1,1);   

  	tautheta[1,1] ~ dgamma(1,1);
  	tautheta[2,1] ~ dgamma(1,1);
  	tautheta[1,2] ~ dgamma(1,1);
  	tautheta[2,2] ~ dgamma(1,1);  			   	 	

}
'

##################################################
##        RUN CROWD DATA SUBSET MODELS          ##
##################################################

if (new.crowd.sims){
	
	for (sim in 1:length(new.crowd.sim.size)){
		
		CodingRatio <- new.crowd.sim.size[sim]
		
   		crowd.subset <- subset(data.frame(Y=Y11, CoderID, SentenceID, nrow.all=1:Ncodings),
                          (!is.expert & in.core))
   		obs.subset <- rep(FALSE, Ncodings)
   		obs.subset.index <- aggregate(crowd.subset$nrow.all,
                                by=list(crowd.subset$SentenceID),
                                sample, CodingRatio)$x
   		obs.subset[obs.subset.index] <- TRUE

		crowdsubdata <- list(Y=as.numeric(Y11)[obs.subset],CoderID=CoderID[obs.subset],SentenceID=SentenceID[obs.subset], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(obs.subset),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)	
		
		print("Running MCMC for Crowd Subset")
		model.jags <- jags.model(file=textConnection(jagscode)
				,data= crowdsubdata,n.adapt= iter.burn)
		update(model.jags, iter.burn)
		crowdposterior.sim <- jags.samples(model.jags, 
				c("thetabar"), 
				n.iter= iter.mcmc, thin = 1, progress.bar="text")
		save(crowdposterior.sim,CodingRatio,file=paste("Crowd Coder Subset Posteriors/CrowdSubsetPosterior", CodingRatio,"CodingRatio",date(),".Rdata",sep=""))
		rm(model.jags)
		gc()	
	
	}
		
}	



##################################################
##        RUN EXPERT DATA SUBSET MODELS         ##
##################################################

if (new.expert.sims){
	
	for (sim in 1:length(new.expert.sim.size)){
		
		CodingRatio <- new.expert.sim.size[sim]
		
   		expert.subset <- subset(data.frame(Y=Y11, CoderID, SentenceID, nrow.all=1:Ncodings),
                          (is.expert & in.core))
   		obs.subset <- rep(FALSE, Ncodings)
   		obs.subset.index <- aggregate(expert.subset$nrow.all,
                                by=list(expert.subset$SentenceID),
                                sample, CodingRatio)$x
   		obs.subset[obs.subset.index] <- TRUE

		expertsubdata <- list(Y=as.numeric(Y11)[obs.subset],CoderID=CoderID[obs.subset],SentenceID=SentenceID[obs.subset], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(obs.subset),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)	
		
		print("Running MCMC for Expert Subset")
		model.jags <- jags.model(file=textConnection(jagscode)
				,data= expertsubdata,n.adapt= iter.burn)
		update(model.jags, iter.burn)
		expertposterior.sim <- jags.samples(model.jags, 
				c("thetabar"), 
				n.iter= iter.mcmc, thin = 1, progress.bar="text")
		save(expertposterior.sim,CodingRatio,file=paste("Expert Coder Subset Posteriors/ExpertSubsetPosterior", CodingRatio,"CodingRatio",date(),".Rdata",sep=""))
		rm(model.jags)
		gc()	
	
	}
		
}	



##################################################
##             RUN FULL DATA MODELS             ##
##################################################

if (new.mcmc.run){
	
	crowddata.all <- list(Y=as.numeric(Y11)[!is.expert & is.cfstage],CoderID=CoderID[!is.expert & is.cfstage],SentenceID=SentenceID[!is.expert & is.cfstage], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(!is.expert & is.cfstage),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)
	
	jointdata.core <- list(Y=as.numeric(Y11)[in.core],CoderID=CoderID[in.core],SentenceID=SentenceID[in.core], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(in.core),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)

	crowddata.core <- list(Y=as.numeric(Y11)[!is.expert & in.core],CoderID=CoderID[!is.expert & in.core],SentenceID=SentenceID[!is.expert & in.core], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(!is.expert & in.core),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)

	expertdata.core <- list(Y=as.numeric(Y11)[is.expert & in.core],CoderID=CoderID[is.expert & in.core],SentenceID=SentenceID[is.expert & in.core], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(is.expert & in.core),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)
	
	expertdata.all <- list(Y=as.numeric(Y11)[is.expert],CoderID=CoderID[is.expert],SentenceID=SentenceID[is.expert], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(is.expert),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)
	
	expertdata.seq <- list(Y=as.numeric(Y11)[is.expert & is.expert.sequential],CoderID=CoderID[is.expert & is.expert.sequential],SentenceID=SentenceID[is.expert & is.expert.sequential], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(is.expert & is.expert.sequential),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)	
	expertdata.rand <- list(Y=as.numeric(Y11)[is.expert & is.expert.random],CoderID=CoderID[is.expert & is.expert.random],SentenceID=SentenceID[is.expert & is.expert.random], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(is.expert & is.expert.random),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)
	
	print("Running MCMC for Crowd on All Manifestos...")
	
	model.jags <- jags.model(file=textConnection(jagscode)
				,data=crowddata.all,n.adapt= iter.burn)		
	update(model.jags, iter.burn)	
	crowdposterior.all <- jags.samples(model.jags, 
				c("chi","psi","theta","thetabar","cut"), 
				n.iter= iter.mcmc, thin = 1, progress.bar="text")		
	save(crowdposterior.all,file="Saved Posteriors/CrowdAllPosterior.Rdata")
	rm(model.jags)
	gc()
		
	print("Running MCMC for Crowd on Core Manifestos...")
	
	model.jags <- jags.model(file=textConnection(jagscode), data=crowddata.core, n.adapt=iter.burn)		
	update(model.jags, iter.burn)	
	crowdposterior.core <- 
	  jags.samples(model.jags, c("chi","psi","theta","thetabar","cut"), 
	               n.iter=iter.mcmc, thin = 1, progress.bar="text")		
	save(crowdposterior.core, file="Saved Posteriors/CrowdCorePosterior.Rdata")
	rm(model.jags)
	gc()
		
	print("Running MCMC for Experts on Core Manifestos...")
	
	model.jags <- jags.model(file=textConnection(jagscode)
				,data= expertdata.core,n.adapt= iter.burn)		
	update(model.jags, iter.burn)	
	expertposterior.core <- jags.samples(model.jags, 
				c("chi","psi","theta","thetabar","cut"), 
				n.iter= iter.mcmc, thin = 1, progress.bar="text")			
	save(expertposterior.core,file="Saved Posteriors/ExpertCorePosterior.Rdata")
	rm(model.jags)
	gc()
	
	print("Running MCMC for Experts on All Manifestos...")
	
	model.jags <- jags.model(file=textConnection(jagscode)
				,data= expertdata.all,n.adapt= iter.burn)		
	update(model.jags, iter.burn)	
	expertposterior.all <- jags.samples(model.jags, 
				c("chi","psi","theta","thetabar","cut"), 
				n.iter= iter.mcmc, thin = 1, progress.bar="text")			
	save(expertposterior.all,file="Saved Posteriors/ExpertAllPosterior.Rdata")
	rm(model.jags)
	gc()	
	
	print("Running MCMC for Experts with Random Ordering...")
	
	model.jags <- jags.model(file=textConnection(jagscode)
				,data= expertdata.rand,n.adapt= iter.burn)		
	update(model.jags, iter.burn)	
	expertposterior.rand <- jags.samples(model.jags, 
				c("chi","psi","theta","thetabar","cut"), 
				n.iter= iter.mcmc, thin = 1, progress.bar="text")			
	save(expertposterior.rand,file="Saved Posteriors/ExpertRandPosterior.Rdata")
	rm(model.jags)
	gc()
	
	print("Running MCMC for Experts with Sequential Ordering...")
	
	model.jags <- jags.model(file=textConnection(jagscode)
				,data= expertdata.seq,n.adapt= iter.burn)		
	update(model.jags, iter.burn)	
	expertposterior.seq <- jags.samples(model.jags, 
				c("chi","psi","theta","thetabar","cut"), 
				n.iter= iter.mcmc, thin = 1, progress.bar="text")			
	save(expertposterior.seq,file="Saved Posteriors/ExpertSeqPosterior.Rdata")
	rm(model.jags)
	gc()	
	
	print("Running MCMC for Joint Data on Core Manifestos...")
	
	model.jags <- jags.model(file=textConnection(jagscode)
				,data=jointdata.core,n.adapt= iter.burn)		
	update(model.jags, iter.burn)	
	jointposterior.core <- jags.samples(model.jags, 
				c("chi","psi","theta","thetabar","cut"), 
				n.iter= iter.mcmc, thin = 1, progress.bar="text")			
	save(jointposterior.core,file="Saved Posteriors/JointCorePosterior.Rdata")
	rm(model.jags)
	gc()	

} else {
	
print("Loading Saved Posteriors...")	

	load(file="Saved Posteriors/JointCorePosterior.Rdata")
	load(file="Saved Posteriors/CrowdCorePosterior.Rdata")
	load(file="Saved Posteriors/ExpertCorePosterior.Rdata")	
	load(file="Saved Posteriors/ExpertSeqPosterior.Rdata")		
	load(file="Saved Posteriors/ExpertRandPosterior.Rdata")		
	load(file="Saved Posteriors/CrowdAllPosterior.Rdata")
	load(file="Saved Posteriors/ExpertAllPosterior.Rdata")		
	
}	



print("Generating Summaries and Plots...")	

##################################################
##################################################
##         EXPERT SURVEY VS SEQUENTIAL          ##
##################################################
##################################################

thetabar.expert.seq.chain <- expertposterior.seq$thetabar

plot.chain <- thetabar.expert.seq.chain
pdf(file="Plots/DiagnosticsSequential.pdf",width=10,height=10)
main.labels <- c("Economics Code","Economics Left-Right","Social Code","Social Left-Right")
par(mfrow=c(2,2))
for (i2 in 1:2){
	for (i3 in 1:2){
		plot(0,0,type="n",xlim=c(0, iter.mcmc +1),ylim=c(-4,4),xlab="MCMC iteration",ylab="",main=main.labels[i3 + 2*(i2-1)])
		for (i1 in 1:dim(plot.chain)[1]){
			lines(1: iter.mcmc , plot.chain[i1,i2,i3,,1],col=fade.colour(manifesto.data$colour[i1],0.5))
		}
	}
}
dev.off()



dimnames(thetabar.expert.seq.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.expert.seq.chain)[[2]] <- levels(Y3)[2:3]
dimnames(thetabar.expert.seq.chain)[[3]] <- c("Code","Scale")

thetabar.expert.seq.est <- apply(thetabar.expert.seq.chain,c(1,2,3),mean)
thetabar.expert.seq.se <- apply(thetabar.expert.seq.chain,c(1,2,3),sd)
thetabar.expert.seq.ci <- apply(thetabar.expert.seq.chain,c(1,2,3),quantile,c(0.025,0.975))



## load survey data ##
require(foreign)
surveyData <- read.dta("../Data - External/GBexpertscores2.dta")
# duplicate 1987 as 1992 (survey from 1989)
tempSurveyData <- surveyData[surveyData$year == "1987",]
tempSurveyData$year <- 1992
surveyData <- rbind(surveyData,tempSurveyData)
survey.expert.est <- array(NA,dim(thetabar.expert.seq.est),dimnames(thetabar.expert.seq.est))
survey.expert.se <- array(NA,dim(thetabar.expert.seq.se),dimnames(thetabar.expert.seq.se))
for (l in 1:18){
	tempmatchname <- strsplit(dimnames(thetabar.expert.seq.est)[[1]][l]," ")[[1]]
	tempSurveyData <- surveyData[(as.character(surveyData$party_abbrev) == tempmatchname[1]) & (as.character(surveyData$year) == tempmatchname[2]),]
	survey.expert.est[l,1,1] <- tempSurveyData$mean[tempSurveyData$dimension == "Taxes v. Spending" & tempSurveyData$scale == "Importance"]
	survey.expert.est[l,1,2] <- tempSurveyData$mean[tempSurveyData$dimension == "Taxes v. Spending" & tempSurveyData$scale == "Position"]
	survey.expert.est[l,2,1] <- tempSurveyData$mean[tempSurveyData$dimension == "Social" & tempSurveyData$scale == "Importance"]
	survey.expert.est[l,2,2] <- tempSurveyData$mean[tempSurveyData$dimension == "Social" & tempSurveyData$scale == "Position"]
	survey.expert.se[l,1,1] <- tempSurveyData$SE[tempSurveyData$dimension == "Taxes v. Spending" & tempSurveyData$scale == "Importance"]
	survey.expert.se[l,1,2] <- tempSurveyData$SE[tempSurveyData$dimension == "Taxes v. Spending" & tempSurveyData$scale == "Position"]
	survey.expert.se[l,2,1] <- tempSurveyData$SE[tempSurveyData$dimension == "Social" & tempSurveyData$scale == "Importance"]
	survey.expert.se[l,2,2] <- tempSurveyData$SE[tempSurveyData$dimension == "Social" & tempSurveyData$scale == "Position"]
	}

cex.manifesto <- 0.5+0.5*is.element(1:18,core.manifesto)

pdf(file="Plots/SequentialVsSurveyManifestoPositionsAll.pdf",width=10,height=5)
par(mfrow=c(1,2))
plot(survey.expert.est[,1,2] ,thetabar.expert.seq.est[,1,2],main="Manifesto Placement\nEconomic",xlab="Expert Survey Placement",ylab="Expert Coding Estimate",pch=16,xlim=c(0,20),ylim=c(-2.5,2.5), col= fade.colour(manifesto.data$colour,0.5), type="n")
text(survey.expert.est[,1,2] ,thetabar.expert.seq.est[,1,2],substr(manifesto.data$year,3,4), col= fade.colour(manifesto.data$colour,0.75),cex=0.8)


y <- thetabar.expert.seq.est[,1,2]
x <- survey.expert.est[,1,2]
lmfit <- lm(y~x)
xpts <- seq(0,20,0.5)
bands <- as.data.frame(predict.lm(lmfit,newdata=data.frame(x=xpts),interval="confidence"))
polygon(x=c(xpts,rev(xpts)),y=c(bands$lwr,rev(bands$upr)),col=rgb(0,0,0,0.1),border=NA)
lines(xpts, bands$fit,col=rgb(0,0,0,0.5))

for (l in 1:18) {
	tempmeanvec <- c(survey.expert.est[l,1,2], thetabar.expert.seq.est[l,1,2])
	tempcovmat <- matrix(c(survey.expert.se[l,1,2]^2,0,0, thetabar.expert.seq.se[l,1,2]^2),2,2)
	add.ellipse.mat(tempmeanvec,tempcovmat,col= fade.colour(manifesto.data$colour[l],0.1),level=0.95)
	}
text(0,2,paste("r=",format(cor(thetabar.expert.seq.est[,1,2], survey.expert.est[,1,2]),digits=2),sep=""),pos=4,col="dark grey",cex=0.75)

plot(survey.expert.est[,2,2] ,thetabar.expert.seq.est[,2,2],main="Manifesto Placement\nSocial",xlab="Expert Survey Placement",ylab="Expert Coding Estimate",pch=16,xlim=c(0,20),ylim=c(-2.5,2.5), col= fade.colour(manifesto.data$colour,0.5), type="n")
text(survey.expert.est[,2,2] ,thetabar.expert.seq.est[,2,2],substr(manifesto.data$year,3,4), col= fade.colour(manifesto.data$colour,0.75),cex=0.8)

y <- thetabar.expert.seq.est[,2,2]
x <- survey.expert.est[,2,2]
lmfit <- lm(y~x)
xpts <- seq(0,20,0.5)
bands <- as.data.frame(predict.lm(lmfit,newdata=data.frame(x=xpts),interval="confidence"))
polygon(x=c(xpts,rev(xpts)),y=c(bands$lwr,rev(bands$upr)),col=rgb(0,0,0,0.1),border=NA)
lines(xpts, bands$fit,col=rgb(0,0,0,0.5))

for (l in 1:18) {
	tempmeanvec <- c(survey.expert.est[l,2,2], thetabar.expert.seq.est[l,2,2])
	tempcovmat <- matrix(c(survey.expert.se[l,2,2]^2,0,0, thetabar.expert.seq.se[l,2,2]^2),2,2)
	add.ellipse.mat(tempmeanvec,tempcovmat,col= fade.colour(manifesto.data$colour[l],0.1),level=0.95)
	}
text(0,2,paste("r=",format(cor(thetabar.expert.seq.est[,2,2], survey.expert.est[,2,2]),digits=2),sep=""),pos=4,col="dark grey",cex=0.75)
dev.off()










##################################################
##################################################
##         EXPERT SEQUENTIAL VS RANDOM          ##
##################################################
##################################################

thetabar.expert.seq.chain <- expertposterior.seq$thetabar

dimnames(thetabar.expert.seq.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.expert.seq.chain)[[2]] <- levels(Y3)[2:3]
dimnames(thetabar.expert.seq.chain)[[3]] <- c("Code","Scale")

thetabar.expert.seq.est <- apply(thetabar.expert.seq.chain,c(1,2,3),mean)
thetabar.expert.seq.ci <- apply(thetabar.expert.seq.chain,c(1,2,3),quantile,c(0.025,0.975))

thetabar.expert.rand.chain <- expertposterior.rand$thetabar

plot.chain <- thetabar.expert.rand.chain
pdf(file="Plots/DiagnosticsRandom.pdf",width=10,height=10)
main.labels <- c("Economics Code","Economics Left-Right","Social Code","Social Left-Right")
par(mfrow=c(2,2))
for (i2 in 1:2){
	for (i3 in 1:2){
		plot(0,0,type="n",xlim=c(0, iter.mcmc +1),ylim=c(-4,4),xlab="MCMC iteration",ylab="",main=main.labels[i3 + 2*(i2-1)])
		for (i1 in 1:dim(plot.chain)[1]){
			lines(1: iter.mcmc , plot.chain[i1,i2,i3,,1],col=fade.colour(manifesto.data$colour[i1],0.5))
		}
	}
}
dev.off()


dimnames(thetabar.expert.rand.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.expert.rand.chain)[[2]] <- levels(Y3)[2:3]
dimnames(thetabar.expert.rand.chain)[[3]] <- c("Code","Scale")

thetabar.expert.rand.est <- apply(thetabar.expert.rand.chain,c(1,2,3),mean)
thetabar.expert.rand.ci <- apply(thetabar.expert.rand.chain,c(1,2,3),quantile,c(0.025,0.975))


cex.manifesto <- 0.5+0.5*is.element(1:18,core.manifesto)

pdf(file="Plots/RandomVsSequentialManifestoPositionsAll.pdf",width=10,height=5)
par(mfrow=c(1,2))
plot(thetabar.expert.seq.est[,1,2], thetabar.expert.rand.est[,1,2],main="Manifesto Placement\nEconomic",xlab="Sequential",ylab="Random",pch=16,xlim=c(-2.5,2.5),ylim=c(-2.5,2.5), col= fade.colour(manifesto.data$colour,0.5), type="n")
text(thetabar.expert.seq.est[,1,2], thetabar.expert.rand.est[,1,2],substr(manifesto.data$year,3,4), col= fade.colour(manifesto.data$colour,0.75),cex=0.8)

x <- thetabar.expert.seq.est[,1,2]
y <- thetabar.expert.rand.est[,1,2]
lmfit <- lm(y~x)
xpts <- seq(-2.5,2.5,0.1)
bands <- as.data.frame(predict.lm(lmfit,newdata=data.frame(x=xpts),interval="confidence"))
polygon(x=c(xpts,rev(xpts)),y=c(bands$lwr,rev(bands$upr)),col=rgb(0,0,0,0.1),border=NA)
lines(xpts, bands$fit,col=rgb(0,0,0,0.5))

for (l in 1:18) add.ellipse(cbind(thetabar.expert.seq.chain[l,1,2,,1],thetabar.expert.rand.chain[l,1,2,,1]),col= fade.colour(manifesto.data$colour[l],0.1),level=0.95,independent=TRUE)
text(-2,2,paste("r=",format(cor(thetabar.expert.seq.est[,1,2], thetabar.expert.rand.est[,1,2]),digits=2),sep=""),pos=4,col="dark grey",cex=0.75)

plot(thetabar.expert.seq.est[,2,2], thetabar.expert.rand.est[,2,2],main="Manifesto Placement\nSocial",xlab="Sequential",ylab="Random",pch=16,xlim=c(-2.5,2.5),ylim=c(-2.5,2.5), col= fade.colour(manifesto.data$colour,0.5), type="n")
text(thetabar.expert.seq.est[,2,2], thetabar.expert.rand.est[,2,2],substr(manifesto.data$year,3,4), col= fade.colour(manifesto.data$colour,0.75),cex=0.8)

x <- thetabar.expert.seq.est[,2,2]
y <- thetabar.expert.rand.est[,2,2]
lmfit <- lm(y~x)
xpts <- seq(-2.5,2.5,0.1)
bands <- as.data.frame(predict.lm(lmfit,newdata=data.frame(x=xpts),interval="confidence"))
polygon(x=c(xpts,rev(xpts)),y=c(bands$lwr,rev(bands$upr)),col=rgb(0,0,0,0.1),border=NA)
lines(xpts, bands$fit,col=rgb(0,0,0,0.5))

for (l in 1:18) add.ellipse(cbind(thetabar.expert.seq.chain[l,2,2,,1],thetabar.expert.rand.chain[l,2,2,,1]),col= fade.colour(manifesto.data$colour[l],0.1),level=0.95,independent=TRUE)
text(-2,2,paste("r=",format(cor(thetabar.expert.seq.est[,2,2], thetabar.expert.rand.est[,2,2]),digits=2),sep=""),pos=4,col="dark grey",cex=0.75)
dev.off()





## Stats for Tables in Supplemental

if (FALSE){

print(round(thetabar.expert.seq.est[,,2],2))
print(round(thetabar.expert.rand.est[,,2],2))

library(abind)

round(cbind(aperm(thetabar.expert.seq.ci[,,,2],c(2,3,1))[,1,1],aperm(thetabar.expert.seq.ci[,,,2],c(2,3,1))[,1,2]),2)

round(cbind(aperm(thetabar.expert.seq.ci[,,,2],c(2,3,1))[,2,1],aperm(thetabar.expert.seq.ci[,,,2],c(2,3,1))[,2,2]),2)

round(cbind(aperm(thetabar.expert.rand.ci[,,,2],c(2,3,1))[,1,1],aperm(thetabar.expert.rand.ci[,,,2],c(2,3,1))[,1,2]),2)

round(cbind(aperm(thetabar.expert.rand.ci[,,,2],c(2,3,1))[,2,1],aperm(thetabar.expert.rand.ci[,,,2],c(2,3,1))[,2,2]),2)

cor(survey.expert.est[,1,2], thetabar.expert.seq.est[,1,2])
cor(survey.expert.est[,2,2], thetabar.expert.seq.est[,2,2])
cor(survey.expert.est[,1,2], thetabar.expert.rand.est[,1,2])
cor(survey.expert.est[,2,2], thetabar.expert.rand.est[,2,2])

# calculate mean of means

	expertdata.seq <- list(Y= Code[is.expert & is.expert.sequential],Z=Scale[is.expert & is.expert.sequential],CoderID=CoderID[is.expert & is.expert.sequential],SentenceID=SentenceID[is.expert & is.expert.sequential], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(is.expert & is.expert.sequential),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)	
	expertdata.rand <- list(Y= Code[is.expert & is.expert.random],Z=Scale[is.expert & is.expert.random],CoderID=CoderID[is.expert & is.expert.random],SentenceID=SentenceID[is.expert & is.expert.random], ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=sum(is.expert & is.expert.random),Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)
	
MoM.seq.est <- array(NA,dim=dim(thetabar.expert.seq.est),dimnames=dimnames(thetabar.expert.seq.est))
MoM.rand.est <- array(NA,dim=dim(thetabar.expert.rand.est),dimnames=dimnames(thetabar.expert.rand.est))	
for (i in 1:Nmanifestos){	
	inmanif <- is.element(expertdata.seq$SentenceID, which(ManifestoIDforSentence == i))
	MoM.seq.est[i,1,2] <- mean(expertdata.seq$Y[inmanif & expertdata.seq$Z == "Economic"],na.rm=TRUE)
	MoM.seq.est[i,2,2] <- mean(expertdata.seq$Y[inmanif & expertdata.seq$Z == "Social"],na.rm=TRUE)	
	inmanif <- is.element(expertdata.rand$SentenceID, which(ManifestoIDforSentence == i))
	MoM.rand.est[i,1,2] <- mean(expertdata.rand$Y[inmanif & expertdata.rand$Z == "Economic"],na.rm=TRUE)
	MoM.rand.est[i,2,2]	<- mean(expertdata.rand$Y[inmanif & expertdata.rand$Z == "Social"],na.rm=TRUE)	
	}
	
cor(MoM.seq.est[,1,2], thetabar.expert.seq.est[,1,2])
cor(MoM.seq.est[,2,2], thetabar.expert.seq.est[,2,2])
cor(MoM.rand.est[,1,2], thetabar.expert.rand.est[,1,2])
cor(MoM.rand.est[,2,2], thetabar.expert.rand.est[,2,2])

}

##################################################
##################################################
##             CROWD CODERS ALL DATA            ##
##################################################
##################################################


plot.chain <- crowdposterior.all$thetabar
pdf(file="Plots/DiagnosticsCrowd.pdf",width=10,height=10)
main.labels <- c("Economics Code","Economics Left-Right","Social Code","Social Left-Right")
par(mfrow=c(2,2))
for (i2 in 1:2){
	for (i3 in 1:2){
		plot(0,0,type="n",xlim=c(0, iter.mcmc +1),ylim=c(-4,4),xlab="MCMC iteration",ylab="",main=main.labels[i3 + 2*(i2-1)])
		for (i1 in 1:dim(plot.chain)[1]){
			lines(1: iter.mcmc , plot.chain[i1,i2,i3,,1],col=fade.colour(manifesto.data$colour[i1],0.5))
		}
	}
}
dev.off()


plot.chain <- expertposterior.all$thetabar
pdf(file="Plots/DiagnosticsExpert.pdf",width=10,height=10)
main.labels <- c("Economics Code","Economics Left-Right","Social Code","Social Left-Right")
par(mfrow=c(2,2))
for (i2 in 1:2){
	for (i3 in 1:2){
		plot(0,0,type="n",xlim=c(0, iter.mcmc +1),ylim=c(-4,4),xlab="MCMC iteration",ylab="",main=main.labels[i3 + 2*(i2-1)])
		for (i1 in 1:dim(plot.chain)[1]){
			lines(1: iter.mcmc , plot.chain[i1,i2,i3,,1],col=fade.colour(manifesto.data$colour[i1],0.5))
		}
	}
}
dev.off()


thetabar.chain <- crowdposterior.all$thetabar
theta.chain <- crowdposterior.all$theta
psi.chain <- crowdposterior.all$psi
chi.chain <- crowdposterior.all$chi

dimnames(thetabar.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.chain)[[2]] <- levels(Y3)[2:3]
dimnames(thetabar.chain)[[3]] <- c("Code","Scale")

dimnames(theta.chain)[[1]] <- levels(Sentence)
dimnames(theta.chain)[[2]] <- levels(Y3)[2:3]
dimnames(theta.chain)[[3]] <- c("Code","Scale")

dimnames(psi.chain)[[1]] <- levels(Coder)
dimnames(psi.chain)[[2]] <- levels(Y3)[2:3]
dimnames(psi.chain)[[3]] <- c("Code","Scale")

dimnames(chi.chain)[[1]] <- levels(Coder)
dimnames(chi.chain)[[2]] <- levels(Y3)[2:3]
dimnames(chi.chain)[[3]] <- c("Code","Scale")


thetabar.est <- apply(thetabar.chain,c(1,2,3),mean)
thetabar.ci <- apply(thetabar.chain,c(1,2,3),quantile,c(0.025,0.975))

theta.est <- apply(theta.chain,c(1,2,3),mean)
theta.ci <- apply(theta.chain,c(1,2,3),quantile,c(0.025,0.975))

psi.est <- apply(psi.chain,c(1,2,3),mean)
psi.ci <- apply(psi.chain,c(1,2,3),quantile,c(0.025,0.975))

chi.est <- apply(chi.chain,c(1,2,3),mean)
chi.ci <- apply(chi.chain,c(1,2,3),quantile,c(0.025,0.975))

## Add ellipses, colors, and trajectories



# Make plots of manifesto scores

pdf(file="Plots/ManifestoTopicsAllCrowd.pdf")
plot(thetabar.est[,,1],type="n",xlab="Economics",ylab="Social",main="Estimated Topic Weight in Manifesto",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
for (l in 1:18) add.ellipse(t(thetabar.chain[l,,1,,1]),col= fade.colour(manifesto.data$colour[l],0.1),level=0.95)
for (p in 1:3){
	obs <- which(manifesto.data$party == manifesto.data$party[p])
	obs <- obs[order(manifesto.data$year[obs])]
	lines(thetabar.est[obs,,1],col=fade.colour(manifesto.data$colour[p],0.2))
	}
text(thetabar.est[,1,1], thetabar.est[,2,1],rownames(thetabar.est)[],col=manifesto.data$colour,cex=0.75)
dev.off()

pdf(file="Plots/ManifestoPositionsAllCrowd.pdf")
plot(thetabar.est[,,2],type="n",xlab="Economics",ylab="Social",main="Estimated Left-Right Positions",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
for (l in 1:18) add.ellipse(t(thetabar.chain[l,,2,,1]),col=fade.colour(manifesto.data$colour[l],0.1),level=0.95)
for (p in 1:3){
	obs <- which(manifesto.data$party == manifesto.data$party[p])
	obs <- obs[order(manifesto.data$year[obs])]
	lines(thetabar.est[obs,,2],col=fade.colour(manifesto.data$colour[p],0.2))
	}
text(thetabar.est[,1,2], thetabar.est[,2,2],rownames(thetabar.est)[],col=manifesto.data$colour,cex=0.75)
dev.off()

## Comparisons to expert survey codings

library(foreign)
d <- read.dta("../Data - External/GBexpertscores.dta")
con_econ <- d$mean[c(1,4,5,8,9,12)]
con_soc <- d$mean[c(2,3,6,7,10,11)]
lab_econ <- d$mean[c(13,16,18,20,21,24)]
lab_soc <- d$mean[c(14,15,17,19,22,23)]
ld_econ <- d$mean[c(25,27,29,31,33,35)]
ld_soc <- d$mean[c(26,28,30,32,34,36)]

pdf(file="Plots/EconSocialPositionsCrowdVsExperts.pdf",12,6)
par(mfrow=c(1,2))
plot(0,0,pch=16,xlab="Expert Survey Estimates",ylab="Crowd Estimates",main="Economic",axes=TRUE,xlim=c(0,20),ylim=c(-3,3),type="n")

obs <- which(manifesto.data$party == "Con")
obs <- obs[order(manifesto.data$year[obs])]
text(con_econ,thetabar.est[obs,1,2],paste("Con",manifesto.years),col= all.party.colours[1,1],cex=0.75)
obs <- which(manifesto.data$party == "LD")
obs <- obs[order(manifesto.data$year[obs])]
text(ld_econ,thetabar.est[obs,1,2],paste("LD",manifesto.years),col=all.party.colours[2,1],cex=0.75)
obs <- which(manifesto.data$party == "Lab")
obs <- obs[order(manifesto.data$year[obs])]
text(lab_econ,thetabar.est[obs,1,2],paste("Lab",manifesto.years),col=all.party.colours[3,1],cex=0.75)

plot(0,0,pch=16,xlab="Expert Survey Estimates",ylab="Crowd Estimates",main="Social",axes=TRUE,xlim=c(0,20),ylim=c(-3,3),type="n")

obs <- which(manifesto.data$party == "Con")
obs <- obs[order(manifesto.data$year[obs])]
text(con_soc,thetabar.est[obs,2,2],paste("Con",manifesto.years),col= all.party.colours[1,1],cex=0.75)
obs <- which(manifesto.data$party == "LD")
obs <- obs[order(manifesto.data$year[obs])]
text(ld_soc,thetabar.est[obs,2,2],paste("LD",manifesto.years),col=all.party.colours[2,1],cex=0.75)
obs <- which(manifesto.data$party == "Lab")
obs <- obs[order(manifesto.data$year[obs])]
text(lab_soc,thetabar.est[obs,2,2],paste("Lab",manifesto.years),col=all.party.colours[3,1],cex=0.75)

dev.off()


#for (i in 1:9) {
#	lines(c(d$meanexperts[i],d$meanexperts[i]),thetabar.ci[,i,2],col="grey",lwd=2)
#	lines(c(d$exp95cilow[i],d$exp95cihi[i]),rep(thetabar.est[i,2],2),col="grey",lwd=2)
#	}
#text(d$meanexperts,thetabar.est[,2], ManifestoPlotNames)	
#text(0,2.9,paste("r=",round(cor(d$meanexperts,thetabar.est[,2],use="pairwise.complete.obs"),2),sep=""),pos=4)
#abline(lm(thetabar.est[,2]~d$meanexperts))

#dev.off()







		

##################################################
##################################################
##             JOINT MODEL CORE DATA            ##
##################################################
##################################################

thetabar.chain <- jointposterior.core$thetabar
theta.chain <- jointposterior.core$theta
psi.chain <- jointposterior.core$psi
chi.chain <- jointposterior.core$chi

dimnames(thetabar.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.chain)[[2]] <- levels(Y3)[2:3]
dimnames(thetabar.chain)[[3]] <- c("Code","Scale")

dimnames(theta.chain)[[1]] <- levels(Sentence)
dimnames(theta.chain)[[2]] <- levels(Y3)[2:3]
dimnames(theta.chain)[[3]] <- c("Code","Scale")

dimnames(psi.chain)[[1]] <- levels(Coder)
dimnames(psi.chain)[[2]] <- levels(Y3)[2:3]
dimnames(psi.chain)[[3]] <- c("Code","Scale")

dimnames(chi.chain)[[1]] <- levels(Coder)
dimnames(chi.chain)[[2]] <- levels(Y3)[2:3]
dimnames(chi.chain)[[3]] <- c("Code","Scale")


thetabar.est <- apply(thetabar.chain,c(1,2,3),mean)
thetabar.ci <- apply(thetabar.chain,c(1,2,3),quantile,c(0.025,0.975))

theta.est <- apply(theta.chain,c(1,2,3),mean)
theta.ci <- apply(theta.chain,c(1,2,3),quantile,c(0.025,0.975))

psi.est <- apply(psi.chain,c(1,2,3),mean)
psi.ci <- apply(psi.chain,c(1,2,3),quantile,c(0.025,0.975))

chi.est <- apply(chi.chain,c(1,2,3),mean)
chi.ci <- apply(chi.chain,c(1,2,3),quantile,c(0.025,0.975))

# Make plots of manifesto scores

pdf(file="Plots/ManifestoTopics.pdf")
plot(thetabar.est[core.manifesto,,1],type="n",xlab="Economics",ylab="Social",main="Estimated Topic Weight in Manifesto",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
text(thetabar.est[core.manifesto,1,1], thetabar.est[core.manifesto,2,1],rownames(thetabar.est)[core.manifesto],cex=0.75)
dev.off()

pdf(file="Plots/ManifestoPositions.pdf")
plot(thetabar.est[core.manifesto,,2],type="n",xlab="Economics",ylab="Social",main="Estimated Left-Right Positions",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
text(thetabar.est[core.manifesto,1,2], thetabar.est[core.manifesto,2,2],rownames(thetabar.est)[core.manifesto],cex=0.75)
dev.off()

# Make plots of coder biases

pdf(file="Plots/CoderOffsetTopics.pdf")
plot(psi.est[,,1],type="n",xlab="Relative Tendency to Label as Economic",ylab="Relative Tendency to Label as Social",main="Coder Offsets on Topic Assignment")
points(psi.est[,,1],pch=16,col=rgb(0,0,0,0.25),cex=0.75)
text(psi.est[which.experts,1,1], psi.est[which.experts,2,1],rownames(psi.est[which.experts,,1]),cex=1)
dev.off()

pdf(file="Plots/CoderOffsetPositions.pdf")
plot(psi.est[,,2],type="n",xlab="Relative Tendency to Label as Right on Economic",ylab="Relative Tendency to Label as Right on Social",main="Coder Offsets on Left-Right Assignment")
points(psi.est[,,2],pch=16,col=rgb(0,0,0,0.25),cex=0.75)
text(psi.est[which.experts,1,2], psi.est[which.experts,2,2],rownames(psi.est[which.experts,,2]),cex=1)
dev.off()

# Make plots of coder sensitivites

pdf(file="Plots/CoderSensitivityTopics.pdf")
plot(chi.est[,,1],type="n",xlab="Relative Sensitivity to Presence of Economic Content",ylab="Relative Sensitivity to Presence of Social Content",main="Coder Sensitivity on Topic Assignment")
points(chi.est[,,1],pch=16,col=rgb(0,0,0,0.25),cex=0.75)
text(chi.est[which.experts,1,1], chi.est[which.experts,2,1],rownames(chi.est[which.experts,,1]),cex=1)
dev.off()

pdf(file="Plots/CoderSensitivityPositions.pdf")
plot(chi.est[,,2],type="n",xlab="Relative Sensitivity to Economic Position",ylab="Relative Sensitivity to Social Position",main="Coder Sensitivity on Left-Right Assignment")
points(chi.est[,,2],pch=16,col=rgb(0,0,0,0.25),cex=0.75)
text(chi.est[which.experts,1,2], chi.est[which.experts,2,2],rownames(chi.est[which.experts,,2]),cex=1)
dev.off()




##################################################
##################################################
##             EXPERT VS CROWD ALL              ##
##################################################
##################################################

thetabar.expert.chain <- expertposterior.all$thetabar
theta.expert.chain <- expertposterior.all$theta
psi.expert.chain <- expertposterior.all$psi
chi.expert.chain <- expertposterior.all$chi

dimnames(thetabar.expert.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.expert.chain)[[2]] <- levels(Y3)[2:3]
dimnames(thetabar.expert.chain)[[3]] <- c("Code","Scale")

dimnames(theta.expert.chain)[[1]] <- levels(Sentence)
dimnames(theta.expert.chain)[[2]] <- levels(Y3)[2:3]
dimnames(theta.expert.chain)[[3]] <- c("Code","Scale")

dimnames(psi.expert.chain)[[1]] <- levels(Coder)
dimnames(psi.expert.chain)[[2]] <- levels(Y3)[2:3]
dimnames(psi.expert.chain)[[3]] <- c("Code","Scale")

dimnames(chi.expert.chain)[[1]] <- levels(Coder)
dimnames(chi.expert.chain)[[2]] <- levels(Y3)[2:3]
dimnames(chi.expert.chain)[[3]] <- c("Code","Scale")

thetabar.expert.est <- apply(thetabar.expert.chain,c(1,2,3),mean)
thetabar.expert.ci <- apply(thetabar.expert.chain,c(1,2,3),quantile,c(0.025,0.975))

theta.expert.est <- apply(theta.expert.chain,c(1,2,3),mean)
theta.expert.ci <- apply(theta.expert.chain,c(1,2,3),quantile,c(0.025,0.975))

psi.expert.est <- apply(psi.expert.chain,c(1,2,3),mean)
psi.expert.ci <- apply(psi.expert.chain,c(1,2,3),quantile,c(0.025,0.975))

chi.expert.est <- apply(chi.expert.chain,c(1,2,3),mean)
chi.expert.ci <- apply(chi.expert.chain,c(1,2,3),quantile,c(0.025,0.975))

thetabar.crowd.chain <- crowdposterior.all$thetabar
theta.crowd.chain <- crowdposterior.all$theta
psi.crowd.chain <- crowdposterior.all$psi
chi.crowd.chain <- crowdposterior.all$chi

dimnames(thetabar.crowd.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.crowd.chain)[[2]] <- levels(Y3)[2:3]
dimnames(thetabar.crowd.chain)[[3]] <- c("Code","Scale")

dimnames(theta.crowd.chain)[[1]] <- levels(Sentence)
dimnames(theta.crowd.chain)[[2]] <- levels(Y3)[2:3]
dimnames(theta.crowd.chain)[[3]] <- c("Code","Scale")

dimnames(psi.crowd.chain)[[1]] <- levels(Coder)
dimnames(psi.crowd.chain)[[2]] <- levels(Y3)[2:3]
dimnames(psi.crowd.chain)[[3]] <- c("Code","Scale")

dimnames(chi.crowd.chain)[[1]] <- levels(Coder)
dimnames(chi.crowd.chain)[[2]] <- levels(Y3)[2:3]
dimnames(chi.crowd.chain)[[3]] <- c("Code","Scale")

thetabar.crowd.est <- apply(thetabar.crowd.chain,c(1,2,3),mean)
thetabar.crowd.ci <- apply(thetabar.crowd.chain,c(1,2,3),quantile,c(0.025,0.975))

theta.crowd.est <- apply(theta.crowd.chain,c(1,2,3),mean)
theta.crowd.ci <- apply(theta.crowd.chain,c(1,2,3),quantile,c(0.025,0.975))

psi.crowd.est <- apply(psi.crowd.chain,c(1,2,3),mean)
psi.crowd.ci <- apply(psi.crowd.chain,c(1,2,3),quantile,c(0.025,0.975))

chi.crowd.est <- apply(chi.crowd.chain,c(1,2,3),mean)
chi.crowd.ci <- apply(chi.crowd.chain,c(1,2,3),quantile,c(0.025,0.975))


# pdf(file="Plots/CrowdVsExpertManifestoPositionsAll.pdf",width=10,height=5)
# par(mfrow=c(1,2))
# plot(thetabar.expert.est[,1,2],thetabar.crowd.est[,1,2],main="Manifesto Placement\nEconomic",xlab="Expert",ylab="Crowd",type="n",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
# text(thetabar.expert.est[,1,2],thetabar.crowd.est[,1,2],names(thetabar.crowd.est[,1,2]),cex=0.75)
# text(-2,2,paste("r=",format(cor(thetabar.expert.est[,1,2],thetabar.crowd.est[,1,2]),digits=2),sep=""),pos=4,col="dark grey",cex=0.75)

# plot(thetabar.expert.est[,2,2],thetabar.crowd.est[,2,2],main="Manifesto Placement\nSocial",xlab="Expert",ylab="Crowd",type="n",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
# text(thetabar.expert.est[,2,2],thetabar.crowd.est[,2,2],names(thetabar.crowd.est[,1,2]),cex=0.75)
# text(-2,2,paste("r=",format(cor(thetabar.expert.est[,2,2],thetabar.crowd.est[,2,2]),digits=2),sep=""),pos=4,col="dark grey",cex=0.75)
# dev.off()



pdf(file="Plots/CrowdVsExpertManifestoPositionsAll.pdf",width=10,height=5)
par(mfrow=c(1,2))
plot(thetabar.expert.est[,1,2], thetabar.crowd.est[,1,2],main="Manifesto Placement\nEconomic",xlab="Expert Coding Estimate",ylab="Crowd Coding Estimate",pch=16,xlim=c(-2.5,2.5),ylim=c(-2.5,2.5), col= fade.colour(manifesto.data$colour,0.5), type="n")
text(thetabar.expert.est[,1,2], thetabar.crowd.est[,1,2],substr(manifesto.data$year,3,4), col= fade.colour(manifesto.data$colour,0.75),cex=0.8)

x <- thetabar.expert.est[,1,2]
y <- thetabar.crowd.est[,1,2]
lmfit <- lm(y~x)
xpts <- seq(-2.5,2.5,0.1)
bands <- as.data.frame(predict.lm(lmfit,newdata=data.frame(x=xpts),interval="confidence"))
polygon(x=c(xpts,rev(xpts)),y=c(bands$lwr,rev(bands$upr)),col=rgb(0,0,0,0.1),border=NA)
lines(xpts, bands$fit,col=rgb(0,0,0,0.5))

for (l in 1:18) add.ellipse(cbind(thetabar.expert.chain[l,1,2,,1], thetabar.crowd.chain[l,1,2,,1]),col= fade.colour(manifesto.data$colour[l],0.1),level=0.95,independent=TRUE)
text(-2,2,paste("r=",format(cor(thetabar.expert.est[,1,2], thetabar.crowd.est[,1,2]),digits=2),sep=""),pos=4,col="dark grey",cex=0.75)

plot(thetabar.expert.est[,2,2], thetabar.crowd.est[,2,2],main="Manifesto Placement\nSocial",xlab="Expert Coding Estimate",ylab="Crowd Coding Estimate",pch=16,xlim=c(-2.5,2.5),ylim=c(-2.5,2.5), col= fade.colour(manifesto.data$colour,0.5), type="n")
text(thetabar.expert.est[,2,2], thetabar.crowd.est[,2,2],substr(manifesto.data$year,3,4), col= fade.colour(manifesto.data$colour,0.75),cex=0.8)

x <- thetabar.expert.est[,2,2]
y <- thetabar.crowd.est[,2,2]
lmfit <- lm(y~x)
xpts <- seq(-2.5,2.5,0.1)
bands <- as.data.frame(predict.lm(lmfit,newdata=data.frame(x=xpts),interval="confidence"))
polygon(x=c(xpts,rev(xpts)),y=c(bands$lwr,rev(bands$upr)),col=rgb(0,0,0,0.1),border=NA)
lines(xpts, bands$fit,col=rgb(0,0,0,0.5))

for (l in 1:18) add.ellipse(cbind(thetabar.expert.chain[l,2,2,,1], thetabar.crowd.chain[l,2,2,,1]),col= fade.colour(manifesto.data$colour[l],0.1),level=0.95,independent=TRUE)
text(-2,2,paste("r=",format(cor(thetabar.expert.est[,2,2], thetabar.crowd.est[,2,2]),digits=2),sep=""),pos=4,col="dark grey",cex=0.75)
dev.off()



save(thetabar.expert.est,thetabar.crowd.est,thetabar.expert.rand.est,thetabar.expert.seq.est,thetabar.expert.ci,thetabar.crowd.ci,thetabar.expert.rand.ci,thetabar.expert.seq.ci,file="Estimates/SavedPartyEstimates.Rdata")




##################################################
##################################################
##            EXPERT VS CROWD CORE              ##
##################################################
##################################################

thetabar.expert.chain <- expertposterior.core$thetabar
theta.expert.chain <- expertposterior.core$theta
psi.expert.chain <- expertposterior.core$psi
chi.expert.chain <- expertposterior.core$chi

dimnames(thetabar.expert.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.expert.chain)[[2]] <- levels(Y3)[2:3]
dimnames(thetabar.expert.chain)[[3]] <- c("Code","Scale")

dimnames(theta.expert.chain)[[1]] <- levels(Sentence)
dimnames(theta.expert.chain)[[2]] <- levels(Y3)[2:3]
dimnames(theta.expert.chain)[[3]] <- c("Code","Scale")

dimnames(psi.expert.chain)[[1]] <- levels(Coder)
dimnames(psi.expert.chain)[[2]] <- levels(Y3)[2:3]
dimnames(psi.expert.chain)[[3]] <- c("Code","Scale")

dimnames(chi.expert.chain)[[1]] <- levels(Coder)
dimnames(chi.expert.chain)[[2]] <- levels(Y3)[2:3]
dimnames(chi.expert.chain)[[3]] <- c("Code","Scale")

thetabar.expert.est <- apply(thetabar.expert.chain,c(1,2,3),mean)
thetabar.expert.ci <- apply(thetabar.expert.chain,c(1,2,3),quantile,c(0.025,0.975))

theta.expert.est <- apply(theta.expert.chain,c(1,2,3),mean)
theta.expert.ci <- apply(theta.expert.chain,c(1,2,3),quantile,c(0.025,0.975))

psi.expert.est <- apply(psi.expert.chain,c(1,2,3),mean)
psi.expert.ci <- apply(psi.expert.chain,c(1,2,3),quantile,c(0.025,0.975))

chi.expert.est <- apply(chi.expert.chain,c(1,2,3),mean)
chi.expert.ci <- apply(chi.expert.chain,c(1,2,3),quantile,c(0.025,0.975))

thetabar.crowd.chain <- crowdposterior.core$thetabar
theta.crowd.chain <- crowdposterior.core$theta
psi.crowd.chain <- crowdposterior.core$psi
chi.crowd.chain <- crowdposterior.core$chi

dimnames(thetabar.crowd.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.crowd.chain)[[2]] <- levels(Y3)[2:3]
dimnames(thetabar.crowd.chain)[[3]] <- c("Code","Scale")

dimnames(theta.crowd.chain)[[1]] <- levels(Sentence)
dimnames(theta.crowd.chain)[[2]] <- levels(Y3)[2:3]
dimnames(theta.crowd.chain)[[3]] <- c("Code","Scale")

dimnames(psi.crowd.chain)[[1]] <- levels(Coder)
dimnames(psi.crowd.chain)[[2]] <- levels(Y3)[2:3]
dimnames(psi.crowd.chain)[[3]] <- c("Code","Scale")

dimnames(chi.crowd.chain)[[1]] <- levels(Coder)
dimnames(chi.crowd.chain)[[2]] <- levels(Y3)[2:3]
dimnames(chi.crowd.chain)[[3]] <- c("Code","Scale")

thetabar.crowd.est <- apply(thetabar.crowd.chain,c(1,2,3),mean)
thetabar.crowd.ci <- apply(thetabar.crowd.chain,c(1,2,3),quantile,c(0.025,0.975))

theta.crowd.est <- apply(theta.crowd.chain,c(1,2,3),mean)
theta.crowd.ci <- apply(theta.crowd.chain,c(1,2,3),quantile,c(0.025,0.975))

psi.crowd.est <- apply(psi.crowd.chain,c(1,2,3),mean)
psi.crowd.ci <- apply(psi.crowd.chain,c(1,2,3),quantile,c(0.025,0.975))

chi.crowd.est <- apply(chi.crowd.chain,c(1,2,3),mean)
chi.crowd.ci <- apply(chi.crowd.chain,c(1,2,3),quantile,c(0.025,0.975))


pdf(file="Plots/CrowdVsExpertManifestoPositionsCore.pdf",width=10,height=5)
par(mfrow=c(1,2))
plot(thetabar.expert.est[core.manifesto,1,2],thetabar.crowd.est[core.manifesto,1,2],main="Manifesto Placement\nEconomic",xlab="Expert",ylab="Crowd",type="n",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
text(thetabar.expert.est[core.manifesto,1,2],thetabar.crowd.est[core.manifesto,1,2],names(thetabar.crowd.est[core.manifesto,1,2]),cex=0.75)
text(-2,2,paste("r=",format(cor(thetabar.expert.est[core.manifesto,1,2],thetabar.crowd.est[core.manifesto,1,2]),digits=2),sep=""),pos=4,col="dark grey",cex=0.75)

plot(thetabar.expert.est[core.manifesto,2,2],thetabar.crowd.est[core.manifesto,2,2],main="Manifesto Placement\nSocial",xlab="Expert",ylab="Crowd",type="n",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
text(thetabar.expert.est[core.manifesto,2,2],thetabar.crowd.est[core.manifesto,2,2],names(thetabar.crowd.est[core.manifesto,1,2]),cex=0.75)
text(-2,2,paste("r=",format(cor(thetabar.expert.est[core.manifesto,2,2],thetabar.crowd.est[core.manifesto,2,2]),digits=2),sep=""),pos=4,col="dark grey",cex=0.75)
dev.off()








##################################################
##################################################
##                CROWD SUBSETS                 ##
##################################################
##################################################

# Get list of all saved subset simulations:

subset.sims <- list.files(path="Crowd Coder Subset Posteriors")

# Create object for saving correlations between expert codings
# and crowd codings using each subset of codings

crowd.subset.correlations <- matrix(NA,length(subset.sims),9)
colnames(crowd.subset.correlations) <- c("Coding Ratio","Code:Economic;Expert","Code:Social;Expert","Scale:Economic;Expert","Scale:Social;Expert","Code:Economic;Crowd","Code:Social;Crowd","Scale:Economic;Crowd","Scale:Social;Crowd")

for (sim in 1:length(subset.sims)){
	
	load(paste("Crowd Coder Subset Posteriors/",subset.sims[sim],sep=""))
	
	crowd.subset.correlations[sim,1] <- CodingRatio
	
	thetabar.sub.est <- apply(crowdposterior.sim$thetabar,c(1,2,3),mean)

	crowd.subset.correlations[sim,2] <- cor(thetabar.expert.est[core.manifesto,1,1], thetabar.sub.est[core.manifesto,1,1])
	crowd.subset.correlations[sim,3] <- cor(thetabar.expert.est[core.manifesto,2,1], thetabar.sub.est[core.manifesto,2,1])
	crowd.subset.correlations[sim,4] <- cor(thetabar.expert.est[core.manifesto,1,2], thetabar.sub.est[core.manifesto,1,2])
	crowd.subset.correlations[sim,5] <- cor(thetabar.expert.est[core.manifesto,2,2], thetabar.sub.est[core.manifesto,2,2])	
	
	crowd.subset.correlations[sim,6] <- cor(thetabar.crowd.est[core.manifesto,1,1], thetabar.sub.est[core.manifesto,1,1])
	crowd.subset.correlations[sim,7] <- cor(thetabar.crowd.est[core.manifesto,2,1], thetabar.sub.est[core.manifesto,2,1])
	crowd.subset.correlations[sim,8] <- cor(thetabar.crowd.est[core.manifesto,1,2], thetabar.sub.est[core.manifesto,1,2])
	crowd.subset.correlations[sim,9] <- cor(thetabar.crowd.est[core.manifesto,2,2], thetabar.sub.est[core.manifesto,2,2])	
	

}

# Plot scale correlations against coding ratios (average codings per sentence)

magic.number <- 5

pdf(file="Plots/TopicCorrelationsCrowdSubsets.pdf",width=10,height=10)
par(mfrow=c(2,2))

plot(crowd.subset.correlations[,1],crowd.subset.correlations[,2],main="Economic\nVersus Expert Consensus",xlab="Codings per Sentence",ylab="Topic Correlation with Consensus Expert Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(crowd.subset.correlations[,1],crowd.subset.correlations[,3],main="Social\nVersus Expert Consensus",xlab="Codings per Sentence",ylab="Topic Correlation with Consensus Expert Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(crowd.subset.correlations[,1],crowd.subset.correlations[,6],main="Economic\nVersus Crowd Consensus",xlab="Codings per Sentence",ylab="Topic Correlation with Consensus Crowd Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(crowd.subset.correlations[,1],crowd.subset.correlations[,7],main="Social\nVersus Crowd Consensus",xlab="Codings per Sentence",ylab="Topic Correlation with Consensus Crowd Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

dev.off()

pdf(file="Plots/PositionCorrelationsCrowdSubsets.pdf",width=10,height=10)
par(mfrow=c(2,2))

plot(crowd.subset.correlations[,1],crowd.subset.correlations[,4],main="Economic\nVersus Expert Consensus",xlab="Codings per Sentence",ylab="Position Correlation with Consensus Expert Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(crowd.subset.correlations[,1],crowd.subset.correlations[,5],main="Social\nVersus Expert Consensus",xlab="Codings per Sentence",ylab="Position Correlation with Consensus Expert Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(crowd.subset.correlations[,1],crowd.subset.correlations[,8],main="Economic\nVersus Crowd Consensus",xlab="Codings per Sentence",ylab="Position Correlation with Consensus Crowd Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(crowd.subset.correlations[,1],crowd.subset.correlations[,9],main="Social\nVersus Crowd Consensus",xlab="Codings per Sentence",ylab="Position Correlation with Consensus Crowd Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

dev.off()








##################################################
##################################################
##                EXPERT SUBSETS                ##
##################################################
##################################################

# Get list of all saved subset simulations:

subset.sims <- list.files(path="Expert Coder Subset Posteriors")

# Create object for saving correlations between expert codings
# and crowd codings using each subset of codings

expert.subset.correlations <- matrix(NA,length(subset.sims),9)
colnames(expert.subset.correlations) <- c("Coding Ratio","Code:Economic;Expert","Code:Social;Expert","Scale:Economic;Expert","Scale:Social;Expert","Code:Economic;Crowd","Code:Social;Crowd","Scale:Economic;Crowd","Scale:Social;Crowd")


for (sim in 1:length(subset.sims)){
	
	load(paste("Expert Coder Subset Posteriors/",subset.sims[sim],sep=""))
	
	expert.subset.correlations[sim,1] <- CodingRatio
	
	thetabar.sub.est <- apply(expertposterior.sim$thetabar,c(1,2,3),mean)

	expert.subset.correlations[sim,2] <- cor(thetabar.expert.est[core.manifesto,1,1], thetabar.sub.est[core.manifesto,1,1])
	expert.subset.correlations[sim,3] <- cor(thetabar.expert.est[core.manifesto,2,1], thetabar.sub.est[core.manifesto,2,1])
	expert.subset.correlations[sim,4] <- cor(thetabar.expert.est[core.manifesto,1,2], thetabar.sub.est[core.manifesto,1,2])
	expert.subset.correlations[sim,5] <- cor(thetabar.expert.est[core.manifesto,2,2], thetabar.sub.est[core.manifesto,2,2])	
	
	expert.subset.correlations[sim,6] <- cor(thetabar.crowd.est[core.manifesto,1,1], thetabar.sub.est[core.manifesto,1,1])
	expert.subset.correlations[sim,7] <- cor(thetabar.crowd.est[core.manifesto,2,1], thetabar.sub.est[core.manifesto,2,1])
	expert.subset.correlations[sim,8] <- cor(thetabar.crowd.est[core.manifesto,1,2], thetabar.sub.est[core.manifesto,1,2])
	expert.subset.correlations[sim,9] <- cor(thetabar.crowd.est[core.manifesto,2,2], thetabar.sub.est[core.manifesto,2,2])	
	

}

# Plot scale correlations against coding ratios (average codings per sentence)

magic.number <- 5

pdf(file="Plots/TopicCorrelationsExpertSubsets.pdf",width=10,height=10)
par(mfrow=c(2,2))

plot(expert.subset.correlations[,1], expert.subset.correlations[,2],main="Economic\nVersus Expert Consensus",xlab="Codings per Sentence",ylab="Topic Correlation with Consensus Expert Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(expert.subset.correlations[,1], expert.subset.correlations[,3],main="Social\nVersus Expert Consensus",xlab="Codings per Sentence",ylab="Topic Correlation with Consensus Expert Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(expert.subset.correlations[,1], expert.subset.correlations[,6],main="Economic\nVersus Crowd Consensus",xlab="Codings per Sentence",ylab="Topic Correlation with Consensus Crowd Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(expert.subset.correlations[,1], expert.subset.correlations[,7],main="Social\nVersus Crowd Consensus",xlab="Codings per Sentence",ylab="Topic Correlation with Consensus Crowd Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

dev.off()

pdf(file="Plots/PositionCorrelationsExpertSubsets.pdf",width=10,height=10)
par(mfrow=c(2,2))

plot(expert.subset.correlations[,1], expert.subset.correlations[,4],main="Economic\nVersus Expert Consensus",xlab="Codings per Sentence",ylab="Position Correlation with Consensus Expert Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(expert.subset.correlations[,1], expert.subset.correlations[,5],main="Social\nVersus Expert Consensus",xlab="Codings per Sentence",ylab="Position Correlation with Consensus Expert Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(expert.subset.correlations[,1], expert.subset.correlations[,8],main="Economic\nVersus Crowd Consensus",xlab="Codings per Sentence",ylab="Position Correlation with Consensus Crowd Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

plot(expert.subset.correlations[,1], expert.subset.correlations[,9],main="Social\nVersus Crowd Consensus",xlab="Codings per Sentence",ylab="Position Correlation with Consensus Crowd Coding",xlim=c(0,25),ylim=c(0,1),pch=16,col=rgb(0,0,0,0.25))

abline(v= magic.number,lty=2)

dev.off()
