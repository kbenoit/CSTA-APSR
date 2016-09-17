
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

iter.mcmc <- 2000
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

#Data <- read.csv("../../Crowdflower/Immigration/f354277_immigration1.csv")
#Data <- read.csv("../../Crowdflower/Immigration/f389381_immigration2.csv")
Data <- read.csv("../../Crowdflower/Immigration/merged_immigration.csv")


is.screener <- Data$manifestoid == "screener"
Data <- subset(Data,!is.screener)

##################################################
##                 SET UP DATA                  ##
##################################################

Ncodings <- dim(Data)[1]
Ncoders <- length(unique(Data$X_worker_id))
Nsentences <- length(unique(Data$sentenceid))
Nmanifestos <- length(unique(Data$manifestoid))

Scale <- as.numeric(Data$immigr_scale + 2)
Code <- as.numeric(Data$policy_area == 4)

Y4 <- Code + replace(Scale-1, is.na(Scale), 0) + 1
print(table(Code, Y4))

Coder <- factor(Data$X_worker_id)
Sentence <- factor(Data$sentenceid)
Manifesto <- factor(Data$manifestoid)


CoderID <- as.numeric(Coder)
SentenceID <- as.numeric(Sentence)
ManifestoID <- as.numeric(Manifesto)

ManifestoIDforSentence <- ManifestoID[which(duplicated(SentenceID) == FALSE)]


##################################################
##              DEFINE JAGS MODEL               ##
##################################################

jagscode <- '
model {

  	for (q in 1:Ncodings){
  		
  		# Define latent response for code/scale in econ/social 
  		
  		mucode[q] <- (theta[SentenceID[q],1] + psi[CoderID[q],1])*chi[CoderID[q],1];
  		muscale[q] <- (theta[SentenceID[q],2] + psi[CoderID[q],2])*chi[CoderID[q],2]; 
  		
  		# Translate latent responses into 11 category probabilities (up to normalization)	
  		
  		mu[q,1] <- 1;
  		mu[q,2] <- exp(mucode[q])*(ilogit(-1*cut[1] - muscale[q]));
  		mu[q,3] <- exp(mucode[q])*(ilogit(1*cut[1] - muscale[q])-ilogit(-1*cut[1] - muscale[q]));
  		mu[q,4] <- exp(mucode[q])*(1-ilogit(1*cut[1] - muscale[q]));

  		# 11 category multinomial
  		
  		Y[q] ~ dcat(mu[q,1:4]);
  		
  	}	
  	
  	# Specify uniform priors for ordinal threshholds (assumes left-right symmetry)
  	
  	cut[1] ~ dunif(0,10);
  	
  	# Priors for coder bias parameters
  	
  	for (i in 1:Ncoders) {
  		psi[i,1] ~ dnorm(0,taupsi[1]);
  		psi[i,2] ~ dnorm(0,taupsi[2]);		
  	}	
  	
  	# Priors for coder sensitivity parameters
  	
  	for (i in 1:Ncoders) {
  		chi[i,1] ~ dnorm(0,1)T(0,);
  		chi[i,2] ~ dnorm(0,1)T(0,);		
  	}	  	  	
  	
  	# Priors for sentence latent parameters 	
  	
  	for (j in 1:Nsentences) {
  		theta[j,1] ~ dnorm(thetabar[ManifestoIDforSentence[j],1],tautheta[1]);
  		theta[j,2] ~ dnorm(thetabar[ManifestoIDforSentence[j],2],tautheta[2]);		
  	}	
  	
  	# Priors for manifesto latent parameters
  	
  	for (k in 1:Nmanifestos) {
  		thetabar[k,1] ~ dnorm(0,1);
  		thetabar[k,2] ~ dnorm(0,1);	
  	}
  	
  	# Variance parameters
  	
  	taupsi[1] ~ dgamma(1,1);
   	taupsi[2] ~ dgamma(1,1);

  	tautheta[1] ~ dgamma(1,1);
  	tautheta[2] ~ dgamma(1,1);			   	 	

}
'


##################################################
##             RUN FULL DATA MODEL              ##
##################################################

if (new.mcmc.run){
	
	data.all <- list(Y=as.numeric(Y4),CoderID=CoderID,SentenceID=SentenceID, ManifestoIDforSentence = ManifestoIDforSentence,Ncodings=Ncodings,Ncoders=Ncoders,Nsentences=Nsentences,Nmanifestos=Nmanifestos)
		
	print("Running MCMC for Immigration Coding...")
	
	model.jags <- jags.model(file=textConnection(jagscode), data=data.all, n.adapt=iter.burn)		
	update(model.jags, iter.burn)	
	immigrationposterior.all <- 
	  jags.samples(model.jags, c("chi","psi","theta","thetabar","cut"), 
	               n.iter=iter.mcmc, thin = 1, progress.bar="text")		
	save(immigrationposterior.all, file="Saved Posteriors/ImmigrationPosterior.Rdata")
	rm(model.jags)
	gc()

} else {
	
print("Loading Saved Posteriors...")	

	load(file="Saved Posteriors/ImmigrationPosterior.Rdata")
	load(file="Saved Posteriors/ImmigrationPosteriorS1.Rdata")
	load(file="Saved Posteriors/ImmigrationPosteriorS2.Rdata")		
	
}	



print("Generating Summaries and Plots...")			

##################################################
##################################################
##              COMPARE ESTIMATES               ##
##################################################
##################################################

thetabar.all.chain <- immigrationposterior.all$thetabar
thetabar.s1.chain <- immigrationposterior.s1$thetabar
thetabar.s2.chain <- immigrationposterior.s2$thetabar

dimnames(thetabar.all.chain)[[1]] <- dimnames(thetabar.s1.chain)[[1]] <- dimnames(thetabar.s2.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.all.chain)[[2]] <- dimnames(thetabar.s1.chain)[[2]] <- dimnames(thetabar.s2.chain)[[2]] <-  c("Code","Scale")

thetabar.all.est <- apply(thetabar.all.chain,c(1,2),mean)
thetabar.s1.est <- apply(thetabar.s1.chain,c(1,2),mean)
thetabar.s2.est <- apply(thetabar.s2.chain,c(1,2),mean)

thetabar.all.se <- apply(thetabar.all.chain,c(1,2),sd)
thetabar.s1.se <- apply(thetabar.s1.chain,c(1,2),sd)
thetabar.s2.se <- apply(thetabar.s2.chain,c(1,2),sd)

thetabar.all.ci <- apply(thetabar.all.chain,c(1,2),quantile,c(0.025,0.975))
thetabar.s1.ci <- apply(thetabar.s1.chain,c(1,2), quantile,c(0.025,0.975))
thetabar.s2.ci <- apply(thetabar.s2.chain,c(1,2), quantile,c(0.025,0.975))

save(thetabar.all.est, thetabar.s1.est,thetabar.s2.est,thetabar.all.se,thetabar.s1.se,thetabar.s2.se,thetabar.all.ci,thetabar.s1.ci,thetabar.s2.ci,file="Estimates/SavedImmigrationEstimates.Rdata")

paste("Code correlations:")
codemat <- cbind(thetabar.all.est[-2,1],thetabar.s1.est[-2,1],thetabar.s2.est[-2,1])
colnames(codemat) <- c("All","Wave 1","Wave 2")
cor(codemat)
paste("Scale correlations:")
scalemat <- cbind(thetabar.all.est[-2,2],thetabar.s1.est[-2,2],thetabar.s2.est[-2,2])
colnames(scalemat) <- c("All","Wave 1","Wave 2")
cor(scalemat)

##################################################
##################################################
##                 JOINT MODEL                  ##
##################################################
##################################################

thetabar.chain <- immigrationposterior.all$thetabar
theta.chain <- immigrationposterior.all$theta
psi.chain <- immigrationposterior.all$psi
chi.chain <- immigrationposterior.all$chi

dimnames(thetabar.chain)[[1]] <- levels(Manifesto)
dimnames(thetabar.chain)[[2]] <- c("Code","Scale")

dimnames(theta.chain)[[1]] <- levels(Sentence)
dimnames(theta.chain)[[2]] <- c("Code","Scale")

dimnames(psi.chain)[[1]] <- levels(Coder)
dimnames(psi.chain)[[2]] <- c("Code","Scale")

dimnames(chi.chain)[[1]] <- levels(Coder)
dimnames(chi.chain)[[2]] <- c("Code","Scale")


thetabar.est <- apply(thetabar.chain,c(1,2),mean)
thetabar.se <- apply(thetabar.chain,c(1,2),sd)
thetabar.ci <- apply(thetabar.chain,c(1,2),quantile,c(0.025,0.975))

theta.est <- apply(theta.chain,c(1,2),mean)
theta.ci <- apply(theta.chain,c(1,2),quantile,c(0.025,0.975))

psi.est <- apply(psi.chain,c(1,2),mean)
psi.ci <- apply(psi.chain,c(1,2),quantile,c(0.025,0.975))

chi.est <- apply(chi.chain,c(1,2),mean)
chi.ci <- apply(chi.chain,c(1,2),quantile,c(0.025,0.975))

ManifestoPlotNames <- strsplit(rownames(thetabar.est)," ",fixed=TRUE)
ManifestoPlotNames <- unlist(ManifestoPlotNames)[seq(1,17,2)]

# Make plots of immigration scores

pdf(file="Plots/ImmigrationSaliance.pdf")
par(mar=c(5,8,4,2))
plot(thetabar.est[,1],1:9,pch=16,ylab="",xlab="Immigration Saliance",main="Estimated Topic Weight in Manifesto",xlim=c(-5,2),axes=FALSE)
axis(1)
axis(2,at=1:9,labels= ManifestoPlotNames,las=2)
for (i in 1:9) lines(thetabar.ci[,i,1],c(i,i))
dev.off()

pdf(file="Plots/ImmigrationPositions.pdf")
par(mar=c(5,8,4,2))
plot(thetabar.est[,2],1:9,pch=16,ylab="",xlab="Immigration Position",main="Estimated Immigration Positions",xlim=c(-3,3),axes=FALSE)
axis(1)
axis(2,at=1:9,labels= ManifestoPlotNames,las=2)
for (i in 1:9) lines(thetabar.ci[,i,2],c(i,i))
dev.off()

# Comparison plots with expert survey

library(foreign)
d <- read.dta("../../CrowdFlower/Immigration/allscores.dta")
d$exp95cilow <- d$meanexperts - d$seexperts * abs(qt(.05/2, d$nexperts))
d$exp95cihi <- d$meanexperts + d$seexperts * abs(qt(.05/2, d$nexperts))
d$colour <- "#FFFFFF"
d <- d[c(1,9,2,3,5,4,6,7,8),]



all.party.colours <- matrix(c(
"#0087DC","Con",
"#DC241F","Lab",
"#008142","PCy",
"#FFFF00","SNP",
"#FDBB30","LD",
"#00008B","BNP",
"#99CC33","Greens",
"#70147A","UKIP",
"#FFFFFF","Coalition"
),ncol=2,byrow=TRUE)

for (l in 1:nrow(d)){
	d$colour[l] <- all.party.colours[which(d$party[l] == all.party.colours[,2]),1]
	}


pdf(file="Plots/ImmigrationPositionsCrowdVsExperts.pdf")
par(mar=c(5,4,4,2))
plot(d$meanexperts,thetabar.est[,2],pch=16,xlab="Experts",ylab="Crowd",main="Estimated Immigration Positions",axes=TRUE,xlim=c(0,20),ylim=c(-3,3),type="n")
for (i in 1:9) {
	lines(c(d$meanexperts[i],d$meanexperts[i]),thetabar.ci[,i,2],col="grey",lwd=2)
	lines(c(d$exp95cilow[i],d$exp95cihi[i]),rep(thetabar.est[i,2],2),col="grey",lwd=2)
	}
text(d$meanexperts,thetabar.est[,2], ManifestoPlotNames)	
text(0,2.9,paste("r=",round(cor(d$meanexperts,thetabar.est[,2],use="pairwise.complete.obs"),2),sep=""),pos=4)
abline(lm(thetabar.est[,2]~d$meanexperts))
dev.off()

# following function plots a posterior ellipse based on mean and covariance matrix
add.ellipse.mat <- function(mu,Sigma,col,level=0.95){
	require(ellipse)	
	ellipse.temp <- ellipse(x=Sigma,centre=mu,level=level)
	polygon(ellipse.temp[,1],ellipse.temp[,2],col=col,border=NA)
	}

fade.colour <- function(col,alpha){
	components <- col2rgb(col)
	return(rgb(components[1]/255,components[2]/255,components[3]/255,alpha=alpha))
	}

pdf(file="Plots/ImmigrationPositionsCrowdVsExperts2.pdf")
par(mar=c(5,4,4,2),bty='n')
plot(d$meanexperts,thetabar.est[,2],pch=16,xlab="Expert Survey",ylab="Crowd",main="Estimated Immigration Positions",axes=TRUE,xlim=c(-1,21),ylim=c(-4,4),type="n")
lines(c(10,10),c(-4,4),col=rgb(0,0,0,0.2))
lines(c(0,20),c(0,0),col=rgb(0,0,0,0.2))
x <- d$meanexperts
y <- thetabar.est[,2]
lmfit <- lm(y~x)
bands <- as.data.frame(predict.lm(lmfit,newdata=data.frame(x=seq(0,20,0.25)),interval="confidence"))
xpts <- seq(0,20,0.25)
polygon(x=c(xpts,rev(xpts)),y=c(bands$lwr,rev(bands$upr)),col=rgb(0,0,0,0.1),border=NA)
lines(xpts, bands$fit,col=rgb(0,0,0,0.5))

for (i in c(1,3:9)) {
	tempmeanvec <- c(d$meanexperts[i],thetabar.est[i,2])
	tempcovmat <- matrix(c(d$seexperts[i]^2,0,0,thetabar.se[i,2]^2),2,2)
	add.ellipse.mat(tempmeanvec,tempcovmat,col= fade.colour(d$colour[i],0.1),level=0.95)
	}
text(d$meanexperts,thetabar.est[,2], ManifestoPlotNames,col=d$colour)	
text(0,2.9,paste("r=",round(cor(d$meanexperts,thetabar.est[,2],use="pairwise.complete.obs"),2),sep=""),pos=4)

dev.off()






Output <- round(cbind(thetabar.est[,1],t(thetabar.ci[,,1]),thetabar.est[,2],t(thetabar.ci[,,2])),2)
write.csv(Output,"Estimates/ImmigrationEstimates.csv")



