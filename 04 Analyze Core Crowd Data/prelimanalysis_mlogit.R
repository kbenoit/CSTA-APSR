##
## Code to perform a preliminary analysis of the sentence-level uncertainty
## in policy area.  This one compares a random-effects logit with predictions
## to entropy measures across categories and to RE predicted values
##
## Ken Benoit (kbenoit@lse.ac.uk)
## October 2013
##
## Packages: lme4, foreign, Zelig, ZeligMultilevel
## Data:     coding_all_long_2013-10-23.dta
##


if (Sys.getenv("USER")=="kbenoit") {
    setwd("~/Dropbox/Papers/CMP_recoding/newanalysis/4_addtoCFresults")
}
rm(list = ls())  # clear workspace

library(foreign)
d <- read.dta("../3_addCFdata/coding_all_long_2013-10-23.dta")
manif6 <- subset(d, manifestoid=="Lab 1997")
manif6 <- manif6[order(manif6$sentenceid),]

# create dummy for "Neither economic nor social" policy
manif6$areadum1 <- (manif6$scale=="None")

# estimate the RE logit in Zelig
library(Zelig)
library(ZeligMultilevel)
mllogit.none.z <- zelig(formula = areadum1 ~ tag(1 | sentenceid),
                        data=manif6, model="logit.mixed")
summary(mllogit.none.z)
x.out <- setx(mllogit.none.z)
mlogit.none.z.pred <- sim(mllogit.none.z, x.out)

# estimate the RE logit in lme4
library(lme4)
none.lme4 <-  glmer(areadum1 ~ 1 + (1 | sentenceid),
                    data = manif6, family = binomial)
summary(none.lme4)

library(entropy)
bysentence <- data.frame(apply(table(manif6$sentenceid, manif6$scale), 1, entropy),
                         aggregate(manif6$areadum1, by=list(manif6$sentenceid), length),
                         aggregate(manif6$areadum1, by=list(manif6$sentenceid), sum),
                         aggregate(1/(1+exp(-1*predict(none.lme4))), by=list(manif6$sentenceid), mean))
bysentence <- bysentence[,c(1,3,5,7)]
names(bysentence) <- c("entropy", "ncodes", "nnone", "predicted")
bysentence$propnone <- bysentence$nnone / bysentence$ncodes
head(bysentence,50)


plot(bysentence$propnone, bysentence$predicted, cex=.7,
     xlab="Empirical proportion of not econ or social",
     ylab="RE logit predicted probability of not econ or social")

plot(bysentence$propnone, bysentence$entropy, cex=.7,
     xlab="Empirical proportion of not econ or social",
     ylab="Entropy measure for category codings")

plot(jitter(bysentence$ncodes), jitter(bysentence$entropy, 3), cex=.7,
     xlab="Number of codings",
     ylab="Entropy measure for category codings", log="x")



# . xtmelogit areadum1 || sentenceid:
# 
# Refining starting values: 
# 
# Iteration 0:   log likelihood = -10149.739  
# Iteration 1:   log likelihood = -9883.3827  
# Iteration 2:   log likelihood = -9875.2667  
# 
# Performing gradient-based optimization: 
# 
# Iteration 0:   log likelihood = -9875.2667  
# Iteration 1:   log likelihood = -9875.0296  
# Iteration 2:   log likelihood = -9875.0293  
# 
# Mixed-effects logistic regression               Number of obs      =     18664
# Group variable: sentenceid                      Number of groups   =      1052
# 
#                                                 Obs per group: min =         6
#                                                                avg =      17.7
#                                                                max =       157
# 
# Integration points =   7                        Wald chi2(0)       =         .
# Log likelihood = -9875.0293                     Prob > chi2        =         .
# 
# ------------------------------------------------------------------------------
#     areadum1 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#        _cons |   .1578053   .0645184     2.45   0.014     .0313516     .284259
# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
#   Random-effects Parameters  |   Estimate   Std. Err.     [95% Conf. Interval]
# -----------------------------+------------------------------------------------
# sentenceid: Identity         |
#                    sd(_cons) |   1.919213   .0590412      1.806914    2.038491
# ------------------------------------------------------------------------------
# LR test vs. logistic regression: chibar2(01) =  5961.66 Prob>=chibar2 = 0.0000

# > summary(none.lme4)
# Generalized linear mixed model fit by maximum likelihood ['glmerMod']
#  Family: binomial ( logit )
# Formula: areadum1 ~ 1 + (1 | sentenceid) 
#    Data: manif6 
# 
#       AIC       BIC    logLik  deviance 
# 19790.864 19806.533 -9893.432 19786.864 
# 
# Random effects:
#  Groups     Name        Variance Std.Dev.
#  sentenceid (Intercept) 3.571    1.89    
# Number of obs: 18664, groups: sentenceid, 1052
# 
# Fixed effects:
#             Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  0.15792    0.06258   2.524   0.0116 *
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
