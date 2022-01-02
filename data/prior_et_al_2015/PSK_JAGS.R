#############################
## "You Cannot Be Serious" ##
## Last Edited: 7.29.15    ##
## Prior, Sood, and Khanna ##
#############################

# Set Dir
# setwd("Set Replication Directory")

# Load library
require(rjags)

# Studies 1 and 2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load and Recode data
kn <- foreign::read.dta("psk_long_recode.dta")
a  <- subset(kn, (is.na(r_bushref) | r_bushref=="" | r_bushref=="neutral frame") & !is.na(cong_33) & !is.na(pay))

# recode, create item level dummies
a$congdv	<- as.factor(a$cong_33)	
a$item2		<- a$seq1
a$item3		<- a$seq2
a$item4		<- a$seq3
a$item5		<- a$seq4
a$item6		<- a$seq5
a$item7		<- a$seq6
a$item8		<- a$seq7
a$item9		<- a$seq8
a$item10 	<- a$seq9

# Hierarchical Ordered Logit
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ols <- lm(as.numeric(congdv) ~ pay + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10, data=a, x=T, y=T)	
inits <- list(list(beta=coef(ols)[-1], tau0=2:3))
	
forJags <- list(y = ols$y, 
					x= ols$x[,-1],
					N=length(ols$y),
					NID=length(unique(a$caseid)),
					b0 = rep(0,10),
					B0=diag(1E-08,10))

forJags$id    <- match(a$caseid, unique(a$caseid))
	
foo <- jags.model(file="study12hologit.jag", data=forJags, inits=inits)	
out <- coda.samples(foo, variable.names=c("beta", "tau", "p"), n.iter=50000, thin=10)

# Betas and C.I.
getout <- t(apply(do.call(rbind, out), 2, function(x) {
              c(mean=mean(x), sd=sd(x), quantile(x, c(.025, .05, .95, .975)))
            }))

getout[grep('beta',  colnames(out[[1]])), , drop=FALSE]

# Raw Means
mean(a$congdv[!is.na(a$pay) & a$pay==0]=="Uncongenial") # ctrl
mean(a$congdv[!is.na(a$pay) & a$pay==0]=="Congenial")	# ctrl

mean(a$congdv[!is.na(a$pay) & a$pay==1]=="Uncongenial") # mon
mean(a$congdv[!is.na(a$pay) & a$pay==1]=="Congenial")	# mon
		
# Predicted vals
str(out)
dimnames(out[[1]])
#ps <- getout[grep('p',  colnames(out[[1]])), , drop=FALSE]

p1	<- out[[1]][,11:4334]
p3	<- out[[1]][,8659:12982]

# Output results
res <- data.frame(cond=c("control", "monetary"), mean.uncong=NA, cq025=NA, cq05=NA, cq95=NA, cq975=NA, mean.cong=NA, uq025=NA, uq05=NA, uq95=NA, uq975=NA)

res[1,2] <- mean(rowMeans(p1[, a$pay==0]))
res[2,2] <- mean(rowMeans(p1[, a$pay==1]))

res[1,7] <- mean(rowMeans(p3[, a$pay==0]))
res[2,7] <- mean(rowMeans(p3[, a$pay==1]))

res[1,3:6] <- quantile(rowMeans(p1[, a$pay==0]), c(.025, .05, .95, .975))
res[2,3:6] <- quantile(rowMeans(p1[, a$pay==1]), c(.025, .05, .95, .975))
	
res[1,8:11] <- quantile(rowMeans(p3[, a$pay==0]), c(.025, .05, .95, .975))
res[2,8:11] <- quantile(rowMeans(p3[, a$pay==1]), c(.025, .05, .95, .975))
	
# Write results to file
write.csv(res, file="study12jagsresults.csv", row.names=F)


# Study 2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load and Recode data
kn <- foreign::read.dta("psk_long_recode.dta")

a  <- subset(kn, kn$pollid==2008 & !is.na(r_bushref) & r_bushref=="neutral frame" & !is.na(cong_33))
	
# recode, create item level dummies
a$congdv <- as.factor(a$cong_33)	
a$item2		<- a$seq1
a$item3		<- a$seq2
a$item4		<- a$seq3
a$item5  	<- a$seq4
a$study  	<- (a$seq1 | a$seq2 | a$seq3 | a$seq4 | a$seq5)

a 	<- subset(a, study==1)

# Recode treats
a$rd1 <- a$rd=="verbal accuracy reminder"
a$rd2 <- a$rd=="monetary incentive"

# With item fixed effects (don't need'em)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ordered Logit
# ~~~~~~~~~~~~~~~~~~~~~~~
ols <- lm(as.numeric(congdv) ~ rd1 + rd2 + item2 + item3 + item4 + item5, data=a, x=T, y=T)	
inits <- list(list(beta=coef(ols)[-1], tau0=2:3))
		
forJags <- list(y = ols$y, 
						x= ols$x[,-1],
						N=length(ols$y),
						b0 = rep(0,6),
						B0=diag(1E-08,6))
foo <- jags.model(file="study2ologit.jag", data=forJags, inits=inits)
out <- coda.samples(foo, variable.names=c("beta", "tau"), n.iter=50000, thin=10)

# Validate: 
# library(MASS)
summary(MASS::polr(congdv ~ rd1 + rd2 + item2 + item3 + item4 + item5, data = a))

# Hierarchical Ordered Logit
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
forJags <- list(y = ols$y, 
					x= ols$x[,-1],
					N=length(ols$y),
					NID=length(unique(a$caseid)),
					b0 = rep(0,6),
					B0=diag(1E-08,6))
		
forJags$id    <- match(a$caseid, unique(a$caseid))
	
foo <- jags.model(file="study2hologit.jag", data=forJags, inits=inits)	
out <- coda.samples(foo, variable.names=c("beta", "tau", "p"), n.iter=5000, thin=10)

# Validate
library(ordinal)
s <- clmm(congdv ~ rd1 + rd2 + item2 + item3 + item4 + item5 + (1|caseid), data=a, Hess=TRUE)	
	
# Raw Means
mean(a$congdv[!is.na(a$rd1) & a$rd1==0 & !is.na(a$rd2) & a$rd2==0]=="Uncongenial") # ctrl
mean(a$congdv[!is.na(a$rd1) & a$rd1==0 & !is.na(a$rd2) & a$rd2==0]=="Congenial")	# ctrl

mean(a$congdv[!is.na(a$rd1) & a$rd1==1 & !is.na(a$rd2) & a$rd2==0]=="Uncongenial") # acc
mean(a$congdv[!is.na(a$rd1) & a$rd1==1 & !is.na(a$rd2) & a$rd2==0]=="Congenial")	# acc

mean(a$congdv[!is.na(a$rd1) & a$rd1==0 & !is.na(a$rd2) & a$rd2==1]=="Uncongenial") # mon
mean(a$congdv[!is.na(a$rd1) & a$rd1==0 & !is.na(a$rd2) & a$rd2==1]=="Congenial")	# mon

# Predicted vals
str(out)
dimnames(out[[1]])
p1	<- out[[1]][,7:(nrow(a)+6)]
p3	<- out[[1]][,(2*nrow(a)+7):9834]

# Output results
res <- data.frame(cond=c("control", "acc", "monetary"), mean.uncong=NA, cq025=NA, cq05=NA, cq95=NA, cq975=NA, mean.cong=NA, uq025=NA, uq05=NA, uq95=NA, uq975=NA)

res[1,2] <- mean(rowMeans(p1[, a$rd1==0 & a$rd2==0]))
res[2,2] <- mean(rowMeans(p1[, a$rd1==1]))
res[3,2] <- mean(rowMeans(p1[, a$rd2==1]))

res[1,7] <- mean(rowMeans(p3[, a$rd1==0 & a$rd2==0]))
res[2,7] <- mean(rowMeans(p3[, a$rd1==1]))
res[3,7] <- mean(rowMeans(p3[, a$rd2==1]))

res[1,3:6]  <- quantile(rowMeans(p1[, a$rd1==0 & a$rd2==0]), c(.025, .05, .95, .975))
res[2,3:6]  <- quantile(rowMeans(p1[, a$rd1==1]), c(.025, .05, .95, .975))
res[3,3:6]  <- quantile(rowMeans(p1[, a$rd2==1]), c(.025, .05, .95, .975))
	
res[1,8:11] <- quantile(rowMeans(p3[, a$rd1==0 & a$rd2==0]), c(.025, .05, .95, .975))
res[2,8:11] <- quantile(rowMeans(p3[, a$rd1==1]), c(.025, .05, .95, .975))
res[3,8:11] <- quantile(rowMeans(p3[, a$rd2==1]), c(.025, .05, .95, .975))
	
# Write results to file
write.csv(res, file="study2jagsresults.csv", row.names=F)