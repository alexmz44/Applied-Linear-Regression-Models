####CHAPTER 9##################################################################
##9.10

#b.
aptitude <- read.delim(file = "https://www.math.arizona.edu/~piegorsch/571A/Data/Chapter09/CH09PR10.txt",
                       header = F, sep = "")
Y_Job_Proficiency_Score <- aptitude$V1
X1_Test1 <- aptitude$V2
X2_Test2 <- aptitude$V3
X3_Test3 <- aptitude$V4
X4_Test4 <- aptitude$V5

CH09PR10.df <- data.frame(Y_Job_Proficiency_Score, X1_Test1,X2_Test2,
                          X3_Test3,X4_Test4)
pairs(CH09PR10.df)
cor(CH09PR10.df)


#c.
CH09PR10.lm <- lm(Y_Job_Proficiency_Score ~ X1_Test1 + X2_Test2 + X3_Test3 + X4_Test4)
CH09PR10.lm
summary(CH09PR10.lm)





##9.11

#a.
library(leaps)

best <- function(CH09PR10.lm, ...) 
{
  subsets <- regsubsets(formula(CH09PR10.lm), model.frame(CH09PR10.lm), ...)
  subsets <- with(summary(subsets),
                  cbind(p = as.numeric(rownames(which)), which, adjr2))
  
  return(subsets)
}  

round(best(CH09PR10.lm, nbest = 6), 4)



##9.18

#a.
step(CH09PR10.lm, direction = "backward", k = 2)
library(rms)
CH09PR10.ols <- ols(CH09PR10.lm)
fastbw(fit=CH09PR10.ols, rule = "p", type = "individual", sls = .05)



###CHAPTER 10 ##################################################################
##10.10

#a.
n = length(Y_Total_Labor)
plot(rstudent(lm.grocery)~ fitted(lm.grocery),ylim = c(-4,4))
abline(h=0)
tcrit = qt(1- .5*(.05/n), n-4-1)
abline(h= tcrit, lty = 2)
abline(h = -tcrit, lty = 2)

tcrit

g.res <- rstudent(lm.grocery)
g.res


#b.
hat <- as.numeric(hatvalues(lm.grocery))
p = 4
2*p/n
ifelse(hat > .1538462, "High Leverage", "Low Leverage")


#c.
plot(X2_Costs_Of_Labor~ X1_Cases, pch='')
text(X1_Cases, X2_Costs_Of_Labor, label=as.character(1:52))
points(X1_Cases[hat>2*p/n], X2_Costs_Of_Labor[hat>2*p/n],
       cex=2.5, col = 'red')


XM <- matrix(c(rep(1,52),X1_Cases,X2_Costs_Of_Labor,X3_Holiday_Week),ncol = 4)
tXMnew <- t(matrix(c(1, 300000,7.2, 0)))
XMnew <- matrix(c(1, 300000,7.2, 0))
tXM <- t(XM)
invXM <- solve(tXM%*%XM)

hatnew <- tXMnew  %*% invXM %*% XMnew

hatnew

#d.

influence.measures(lm.grocery)

#f.
ei = resid(lm.grocery)
yhat = fitted(lm.grocery)
radius = sqrt( cooks.distance(lm.grocery)/pi ) 
plot( ei ~ yhat, pch=''); abline( h=0 )
symbols( yhat, ei, circles=radius, inches=.15,
         bg='white', fg='black', add=T )
text(yhat, ei, label=as.character(1:52))



##10.16

#a.
cor(grocery.df)
pairs(grocery.df)

#b.
library(car)
vif(lm.grocery)
mean(vif(lm.grocery))



###CHAPTER 11###################################################################
##11.7

#a.
machine <- read.delim(file = "https://www.math.arizona.edu/~piegorsch/571A/Data/Chapter11/CH11PR07.txt",
                       header = F, sep = "")
Y_Defects <- machine$V1
X_Speed <-  machine$V2
speed.lm


plot(resid(speed.lm) ~ X_Speed)

#b.
library("lawstat")
ei <- resid(speed.lm)
BF.htest <- levene.test(ei[order(X_Speed)], group = c(rep(1,6),rep(2,6)), location = "median")
BF.htest
sqrt(BF.htest$statistic)

#c.
plot((resid(speed.lm)^2) ~ X_Speed)

#d.
variancef <- lm((speed.lm)^2 ~ X_Speed)
variancef

v <- fitted(variancef) ; w <- 1/v
w

#e.
summary(lm(Y_Defects ~ X_Speed, weights= w))

#f
summary(lm(Y_Defects ~ X_Speed))
summary(lm(Y_Defects ~ X_Speed, weights= w))


##11.25

#a.
cropyield <- read.delim( file = "https://www.math.arizona.edu/~piegorsch/571A/Data/Chapter11/CH11PR25.txt", 
                         header = F,
                         sep = "")
X1_Moisture <- cropyield$V2
X2_Temperature <- cropyield$V3
Y_tomato <- cropyield$V1
x1sq <- X1_Moisture^2
secondorder.lm <- lm(Y_tomato ~ X1_Moisture + X2_Temperature + x1sq )
summary(secondorder.lm)


