###############################################################################
###3.4
#c.
##getting data for the stem plot
X <- c(2,4,3,2,1,10,5,5,1,2,9,10,6,3,4,8,7,8,10,4,5,7,7,5,
       9,7,2,5,7,6,8,5,2,2,1,4,5,9,7,1,9,2,2,4,5)
length(X)
Y <- c(20,60,46,41,12,137,68,89,4,32,144,156,93,36,72,100,105,
       131,127,57,66,101,109,74,134,112,18,73,111,96,123,90,20,
       28,3,57,86,132,112,27,131,34,27,61,77)
length(Y)

HW1P1.20.lm <- lm(Y ~ X)
HW1P1.20.lm

summary(HW1P1.20.lm)

R <- resid(HW1P1.20.lm)
R
stem(R,scale = 4)

#d.
##against the fitted value
par(mfrow = c(1,2))
plot(resid(HW1P1.20.lm)~fitted(HW1P1.20.lm))
abline(h=0)

##against X_i's
plot(resid(HW1P1.20.lm) ~ X)
abline(h = 0)

#e.
#normal probability plot
qqnorm(resid(HW1P1.20.lm), main = "")

#g.
install.packages("lawstat")
library("lawstat")
ei <- resid(HW1P1.20.lm)
require(lawstat)
BF.htest <- levene.test(ei[order(X)], group = c(rep(1,22),rep(2,23)), 
                        location = "median")
BF.htest

##########################################################################
###3.5
#c.
air <- read.delim("https://www.math.arizona.edu/~piegorsch/571A/Data/Chapter01/CH01PR21.txt",
           header = F, sep ="")
X1 <- air$V2
Y1 <- air$V1

HW1P1.21.lm <- lm(Y1~ X1)
R1 <- resid(HW1P1.21.lm)
R1
stem(R1)

#d.
##plot against X
plot(resid(HW1P1.21.lm) ~ X1)
abline(h=0)

#e.
## normal probability plot
qqnorm(resid(HW1P1.21.lm), main = "")

#d.
#BF test
library("lawstat")
ei1 <- resid(HW1P1.21.lm)
require(lawstat)
BF.htest1<- levene.test(ei1[order(X1)], group = c(rep(1,5),rep(2,5)), 
                        location = "median")
BF.htest1

###############################################################################
###3.13
#b.
plot(Y~X)
rmHW1P1.20.lm = lm( Y~ X)
rmHW1P1.20.lm

factor(X)
fmHW1P1.20.lm = lm(Y ~ factor(X))
fmHW1P1.20.lm

anova(rmHW1P1.20.lm)
anova(rmHW1P1.20.lm,fmHW1P1.20.lm)

################################################################################
###3.17
#a.
sales <- read.delim("https://www.math.arizona.edu/~piegorsch/571A/Data/Chapter03/CH03PR17.txt",
           header = F, sep = "")
XS <- sales$V2
YS <- sales$V1
length(YS)
length(XS)
plot(YS ~XS, xlab = "Year (Coded)", ylab = "sales in thousands")

#b.
HW2P3.17.lm <- lm(YS ~ XS)
library("MASS")
HW2P3.17.lm.bc = boxcox(HW2P3.17.lm, 
                     lambda = c(.3,.4,.5,.6,.7),interp = F)
cbind(HW2P3.17.lm.bc$x, HW2P3.17.lm.bc$y)

#c.
tHW2P3.17.lm <- lm(sqrt(YS) ~ XS )
tHW2P3.17.lm

#d.
plot(sqrt(YS) ~ XS, xlab = "Year (Coded)", ylab = "sqrt(sales in thousands)")

#e.
par(mfrow= c(1,2))
resid(tHW2P3.17.lm)
plot(resid(tHW2P3.17.lm)~fitted(tHW2P3.17.lm))
abline(h = 0)
qqnorm(resid(tHW2P3.17.lm))

#f
HW2P3.17.lm <- lm(YS ~ XS)
HW2P3.17.lm
##################################################################################
##4.3
#b
##finding the standard error for beta_0 (i just wanted to do it manually
##to practice my R skills)
Avg.squared <- (mean(X))^2
Avg.squared
df4.3 <- data.frame(resid(HW1P1.20.lm))
df4.3
SSE4.3 <- sum((df4.3[1:45,])^2)
SSE4.3
MSE4.3 <- SSE4.3/43
MSE4.3

for (i in 1:43){
  i <- X - mean(X)
}
sum_X_squared <- sum((i)^2)
SE.beta0 <- sqrt(MSE4.3*(1/45 +
                           (Avg.squared/sum_X_squared)))
SE.beta0
## finding the standard error for beta_1
SE.beta1 <- sqrt(MSE4.3/sum_X_squared)
SE.beta1

B <- qt(1-(.05/4), 43)

CIupr <- -.580157 + B*(SE.beta0)
CIlwr <- -.580157 - B*(SE.beta0)

CIupr1 <- 15.0352 + B*(SE.beta1)
CIlwr1 <- 15.0352 - B*(SE.beta1)

#########################################################################
##4.4
#c.
summary(HW1P1.21.lm)

B1 <- qt(1-(.01/4), 8)
B1

CIupr2 = 10.2 + B1*(.6633)
CIlwr2 = 10.2 - B1*(.6633)
CIupr2
CIlwr2

CIupr3 = 4 + B1*(.469)
CIlwr3 = 4 - B1*(.469)
CIupr3
CIlwr3

###############################################################
##4.7
#a.
W <- sqrt(2*qf(.90,2,43))
W
seYhat3 <- sqrt(MSE4.3*(1/45 +
                           (3-mean(X))^2/sum_X_squared))
seYhat3

seYhat5 <- sqrt(MSE4.3*(1/45 +
                          (5-mean(X))^2/sum_X_squared))
seYhat5

seYhat7 <- sqrt(MSE4.3*(1/45 +
                          (7-mean(X))^2/sum_X_squared))
seYhat7

##math
44.5256+W*(1.675012)
44.5256-W*(1.675012)

74.5961 + W*(1.329831)
74.5961 - W*(1.329831)

104.667 + W*(1.6119)
104.667 - W*(1.6119)

##########################################################
##4.8
#a.
W1 <- sqrt(2*qf(.95,2,8))
W1
anova(HW1P1.21.lm)
MSE4.8 <-  17.6/8
seYhat0 <- sqrt(MSE4.8*(1/10 +
                         (0-mean(X1))^2/sum_X1_squared))
seYhat0

seYhat1 <- sqrt(MSE4.8*(1/10 +
                          (1-mean(X1))^2/sum_X1_squared))
seYhat1
seYhat2 <- sqrt(MSE4.8*(1/10 +
                          (2-mean(X1))^2/sum_X1_squared))
seYhat2

10.2+W1*(.6633)
10.2-W1*(.6633)

14.2 + W1*(.469)
14.2 - W1*(.469)

18.2 + W1*(.6633)
18.2 - W1*(.6633)

B2 <- qt(1-(.05/6),8)
B2

###########################################################################
##4.16
#b
SE.beta1
qt(.95,44)
15.032 - 1.68023*(.4830872)
15.032 + 1.68023*(.4830872)

#c.
PI4.16 <- predict(HW1P1.20.lm, newdata= data.frame(X=6), 
                  interval = "pred", level =.90)
PI4.16

#########################################################################
##4.17
#a.
Origin.lm = lm(Y ~ 0 + X)
summary(Origin.lm)

confint(Origin.lm)

plot(Y ~ X , pch =19)
abline(Origin.lm)

#b.
sum(resid(Origin.lm))
plot(resid(Origin.lm)~fitted(Origin.lm))

#c.
plot(Y~X,pch = 19)

#### we now need MSLF and MSPE

#fit reduced model
rmOrigin.lm = lm(Y ~ 0 + X)
rmOrigin.lm

# fit full model\
factor(X)
fmOrigin.lm = lm(Y ~ 0 + factor(X))
fmOrigin.lm

anova(rmOrigin.lm,fmOrigin.lm)
####################################################################
##5.4
#1
MX <- matrix(c(1,1,1,1,1,8,4,0,-4,-8), ncol =2)
MX

MY <- matrix(c(7.8,9,10.2,11,11.7))
MY

tMY <- t(MY)
tMY

tMY%*%MY

#2
tMX <- t(MX)

tMX%*%MX

#3
tMX%*%MY

#######################################################################
##5.6
#a.
MY1 <- matrix(Y1)
MY1

tMY1 <- t(MY1)

tMY1 %*% MY1

#b.
MX1 <- matrix(c(rep(1,10),X1), ncol = 2)
MX1

tMX1 <- t(MX1)
tMX1

tMX1 %*% MX1

#c.
tMX1 %*% MY1

##############################################################
##5.12
solve(tMX%*%MX)

################################################################
##5.23
#a (1)
tXX <- solve(tMX %*%MX)
tXY <- tMX %*% MY
b <- tXX %*% tXY

#(2)
H <- MX%*%tXX%*%tMX
H
I <- diag(5)
I 

e <- (I - H)%*%MY
e

#(3)
tb <- t(b)
J <- matrix(rep(1,25), ncol = 5)
J
SSR <- tb%*%tMX%*%MY - (1/5)%*%tMY%*%J%*%MY
SSR

#(4)
SSE <- tMY %*% MY - tb%*%tMX%*%MY
SSE

#(5)
XF <- c(8,4,0,-4,-8)
YF <-c(7.8,9,10.2,11,11.7)
flavor.lm <- lm(YF ~ XF)
flavor_summ <- summary(flavor.lm)
mean(flavor_summ$residuals^2)
MSEF <- .04928
Var_CovM <- MSEF*tXX
Var_CovM

#(6)
Xh <- matrix(c(1, -6))
tXh <- t(Xh)
Y_hat1 <- tXh %*% b
Y_hat1

#(7)
MSEF*(1 + tXh%*%tXX%*%Xh)

#c.
H <- MX%*%tXX%*%tMX
H

#########################################################################
##5.25
#a

#(1)
tXX1 <- solve(tMX1 %*% MX1)
tXX1

#(2)
tXY1 <- tMX1 %*% MY1
b1 <- tXX1 %*% tXY1
b1

#(3)
H1 <- MX1%*%tXX1%*%tMX1
H1
I1 <- diag(10)
I1 

e1 <- (I1 - H1)%*%MY1
e1
#(4)
H1

#(5)
tb1 <- t(b1)
SSE1 <- tMY1 %*% MY1 - tb1%*%tMX1%*%MY1
SSE1
