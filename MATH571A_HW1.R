## 1.20 a. and b.
#a.
X <- c(2,4,3,2,1,10,5,5,1,2,9,10,6,3,4,8,7,8,10,4,5,7,7,5,
       9,7,2,5,7,6,8,5,2,2,1,4,5,9,7,1,9,2,2,4,5)
length(X)
Y <- c(20,60,46,41,12,137,68,89,4,32,144,156,93,36,72,100,105,
       131,127,57,66,101,109,74,134,112,18,73,111,96,123,90,20,
       28,3,57,86,132,112,27,131,34,27,61,77)
length(Y)

HW1P1.20.lm <- lm(Y ~ X)

summary(HW1P1.20.lm)
mean(X)
#d.
15.0352*(5) -.5802

## 1.21 #####################################################
#a.
X1 <- c(1,0,2,0,3,1,0,1,2,0)
length(X1)

Y1 <- c(16,9,17,12,22,13,8,15,19,11)
length(Y1)

HW1P1.21.lm <- lm(Y1 ~ X1)
summary(HW1P1.21.lm)

plot(Y1 ~ X1)
abline(HW1P1.21.lm)
#d.
mean(X1)
mean(Y1)


## 1.24 #####################################################

#a.
df1.24 <- data.frame(resid(HW1P1.20.lm))
df1.24
SSE1.24 <- sum((df1.24[1:45,])^2)
SSE1.24
#b. finding MSE
MSE1.24 <- SSE1.24/43
MSE1.24
sqrt(MSE1.24)

## 1.25 #####################################################
#b.
df1.25 <- data.frame(resid(HW1P1.21.lm))
df1.25
SSE1.25 <- sum((df1.25[1:10,])^2)
SSE1.25
MSE1.25 <- SSE1.25/8
MSE1.25
####################CHAPTER 2################################
##2.5########################################################
#a.
for (i in 1:43){
 i <- X - mean(X)
}
sum_X_squared <- sum((i)^2)
se2.5 <- sqrt(MSE1.24/sum_X_squared)
#b.
CI2.5 <- confint(HW1P1.20.lm, level =.90)
tcrit2.5 <- qt(.95,df = HW1P1.20.lm$df)
summary(HW1P1.20.lm)
##2.6########################################################
#a.
for (i in 1:10){
  i <- X1 - mean(X1)
}
i
sum_X1_squared <- sum((i)^2)
sum_X1_squared
MSE1.25
se2.6 <- sqrt(MSE1.25/sum_X1_squared)
se2.6
##2.14#######################################################
#a.

CI2.14 <- predict(HW1P1.20.lm, newdata= data.frame(X=6), 
                  interval = "conf", level =.90)
CI2.14

#b.
PI2.14 <- predict(HW1P1.20.lm, newdata= data.frame(X=6), 
        interval = "pred", level =.90)
PI2.14

#c.
##2.15########################################################
#a. 
CI2.15 <- predict(HW1P1.21.lm, newdata= data.frame(X1=2), 
           interval = "conf", level =.99)
CI2.15

CI2.15_2 <- predict(HW1P1.21.lm, newdata= data.frame(X1=4), 
                    interval = "conf", level =.99)
CI2.15_2

#b
PI2.15 <- predict(HW1P1.21.lm, newdata= data.frame(X1=2), 
                  interval = "pred", level =.99)
PI2.15
##2.24##########################################################
#a 
## basic calculations
Correction_for_mean <- 45*(mean(Y)^2)
Correction_for_mean

Uncorrected_Total <- sum((Y)^2)
Uncorrected_Total

anova(HW1P1.20.lm)

#b.
qf(.90,df1=1, df2=HW1P1.20.lm$df)
##2.25###########################################################
#a. 
anova(HW1P1.21.lm)
#b.
qf(.95,df1=1, df2=HW1P1.21.lm$df)
##########CORRELATION#############################################
##2.42############################################################
#a
Y_i <- c(13.9,16,10.3,11.8,16.7,12.5,10,11.4,13.9,12.2,15.4,
          14.8,14.9,12.9,15.8)
length(Y_i)

Y_ii <- c(28.6,34.7,21,25.5,36.8,24,19.1,22.5,28.3,25,31.1,
          29.6,35.1,30,36.2)

length(Y_ii)

plot(Y_i,Y_ii, main = "Property Assessments",
     xlab = "Assessed Value for Property Tax Purposes 
     (in thousands of dollars)",
     ylab = "Sales Prices(in thousands of dollars)")
#b
cor(Y_i,Y_ii)
##2.46############################################################
#a.
cor(Y_i,Y_ii, method = "spearman")

#b.
cor.test(Y_i, Y_ii, method="spearman", exact = F)


