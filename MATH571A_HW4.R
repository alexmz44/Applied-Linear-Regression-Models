################################################################
##7.4
#a.
grocery <- read.delim(file = "https://www.math.arizona.edu/~piegorsch/571A/Data/Chapter06/CH06PR09.txt", 
                      header = F, sep = "")
head(grocery)
X1_Cases <- grocery$V2
head(X1_Cases)
X2_Costs_Of_Labor <- grocery$V3
head(X2_Costs_Of_Labor)
X3_Holiday_Week <- grocery$V4
head(X3_Holiday_Week)
Y_Total_Labor <- grocery$V1
head(Y_Total_Labor)
lm.grocery <- lm(Y_Total_Labor ~ X1_Cases + X2_Costs_Of_Labor + X3_Holiday_Week)
##SRR(X_1)
anova(lm.grocery)
##SSR(X_3|X_1)
anova(lm(Y_Total_Labor ~ X1_Cases + X3_Holiday_Week))
##SSR(X_2|X_1, X_3)
anova(lm(Y_Total_Labor ~ X3_Holiday_Week + X1_Cases), lm.grocery)
##SSE(X_1, X_2, X_3)
anova(lm.grocery)

#b.
anova(lm(Y_Total_Labor ~ X3_Holiday_Week + X1_Cases), lm.grocery)

#c.
anova(lm(Y_Total_Labor ~ X1_Cases))
anova(lm(Y_Total_Labor ~ X1_Cases + X2_Costs_Of_Labor))      
## add them together
anova(lm(Y_Total_Labor ~X1_Cases))[1,2] + anova(lm(Y_Total_Labor ~ X1_Cases + X2_Costs_Of_Labor))[2,2]

anova(lm(Y_Total_Labor ~ X2_Costs_Of_Labor))[1,2]+ anova(lm(Y_Total_Labor ~ X2_Costs_Of_Labor + X1_Cases))[2,2] 
#############################################################################################
##7.25
#a.
summary(lm(Y_Total_Labor ~ X1_Cases))


#c.
anova(lm(Y_Total_Labor ~ X1_Cases))
anova(lm(Y_Total_Labor ~ X2_Costs_Of_Labor + X1_Cases))

#d.
grocery.df <- data.frame(Y_Total_Labor ,X1_Cases , X2_Costs_Of_Labor, X3_Holiday_Week)
grocery.df
cor(grocery.df)


#########################################################################################
######################CHAPTER 8
##8.4
#a.
muscle <- read.delim(file = "https://www.math.arizona.edu/~piegorsch/571A/Data/Chapter01/CH01PR27.txt",
                     header = F, sep = "")
X <- muscle$V2
X
Y <- muscle$V1
Y

X1 <- (X - mean(X))
X1sq <- (X1*X1)
plot(X ~ Y)
model<- lm(Y ~ X1 + X1sq)
model

#b.
f <- qf(.95,2,57)
f
summary(model)
anova(model)

#c.
t <- qt(.975,57)
newdata.df = data.frame(X1 = 48 - mean(X), X1sq = (48-mean(X))^2)
predict(model, newdata=newdata.df,se.fit=T, interval= "confidence",level = 1-(.05/2))

#d.
predict(model, newdata=newdata.df,se.fit=T, interval= "prediction",level = 1-(.05/2))

#e.
f1 <- qf(.95,1,57)
f1
summary(model)
anova(model)

#f
X
Xsq <- X*X
model1 <- lm(Y ~ X + Xsq)
model1

#g
cor(cbind(X1,X1sq,X,Xsq))

#################################################################################
##8.5
#a.
plot(resid(model) ~ fitted(model))
abline(h=0)

plot(resid(model)~ X1)
abline(h=0)

qqnorm(resid(model), main = "")

#b.
fmmodel <- lm(Y ~ factor(X1)+ X1sq)
rmmodel <- lm(Y ~ X1)
anova(rmmodel)
anova(fmmodel)
anova(rmmodel, fmmodel)


#c.
X1cu <- (X1*X1*X1)
model2 <- lm(Y ~ X1 + X1sq + X1cu)
model2
summary(model2)
anova(model2)
qf(.95,1,56)
#############################################################################
##8.15
#b.
X.8.15<- c(2,4,3,2,1,10,5,5,1,2,9,10,6,3,4,8,7,8,10,4,5,7,7,5,
       9,7,2,5,7,6,8,5,2,2,1,4,5,9,7,1,9,2,2,4,5)
length(X.8.15)
Y.8.15<- c(20,60,46,41,12,137,68,89,4,32,144,156,93,36,72,100,105,
       131,127,57,66,101,109,74,134,112,18,73,111,96,123,90,20,
       28,3,57,86,132,112,27,131,34,27,61,77)
length(Y.8.15)
type <- read.delim(file = "https://www.math.arizona.edu/~piegorsch/571A/Data/Chapter08/CH08PR15.txt",
                   header = F, sep = "")
X1.8.15 <- type$V1
head(X1.8.15)
length(X1.8.15)

copy.lm <- lm(Y.8.15 ~ X.8.15 + X1.8.15)
coef(copy.lm)
anova(copy.lm)
summary(copy.lm)
#c.
confint(copy.lm)


#############################################################
##8.19
#a.
copy2.lm <- lm(Y.8.15 ~ X.8.15*X1.8.15)
copy2.lm


#b.
summary(copy2.lm)
anova(copy2.lm)
qt(.95,41)
