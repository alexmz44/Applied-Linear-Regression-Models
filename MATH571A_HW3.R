####################################################################
##6.9
#a.
grocery <- read.delim(file = "https://www.math.arizona.edu/~piegorsch/571A/Data/Chapter06/CH06PR09.txt", 
                      header = F, sep = "")
head(grocery)
X1_Cases <- grocery$V2
X2_Costs_Of_Labor <- grocery$V3
X3_Holiday_Week <- grocery$V4
Y_Total_Labor <- grocery$V1

stem(X1_Cases,scale = 4)
stem(X2_Costs_Of_Labor, scale = 8)

#b.
par(mfrow = c(2,2))
weeks = 1:52
plot(X1_Cases ~ weeks, col = "blue")

plot(X2_Costs_Of_Labor ~ weeks, col = "red")

plot(X3_Holiday_Week ~ weeks, col= "green")

#c.
##scatter plot matrix 
grocery.df <- data.frame(Y_Total_Labor ,X1_Cases , X2_Costs_Of_Labor, X3_Holiday_Week)
grocery.df
pairs(grocery.df)
cor(grocery.df)
########################################################################
##6.10
#a.

lm.grocery <- lm(Y_Total_Labor ~ X1_Cases + X2_Costs_Of_Labor + X3_Holiday_Week)
summary(lm.grocery)
coef(lm.grocery)
#b.
residuals <- resid(lm.grocery)
sum(residuals)
boxplot(resid(lm.grocery))

#c.
par(mfrow= c(2,2))
plot(resid(lm.grocery) ~ fitted(lm.grocery))

plot(resid(lm.grocery) ~ X1_Cases)

plot(resid(lm.grocery) ~ X2_Costs_Of_Labor)

plot(resid(lm.grocery) ~ X3_Holiday_Week)

X_1.X_2 <- X1_Cases*X2_Costs_Of_Labor

plot(resid(lm.grocery) ~ X_1.X_2)


#NPP

qqnorm(resid(lm.grocery), main = "")

#e.

library("lawstat")
ei <- resid(lm.grocery)
require(lawstat)
BF.htest <- levene.test(ei[order(Y_Total_Labor)], group = c(rep(1,26),rep(2,26)), location = "median")
BF.htest

################################################################
##6.11
#a.
anova(lm(Y_Total_Labor ~ 1), lm.grocery)
qf( .95, 3,48)

#b.
g = length(coef(lm.grocery))-2
g

alpha = .05
confint(lm.grocery, level = 1 - (alpha/g))

B <- qt(1-(.05/4), 48)

#c.
summary(lm.grocery)


#########################################################################
##6.12
#a.
g1 = 5
alpha1= .05
Spoint = sqrt(g1*qf(1-alpha1,g1, lm.grocery$df.residual))
Spoint

Bpoint = qt(1 - (.5*(alpha1/g1)), lm.grocery$df.residual)
Bpoint

newdata.df = data.frame(X1_Cases = c(302000,245000,280000,350000,295000),
                        X2_Costs_Of_Labor= c(7.2,7.4,6.9,7,6.7),
                        X3_Holiday_Week = c(0,0,0,0,1))

predict.lm(lm.grocery, newdata=newdata.df,se.fit=T, interval='confidence',level = 1- (alpha1/5)) 


#b.

grocery.df

###################################################################
##6.13
Spoint1 = sqrt(g2*qf(1-alpha2,g2, lm.grocery$df.residual))
Spoint1

Bpoint1 = qt(1 - (.5*(alpha2/g2)), lm.grocery$df.residual)
Bpoint1


newdata.df2 = data.frame(X1_Cases = c(230000,250000,280000,340000),
                        X2_Costs_Of_Labor = c(7.5,7.3,7.1,6.9),
                        X3_Holiday_Week = c(rep(0,4)))
g2=4
alpha2 = .05
predict.lm(lm.grocery, newdata=newdata.df2, interval='prediction',
           level = 1 - (alpha2/g2)) 
###########################################################################
##6.14
#a.
alpha3 = .05
g3 = 3 
newdata.df3 = data.frame(X1_Cases = c(282000),
                         X2_Costs_Of_Labor = c(7.1),
                         X3_Holiday_Week = c(0))

predict.lm(lm.grocery, newdata=newdata.df3, interval='prediction') 
