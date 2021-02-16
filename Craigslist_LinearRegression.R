#install.packages("leaps")
library(leaps)

#install.packages("readxl")
library("readxl")

#install.packages("car")
library(car)

options("scipen"= 100)

mydata = read_excel("C:/Users/Matt/Desktop/UNCC/UNCC Spring 20/STAT 2223-002 Pramesh Subedi - Elements of Statistics II/STAT Final Project/Final Project/CraigslistCarData.xlsx")

y = mydata$Price

#Quantitative Variables
x1 = mydata$Age #in years
x2 = mydata$Cylinders
x3 = mydata$Odometer
x4 = mydata$Min_Price
x5 = mydata$Max_Price
x6 = mydata$Rating

#Qualitative Variables

#Title Status
x7 = ifelse(mydata$`Title_Status` == "rebuilt", 1, 0) #Clean is base
x8 = ifelse(mydata$`Title_Status` == "salvage", 1, 0) #Clean is base

#Size
x9 = ifelse(mydata$Size == "compact", 1, 0) #Full-size is base
x10 = ifelse(mydata$Size == "mid-size", 1, 0) #Full-size is base

#Color
x11 = ifelse(mydata$Color == "white", 1, 0) #Brown is base
x12 = ifelse(mydata$Color == "silver", 1, 0) #Brown is base
x13 = ifelse(mydata$Color == "black", 1, 0) #Brown is base
x14 = ifelse(mydata$Color == "grey", 1, 0) #Brown is base
x15 = ifelse(mydata$Color == "red", 1, 0) #Brown is base
x16 = ifelse(mydata$Color == "blue", 1, 0) #Brown is base

#Vehicle Type
x17 = ifelse(mydata$Type == "coupe", 1, 0) #Truck is base
x18 = ifelse(mydata$Type == "sedan", 1, 0) #Truck is base
x19 = ifelse(mydata$Type == "SUV", 1, 0) #Truck is base
x20 = ifelse(mydata$Type == "van", 1, 0) #Truck is base

#Vehicle Make
x21 = ifelse(mydata$Make == "Ford", 1, 0) #Nissan is base
x22 = ifelse(mydata$Make == "Chevy", 1, 0) #Nissan is base
x23 = ifelse(mydata$Make == "Honda", 1, 0) #Nissan is base
x24 = ifelse(mydata$Make == "Toyota", 1, 0) #Nissan is base
x25 = ifelse(mydata$Make == "Dodge", 1, 0) #Nissan is base
x26 = ifelse(mydata$Make == "GMC", 1, 0) #Nissan is base
x27 = ifelse(mydata$Make == "Jeep", 1, 0) #Nissan is base
x28 = ifelse(mydata$Make == "BMW", 1, 0) #Nissan is base

x29 = mydata$`Num Doors`

null.model = lm(y~1,data=mydata)
full.model = lm(y~x1+x2+x3+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x25+x26+x27+x28+x29, data=mydata)summary(full.model)

step(null.model, scope = list(upper=full.model), data=mydata, direction="both")

full.model2 = regsubsets(y~x1+x2+x3+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x25+x26+x27+x28+x29, data=mydata, nvmax = 27)
summary2 = summary(full.model2)
summary2

summary2$adjr2
summary2$rsq
summary2$rss
summary2$cp
summary2$bic

which.max(summary2$adjr2)
which.max(summary2$rsq)
which.min(summary2$rss)
which.min(summary2$cp)
which.min(summary2$bic)

full.model.y.hat=full.model$fitted.values 
full.model.res=full.model$residuals

coef(full.model2,8)

plot(x1,y, xlab="Age (Years)", ylab="Price", main="Price vs Age", col="6", pch=19)
plot(x2,y, xlab="Cylinders", ylab="Price", main="Price vs Cylinders", col="6", pch=19)
plot(x3,y, xlab="Odometer (Miles)", ylab="Price", main="Price vs Odometer", col="6", pch=19)
length(y)

fitted.model2 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25, data=mydata)

full.model2 = regsubsets(y~x1+x2+x3+x8+x18+x21+x25, data = mydata)
full.model2 = lm((y^.5)~x1+x2+x3+x8+x18+x21+x25, data = mydata)

model2.1 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25+(x1*x2)+(x1*x3)+(x1*x8)+(x1*x11)+(x1*x18)+(x1*x21)+(x1*x25)+(x2*x3)+(x2*x8)+(x2*x11)+(x2*x18)+(x2*x21)+(x2*x25)+(x3*x8)+(x3*x11)+(x3*x18)+(x3*x21)+(x3*x25)+(x1*x2*x3)+(x1*x2*x8)+(x1*x2*x11)+(x1*x2*x18)+(x1*x2*x21)+(x1*x2*x25)+(x1*x3*x8)+(x1*x3*x11)+(x1*x3*x18)+(x1*x3*x21)+(x1*x3*x25)+(x2*x3*x8)+(x2*x3*x11)+(x2*x3*x18)+(x2*x3*x21)+(x2*x3*x25)+I(x1^2)+I(x2^2)+I(x3^2)+(I(x1^2)*x2)+(I(x1^2)*x3)+(I(x1^2)*x8)+(I(x1^2)*x11)+(I(x1^2)*x18)+(I(x1^2)*x21)+(I(x1^2)*x25)+(I(x2^2)*x1)+(I(x2^2)*x3)+(I(x2^2)*x8)+(I(x2^2)*x11)+(I(x2^2)*x18)+(I(x2^2)*x21)+(I(x2^2)*x25)+(I(x3^2)*x1)+(I(x3^2)*x2)+(I(x3^2)*x8)+(I(x3^2)*x11)+(I(x3^2)*x18)+(I(x3^2)*x21)+(I(x3^2)*x25)+(I(x1^2)*x2*x3*x8)+(I(x1^2)*x2*x3*x11)+(I(x1^2)*x2*x3*x18)+(I(x1^2)*x2*x3*x19)+(I(x1^2)*x2*x3*x21)+(I(x1^2)*x2*x3*x25)+(I(x2^2)*x1*x3*x8)+(I(x2^2)*x1*x3*x11)+(I(x2^2)*x1*x3*x18)+(I(x2^2)*x1*x3*x21)+(I(x2^2)*x1*x3*x25)+(I(x3^2)*x1*x2*x8)+(I(x3^2)*x1*x2*x11)+(I(x3^2)*x1*x2*x18)+(I(x3^2)*x1*x2*x21)+(I(x3^2)*x1*x2*x25)+(I(x1^2)*x2*x3)+(I(x1^2)*x2*x8)+(I(x1^2)*x2*x11)+(I(x1^2)*x2*x18)+(I(x1^2)*x2*x21)+(I(x1^2)*x2*x25)+(I(x1^2)*x3*x8)+(I(x1^2)*x3*x11)+(I(x1^2)*x3*x18)+(I(x1^2)*x3*x21)+(I(x1^2)*x3*x25)+(I(x2^2)*x1*x3)+(I(x2^2)*x1*x8)+(I(x2^2)*x1*x11)+(I(x2^2)*x1*x18)+(I(x2^2)*x1*x21)+(I(x2^2)*x1*x25)+(I(x2^2)*x3*x8)+(I(x2^2)*x3*x11)+(I(x2^2)*x3*x18)+(I(x2^2)*x3*x21)+(I(x2^2)*x3*x25)+(I(x3^2)*x1*x3)+(I(x3^2)*x1*x8)+(I(x3^2)*x1*x11)+(I(x3^2)*x1*x18)+(I(x3^2)*x1*x21)+(I(x3^2)*x1*x25)+(I(x3^2)*x2*x8)+(I(x3^2)*x2*x11)+(I(x3^2)*x2*x18)+(I(x3^2)*x2*x21)+(I(x3^2)*x2*x25)+(I(x1^2)*I(x2^2)*I(x3^2)*x8)+(I(x1^2)*I(x2^2)*I(x3^2)*x11)+(I(x1^2)*I(x2^2)*I(x3^2)*x18)+(I(x1^2)*I(x2^2)*I(x3^2)*x21)+(I(x1^2)*I(x2^2)*I(x3^2)*x25)+(I(x1^2)*I(x2^2)*x3)+(I(x1^2)*I(x2^2)*x8)+(I(x1^2)*I(x2^2)*x11)+(I(x1^2)*I(x2^2)*x18)+(I(x1^2)*I(x2^2)*x21)+(I(x1^2)*I(x2^2)*x25)+(I(x1^2)*I(x3^2)*x2)+(I(x1^2)*I(x3^2)*x8)+(I(x1^2)*I(x3^2)*x11)+(I(x1^2)*I(x3^2)*x18)+(I(x1^2)*I(x3^2)*x21)+(I(x1^2)*I(x3^2)*x25)+(I(x2^2)*I(x3^2)*x1)+(I(x2^2)*I(x3^2)*x8)+(I(x2^2)*I(x3^2)*x11)+(I(x2^2)*I(x3^2)*x18)+ (I(x2^2)*I(x3^2)*x21)+(I(x2^2)*I(x3^2)*x25))
anova(model2.2, model2.1)
model2.2 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25+(x1*x2)+(x1*x3)+(x1*x8)+(x1*x11)+(x1*x18)+(x1*x21)+(x1*x25)+(x2*x3)+(x2*x8)+(x2*x11)+(x2*x18)+(x2*x21)+(x2*x25)+(x3*x8)+(x3*x11)+(x3*x18)+(x3*x21)+(x3*x25)+(x1*x2*x3)+(x1*x2*x8)+(x1*x2*x11)+(x1*x2*x18)+(x1*x2*x21)+(x1*x2*x25)+(x1*x3*x8)+(x1*x3*x11)+(x1*x3*x18)+(x1*x3*x21)+(x1*x3*x25)+(x2*x3*x8)+(x2*x3*x11)+(x2*x3*x18)+(x2*x3*x21)+(x2*x3*x25)+I(x1^2)+I(x2^2)+I(x3^2)+(I(x1^2)*x2)+(I(x1^2)*x3)+(I(x1^2)*x8)+(I(x1^2)*x11)+(I(x1^2)*x18)+(I(x1^2)*x21)+(I(x1^2)*x25)+(I(x2^2)*x1)+(I(x2^2)*x3)+(I(x2^2)*x8)+(I(x2^2)*x11)+(I(x2^2)*x18)+(I(x2^2)*x21)+(I(x2^2)*x25)+(I(x3^2)*x1)+(I(x3^2)*x2)+(I(x3^2)*x8)+(I(x3^2)*x11)+(I(x3^2)*x18)+(I(x3^2)*x21)+(I(x3^2)*x25)+(I(x1^2)*x2*x3*x8)+(I(x1^2)*x2*x3*x11)+(I(x1^2)*x2*x3*x18)+(I(x1^2)*x2*x3*x19)+(I(x1^2)*x2*x3*x21)+(I(x1^2)*x2*x3*x25)+(I(x2^2)*x1*x3*x8)+(I(x2^2)*x1*x3*x11)+(I(x2^2)*x1*x3*x18)+(I(x2^2)*x1*x3*x21)+(I(x2^2)*x1*x3*x25)+(I(x3^2)*x1*x2*x8)+(I(x3^2)*x1*x2*x11)+(I(x3^2)*x1*x2*x18)+(I(x3^2)*x1*x2*x21)+(I(x3^2)*x1*x2*x25)+(I(x1^2)*x2*x3)+(I(x1^2)*x2*x8)+(I(x1^2)*x2*x11)+(I(x1^2)*x2*x18)+(I(x1^2)*x2*x21)+(I(x1^2)*x2*x25)+(I(x1^2)*x3*x8)+(I(x1^2)*x3*x11)+(I(x1^2)*x3*x18)+(I(x1^2)*x3*x21)+(I(x1^2)*x3*x25)+(I(x2^2)*x1*x3)+(I(x2^2)*x1*x8)+(I(x2^2)*x1*x11)+(I(x2^2)*x1*x18)+(I(x2^2)*x1*x21)+(I(x2^2)*x1*x25)+(I(x2^2)*x3*x8)+(I(x2^2)*x3*x11)+(I(x2^2)*x3*x18)+(I(x2^2)*x3*x21)+(I(x2^2)*x3*x25)+(I(x3^2)*x1*x3)+(I(x3^2)*x1*x8)+(I(x3^2)*x1*x11)+(I(x3^2)*x1*x18)+(I(x3^2)*x1*x21)+(I(x3^2)*x1*x25)+(I(x3^2)*x2*x8)+(I(x3^2)*x2*x11)+(I(x3^2)*x2*x18)+(I(x3^2)*x2*x21)+(I(x3^2)*x2*x25)+(I(x1^2)*I(x2^2)*I(x3^2)*x8)+(I(x1^2)*I(x2^2)*I(x3^2)*x11)+(I(x1^2)*I(x2^2)*I(x3^2)*x18)+(I(x1^2)*I(x2^2)*I(x3^2)*x21)+(I(x1^2)*I(x2^2)*I(x3^2)*x25))
anova(model2.3, model2.2)
model2.3 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25+(x1*x2)+(x1*x3)+(x1*x8)+(x1*x11)+(x1*x18)+(x1*x21)+(x1*x25)+(x2*x3)+(x2*x8)+(x2*x11)+(x2*x18)+(x2*x21)+(x2*x25)+(x3*x8)+(x3*x11)+(x3*x18)+(x3*x21)+(x3*x25)+(x1*x2*x3)+(x1*x2*x8)+(x1*x2*x11)+(x1*x2*x18)+(x1*x2*x21)+(x1*x2*x25)+(x1*x3*x8)+(x1*x3*x11)+(x1*x3*x18)+(x1*x3*x21)+(x1*x3*x25)+(x2*x3*x8)+(x2*x3*x11)+(x2*x3*x18)+(x2*x3*x21)+(x2*x3*x25)+I(x1^2)+I(x2^2)+I(x3^2)+(I(x1^2)*x2)+(I(x1^2)*x3)+(I(x1^2)*x8)+(I(x1^2)*x11)+(I(x1^2)*x18)+(I(x1^2)*x21)+(I(x1^2)*x25)+(I(x2^2)*x1)+(I(x2^2)*x3)+(I(x2^2)*x8)+(I(x2^2)*x11)+(I(x2^2)*x18)+(I(x2^2)*x21)+(I(x2^2)*x25)+(I(x3^2)*x1)+(I(x3^2)*x2)+(I(x3^2)*x8)+(I(x3^2)*x11)+(I(x3^2)*x18)+(I(x3^2)*x21)+(I(x3^2)*x25)+(I(x1^2)*x2*x3*x8)+(I(x1^2)*x2*x3*x11)+(I(x1^2)*x2*x3*x18)+(I(x1^2)*x2*x3*x19)+(I(x1^2)*x2*x3*x21)+(I(x1^2)*x2*x3*x25)+(I(x2^2)*x1*x3*x8)+(I(x2^2)*x1*x3*x11)+(I(x2^2)*x1*x3*x18)+(I(x2^2)*x1*x3*x21)+(I(x2^2)*x1*x3*x25)+(I(x3^2)*x1*x2*x8)+(I(x3^2)*x1*x2*x11)+(I(x3^2)*x1*x2*x18)+(I(x3^2)*x1*x2*x21)+(I(x3^2)*x1*x2*x25)+(I(x1^2)*x2*x3)+(I(x1^2)*x2*x8)+(I(x1^2)*x2*x11)+(I(x1^2)*x2*x18)+(I(x1^2)*x2*x21)+(I(x1^2)*x2*x25)+(I(x1^2)*x3*x8)+(I(x1^2)*x3*x11)+(I(x1^2)*x3*x18)+(I(x1^2)*x3*x21)+(I(x1^2)*x3*x25)+(I(x2^2)*x1*x3)+(I(x2^2)*x1*x8)+(I(x2^2)*x1*x11)+(I(x2^2)*x1*x18)+(I(x2^2)*x1*x21)+(I(x2^2)*x1*x25)+(I(x2^2)*x3*x8)+(I(x2^2)*x3*x11)+(I(x2^2)*x3*x18)+(I(x2^2)*x3*x21)+(I(x2^2)*x3*x25)+(I(x3^2)*x1*x3)+(I(x3^2)*x1*x8)+(I(x3^2)*x1*x11)+(I(x3^2)*x1*x18)+(I(x3^2)*x1*x21)+(I(x3^2)*x1*x25)+(I(x3^2)*x2*x8)+(I(x3^2)*x2*x11)+(I(x3^2)*x2*x18)+(I(x3^2)*x2*x21)+(I(x3^2)*x2*x25))
anova(model2.4, model2.3)
model2.4 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25+(x1*x2)+(x1*x3)+(x1*x8)+(x1*x11)+(x1*x18)+(x1*x21)+(x1*x25)+(x2*x3)+(x2*x8)+(x2*x11)+(x2*x18)+(x2*x21)+(x2*x25)+(x3*x8)+(x3*x11)+(x3*x18)+(x3*x21)+(x3*x25)+(x1*x2*x3)+(x1*x2*x8)+(x1*x2*x11)+(x1*x2*x18)+(x1*x2*x21)+(x1*x2*x25)+(x1*x3*x8)+(x1*x3*x11)+(x1*x3*x18)+(x1*x3*x21)+(x1*x3*x25)+(x2*x3*x8)+(x2*x3*x11)+(x2*x3*x18)+(x2*x3*x21)+(x2*x3*x25)+I(x1^2)+I(x2^2)+I(x3^2)+(I(x1^2)*x2)+(I(x1^2)*x3)+(I(x1^2)*x8)+(I(x1^2)*x11)+(I(x1^2)*x18)+(I(x1^2)*x21)+(I(x1^2)*x25)+(I(x2^2)*x1)+(I(x2^2)*x3)+(I(x2^2)*x8)+(I(x2^2)*x11)+(I(x2^2)*x18)+(I(x2^2)*x21)+(I(x2^2)*x25)+(I(x3^2)*x1)+(I(x3^2)*x2)+(I(x3^2)*x8)+(I(x3^2)*x11)+(I(x3^2)*x18)+(I(x3^2)*x21)+(I(x3^2)*x25)+(I(x1^2)*x2*x3*x8)+(I(x1^2)*x2*x3*x11)+(I(x1^2)*x2*x3*x18)+(I(x1^2)*x2*x3*x19)+(I(x1^2)*x2*x3*x21)+(I(x1^2)*x2*x3*x25)+(I(x2^2)*x1*x3*x8)+(I(x2^2)*x1*x3*x11)+(I(x2^2)*x1*x3*x18)+(I(x2^2)*x1*x3*x21)+(I(x2^2)*x1*x3*x25)+(I(x3^2)*x1*x2*x8)+(I(x3^2)*x1*x2*x11)+(I(x3^2)*x1*x2*x18)+(I(x3^2)*x1*x2*x21)+(I(x3^2)*x1*x2*x25))
anova(model2.5, model2.4)
model2.5 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25+(x1*x2)+(x1*x3)+(x1*x8)+(x1*x11)+(x1*x18)+(x1*x21)+(x1*x25)+(x2*x3)+(x2*x8)+(x2*x11)+(x2*x18)+(x2*x21)+(x2*x25)+(x3*x8)+(x3*x11)+(x3*x18)+(x3*x21)+(x3*x25)+(x1*x2*x3)+(x1*x2*x8)+(x1*x2*x11)+(x1*x2*x18)+(x1*x2*x21)+(x1*x2*x25)+(x1*x3*x8)+(x1*x3*x11)+(x1*x3*x18)+(x1*x3*x21)+(x1*x3*x25)+(x2*x3*x8)+(x2*x3*x11)+(x2*x3*x18)+(x2*x3*x21)+(x2*x3*x25)+I(x1^2)+I(x2^2)+I(x3^2)+(I(x1^2)*x2)+(I(x1^2)*x3)+(I(x1^2)*x8)+(I(x1^2)*x11)+(I(x1^2)*x18)+(I(x1^2)*x21)+(I(x1^2)*x25)+(I(x2^2)*x1)+(I(x2^2)*x3)+(I(x2^2)*x8)+(I(x2^2)*x11)+(I(x2^2)*x18)+(I(x2^2)*x21)+(I(x2^2)*x25)+(I(x3^2)*x1)+(I(x3^2)*x2)+(I(x3^2)*x8)+(I(x3^2)*x11)+(I(x3^2)*x18)+(I(x3^2)*x21)+(I(x3^2)*x25))
anova(model2.6, model2.5)
model2.6 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25+(x1*x2)+(x1*x3)+(x1*x8)+(x1*x11)+(x1*x18)+(x1*x21)+(x1*x25)+(x2*x3)+(x2*x8)+(x2*x11)+(x2*x18)+(x2*x21)+(x2*x25)+(x3*x8)+(x3*x11)+(x3*x18)+(x3*x21)+(x3*x25)+(x1*x2*x3)+(x1*x2*x8)+(x1*x2*x11)+(x1*x2*x18)+(x1*x2*x21)+(x1*x2*x25)+(x1*x3*x8)+(x1*x3*x11)+(x1*x3*x18)+(x1*x3*x21)+(x1*x3*x25)+(x2*x3*x8)+(x2*x3*x11)+(x2*x3*x18)+(x2*x3*x21)+(x2*x3*x25)+I(x1^2)+I(x2^2)+I(x3^2))
anova(model2.7, model2.6)
model2.7 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25+(x1*x2)+(x1*x3)+(x1*x8)+(x1*x11)+(x1*x18)+(x1*x21)+(x1*x25)+(x2*x3)+(x2*x8)+(x2*x11)+(x2*x18)+(x2*x21)+(x2*x25)+(x3*x8)+(x3*x11)+(x3*x18)+(x3*x21)+(x3*x25)+(x1*x2*x3)+(x1*x2*x8)+(x1*x2*x11)+(x1*x2*x18)+(x1*x2*x21)+(x1*x2*x25)+(x1*x3*x8)+(x1*x3*x11)+(x1*x3*x18)+(x1*x3*x21)+(x1*x3*x25)+(x2*x3*x8)+(x2*x3*x18)+(x2*x3*x21)+(x2*x3*x25))
anova(model2.8, model2.7)
model2.8 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25+(x1*x2)+(x1*x3)+(x1*x8)+(x1*x11)+(x1*x18)+(x1*x21)+(x1*x25)+(x2*x3)+(x2*x8)+(x2*x11)+(x2*x18)+(x2*x21)+(x2*x25)+(x3*x8)+(x3*x11)+(x3*x18)+(x3*x21)+(x3*x25))
model2.8 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25+(x1*x3)+(x1*x11)+(x2*x3)+(x2*x11)+(x2*x18)+(x2*x25)+(x3*x8)+(x3*x11))
anova(model2.9, model2.8)
model2.9 = lm(y~x1+x2+x3+x8+x11+x18+x21+x25)

fit = lm(y~x1+x2+x3+x8+x11+x18+x21+x25+(x1*x3)+(x1*x11)+(x2*x3)+(x2*x11)+(x2*x18)+(x2*x25)+(x3*x8)+(x3*x11)+(x1*x2*x3)+(x1*x3*x8)+(x2*x3*x11)+I(x1^2)+I(x2^2)+(I(x1^2)*x2)+(I(x1^2)*x3)+(I(x1^2)*x21)+(I(x1^2)*x25)+(I(x2^2)*x1)+(I(x2^2)*x25)+(I(x3^2)*x1)+(I(x3^2)*x25)+(I(x1^2)*x2*x3*x8)+(I(x1^2)*x2*x3*x11)+(I(x1^2)*x2*x3*x19)+(I(x1^2)*x2*x3*x25)+(I(x2^2)*x1*x3*x21)+(I(x3^2)*x1*x2*x21)+(I(x3^2)*x1*x2*x25))
summary(fit)

plot(x1,fit$residuals, ylab="Residuals", xlab="Age (years)", col="blue", pch=20, main="Residuals vs Age")
plot(x2,fit$residuals, ylab="Residuals", xlab="Cylinders", col="blue", pch=20, main="Residuals vs Cylinders")
plot(x3,fit$residuals, ylab="Residuals", xlab="Odometer (miles)", col="blue", pch=20, main="Residuals vs Odometer")
abline(0,0,lty=2, col="6")

ncvTest(fit)
plot(y,res, main="Residuals Versus PRICE", ylab="Residuals", xlab="PRICE", pch=19, col="6")

y.hat = fit$fitted.values
res = fit$residuals
plot(y.hat,res, main="Residuals Versus the Fitted Values", ylab="Residuals", xlab="Fitted value", pch=19, col="6")

shapiro.test(res)
qqnorm(fit$residuals)
qqline(fit$residuals)

anova(fit)
View(fit$residuals)
hatvalues(fit)
View(hatvalues(fit))
View(stdres(fit))
View(rstandard(fit))
rstandard(fit)
View(rstudent(fit))
rstudent(fit)
View(cooks.distance(fit))
cooks.distance(fit)
View(fit)
View(dfbetas(fit))
View(dffits(fit))
dffits(fit)

set.seed(100)
trainingRowIndex=sample(1:nrow(mydata),0.8*nrow(mydata))
trainingData = mydata[trainingRowIndex, ]
testData = mydata[-trainingRowIndex, ]
pricePredict = predict(fit, mydata)
real_price <- data.frame(cbind(actuals=mydata$Price, predicteds=pricePredict))
correlation_accuracy=cor(real_price)
correlation_accuracy
head(real_price)



mydata = read_excel("C:/Users/Matt/Desktop/UNCC/UNCC Spring 20/STAT 2223-002 Pramesh Subedi - Elements of Statistics II/STAT Final Project/Final Project/CraigslistCarDataV2.xlsx")
y = mydata$Price

#Quantitative Variables
x1 = mydata$Age #in years
x2 = mydata$Cylinders
x3 = mydata$Odometer
x4 = mydata$Min_Price
x5 = mydata$Max_Price
x6 = mydata$Rating

#Qualitative Variables

#Title Status
x7 = ifelse(mydata$`Title_Status` == "rebuilt", 1, 0) #Clean is base
x8 = ifelse(mydata$`Title_Status` == "salvage", 1, 0) #Clean is base

#Size
x9 = ifelse(mydata$Size == "compact", 1, 0) #Full-size is base
x10 = ifelse(mydata$Size == "mid-size", 1, 0) #Full-size is base

#Color
x11 = ifelse(mydata$Color == "white", 1, 0) #Brown is base
x12 = ifelse(mydata$Color == "silver", 1, 0) #Brown is base
x13 = ifelse(mydata$Color == "black", 1, 0) #Brown is base
x14 = ifelse(mydata$Color == "grey", 1, 0) #Brown is base
x15 = ifelse(mydata$Color == "red", 1, 0) #Brown is base
x16 = ifelse(mydata$Color == "blue", 1, 0) #Brown is base

#Vehicle Type
x17 = ifelse(mydata$Type == "coupe", 1, 0) #Truck is base
x18 = ifelse(mydata$Type == "sedan", 1, 0) #Truck is base
x19 = ifelse(mydata$Type == "SUV", 1, 0) #Truck is base
x20 = ifelse(mydata$Type == "van", 1, 0) #Truck is base

#Vehicle Make
x21 = ifelse(mydata$Make == "Ford", 1, 0) #Nissan is base
x22 = ifelse(mydata$Make == "Chevy", 1, 0) #Nissan is base
x23 = ifelse(mydata$Make == "Honda", 1, 0) #Nissan is base
x24 = ifelse(mydata$Make == "Toyota", 1, 0) #Nissan is base
x25 = ifelse(mydata$Make == "Dodge", 1, 0) #Nissan is base
x26 = ifelse(mydata$Make == "GMC", 1, 0) #Nissan is base
x27 = ifelse(mydata$Make == "Jeep", 1, 0) #Nissan is base
x28 = ifelse(mydata$Make == "BMW", 1, 0) #Nissan is base

x29 = mydata$`Num Doors`

set.seed(100)
trainingRowIndex=sample(1:nrow(mydata),0.8*nrow(mydata))
trainingData = mydata[trainingRowIndex, ]
testData = mydata[-trainingRowIndex, ]
pricePredict = predict(fit, mydata)
real_price <- data.frame(cbind(actuals=mydata$Price, predicteds=pricePredict))
correlation_accuracy=cor(real_price)
correlation_accuracy
head(real_price)
