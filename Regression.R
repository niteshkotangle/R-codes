###################################################
############# Orange Juice Regression #############
###################################################

## read in the data
#(brand : orange juice brand
#sales : weekly sales volume
#price : price per carton
#feat : whether the product is featured (If featured, feat=1; otherwise
                                         
#modePrice : the mode price the most frequent price for the brand, which can be considered as the regular price
#discount : if the price is below the modePrice , it is considered as having discount, otherwise it is not having discount (If having discount, discount=1; otherwise 0)

oj <- read.csv("oj.csv") 
head(oj)
dim(oj)
summary (oj)


levels(oj$brand)

# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# ModePrice = aggregate(price~brand, data = oj, getmode)
# names(ModePrice)[names(ModePrice) == "price"] <- "modePrice"
# oj = merge(oj, ModePrice)
# oj$discount = ifelse(oj$price < oj$modePrice, 1, 0)
# write.csv(oj, "oj2.csv", row.names = FALSE)


#What is the average sales when feat=0 and feat=1?

aggregate(sales~feat, data = oj, mean)

summary(oj[oj$feat == 0, "sales"])
summary(oj[oj$feat == 1, "sales"])

feat=0: 10071.17

feat=1: 40590.47

Difference = 40590.47 10071.17 = 30519.3

# lm is a fucntion to run regression 

aggregate(price~feat, data = oj, mean)

aggregate(price~brand, data = oj, max)
aggregate(price~brand, data = oj, mean)

#Given the product is featured, how likely it is also having a price discount?
aggregate(discount~feat, data = oj, mean)

#feat=0: there was a price discount 25% of the time
#feat=1: there was a price discount 44% of the time

aggregate(sales~discount, data = oj, mean)

reg1 = lm(sales~feat, data = oj)
summary(reg1)

#feat 0 = 10071.17
#feat 1 = 40590.47
#Difference = 30519.3
# Run a regression model to investigate the impact of discount on sales:

reg2 = lm(sales~discount, data = oj)
summary(reg2)

reg3 = lm(sales~price, data = oj)
summary(reg3)

#So if we only consider product feature, we may omit the important factor price dicountMaybe feature is not as important as we thought.

plot(oj$price,oj$sales, main = "Price and sales",xlab = "Price", ylab = "Sales")
abline(lm(sales~price, data = oj), col = "blue")

reg4 = lm(sales ~ feat + discount, data = oj)
summary(reg4)
# Compare with regression with single independent variable
summary(reg1)
summary(reg2)
# Comparing the coefficients on "feat" in Reg4 and Reg1, are they the same.Whichregression model gives larger coefficient on "feat"

#Comparing the coefficients on "discount" in Reg4 and Reg2, are they the sameWhich regression model gives larger coefficient on "discount"


reg5 = lm(sales ~ feat + price, data = oj)
summary(reg5)
summary(reg3)

summary(lm(sales ~ price, data = oj[oj$feat==0,]))
summary(lm(sales ~ price, data = oj[oj$feat==1,]))

# The coefficient magnitude on price is way larger when the product is featured.

#For the regression model
????????????????????=????0+????1???????????????????????+????2???????????????????????????????+????3??????????????????????????????????????????????????????

oj$feat_price = oj$feat * oj$price
reg6 = lm(sales ~ feat + price + feat_price, data = oj)
summary(reg6)

# here we see how the impact of the first variable on sales volume may depend on the value of the second variable. When the price and the feat are combines, their impact is less than the sum of the individual effects
