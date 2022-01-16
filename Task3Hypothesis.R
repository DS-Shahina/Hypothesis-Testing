"Assume Null hyposthesis as Ho:  (proportion of male and female across regions is not  same) 
Thus Alternate hypothesis as Ha:  (proportion of male and female across regions is same) 
"
# Load the Dataset
library(readr)
# Load the data: BuyerRatio Data
BuyerRatio <- read_csv(file.choose())
View(BuyerRatio)
attach(BuyerRatio)
summary(BuyerRatio)
typeof(BuyerRatio)
str(BuyerRatio)

#Y is discrete and X Factor is discrete
######### Chi Square Test ##########

table(East,`Observed Values`);table(West,`Observed Values`);table(North,`Observed Values`);table(South,`Observed Values`)

chisq.test(table(East,`Observed Values`))
chisq.test(table(West,`Observed Values`))
chisq.test(table(North,`Observed Values`))
chisq.test(table(South,`Observed Values`))
# p-value = 1 > 0.05  => Accept null hypothesis (All p-value)
# => Inference : proportion of male and female across regions is not same

# All Proportions are not equal 
