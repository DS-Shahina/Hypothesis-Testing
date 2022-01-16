"Telecall uses 4 centers around the globe to process customer order forms.  
The manager wants to check whether the defective % varies by center.
"
# Load the Dataset
library(readr)
# Load the data: CustomerOrderform Data
COF <- read_csv(file.choose())
View(COF)
attach(COF)
summary(COF)
typeof(COF)
str(COF)

################################################
## Missing values - imputation

# Omitting NA values from the Data 
COF_omit <- na.omit(COF) # na.omit => will omit the rows which has atleast 1 NA value
dim(COF_omit)
sum(is.na(COF_omit)) 

View(COF_omit)
attach(COF_omit)

Stacked_Data <- stack(COF_omit) #combining
?stack
View(Stacked_Data)

attach(Stacked_Data)
colnames(Stacked_Data)


#Y is discrete and X Factor is discrete
######### Chi Square Test ##########

table(values,ind)

chisq.test(table(values,ind))
# p-value = 0.2771 > 0.05  => Accept null hypothesis
# => Inference : Inference : proportion of defective % across the center is same.

# All Proportions are equal 
