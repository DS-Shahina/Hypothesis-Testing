# Hypothesis Testing

# Load the Dataset
library(readr)

# 2 sample t Test

######## Cutlets.csv data ##########
# Business Problem: A F&B manager wants to determine whether there is any significant difference in the diameter of the cutlet between two units. 
# Ho: ??1 = ??2 (There is no difference in diameters of cutlets between two units) --> no action
# Ha: ??1 ??? ??2 (There is significant difference in diameters of cutlets between two units) --> take action

Cutlets <- read_csv(file.choose())
View(Cutlets)
# Changing column names
colnames(Cutlets) <- c("UNIT A", "UNIT B") #renaming so that no sapces is there.

################################################
## Missing values - imputation

# Omitting NA values from the Data 
Cutlets_omit <- na.omit(Cutlets) # na.omit => will omit the rows which has atleast 1 NA value
dim(Cutlets_omit)
sum(is.na(Cutlets_omit)) 

View(Cutlets_omit)
attach(Cutlets_omit)

# Normality test
# Ho: Data are normal
# Ha: Data are not normal --> take action = transformation

# shapiro test
shapiro.test(`UNIT A`)
# p-value = 0.32 > 0.05 so p high null fly => It follows normal distribution

shapiro.test(`UNIT B`)
# p-value = 0.5225 > 0.05 so p high null fly => It follows normal distribution

# Variance test
var.test(`UNIT A`, `UNIT B`)
# p-value = 0.3136 > 0.05 so p high null fly => Equal variances
# we fail to reject null hypothesis, go with null hypothesis H0
# Variances are equal, of both the variables.

# 2 sample t Test assuming equal variances
t.test(`UNIT A`, `UNIT B`, alternative = "two.sided", conf.level = 0.95, var.equal = T)

# alternative = "two.sided" means we are checking for equal and unequal means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal means
# p-value = 0.4722 > 0.05 accept Null Hypothesis equal means

?t.test
t.test(`UNIT A`, `UNIT B`, alternative = "greater")

# alternative = "greater means true difference is greater than 0
# Null Hypothesis -> (`UNIT A` - `UNIT B`) < 0
# Alternative Hypothesis -> (`UNIT B` - `UNIT A`) > 0
# p-value = 0.2362 > 0.05 => p High null fly => accept Null hypothesis

# Conclusion:
# There is no difference in diameters of cutlets between two units