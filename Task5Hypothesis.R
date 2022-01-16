"Fantaloons Sales managers commented want to know % of males versus females walking into the store differ based on day of the week.                 
"
# Load the Dataset
library(readr)
# Load the data: Fantaloons Data
Fantaloons <- read_csv(file.choose())
View(Fantaloons)

########### Proportional T Test ##########

attach(Fantaloons)

table1 <- table(Weekdays)
table1
table2 <- table(Weekend)
table2
table3 <- table(Weekdays, Weekend)
table3

?prop.test
prop.test(x = c(167,47), n = c(233,167), conf.level = 0.95, alternative = "two.sided")
# two.sided -> means checking for equal proportions of Male and Female based on day of the week
# p-value = 2.2e-16 < 0.05 reject Null hypothesis i.e.
# Unequal proportions 

prop.test(x = c(167,47), n = c(233,167), conf.level = 0.95, alternative = "greater")
# Ho -> Proportions of Male > Proportions of Female
# Ha -> Proportions of Female > Proportions of Male
# p-value = 2.2e-16 < 0.05 reject null hypothesis 
# so proportion of Female > proportion of Male 

#P-value <0.05 and hence we reject null. We reject null Hypothesis. Hence proportion of Female is greater than Male.


