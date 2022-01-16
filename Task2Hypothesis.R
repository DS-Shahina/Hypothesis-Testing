"Assume Null hyposthesis as Ho: (There is no difference in the average Turn Around Time (TAT)) 
Thus Alternate hypothesis as Ha: (There is significant difference in the average Turn Around Time (TAT)) 
"
########## Anova ##########
# Load the Dataset
library(readr)
# Load the data: LabTAT.csv Data
Lab <- read_csv(file.choose())
View(Lab)
attach(Lab)

# Normality test
# Ho: Data are normal
# Ha: Data are not normal --> take action = transformation

# shapiro test
shapiro.test(`Laboratory_1`)
# p-value = 0.4232 > 0.05 so p high null fly => It follows normal distribution
shapiro.test(`Laboratory_2`)
# p-value = 0.8637 > 0.05 so p high null fly => It follows normal distribution
shapiro.test(`Laboratory_3`)
# p-value = 0.06547 > 0.05 so p high null fly => It follows normal distribution
shapiro.test(`Laboratory_4`)
# p-value = 0.6619 > 0.05 so p high null fly => It follows normal distribution

# Variance test
var.test(`Laboratory_1`, `Laboratory_2`)
# p-value = 0.4341 > 0.05 so p high null fly => Equal variances
# Variances are equal, of both the variables.
var.test(`Laboratory_2`, `Laboratory_3`)
# p-value = 0.5531 > 0.05 so p high null fly => Equal variances
# Variances are equal, of both the variables.
var.test(`Laboratory_3`, `Laboratory_4`)
# p-value = 0.6168 > 0.05 so p high null fly => Equal variances
# Variances are equal, of both the variables.
var.test(`Laboratory_4`, `Laboratory_1`)
# p-value = 0.3817 > 0.05 so p high null fly => Equal variances
# Variances are equal, of both the variables.

Stacked_Data <- stack(Lab) #combining
?stack
View(Stacked_Data)

attach(Stacked_Data)
colnames(Stacked_Data)

Anova_results <- aov(values ~ ind, data = Stacked_Data)
summary(Anova_results)

# p-value = 2e-16 < 0.05 Reject null hypothesis
#There is difference in average TAT among the different laboratories - 4 laboratories

