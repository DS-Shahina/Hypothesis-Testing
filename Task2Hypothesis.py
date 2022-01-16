"Assume Null hyposthesis as Ho: μ1 = μ2 = μ3 = μ4 (There is no difference in the average Turn Around Time (TAT)) Thus Alternate hypothesis as Ha: μ1 ≠ μ2 ≠ μ3 ≠ μ4 (There is significant difference in the average Turn Around Time (TAT)) 
"
import pandas as pd
import scipy
from scipy import stats

############# One - Way Anova ################

lab = pd.read_csv("D:/C DRIVE-SSD DATA backup 15-12-2020/Desktop/360DigiTmg Assignment/Hypothesis Testing/lab_tat_updated.csv")
lab

#check Null values
lab.isna().sum()
# Normality Test

# Ho: Data are normal
# Ha: Data are not normal --> take action = transformation

# Shapiro Test
stats.shapiro(lab.Laboratory_1) 
# p-value = 0.42317795753479004 > 0.05 so p high null fly => It follows normal distribution
stats.shapiro(lab.Laboratory_2) 
# p-value = 0.8637524843215942 > 0.05 so p high null fly => It follows normal distribution
stats.shapiro(lab.Laboratory_3)
# p-value = 0.06547004729509354 > 0.05 so p high null fly => It follows normal distribution 
stats.shapiro(lab.Laboratory_4) 
# p-value = 0.6618951559066772 > 0.05 so p high null fly => It follows normal distribution 

# Variance test
help(scipy.stats.levene)
# All 3 Laboratories are being checked for variances
scipy.stats.levene(lab.Laboratory_1, lab.Laboratory_2, lab.Laboratory_3, lab.Laboratory_4)
# p-value = 0.38107781677304564 > 0.05 so p high null fly => Equal variances
# Variances are equal, of all the 4 variables.
# One - Way Anova
F, p = stats.f_oneway(lab.Laboratory_1, lab.Laboratory_2, lab.Laboratory_3, lab.Laboratory_4)

# p value
p  # P low Null go
#There is difference in average TAT among the different laboratories - 4 laboratories

