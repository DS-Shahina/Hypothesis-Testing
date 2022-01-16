"Assume Null hyposthesis as Ho: μ1 = μ2 (There is no difference in diameters of cutlets between two units) Thus Alternate hypothesis as Ha: μ1 ≠ μ2 (There is significant difference in diameters of cutlets between two units) 
"

import pandas as pd
import scipy
from scipy import stats

############ 2 sample T Test ##################

# Load the data
cutlet = pd.read_csv("D:/C DRIVE-SSD DATA backup 15-12-2020/Desktop/360DigiTmg Assignment/Hypothesis Testing/Cutlets.csv")
cutlet

Cutlet = cutlet.dropna()
Cutlet.columns = "UnitA", "UnitB" ##renaming so that no sapces is there otherwise error.

UnitA=pd.Series(Cutlet.iloc[:,0])
UnitA

UnitB=pd.Series(Cutlet.iloc[:,1])
UnitB

## Normality test
# Ho: Data are normal
# Ha: Data are not normal --> take action = transformation

# Normality Test
stats.shapiro(Cutlet.UnitA) # Shapiro Test
#ShapiroResult(statistic=0.9649458527565002, pvalue=0.3199819028377533)
# p-value = 0.3199819028377533 > 0.05 so p high null fly => It follows normal distribution


print(stats.shapiro(Cutlet.UnitB))
help(stats.shapiro)
#ShapiroResult(statistic=0.9727300405502319, pvalue=0.5224985480308533)
# p-value = 0.5224985480308533 > 0.05 so p high null fly => It follows normal distribution

# Variance test
scipy.stats.levene(Cutlet.UnitA, Cutlet.UnitB)
help(scipy.stats.levene)
# p-value = 0.4176162212502553 > 0.05 so p high null fly => Equal variances
#LeveneResult(statistic=0.665089763863238, pvalue=0.4176162212502553)

# 2 Sample T test
scipy.stats.ttest_ind(Cutlet.UnitA, Cutlet.UnitB)
help(scipy.stats.ttest_ind)
#Ttest_indResult(statistic=0.7228688704678061, pvalue=0.4722394724599501)
# p-value = 0.4722394724599501 > 0.05 => p High null fly => accept Null hypothesis

# Conclusion:
# There is no difference in diameters of cutlets between two units