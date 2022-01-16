"Fantaloons Sales managers commented want to know % of males versus females walking into the store differ based on day of the week.                 
"
import pandas as pd
import scipy
import scipy as sp
from scipy import stats

Fantaloons = pd.read_csv("D:/C DRIVE-SSD DATA backup 15-12-2020/Desktop/360DigiTmg Assignment/Hypothesis Testing/Fantaloons.csv")
Fantaloons

Fantaloons=Fantaloons.iloc[0:400, ]
######### 2-proportion test ###########
import numpy as np

from statsmodels.stats.proportion import proportions_ztest

tab1 = Fantaloons.Weekdays.value_counts()
tab1
tab2 = Fantaloons.Weekend.value_counts()
tab2

# crosstable table
pd.crosstab(Fantaloons.Weekdays, Fantaloons.Weekend)

count = np.array([280, 520]) #How many Male and Female
nobs = np.array([400, 400]) #Total number of Male and Female are there 

stats, pval = proportions_ztest(count, nobs, alternative = 'two-sided') 
print(pval) # Pvalue 0.0
# two. sided -> means checking for equal proportions of Male and Female 
# p-value < 0.05 accept alternate hypothesis i.e.
# Unequal proportions 

stats, pval = proportions_ztest(count, nobs, alternative = 'larger')
print(pval)  # Pvalue 1.0
# Ho -> Proportions of Female > Proportions of Male
# Ha -> Proportions of Male > Proportions of Female
# p-value > 0.05 accept null hypothesis 
# so proportion of Female > proportion of Male

#P-value <0.05 and hence we reject null. We reject null Hypothesis. Hence proportion of Female is greater than Male

"2nd Medthod"

Fantaloons.Weekdays.value_counts() #Female    287 Male      113
Fantaloons.Weekend.value_counts() #Female    233 Male      167

# Null Hypothesis= Percentage of males versus females walking in to the store does not differ based on day of the week
# Alternative hypothesis =  Percentage of males versus females walking in to the store differ based on day of the week

# contingency table
obs=np.array([[287,233],[113,167]])
obs

# In the contingency table we are inputting the observed frequencies 
# (i.e. number of occurrences) in each country
Chisquares_results = scipy.stats.chi2_contingency(obs)
chisquare=[['Test Statistic', 'p-value'], [Chisquares_results[0], Chisquares_results[1]]]
chisquare # p value = 8.54342267020237e-05
# p low null go-> Thus null hypothesis is rejected

# CONCLUSION-> Percentage of males versus females walking in to the store differ based on day of the week













