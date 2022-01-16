"Assume Null hyposthesis as Ho: μ1 = μ2 =  μ3 = μ4 (proportion of male and female across regions is same) Thus Alternate hypothesis as Ha: μ1 ≠ μ2 μ1 ≠  μ2  ≠ μ3 ≠  μ4 (proportion of male and female across regions is different) 
"
import pandas as pd
import scipy
import scipy as sp
from scipy import stats

#Y is discrete and X Factor is discrete
################ Chi-Square Test ################

Customer = pd.read_csv("D:/C DRIVE-SSD DATA backup 15-12-2020/Desktop/360DigiTmg Assignment/Hypothesis Testing/CustomerOrderform.csv")
Customer

Customer.head()
Customer.describe()

Phillippines_value=Customer['Phillippines'].value_counts()
Indonesia_value=Customer['Indonesia'].value_counts()
Malta_value=Customer['Malta'].value_counts()
India_value=Customer['India'].value_counts()
print(Phillippines_value)
print(Indonesia_value)
print(Malta_value)
print(India_value)

chiStats = sp.stats.chi2_contingency([[271,267,269,280],[29,33,31,20]])
print('Test t=%f p-value=%f' % (chiStats[0], chiStats[1]))
print('Interpret by p-Value')
if chiStats[1] < 0.05:
  print('we reject null hypothesis')
else:
  print('we accept null hypothesis')

#Test t=3.858961 p-value=0.277102
#Interpret by p-Value
#we accept null hypothesis

#critical value = 0.1
alpha = 0.05
critical_value = sp.stats.chi2.ppf(q = 1 - alpha,df=chiStats[2])
observed_chi_val = chiStats[0]
print('Interpret by critical value')
if observed_chi_val <= critical_value:
       print ('Null hypothesis cannot be rejected (variables are not related)')
else:
       print ('Null hypothesis cannot be excepted (variables are not independent)')

#Interpret by critical value
#Null hypothesis cannot be rejected (variables are not related)
#Inference is that proportion of defective % across the center is same.

