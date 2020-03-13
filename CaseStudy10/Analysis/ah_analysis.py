#%%
from sklearn.datasets import load_boston
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import Imputer


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


boston = load_boston()
#print(boston['DESCR'])

#Setting up data
X = boston.data
y = boston.target

bos = pd.DataFrame(boston.data)
bos.columns = boston.feature_names
print(bos.head(5))


bos.info()
bos.describe()

#CheckMissingValues. 
bos.isnull().sum()

#Check histograms of data
bos.hist(bins=50, figsize = (20,15))

#Looking at correlation
plt.figure(figsize=(20,10))
sns.heatmap(bos.corr(),vmax=1, annot=True, cmap = 'YlGnBu',annot_kws={"fontsize":14})

#Looking at outlier percentages outside 1st and 3rd Quantiles
for k, v in bos.items():
	q1 = v.quantile(0.25)
	q3 = v.quantile(0.75)
	irq = q3 - q1
	v_col = v[(v <= q1 - 1.5 * irq) | (v >= q3 + 1.5 * irq)]
	perc = np.shape(v_col)[0] * 100.0 / np.shape(bos)[0]
	print("%s outliers = %8.2f%%" % (k, perc))

# ======================================================================================
# Problem 1:
# Using Sklearn get the Boston Housing dataset.
# Fit a linear regressor to the data as a baeline.  
# There is no need to do Cross-Validation.  We are exploring the change in results

# What is the loss and what are the goodness of fit parameters?  This will be our baseline for comparison
# ======================================================================================

#Looking at a baseline RSME 
linreg = LinearRegression().fit(X,y)
y_pred = linreg.predict(X)
baseline_MSE = mean_squared_error(y,y_pred)
r2 = r2_score(y, y_pred)

#Coefficients and intercept
# linreg.coef_
# linreg.intercept_
#Looking at Coefficients. 
print(pd.DataFrame(zip(bos.columns, linreg.coef_), columns = ['features', 'BaselineCoefficients']))
print("Baseline MSE is = %.2f" % baseline_MSE)
print("Goodness of fit (R_squared) is = %.2f" % r2)


# ======================================================================================
# Problem 2: (repeated)
# For select between 1, 5 10, 20, 33, and 50% of your data on a single column (Completely at random), replace the present value with a NAN and then perform an imputation of that value.   

# In. each case perform a fit with the imputed data and compare the loss and goodness of fit to your baseline.
# ======================================================================================


#Split that data like Paul Bunyan
# X = Housing Price target
# y = All other housing data
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state = 42)

#Check shape of test/train
print(X_train.shape)
print(X_test.shape)
print(y_train.shape)
print(y_test.shape)

#Define function for SKlearn Imputer
def impute_nation(imputedata):
	impute = Imputer(missing_values=np.nan, strategy="mean", axis=1)
	impute.fit(imputedata)
	impute.transform(imputedata)
	return


#Setup linear regressor function
def LinearMadness(X, y, perc = 1, imp_col = []):

	linreg = LinearRegression().fit(X,y)
	y_pred = linreg.predict(X)
	return_MSE = mean_squared_error(y,y_pred)
	r2 = r2_score(y, y_pred)

	print("The MSE is = %.2f" % return_MSE)
	print("Goodness of fit (R_squared) is = %.2f" % r2)
	print('===============================')
#%%
bos_imp = pd.DataFrame([])
perc_list = [0.10,0.20,0.33,0.50]
imp_col = ['NOX']


for x in perc_list:
	np.random.seed(42)
	rand_index = np.random.randint(
		low = 0,
		high = X_train.shape[0],
		size = int(len(X_train)*perc))
	
	bos_imp = X_train.copy()
	b = np.nan

	nan_sum = sum(np.isnan(bos_imp[imp_col]))
	impute_nation(bos_imp[imp_col])

	LinearMadness(bos_imp, y_train, perc=x,imp_col=imp_col)









