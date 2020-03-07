from sklearn.datasets import load_boston
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
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

#Check histograms 

bos.hist(bins=50, figsize = (20,15))

#Looking at correlation
plt.figure(figsize=(20,10))
sns.heatmap(bos.corr(),vmax=1, annot=True, cmap = 'YlGnBu',annot_kws={"fontsize":8})

#Looking at outlier percentages outside 1st and 3rd Quantiles
for k, v in bos.items():
	q1 = v.quantile(0.25)
	q3 = v.quantile(0.75)
	irq = q3 - q1
	v_col = v[(v <= q1 - 1.5 * irq) | (v >= q3 + 1.5 * irq)]
	perc = np.shape(v_col)[0] * 100.0 / np.shape(bos)[0]
	print("Column %s outliers = %.2f%%" % (k, perc))


#Looking at a baseline RSME
linreg = LinearRegression().fit(X,y)
y_pred = linreg.predict(X)
baseline_RSME = np.sqrt(mean_squared_error(y,y_pred))
print("Baseline RSME is:", baseline_RSME)

#Split that data like Paul Bunyan
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state = 42)

#Check shape of test/train
print(X_train.shape)
print(X_test.shape)
print(y_train.shape)
print(y_test.shape)

#Define Function for SKlearn Imputer
def impute_nation(imputedata):
	impute = Imputer(missing_values=np.nan, strategy="mean", axis=1)
	impute.fit(imputedata)
	impute.transform(imputedata)
	return


