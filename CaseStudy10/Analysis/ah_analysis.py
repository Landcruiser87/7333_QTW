from sklearn.datasets import load_boston
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


boston = load_boston()
#print(boston['DESCR'])

bos = pd.DataFrame(boston.data)
bos.columns = boston.feature_names
print(bos.head(5))

X = boston.data
Y = boston.target

bos.info()
bos.describe()

#Check histograms 

bos.hist(bins=50, figsize = (20,15))

#Looking at correlation
plt.figure(figsize=(20,10))
sns.heatmap(bos.corr(),vmax=1, annot=True, cmap = 'YlGnBu',annot_kws={"fontsize":8})

#Looking at outlier percentages. 
for k, v in bos.items():
	q1 = v.quantile(0.25)
	q3 = v.quantile(0.75)
	irq = q3 - q1
	v_col = v[(v <= q1 - 1.5 * irq) | (v >= q3 + 1.5 * irq)]
	perc = np.shape(v_col)[0] * 100.0 / np.shape(bos)[0]
	print("Column %s outliers = %.2f%%" % (k, perc))


