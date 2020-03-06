from sklearn.datasets import load_boston
import sklearn
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


boston = load_boston()
#print(boston['DESCR'])

bos = pd.DataFrame(boston.data)
bos.columns = boston.feature_names
print(bos.head(5))

X = boston.data
Y = boston.target

bos.info()
bos.describe()
bos.hist(bins=50, figsize = (20,15))


