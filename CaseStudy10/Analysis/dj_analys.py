from sklearn.datasets import load_boston
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import Imputer
from sklearn.base import BaseEstimator
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
plt.style.use('bmh')

# whip up the boston dataset
X, y = load_boston(return_X_y=True)

# default parameters to use
parameter_dict = {'normalize':True, 'n_jobs':-1}
# dictionary for results
results_dict = {}

def get_scores(features: np.ndarray,
               targets: np.ndarray,
               model_class: BaseEstimator=LinearRegression,
               pars: dict=parameter_dict) -> dict:
    model = model_class(**pars)
    model.fit(features, targets)
    preds = model.predict(features)
    goodness = r2_score(y, preds)
    loss = mean_squared_error(y, preds)
    return {'goodness_of_fit':goodness, 'mse':loss}


results_dict['baseline'] = get_scores(X,y)

def random_missing_col(data: np.ndarray, prop: int) -> np.ndarray:
    if type(prop) is not int or prop <= 0 or prop >=100:
        raise ValueError('needs to be an int less than 100 and greater than zero!')
    nrows = data.shape[0]
    idy = np.random.choice(data.shape[-1], 1)
    idx = np.random.choice(nrows, int(nrows*prop/100), replace=False)
    out = data.copy()
    out[idx, idy] = np.nan
    return out







