# !pip install statsmodels

import statsmodels.api as sm
import pandas as pd
import numpy as np

def lm(X, Y):
    """ Easy way to compute linear model with a summary in Python """
    arrays = []
    
    for col in X.columns:
        arrays.append(X[col].to_numpy().reshape(-1, 1))
    
    mdl = sm.OLS(Y, X).fit()
    print(mdl.summary())

    return mdl