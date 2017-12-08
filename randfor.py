import pandas as pd 
from sklearn import ensemble
import numpy as np
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder



#import training data  from csv
full = pd.read_csv("Full.csv")
perf = pd.read_csv("Perf.csv")
char_cols = full.dtypes.pipe(lambda x: x[x == 'object']).index

le = LabelEncoder()
for c in char_cols:
    full[c] = pd.factorize(full[c])[0]

char_cols = perf.dtypes.pipe(lambda x: x[x == 'object']).index

for c in char_cols:
    perf[c] = pd.factorize(perf[c])[0]

fullxtrain, fullxtest, fullytrain, fullytest = train_test_split(full.loc[:, full.columns != "cap_hit"], full["cap_hit"], test_size=0.33, random_state=42)
perfxtrain, perfxtest, perfytrain, perfytest = train_test_split(perf.loc[:, perf.columns != "cap_hit"], perf["cap_hit"], test_size=0.33, random_state=42)

#Establish Random forest
fullRF = ensemble.RandomForestRegressor()
perfRF = ensemble.RandomForestRegressor()

#Create dictionary of parameters to be cross validated 
#200 and 4 are the best
parameters = {'n_estimators':[50,100,200,300], 'max_features':[4,7,15,20,30]}

print("Full")
#Create Grid search object to cross validate
clf = GridSearchCV(fullRF,parameters, scoring = "neg_mean_squared_error")
clf.fit(full.loc[:, full.columns != "cap_hit"], full["cap_hit"])
print(clf.cv_results_)
#Predict probabilities and save them to csv

print("Perf")
clf = GridSearchCV(perfRF,parameters, scoring = "neg_mean_squared_error")
clf.fit(perf.loc[:, perf.columns != "cap_hit"], perf["cap_hit"])
print(clf.cv_results_)