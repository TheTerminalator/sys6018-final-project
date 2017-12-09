import pandas as pd 
from sklearn import ensemble
import numpy as np
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np



#import datasets from csv
full = pd.read_csv("Full.csv") #full data
perf = pd.read_csv("Perf.csv") #performance stats only

#Determine which characters contain categorical variables
char_cols = full.dtypes.pipe(lambda x: x[x == 'object']).index

#Encode character variable variables as numeric variables
for c in char_cols:
    full[c] = pd.factorize(full[c])[0]

#Do the same as above for the performance only data set
char_cols = perf.dtypes.pipe(lambda x: x[x == 'object']).index

for c in char_cols:
    perf[c] = pd.factorize(perf[c])[0]


#Establish Random forests
fullRF = ensemble.RandomForestRegressor()
perfRF = ensemble.RandomForestRegressor()

#Create dictionary of parameters to be cross validated 
n_estimators = [10,25,50,100,200,300,500]
max_features = [4,7,15,20,25,30,40,55,65,75]

parameters = dict(n_estimators = n_estimators,max_features = max_features)


print("Full")
#Create Grid search object to cross validate
clf = GridSearchCV(fullRF,parameters, scoring = "neg_mean_absolute_error")
#Fit grid search object
clf.fit(full.loc[:, full.columns != "cap_hit"], full["cap_hit"])
print(clf.cv_results_)
print(clf.best_estimator_ )
#extract mean test scores from CV to plot
scores_mean = clf.cv_results_['mean_test_score']
#reshape 
scores_mean = np.array(scores_mean).reshape(len(max_features),len(n_estimators))

#create figure and plot object
fig = plt.figure()
ax = fig.add_subplot(111)

# Plot Grid search scores
#Colors to be used for graphing
colors = ['b', 'g', 'r', 'c', 'm', 'y', 'k','C3','tab:gray','xkcd:sky blue','xkcd:turquoise']
# N_estimators is the X-axis, Max_features is represented as a different curve (color line)
#plot each iteration of grid search
for idx, val in enumerate(max_features):
    ax.plot(n_estimators, scores_mean[idx,:], '-o', label= "Max Features" + ': ' + str(val), color = colors[idx])

#set graph characteristics
ax.set_title("Full Grid Search Scores", fontsize=20, fontweight='bold')
ax.set_xlabel("Number of Estimators", fontsize=16)
ax.set_ylabel('CV Average Score', fontsize=16)
ax.legend(loc="best", fontsize=8)
ax.grid('on')
#save output
fig.savefig('Full.png')

#repeat everything from above but with perfonly
print("Perf")
clf = GridSearchCV(perfRF,parameters, scoring = "neg_mean_absolute_error")
clf.fit(perf.loc[:, perf.columns != "cap_hit"], perf["cap_hit"])
print(clf.cv_results_)
print(clf.best_estimator_ )


scores_mean = clf.cv_results_['mean_test_score']
scores_mean = np.array(scores_mean).reshape(len(max_features),len(n_estimators))



fig = plt.figure()
ax = fig.add_subplot(111)


for idx, val in enumerate(max_features):
    ax.plot(n_estimators, scores_mean[idx,:], '-o', label= "Max Features" + ': ' + str(val),color = colors[idx])

ax.set_title("Perf Only Grid Search Scores", fontsize=20, fontweight='bold')
ax.set_xlabel("Number of Estimators", fontsize=16)
ax.set_ylabel('CV Average Score', fontsize=16)
ax.legend(loc="best", fontsize=8)
ax.grid('on')
fig.savefig('Perf Only.png')
 


