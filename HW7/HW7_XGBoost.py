# -*- coding: utf-8 -*-
"""

 Stat 202A 2019 Fall - Homework 07
 Author: 
 Date : 

 INSTRUCTIONS: Please fill in the corresponding function. Do not change function names, 
 function inputs or outputs. Do not write anything outside the function.
 
"""

import numpy as np
from sklearn.datasets import load_breast_cancer
from sklearn.model_selection import KFold, GridSearchCV, cross_val_score, train_test_split
from matplotlib import pyplot as plt
import xgboost as xgb
from xgboost import plot_importance
from sklearn.metrics import auc, accuracy_score, confusion_matrix, mean_squared_error
#from statistics import mean, stdev

cancer = load_breast_cancer()
X = cancer.data
y = cancer.target

#############################################################################################################
# TODO (1) Perform 5-fold validation for cancer data.
# You may consider use KFold function in sklearn.model_selection
# Print the mean and std of the 5-fold validation accuracy
#############################################################################################################

kf = KFold(n_splits=5, random_state=42)
accuracy = []
xgb_model = xgb.XGBClassifier(objective="binary:logistic", random_state=42)


for train_index, test_index in kf.split(X):
    #print("TRAIN:", train_index, "TEST:", test_index)
    X_train, X_test = X[train_index], X[test_index]
    y_train, y_test = y[train_index], y[test_index]
    xgb_model.fit(X_train, y_train)
    y_pred = xgb_model.predict(X_test)
    #print(confusion_matrix(y_test, y_pred))
    #print(accuracy_score(y_test, y_pred))
    accuracy.append(accuracy_score(y_test, y_pred))


print(np.mean(accuracy))
print(np.std(accuracy))

#############################################################################################################
# TODO (2) Perform Grid Search for parameter max_depth in [3,5,7] and min_child_weight in [0.1, 1, 5]
# For each combination use 5-Fold validation
# You may consider use GridSearchCV function in sklearn.modelselection
# Print the grid search mean test score for each paramter combination (use cv_results_)
#############################################################################################################
gsc = GridSearchCV(
        estimator = xgb.XGBClassifier(objective="binary:logistic", random_state=42),
        param_grid={
            'max_depth': [3, 5, 7],
            'min_child_weight': [0.1, 1, 5] },
                
        cv=5, scoring='accuracy',  n_jobs= -1)

gsc.fit(X, y)
best_parameters = gsc.best_params_
print(best_parameters)
result=zip(gsc.cv_results_['params'], gsc.cv_results_['mean_test_score'])
print(result)

##plot
# split data into train and test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=7)

for max_depth in [3, 5, 7]:
    for min_child_weight in [0.1, 1, 5]:
        # fit model no training data
        model = xgb.XGBClassifier(objective="binary:logistic", random_state=42, max_depth=max_depth, min_child_weight=min_child_weight)
        eval_set = [(X_train, y_train), (X_test, y_test)]
        model.fit(X_train, y_train, eval_metric=["error"], eval_set=eval_set, verbose=True)
        # make predictions for test data
        #y_pred = model.predict(X_test)
        # retrieve performance metrics
        results = model.evals_result()
        epochs = len(results['validation_0']['error'])
        x_axis = range(0, epochs)
        # plot classification error
        fig, ax = plt.subplots()
        ax.plot(x_axis, results['validation_0']['error'], label= 'Train')
        ax.plot(x_axis, results['validation_1']['error'], label='Test')
        ax.legend()
        plt.ylabel('Classification Error')
        plt.title('XGBoost Classification Error with max_depth={}, min_child_weight={}'.format(max_depth, min_child_weight))
        plt.show()
        fig.savefig('max_depth={}, min_child_weight={}.png'.format(max_depth, min_child_weight))
 
#############################################################################################################
# TODO (3) Plot the feature importance of the best model
# You may fit a new xgboost model using all the data and then plot the importance using xgb.plot_importance()
#############################################################################################################
xgb_model = xgb.XGBClassifier(objective="binary:logistic", random_state=42, max_depth=5, min_child_weight=1)
xgb_model.fit(X, y)


fig1, ax = plt.subplots()
plot_importance(xgb_model, ax=ax)
#plot_importance(xgb_model)
plt.tight_layout() 
plt.show()
fig1.savefig('The Best Parameters.png'.format(best_parameters))
