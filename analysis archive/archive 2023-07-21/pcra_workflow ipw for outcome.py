import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, confusion_matrix, precision_score, recall_score, ConfusionMatrixDisplay
from sklearn.model_selection import RandomizedSearchCV, train_test_split
from scipy.stats import randint
import pyarrow.parquet as pq

dir_name = 'C:\Cloud\OneDrive - Emory University\Papers\PASC Cardiometabolic Risk Factors'
outcome_df = pd.read_parquet(dir_name + '\working\models\pcra201_ipw for cohort membership data.parquet' )

sample_outcome_df = outcome_df.sample(frac=0.1, random_state=1)
# https://stackoverflow.com/questions/11587782/creating-dummy-variables-in-pandas-for-python
sample_outcome_df_dummies = pd.get_dummies(sample_outcome_df,columns=['site','calendar_month','payer_type_primary','payer_type_secondary'])

dummies = pd.get_dummies(sample_outcome_df['Category']).rename(columns=lambda x: 'Category_' + str(x))
df = pd.concat([df, dummies], axis=1)
df = df.drop(['Category'], inplace=True, axis=1)

rf = RandomForestClassifier(random_state=1)
# n_estimators = number of trees in the forest
# random_state = controls both the randomness of the bootstrapping of the samples used when building trees (if bootstrap=True), 
# and the sampling of the features to consider when looking for the best split at each node (if max_features < n_features)
min_samples_leaf = [100,1000]
# n_estimators = [500, 1000]
n_estimators = [5, 10]

random_grid = {'min_samples_leaf': min_samples_leaf,
               'n_estimators': n_estimators}
print(random_grid)

# n_iter = number of different combinations
# cv = number of folds
# verbose = controls the verbosity: the higher, the more messages
# n_jobs = number of jobs to run in parallel
# random_state = controls both the randomness of the bootstrapping of the samples used when building trees (if bootstrap=True),
# and the sampling of the features to consider when looking for the best split at each node (if max_features < n_features)

rf_random = RandomizedSearchCV(estimator = rf, param_distributions = random_grid, 
                               n_iter = 2, cv = 3, 
                               verbose=2, random_state=1, n_jobs = -1)

