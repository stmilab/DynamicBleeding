import numpy as np
import pandas as pd
from datetime import datetime
from joblib import dump

from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer

STAGE = 'B'

df_train = pd.read_csv('data/pre_impute_train_jan_26_2021.csv', index_col=0)
df_test = pd.read_csv('data/pre_impute_test_jan_26_2021.csv',   index_col=0)

presentation_vars = list(pd.read_csv('presentation.txt', header=None)[0])
cath_lab_vars = list(pd.read_csv('cath_lab.txt', header=None)[0])
med_vars = list(pd.read_csv('meds.txt', header=None)[0])
pci_vars = list(pd.read_csv('pci.txt', header=None)[0])
access_vars = [col for col in df_train.columns if 
               'femor' in col.lower() or
               'radia' in col.lower()]
closure_vars = [col for col in df_train.columns if 'clos' in col.lower()]
outcome_var = ['Bleed']

all_vars = outcome_var + presentation_vars + access_vars + cath_lab_vars + med_vars + pci_vars + closure_vars

if STAGE == 'A':
    stage = presentation_vars
elif STAGE == 'B':
    stage = presentation_vars + access_vars + cath_lab_vars
elif STAGE == 'C':
    stage = presentation_vars + access_vars + cath_lab_vars + med_vars + pci_vars

#stage.remove('PreProcTnT')  #Only for test; never present in for 1000 entries
    
imp = IterativeImputer(random_state=0, sample_posterior=True, verbose=2)
print(f'{datetime.now().strftime("%c")} Now Fitting')

imp.fit(df_train[stage])
print(f'{datetime.now().strftime("%c")} Fit complete.  Saving imputer to disk.')

dump(imp, f'data/stage_{STAGE}_imputer.joblib')
print(f'{datetime.now().strftime("%c")} Imputer saved to disk.  Beginning imputation.')

for i in range(5):
    mi_train = imp.transform(df_train[stage])
    mi_test  = imp.transform(df_test[stage])
    
    print(f'{datetime.now().strftime("%c")} Imputation {i} complete; saving to disk')
    df_train.fillna(
        pd.DataFrame(mi_train,
                     columns=stage,
                     index=df_train.index
        ),
    ).to_csv(f'data/train_{STAGE}_fold_{i}.csv')
    df_test.fillna(
        pd.DataFrame(mi_test,
                     columns=stage,
                     index=df_test.index
        ),
    ).to_csv(f'data/test_{STAGE}_fold_{i}.csv')
    print(f'{datetime.now().strftime("%c")} Imputation {i} saved to disk.')