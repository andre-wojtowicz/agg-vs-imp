# OVERVIEW:
#
# Classifiers:
#   K1 - logistic regression
#   K2 - decision tree
#   K3 - SVM
# 
# Imputation:
#   I1 - median
#   I2 - kNN
#   I3 - SVD
# 
# Agregation operators:
#   A1 - ...
# 
# Datasets:
#   D1 - 
#   D2 - 
#   D3 - 
#     
# Procedure for each Di:
#   1. Divide Di into Di^1 and Di^2
#   2. Learn classifiers Kj on Di^1 (10-CV)
#   3. Du^2 := randomly obscured Di^2 with data loss from 0% to 50%
#   4. Calculate accuracy, sensitivity, specificity and decisiveness for
#      classifiers Kj on Di^2
#   5. Choose the best impuation on Di^2
#   6. Choose the best aggregation operator on Di^2
#      6.1 Use optimx to calculate intervals of Kj
#   7. Compare Kj with the best imputation and agregation operator
#

# ---- init ----

rm(list=ls())
 

# ---- step-1-divide-data ----



# ---- step-2-learn-classifiers ----



# ---- step-3-obsucre-dataset ----



# ---- step-4-calculate-classifiers-performance ----



# ---- step-5-choose-best-imputation ----



# ---- step-6-choose-best-aggregation ----



# ---- step-7-compare-results ----


 
