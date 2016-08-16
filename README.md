# Aggregation vs imputation

## Configuration

### Datasets

 * D1 - bank-marketing
 * D2 - census-income
 * D3 - credit-card
 * D4 - magic
 * D5 - wine-quality

### Classifiers

 * K1 - glm
 * K2 - nnet
 * K3 - SVM
 * K4 - C5.0
 * K5 - kNN

### Imputation

 * I1 - median & mode
 * I2 - random forest
 * I3 - chained equations

### Agregation operators

 * A1 - ...

## Experiment procedure

For each Di:

 1. Divide Di into Di^1 and Di^2
 1. Learn classifiers Kj on Di^1 (10-CV)
 1. Du^2 := randomly obscured Di^2 with data loss from 0% to 50%
 1. Calculate accuracy, sensitivity, specificity and decisiveness for classifiers Kj on Di^2
 1. Choose the best impuation on Di^2
 1. Find aggregation operator:
    1. Calculate inteval predictions on Di^2
    1. Choose the best aggregation operator on inteval predictions
 1. Compare Kj with the best imputation and agregation operator
