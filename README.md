# Aggregation vs imputation

## Configuration

### Datasets

See: [UCI Machine Learning datasets for R](https://github.com/andre-wojtowicz/uci-ml-to-r)

 * D1 - bank-marketing
 * D2 - census-income
 * D3 - credit-card
 * D4 - magic
 * D5 - wine-quality

### Classifiers

See: [caret](https://topepo.github.io/caret)

 * K1 - glm
 * K2 - nnet
 * K3 - SVM
 * K4 - J48
 * K5 - kNN

### Imputation

 * I1 - median & mode
 * I2 - random forest ([missForest](https://github.com/stekhoven/missForest))
 * I3 - chained equations ([mice](http://dx.doi.org/10.18637/jss.v045.i03))

### Agregation operators

 * Ai - aggregation strategies from the article [Solving the problem of incomplete data in medical diagnosis via interval modeling](http://dx.doi.org/10.1016/j.asoc.2016.05.029)

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
