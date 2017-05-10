[![Build Status](https://travis-ci.org/mnwright/bnnSurvival.svg?branch=master)](https://travis-ci.org/mnwright/bnnSurvival)

## bnnSurvival: Bagged k-nearest neighbors survival probability prediction
Marvin N. Wright

This package implements a bootstrap aggregated (bagged) version of the k-nearest neighbors survival probability prediction method (Lowsky et al. 2013). In addition to the bootstrapping of training samples, the features can be subsampled in each baselearner to break the correlation between them. The Rcpp package is used to speed up the computation.

### References
* Lowsky, D.J. et al. (2013). A K-nearest neighbors survival probability prediction method. Stat Med 32(12):2062-2069.
