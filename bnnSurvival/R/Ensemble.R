

setClass("Ensemble", 
         representation(
           data = "data.frame", 
           classifiers = "list",
           num_classifiers = "integer",
           num_features_per_classifier = "integer", 
           num_neighbors = "integer")
)

## Constructor
Ensemble <- function(data, classifiers, num_classifiers, num_features_per_classifier, 
                     num_neighbors) {
  new("Ensemble", 
      data = data,
      classifiers = classifiers,
      num_classifiers = num_classifiers,
      num_features_per_classifier = num_features_per_classifier, 
      num_neighbors = num_neighbors)
}

