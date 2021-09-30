#' survival_tree
#' time: (mandatory) vector of survival times
#' event: (mandatory) vector of event indicators
#' xx: (mandatory) data frame of covariates
#' significance: (default=0.05) significance level
#' minsize: (default=20) minimum number of samples in the terminal
#' missing: (default="majority") one of "majority" or "drop"
#' test: (default="cox") one of "cox" or "logrank"
#' details: (default="complete") one of "complete" or "rule"
#' types: (optional) vector of characters "numeric", "ordinal", "factor", specifies the types of each column
#' 
#' RETURN: object survival_tree

#' predict.survival_tree
#' survival_tree: (mandatory) a survival_tree object
#' xx: (mandatory) data frame of covariates
#' type: (default="distance") one of "label", "distance" or "survival"
#' 
#' RETURN: predicted labels

#' plot.survival_tree
#' survival_tree: a survival_tree object
#' 
#' RETURN: a figure

#' survival_forest
#' (same as survival tree)
#' 
#' RETURN: object survival_forest, a bunch of survival trees 

#' predict.survival_forest
#' survival_forest
#' xx: (mandatory) data frame of covariates
#' type: (default="distance") probably just distance