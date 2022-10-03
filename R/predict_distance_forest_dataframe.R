#' works for dataframes
predict_distance_forest<-function(
  survival_forest,
  numeric_predictor,
  factor_predictor,
  data,
  missing="omit"){
  
  if(!formula.tools::is.one.sided(numeric_predictor))stop("Invalid 'numeric_predictor' formula.")
  if(!formula.tools::is.one.sided(factor_predictor))stop("Invalid 'factor_predictor' formula.")
  
  if(missing(data)){
    mf_numeric_predictor<-eval(substitute(model.frame(numeric_predictor,na.action="na.pass")))
    mf_factor_predictor<-eval(substitute(model.frame(factor_predictor),na.action="na.pass"))
  }else{
    mf_numeric_predictor<-eval(substitute(model.frame(numeric_predictor,data=data,na.action="na.pass")))
    mf_factor_predictor<-eval(substitute(model.frame(factor_predictor,data=data,na.action="na.pass")))
  }
  
  if(!all(sapply(mf_numeric_predictor[[1]],class)%in%c("integer","numeric")))stop("Invalid 'numeric_predictor' formula")
  if(!all(sapply(mf_factor_predictor[[1]],class)%in%c("factor","character")))stop("Invalid 'factor_predictor' formula")
  
  matrix_numeric<-as.matrix(mf_numeric_predictor)
  matrix_factor<-as.matrix(mf_factor_predictor)
  matrix_factor<-apply(matrix_factor,c(1,2),as.character)
  ndim_numeric<-ncol(matrix_numeric)
  ndim_factor<-ncol(matrix_factor)
  nind_test<-nrow(matrix_numeric)
  
  # check dimensions
  if(nrow(matrix_numeric)!=nrow(matrix_factor))stop("'nrow(matrix_numeric)' and 'nrow(matrix_factor) are different.'")
  if(ndim_numeric!=survival_forest$ndim_numeric)stop("'ncol(matrix_numeric)' inconsistent with training data.'")
  if(ndim_factor!=survival_forest$ndim_factor)stop("'ncol(matrix_factor)' inconsistent with training data.'")
  
  sum_distance<-matrix(0,nind_test,nind_test)
  sum_non_na<-matrix(0,nind_test,nind_test)
  
  for(boot_idx in 1:length(survival_forest$survival_forest)){
    a_distance<-predict_distance_tree_matrix(
      survival_forest$survival_forest[[boot_idx]],
      matrix_numeric,
      matrix_factor,
      missing=missing)$ind_distance
    sum_non_na<-sum_non_na+!is.na(a_distance)
    a_distance[is.na(a_distance)]<-0
    sum_distance<-sum_distance+a_distance
  }
  
  mean_distance<-sum_distance/sum_non_na
  return(list(
    mean_distance=mean_distance,
    sum_distance=sum_distance,
    sum_non_na=sum_non_na))
}
