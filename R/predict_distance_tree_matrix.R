#' works for raw matrices
predict_distance_tree_matrix<-function(
  survival_tree,
  matrix_numeric,
  matrix_factor,
  missing="omit"){
  
  # ndim_numeric<-survival_tree$ndim_numeric
  # ndim_factor<-survival_tree$ndim_factor
  # nind_test<-nrow(matrix_numeric)
  # 
  # # check dimensions
  # if(nrow(matrix_numeric)!=nrow(matrix_factor))stop("'nrow(matrix_numeric)' and 'nrow(matrix_factor) are different.'")
  # if(ncol(matrix_numeric)!=ndim_numeric)stop("'ncol(matrix_numeric)' inconsistent with training data.'")
  # if(ncol(matrix_factor)!=ndim_factor)stop("'ncol(matrix_factor)' inconsistent with training data.'")
  
  ndim_numeric<-ncol(matrix_numeric)
  ndim_factor<-ncol(matrix_factor)
  nind_test<-nrow(matrix_numeric)
  
  if(nrow(matrix_numeric)!=nrow(matrix_factor))stop("'nrow(matrix_numeric)' and 'nrow(matrix_factor) are different.'")
  if(ndim_numeric!=survival_tree$ndim_numeric)stop("'ncol(matrix_numeric)' inconsistent with training data.'")
  if(ndim_factor!=survival_tree$ndim_factor)stop("'ncol(matrix_factor)' inconsistent with training data.'")
  
  # clean [matrix_numeric] and [matrix_factor]
  factor_dictionary<-survival_tree$factor_dictionary
  matrix_factor_int<-matrix(NA,nind_test,ndim_factor)
  if(ncol(matrix_factor)>0){
    colnames(matrix_factor_int)<-colnames(matrix_factor)
    for(idx in 1:ncol(matrix_factor)){
      aname<-colnames(matrix_factor)[idx]
      matrix_factor_int[,idx]<-(factor_dictionary[[aname]])[matrix_factor[,aname]]
    }
  }
  matrix_factor<-matrix_factor_int
  
  a_distance<-calculate_distance(
    survival_tree$survival_tree,
    matrix_numeric,
    matrix_factor,
    missing)
  
  return(a_distance)
}

