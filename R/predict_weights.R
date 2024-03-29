predict_weights<-function(
  survival_tree,
  matrix_numeric,
  matrix_factor,
  missing="majority"){

  ndim_numeric<-survival_tree$ndim_numeric
  ndim_factor<-survival_tree$ndim_factor
  nind_test<-nrow(matrix_numeric)

  # check dimensions
  if(nrow(matrix_numeric)!=nrow(matrix_factor))stop("'nrow(matrix_numeric)' and 'nrow(matrix_factor) are different.'")
  if(ncol(matrix_numeric)!=ndim_numeric)stop("'ncol(matrix_numeric)' inconsistent with training data.'")
  if(ncol(matrix_factor)!=ndim_factor)stop("'ncol(matrix_factor)' inconsistent with training data.'")

  # check colnames
  # if(any(c(colnames(matrix_numeric),colnames(matrix_factor))!=survival_tree$variable_names)){
  #   stop("column names do not match!")
  # }

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

  a_table<-tree_to_table(survival_tree$survival_tree)
  weights<-calculate_weights_by_table(a_table,matrix_numeric,matrix_factor,missing=missing)
  
  return(weights)
}
