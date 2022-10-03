predict_distance_tree<-function(
  survival_tree,
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
