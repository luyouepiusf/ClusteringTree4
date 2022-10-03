survival_tree_matrix<-function(
  time,
  event,
  matrix_numeric,
  matrix_factor,
  weights=rep(1,length(time)),
  significance=0.05,
  min_weights=50,
  missing="omit",
  test_type="univariate"){
  
  # check [missing], [test_type]
  if(!missing%in%c("majority","omit","weighted"))stop("Invalid 'missing' argument.")
  if(!test_type%in%c("univariate",p.adjust.methods))stop("Invalid 'test_type' argument.")
  
  if(!is.numeric(time))stop("Invalid 'time' argument")
  event<-as.logical(event)
  if(!is.logical(event))stop("Invalid 'event' argument")
  if(!is.numeric(matrix_numeric))stop("Invalid 'matrix_numeric' formula")
  if(!is.character(matrix_factor))stop("Invalid 'matrix_factor' formula")
  
  ndim_numeric<-ncol(matrix_numeric)
  ndim_factor<-ncol(matrix_factor)
  nind<-length(time)
  
  if(ndim_numeric+ndim_factor<1)stop("There are no predictors in the model.")
  
  # check dimensions
  if(nrow(matrix_numeric)!=nind)stop("Dimension mismatch between 'matrix_numeric' and 'time'.")
  if(nrow(matrix_factor)!=nind)stop("Dimension mismatch between 'matrix_factor' and 'time'.")
  if(any(is.na(time))|any(is.na(event)))stop("Missing values in 'time' or 'event'.")
  
  # creat names
  if(is.null(colnames(matrix_numeric))&ndim_numeric>=1)colnames(matrix_numeric)<-paste0("numeric",1:ncol(matrix_numeric),sep="")
  if(is.null(colnames(matrix_factor))&ndim_factor>=1)colnames(matrix_factor)<-paste0("factor",1:ncol(matrix_factor),sep="")
  variable_names<-c(colnames(matrix_numeric),colnames(matrix_factor))
  
  # convert matrix_factor to an integer matrix
  factor_dictionary<-list()
  matrix_factor_int<-matrix(NA,nind,ndim_factor)
  if(ncol(matrix_factor)>0){
    colnames(matrix_factor_int)<-colnames(matrix_factor)
    for(idx in 1:ncol(matrix_factor)){
      aname<-colnames(matrix_factor)[idx]
      a_dictionary<-create_dictionary(matrix_factor[,idx])
      factor_dictionary[[aname]]<-a_dictionary
      matrix_factor_int[,idx]<-a_dictionary[matrix_factor[,aname]]
    }
  }
  matrix_factor<-matrix_factor_int
  
  # run
  a_survival_tree<-grow_tree(
    time=time,
    event=event,
    weights=weights,
    xx_numeric=matrix_numeric,
    xx_factor=matrix_factor,
    significance=significance,
    min_weights=min_weights,
    missing=missing,
    test_type=test_type)
  
  return(list(
    variable_names=variable_names,
    ndim_numeric=ndim_numeric,
    ndim_factor=ndim_factor,
    factor_dictionary=factor_dictionary,
    survival_tree=a_survival_tree))
}
