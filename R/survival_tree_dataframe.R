survival_tree<-function(
  survival_outcome,
  numeric_predictor,
  factor_predictor,
  weights=NULL,
  data,
  significance=0.05,
  min_weights=50,
  missing="omit",
  test_type="univariate"){
  
  # check [missing], [test_type]
  if(!missing%in%c("majority","omit","weighted"))stop("Invalid 'missing' argument.")
  if(!test_type%in%c("univariate",p.adjust.methods))stop("Invalid 'test_type' argument.")
  
  # clean [survival_outcome], [numeric_predictor], [factor_predictor]
  if(!formula.tools::is.two.sided(survival_outcome))stop("Invalid 'survival_outcome' formula.")
  if(!formula.tools::is.one.sided(numeric_predictor))stop("Invalid 'numeric_predictor' formula.")
  if(!formula.tools::is.one.sided(factor_predictor))stop("Invalid 'factor_predictor' formula.")
  
  if(missing(data)){
    mf_survival_outcome<-eval(substitute(model.frame(survival_outcome,na.action="na.pass")))
    mf_numeric_predictor<-eval(substitute(model.frame(numeric_predictor,na.action="na.pass")))
    mf_factor_predictor<-eval(substitute(model.frame(factor_predictor),na.action="na.pass"))
  }else{
    mf_survival_outcome<-eval(substitute(model.frame(survival_outcome,data=data,na.action="na.pass")))
    mf_numeric_predictor<-eval(substitute(model.frame(numeric_predictor,data=data,na.action="na.pass")))
    mf_factor_predictor<-eval(substitute(model.frame(factor_predictor,data=data,na.action="na.pass")))
  }
  
  if(!is.Surv(mf_survival_outcome[[1]])|attr(mf_survival_outcome[[1]],"type")!="right")stop("Invalid 'survival_outcome' formula")
  if(length(mf_factor_predictor)>=1&!all(sapply(mf_numeric_predictor,class)%in%c("integer","numeric")))stop("Invalid 'numeric_predictor' formula")
  if(length(mf_factor_predictor)>=1&!all(sapply(mf_factor_predictor,class)%in%c("factor","character")))stop("Invalid 'factor_predictor' formula")
  
  time<-model.response(mf_survival_outcome)[,1]
  event<-model.response(mf_survival_outcome)[,2]
  matrix_numeric<-as.matrix(mf_numeric_predictor)
  matrix_factor<-as.matrix(mf_factor_predictor)
  matrix_factor<-apply(matrix_factor,c(1,2),as.character)
  ndim_numeric<-ncol(matrix_numeric)
  ndim_factor<-ncol(matrix_factor)
  nind<-length(time)
  
  if(ndim_numeric+ndim_numeric<1)stop("There are no predictors in the model.")
  
  # check dimensions
  if(nrow(matrix_numeric)!=nind)stop("Dimension mismatch between 'survival_outcome' and 'numeric_predictor'.")
  if(nrow(matrix_factor)!=nind)stop("Dimension mismatch between 'survival_outcome' and 'factor_predictor'.")
  if(any(is.na(time))|any(is.na(event)))stop("Missing values in 'survival_outcome'.")
  
  # create names
  if(is.null(colnames(matrix_numeric))&ndim_numeric>=1)colnames(matrix_numeric)<-paste0("numeric",1:ncol(matrix_numeric),sep="")
  if(is.null(colnames(matrix_factor))&ndim_factor>=1)colnames(matrix_factor)<-paste0("factor",1:ncol(matrix_factor),sep="")
  variable_names<-c(colnames(matrix_numeric),colnames(matrix_factor))
  
  # clean [weights]
  if(is.null(weights)){
    weights<-rep(1,length(time))
  }else if(missing(data)){
    mf_weights<-eval(substitute(model.frame(~1,weights=weights)))
    weights<-mf_weights[[1]]
  }else{
    mf_weights<-eval(substitute(model.frame(~1,weights=weights,data=data)))
    weights<-mf_weights[[1]]
  }
  if(length(weights)!=nind)stop("Dimension mismatch between 'survival_outcome' and 'weights'.")
  
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
    xx_numeric=matrix_numeric,
    xx_factor=matrix_factor,
    weights=weights,
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

