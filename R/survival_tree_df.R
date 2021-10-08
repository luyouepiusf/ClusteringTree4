#' Title
#'
#' @param dt dataframe which include the data
#' @param name_time string, name of the time variable (numeric)
#' @param name_event string name of the event (boolean)
#' @param weight vector weight of each observation (numeric)
#' @param significance 
#' @param min_weight minimum weight per observatin
#' @param missing how missing values are dealts with.  missing = c("majority","omit","weighted), Need Lu to complete
#'
#' @return a clustertree object Need lu to detail
#' @export
#'
#' @examples
survival_tree_df <- function(
  dt,
  name_time,
  name_event,
  weight=rep(1,length(time)),
  significance=0.05,
  min_weight=50,
  missing="majority"){
  
  
  matrix_numeric <- dt %>% select(where(is.numeric))
  matrix_factor <-  dt %>% select(where(is.factor))
  time <- dt[,name_time]  
  event <- dt[,name_event]
  
  ndim_numeric<-ncol(matrix_numeric)
  ndim_factor<-ncol(matrix_factor)
  
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
  
  a_survival_tree<-grow_tree(
    time=time,
    event=event,
    weight=weight,
    xx_numeric=matrix_numeric,
    xx_factor=matrix_factor,
    significance=significance,
    min_weight=min_weight,
    missing=missing)
  
  return(list(
    variable_names=variable_names,
    ndim_numeric=ndim_numeric,
    ndim_factor=ndim_factor,
    factor_dictionary=factor_dictionary,
    survival_tree=a_survival_tree))
}
