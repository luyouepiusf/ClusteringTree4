survival_forest<-function(
  time,
  event,
  weight=rep(1,length(time)),
  matrix_numeric,
  matrix_factor,
  significance=0.05,
  min_weight=50,
  missing="majority",
  nboot=100,
  seed=0){
  
  nind<-length(time)
  a_survival_forest<-list()
  set.seed(seed)
  for(boot_idx in 1:nboot){
    cat(boot_idx)
    shuffle<-sample(1:nind,nind,replace=T)
    time_boot<-time[shuffle]
    event_boot<-event[shuffle]
    matrix_numeric_boot<-matrix_numeric[shuffle,]
    matrix_factor_boot<-matrix_factor[shuffle,]
    a_survival_tree<-survival_tree(
      time=time_boot,
      event=event_boot,
      matrix_numeric=matrix_numeric_boot,
      matrix_factor=matrix_factor_boot,
      significance=significance,
      min_weight=min_weight,
      missing=missing)
    cat(" - ")
    
    a_survival_forest[[boot_idx]]<-a_survival_tree
  }
  return(a_survival_forest)
}


predict_distance_forest<-function(
  survival_forest,
  matrix_numeric,
  matrix_factor,
  missing="majority"){
  
  nind<-nrow(matrix_numeric)
  
  sum_distance<-matrix(0,nind,nind)
  sum_non_na<-matrix(0,nind,nind)
  
  for(boot_idx in 1:length(survival_forest)){
    a_distance<-predict_distance(survival_forest[[boot_idx]],matrix_numeric,matrix_factor,missing=missing)$ind_distance
    sum_non_na<-sum_non_na+!is.na(sum_non_na)
    a_distance[is.na(a_distance)]<-0
    sum_distance<-sum_distance+a_distance
  }
  
  mean_distance<-sum_distance/sum_non_na
  return(list(
    mean_distance=mean_distance,
    sum_distance=sum_distance,
    sum_non_na=sum_non_na))
}
