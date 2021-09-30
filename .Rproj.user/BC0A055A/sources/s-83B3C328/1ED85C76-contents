
grow_tree<-function(
  time,
  event,
  weight,
  xx_numeric,
  xx_factor,
  significance,
  min_weight,
  missing){
  
  xx_numeric<-data.matrix(xx_numeric)
  xx_factor<-data.matrix(xx_factor)
  
  result_find_best_split<-find_best_split_cox_bone(time,event,weight,xx_numeric,xx_factor,min_weight)
  result_find_best_split$sum_weight<-sum(weight)
  
  if(result_find_best_split$best_pvalue>significance|result_find_best_split$best_jj==0){
    result_find_best_split$best_zscore<-0
    result_find_best_split$best_chisq<-0
    result_find_best_split$best_pvalue<-1
    result<-list(
      left_node=NULL,
      right_node=NULL,
      terminal=TRUE,
      more_to_left=NA,
      time=time,
      event=event,
      weight=weight,
      split_info=result_find_best_split)
    return(result)
  }else{
    
    sum_weight_left<-result_find_best_split$sum_weight_left
    sum_weight_right<-result_find_best_split$sum_weight_right
    more_to_left<-sum_weight_left>=sum_weight_right
    
    if(missing=="majority"){
      
      left_weight<-dplyr::case_when(
        result_find_best_split$to_left~weight,
        result_find_best_split$to_right~0,
        result_find_best_split$to_unsure~ifelse(more_to_left,weight,0))
      right_weight<-dplyr::case_when(
        result_find_best_split$to_left~0,
        result_find_best_split$to_right~weight,
        result_find_best_split$to_unsure~ifelse(more_to_left,0,weight))
      
    }else if(missing=="omit"){
      
      left_weight<-dplyr::case_when(
        result_find_best_split$to_left~weight,
        result_find_best_split$to_right~0,
        result_find_best_split$to_unsure~0)
      right_weight<-dplyr::case_when(
        result_find_best_split$to_left~0,
        result_find_best_split$to_right~weight,
        result_find_best_split$to_unsure~0)
      
    }else if(missing=="weighted"){
      
      left_weight<-dplyr::case_when(
        result_find_best_split$to_left~weight,
        result_find_best_split$to_right~0,
        result_find_best_split$to_unsure~weight*sum_weight_left/(sum_weight_left+sum_weight_right))
      right_weight<-dplyr::case_when(
        result_find_best_split$to_left~0,
        result_find_best_split$to_right~weight,
        result_find_best_split$to_unsure~weight*sum_weight_right/(sum_weight_left+sum_weight_right))
      
    }else{
      stop("Wrong 'missing' argument")
    }
    
    left_idx<-left_weight>0
    right_idx<-right_weight>0
    
    time_left<-time[left_idx]
    event_left<-event[left_idx]
    weight_left<-left_weight[left_idx]
    xx_numeric_left<-xx_numeric[left_idx,]
    xx_factor_left<-xx_factor[left_idx,]
    
    time_right<-time[right_idx]
    event_right<-event[right_idx]
    weight_right<-right_weight[right_idx]
    xx_numeric_right<-xx_numeric[right_idx,]
    xx_factor_right<-xx_factor[right_idx,]
    
    result<-list(
      left_node=grow_tree(time_left,event_left,weight_left,xx_numeric_left,xx_factor_left,significance,min_weight,missing),
      right_node=grow_tree(time_right,event_right,weight_right,xx_numeric_right,xx_factor_right,significance,min_weight,missing),
      terminal=FALSE,
      more_to_left=more_to_left,
      time=time,
      event=event,
      weight=weight,
      split_info=result_find_best_split)
    return(result)
  }
}
