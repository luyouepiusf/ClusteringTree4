tree_to_table<-function(survival_tree){
  
  grow_table<-function(node,layer=1,track=""){
    if(!is.null(node$left_node)){
      return(rbind(
        data.frame(
          layer=layer,
          type=ifelse(node$split_info$best_jj<=ndim_numeric,"numeric","factor"),
          w=node$split_info$sum_weight,
          j=node$split_info$best_jj,
          z=node$split_info$best_zscore,
          p=node$split_info$best_pvalue,
          split_numeric=node$split_info$best_split_numeric,
          split_factor=I(list(node$split_info$best_split_factor_left)),
          w_left=node$split_info$sum_weight_left,
          w_right=node$split_info$sum_weight_right,
          survival=I(list(list(time=node$time,event=node$event,weight=node$weight))),
          terminal=FALSE,
          more_to_left=node$more_to_left,
          track=track),
        grow_table(node$left_node,layer+1,paste0(track,"l")),
        grow_table(node$right_node,layer+1,paste0(track,"r"))))
    }else{
      return(data.frame(
        layer=layer,
        type=ifelse(node$split_info$best_jj<=ndim_numeric,"numeric","factor"),
        w=node$split_info$sum_weight,
        j=node$split_info$best_jj,
        z=node$split_info$best_zscore,
        p=node$split_info$best_pvalue,
        split_numeric=node$split_info$best_split_numeric,
        split_factor=I(list(node$split_info$best_split_factor_left)),
        w_left=node$split_info$sum_weight_left,
        w_right=node$split_info$sum_weight_right,
        survival=I(list(list(time=node$time,event=node$event,weight=node$weight))),
        terminal=TRUE,
        more_to_left=node$more_to_left,
        track=track))
    }
  }
  
  ndim_numeric<-survival_tree$ndim_numeric
  ndim_factor<-survival_tree$ndim_factor
  
  a_table<-grow_table(survival_tree$survival_tree)
  a_table$print_order<-1:nrow(a_table)
  a_table$plot_order<-rank(paste0(a_table$track,"m"))
  a_table$id<-1:nrow(a_table)
  # there can other ways to define id
  
  a_table$left_id<-NA
  a_table$right_id<-NA
  a_table$parent_id<-NA
  for(idx in 1:nrow(a_table)){
    if(a_table$terminal[idx])next
    a_track<-a_table$track[idx]
    left_idx<-which(a_table$track==paste0(a_track,"l"))
    right_idx<-which(a_table$track==paste0(a_track,"r"))
    a_table$left_id[idx]<-a_table$id[left_idx]
    a_table$right_id[idx]<-a_table$id[right_idx]
    a_table$parent_id[left_idx]<-a_table$id[idx]
    a_table$parent_id[right_idx]<-a_table$id[idx]
  }
  
  return(a_table)
}









