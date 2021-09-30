predict_weight<-function(
  survival_tree,
  matrix_numeric,
  matrix_factor,
  missing="majority"){
  
  nind_test<-nrow(matrix_numeric)
  
  # need to check if the colnames are identify 
  if(any(c(colnames(matrix_numeric),colnames(matrix_factor))!=survival_tree$variable_names)){
    stop("column names do not match!")
  }
  ndim_numeric<-survival_tree$ndim_numeric
  ndim_factor<-survival_tree$ndim_factor
  
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
  
  calculate_weight<-function(idx,weight,missing="majority"){
    if(missing=="majority"){
      if(a_table$terminal[idx]){
        weight_column<-cbind(weight)
        colnames(weight_column)<-idx
        return(weight_column)
      }else if(a_table$type[idx]=="numeric"){
        jj<-a_table$j[idx]
        weight_left<-dplyr::case_when(
          is.na(matrix_numeric[,jj])&a_table$more_to_left[idx]~weight,
          is.na(matrix_numeric[,jj])&!a_table$more_to_left[idx]~0,
          matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~weight,
          TRUE~0)
        weight_right<-dplyr::case_when(
          is.na(matrix_numeric[,jj])&a_table$more_to_left[idx]~0,
          is.na(matrix_numeric[,jj])&!a_table$more_to_left[idx]~weight,
          matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~0,
          TRUE~weight)
        weight_column<-cbind(rep(0,nind_test))
        colnames(weight_column)<-idx
        return(cbind(
          weight_column,
          calculate_weight(a_table$left_id[idx],weight_left,missing),
          calculate_weight(a_table$right_id[idx],weight_right,missing)))
      }else if(a_table$type[idx]=="factor"){
        jj<-a_table$j[idx]-ndim_numeric
        weight_left<-dplyr::case_when(
          is.na(matrix_factor[,jj])&a_table$more_to_left[idx]~weight,
          is.na(matrix_factor[,jj])&!a_table$more_to_left[idx]~0,
          matrix_factor[,jj]%in%a_table$split_factor[[idx]]~weight,
          TRUE~0)
        weight_right<-dplyr::case_when(
          is.na(matrix_factor[,jj])&a_table$more_to_left[idx]~0,
          is.na(matrix_factor[,jj])&!a_table$more_to_left[idx]~weight,
          matrix_factor[,jj]%in%a_table$split_factor[[idx]]~0,
          TRUE~weight)
        weight_column<-cbind(rep(0,nind_test))
        colnames(weight_column)<-idx
        return(cbind(
          weight_column,
          calculate_weight(a_table$left_id[idx],weight_left,missing),
          calculate_weight(a_table$right_id[idx],weight_right,missing)))
      }
    }else if(missing=="omit"){
      if(a_table$terminal[idx]){
        weight_column<-cbind(weight)
        colnames(weight_column)<-idx
        return(weight_column)
      }else if(a_table$type[idx]=="numeric"){
        jj<-a_table$j[idx]
        weight_left<-dplyr::case_when(
          is.na(matrix_numeric[,jj])~0,
          matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~weight,
          TRUE~0)
        weight_right<-dplyr::case_when(
          is.na(matrix_numeric[,jj])~0,
          matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~0,
          TRUE~weight)
        weight_column<-cbind(dplyr::case_when(
          is.na(matrix_numeric[,jj])~weight,
          TRUE~0))
        colnames(weight_column)<-idx
        return(cbind(
          weight_column,
          calculate_weight(a_table$left_id[idx],weight_left,missing),
          calculate_weight(a_table$right_id[idx],weight_right,missing)))
      }else if(a_table$type[idx]=="factor"){
        jj<-a_table$j[idx]-ndim_numeric
        weight_left<-dplyr::case_when(
          is.na(matrix_factor[,jj])~0,
          matrix_factor[,jj]%in%a_table$split_factor[[idx]]~weight,
          TRUE~0)
        weight_right<-dplyr::case_when(
          is.na(matrix_factor[,jj])~0,
          matrix_factor[,jj]%in%a_table$split_factor[[idx]]~0,
          TRUE~weight)
        weight_column<-cbind(dplyr::case_when(
          is.na(matrix_factor[,jj])~weight,
          TRUE~0))
        colnames(weight_column)<-idx
        return(cbind(
          weight_column,
          calculate_weight(a_table$left_id[idx],weight_left,missing),
          calculate_weight(a_table$right_id[idx],weight_right,missing)))
      }
    }else if(missing=="weighted"){
      if(a_table$terminal[idx]){
        weight_column<-cbind(weight)
        colnames(weight_column)<-idx
        return(weight_column)
      }else if(a_table$type[idx]=="numeric"){
        jj<-a_table$j[idx]
        weight_left<-dplyr::case_when(
          is.na(matrix_numeric[,jj])~weight*a_table$w_left[idx]/(a_table$w_left[idx]+a_table$w_right[idx]),
          matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~weight,
          TRUE~0)
        weight_right<-dplyr::case_when(
          is.na(matrix_numeric[,jj])~weight*a_table$w_right[idx]/(a_table$w_left[idx]+a_table$w_right[idx]),
          matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~0,
          TRUE~weight)
        weight_column<-cbind(rep(0,nind_test))
        colnames(weight_column)<-idx
        return(cbind(
          weight_column,
          calculate_weight(a_table$left_id[idx],weight_left,missing),
          calculate_weight(a_table$right_id[idx],weight_right,missing)))
      }else if(a_table$type[idx]=="factor"){
        jj<-a_table$j[idx]-ndim_numeric
        weight_left<-dplyr::case_when(
          is.na(matrix_factor[,jj])~weight*a_table$w_left[idx]/(a_table$w_left[idx]+a_table$w_right[idx]),
          matrix_factor[,jj]%in%a_table$split_factor[[idx]]~weight,
          TRUE~0)
        weight_right<-dplyr::case_when(
          is.na(matrix_factor[,jj])~weight*a_table$w_right[idx]/(a_table$w_left[idx]+a_table$w_right[idx]),
          matrix_factor[,jj]%in%a_table$split_factor[[idx]]~0,
          TRUE~weight)
        weight_column<-cbind(rep(0,nind_test))
        colnames(weight_column)<-idx
        return(cbind(
          weight_column,
          calculate_weight(a_table$left_id[idx],weight_left,missing),
          calculate_weight(a_table$right_id[idx],weight_right,missing)))
      }
    }
  }
  
  weight<-calculate_weight(1,weight=rep(1,nind_test),missing=missing)
  return(weight)
}