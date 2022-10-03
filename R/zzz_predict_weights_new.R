# predict_weights<-function(
#   survival_tree,
#   matrix_numeric,
#   matrix_factor,
#   missing="majority"){
#   
#   ndim_numeric<-survival_tree$ndim_numeric
#   ndim_factor<-survival_tree$ndim_factor
#   nind_test<-nrow(matrix_numeric)
#   
#   # check dimensions
#   if(nrow(matrix_numeric)!=nrow(matrix_factor))stop("'nrow(matrix_numeric)' and 'nrow(matrix_factor) are different.'")
#   if(ncol(matrix_numeric)!=ndim_numeric)stop("'ncol(matrix_numeric)' inconsistent with training data.'")
#   if(ncol(matrix_factor)!=ndim_factor)stop("'ncol(matrix_factor)' inconsistent with training data.'")
#   
#   # clean [matrix_numeric] and [matrix_factor]
#   factor_dictionary<-survival_tree$factor_dictionary
#   matrix_factor_int<-matrix(NA,nind_test,ndim_factor)
#   if(ncol(matrix_factor)>0){
#     colnames(matrix_factor_int)<-colnames(matrix_factor)
#     for(idx in 1:ncol(matrix_factor)){
#       aname<-colnames(matrix_factor)[idx]
#       matrix_factor_int[,idx]<-(factor_dictionary[[aname]])[matrix_factor[,aname]]
#     }
#   }
#   matrix_factor<-matrix_factor_int
#   
#   a_table<-tree_to_table(survival_tree$survival_tree)
#   
#   grow_weights<-function(idx,weights,missing="omit"){
#     if(missing=="majority"){
#       if(a_table$terminal[idx]){
#         weights_column<-cbind(weights)
#         colnames(weights_column)<-idx
#         return(weights_column)
#       }else if(a_table$type[idx]=="numeric"){
#         jj<-a_table$j[idx]
#         weights_left<-dplyr::case_when(
#           is.na(matrix_numeric[,jj])&a_table$more_to_left[idx]~weights,
#           is.na(matrix_numeric[,jj])&!a_table$more_to_left[idx]~0,
#           matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~weights,
#           TRUE~0)
#         weights_right<-dplyr::case_when(
#           is.na(matrix_numeric[,jj])&a_table$more_to_left[idx]~0,
#           is.na(matrix_numeric[,jj])&!a_table$more_to_left[idx]~weights,
#           matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~0,
#           TRUE~weights)
#         weights_column<-cbind(rep(0,nind_test))
#         colnames(weights_column)<-idx
#         return(cbind(
#           weights_column,
#           grow_weights(a_table$left_id[idx],weights_left,missing),
#           grow_weights(a_table$right_id[idx],weights_right,missing)))
#       }else if(a_table$type[idx]=="factor"){
#         jj<-a_table$j[idx]-ndim_numeric
#         weights_left<-dplyr::case_when(
#           is.na(matrix_factor[,jj])&a_table$more_to_left[idx]~weights,
#           is.na(matrix_factor[,jj])&!a_table$more_to_left[idx]~0,
#           matrix_factor[,jj]%in%a_table$split_factor[[idx]]~weights,
#           TRUE~0)
#         weights_right<-dplyr::case_when(
#           is.na(matrix_factor[,jj])&a_table$more_to_left[idx]~0,
#           is.na(matrix_factor[,jj])&!a_table$more_to_left[idx]~weights,
#           matrix_factor[,jj]%in%a_table$split_factor[[idx]]~0,
#           TRUE~weights)
#         weights_column<-cbind(rep(0,nind_test))
#         colnames(weights_column)<-idx
#         return(cbind(
#           weights_column,
#           grow_weights(a_table$left_id[idx],weights_left,missing),
#           grow_weights(a_table$right_id[idx],weights_right,missing)))
#       }
#     }else if(missing=="omit"){
#       if(a_table$terminal[idx]){
#         weights_column<-cbind(weights)
#         colnames(weights_column)<-idx
#         return(weights_column)
#       }else if(a_table$type[idx]=="numeric"){
#         jj<-a_table$j[idx]
#         weights_left<-dplyr::case_when(
#           is.na(matrix_numeric[,jj])~0,
#           matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~weights,
#           TRUE~0)
#         weights_right<-dplyr::case_when(
#           is.na(matrix_numeric[,jj])~0,
#           matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~0,
#           TRUE~weights)
#         weights_column<-cbind(dplyr::case_when(
#           is.na(matrix_numeric[,jj])~weights,
#           TRUE~0))
#         colnames(weights_column)<-idx
#         return(cbind(
#           weights_column,
#           grow_weights(a_table$left_id[idx],weights_left,missing),
#           grow_weights(a_table$right_id[idx],weights_right,missing)))
#       }else if(a_table$type[idx]=="factor"){
#         jj<-a_table$j[idx]-ndim_numeric
#         weights_left<-dplyr::case_when(
#           is.na(matrix_factor[,jj])~0,
#           matrix_factor[,jj]%in%a_table$split_factor[[idx]]~weights,
#           TRUE~0)
#         weights_right<-dplyr::case_when(
#           is.na(matrix_factor[,jj])~0,
#           matrix_factor[,jj]%in%a_table$split_factor[[idx]]~0,
#           TRUE~weights)
#         weights_column<-cbind(dplyr::case_when(
#           is.na(matrix_factor[,jj])~weights,
#           TRUE~0))
#         colnames(weights_column)<-idx
#         return(cbind(
#           weights_column,
#           grow_weights(a_table$left_id[idx],weights_left,missing),
#           grow_weights(a_table$right_id[idx],weights_right,missing)))
#       }
#     }else if(missing=="weighted"){
#       if(a_table$terminal[idx]){
#         weights_column<-cbind(weights)
#         colnames(weights_column)<-idx
#         return(weights_column)
#       }else if(a_table$type[idx]=="numeric"){
#         jj<-a_table$j[idx]
#         weights_left<-dplyr::case_when(
#           is.na(matrix_numeric[,jj])~weights*a_table$w_left[idx]/(a_table$w_left[idx]+a_table$w_right[idx]),
#           matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~weights,
#           TRUE~0)
#         weights_right<-dplyr::case_when(
#           is.na(matrix_numeric[,jj])~weights*a_table$w_right[idx]/(a_table$w_left[idx]+a_table$w_right[idx]),
#           matrix_numeric[,jj]<=a_table$split_numeric[[idx]]~0,
#           TRUE~weights)
#         weights_column<-cbind(rep(0,nind_test))
#         colnames(weights_column)<-idx
#         return(cbind(
#           weights_column,
#           grow_weights(a_table$left_id[idx],weights_left,missing),
#           grow_weights(a_table$right_id[idx],weights_right,missing)))
#       }else if(a_table$type[idx]=="factor"){
#         jj<-a_table$j[idx]-ndim_numeric
#         weights_left<-dplyr::case_when(
#           is.na(matrix_factor[,jj])~weights*a_table$w_left[idx]/(a_table$w_left[idx]+a_table$w_right[idx]),
#           matrix_factor[,jj]%in%a_table$split_factor[[idx]]~weights,
#           TRUE~0)
#         weights_right<-dplyr::case_when(
#           is.na(matrix_factor[,jj])~weights*a_table$w_right[idx]/(a_table$w_left[idx]+a_table$w_right[idx]),
#           matrix_factor[,jj]%in%a_table$split_factor[[idx]]~0,
#           TRUE~weights)
#         weights_column<-cbind(rep(0,nind_test))
#         colnames(weights_column)<-idx
#         return(cbind(
#           weights_column,
#           grow_weights(a_table$left_id[idx],weights_left,missing),
#           grow_weights(a_table$right_id[idx],weights_right,missing)))
#       }
#     }
#   }
#   
#   weights<-grow_weights(1,weights=rep(1,nind_test),missing=missing)
#   weights<-calculate_weights_by_table(a_table,matrix_numeric,matrix_factor,missing=missing)
#   
#   return(weights)
# }
# 
# # calculate_weights_by_table<-function(a_table,matrix_numeric,matrix_factor,missing="omit"){
# #   nind_test<-nrow(matrix_numeric)
# #   ndim_numeric<-ncol(matrix_numeric)
# #   ndim_factor<-ncol(matrix_factor)
# #   
# #   weights<-grow_weights(1,weights=rep(1,nind_test),missing=missing)
# #   return(weights)
# # }
# 
