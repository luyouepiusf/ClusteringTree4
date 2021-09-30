# sourceCpp("Rcpp/find_best_split_cox_bone.cpp")
# source("grow_tree.R")
# source("grow_table.R")
# source("plot_survival_tree.R")
# source("predict_survival_tree.R")
# source("survival_tree.R")
# source("utility.R")

library(ClusteringTree4)

set.seed(5)
nind<-500
ndim_numeric<-5
ndim_factor<-3
ft<-rexp(nind,3)
ct<-rexp(nind,3)
ot<-pmin(ft,ct)
delta<-ft<ct
xx_numeric<-matrix(rnorm(nind*ndim_numeric),nind,ndim_numeric)
xx_numeric[1:10,1]<-NA
xx_numeric[11:20,2]<-NA
xx_numeric[21:30,3]<-NA
xx_numeric[31:40,4]<-NA
xx_numeric[41:50,5]<-NA
colnames(xx_numeric)<-paste0("X",1:ncol(xx_numeric))
xx_factor<-matrix(NA,nind,ndim_factor)
xx_factor[,1]<-sample(c("AA","BB","CC","DD",NA),nind,replace=TRUE)
xx_factor[,2]<-sample(c("apple","banana","banana","cherry",NA),nind,replace=TRUE)
xx_factor[,3]<-sample(c("good","medium","poor",NA),nind,replace=TRUE)
# xx_factor[51:60,1]<-NA
# xx_factor[61:70,2]<-NA
# xx_factor[71:80,3]<-NA
colnames(xx_factor)<-paste0("V",1:ncol(xx_factor))

library(dplyr)
library(grid)
library(gridtext)
library(survival)

a_survival_tree<-survival_tree(
  time=ot,
  event=delta,
  matrix_numeric=xx_numeric,
  matrix_factor=xx_factor,
  significance=0.05,
  missing="weighted")

a_table<-tree_to_table(a_survival_tree$survival_tree)
a_table$survival[[2]]

plot_survival_tree(a_survival_tree)

nind_test<-200
set.seed(-1)
xx_numeric_test<-matrix(rnorm(nind_test*ndim_numeric),nind_test,ndim_numeric)
xx_numeric_test[1:10,1]<-NA
xx_numeric_test[11:20,2]<-NA
xx_numeric_test[21:30,3]<-NA
xx_numeric_test[31:40,4]<-NA
xx_numeric_test[41:50,5]<-NA
colnames(xx_numeric_test)<-paste0("X",1:ncol(xx_numeric_test))
xx_factor_test<-matrix(NA,nind_test,ndim_factor)
xx_factor_test[,1]<-sample(c("AA","BB","CC","DD",NA),nind_test,replace=TRUE)
xx_factor_test[,2]<-sample(c("apple","banana","banana","cherry",NA),nind_test,replace=TRUE)
xx_factor_test[,3]<-sample(c("good","medium","poor",NA),nind_test,replace=TRUE)
colnames(xx_factor_test)<-paste0("V",1:ncol(xx_factor_test))

weight_majority<-predict_weight(a_survival_tree,xx_numeric_test,xx_factor_test,missing="majority")
weight_omit<-predict_weight(a_survival_tree,xx_numeric_test,xx_factor_test,missing="omit")
weight_weighted<-predict_weight(a_survival_tree,xx_numeric_test,xx_factor_test,missing="weighted")
rowSums(weight_majority)
rowSums(weight_omit)
rowSums(weight_weighted)

result_majority<-predict_distance(a_survival_tree,xx_numeric_test,xx_factor_test,"majority")
result_omit<-predict_distance(a_survival_tree,xx_numeric_test,xx_factor_test,"omit")
result_weighted<-predict_distance(a_survival_tree,xx_numeric_test,xx_factor_test,"weighted")

heatmap(result_majority$ind_distance)
temp<-result_omit$ind_distance
temp[is.na(temp)]<-20
image(result_omit$ind_distance)
image(temp)
diag(result_omit$ind_distance)
heatmap(result_weighted$ind_distance)


nboot<-200
all_distance<-array(NA,c(nind_test,nind_test,nboot))
for(boot_idx in 1:nboot){
  a_distance<-predict_distance(a_survival_tree,xx_numeric_test,xx_factor_test)
}








