library(ClusteringTree4)

set.seed(0)
nind<-400
ndim_num<-5
ndim_fac<-3
ft<-rexp(nind,3)
ct<-rexp(nind,3)
ot<-pmin(ft,ct)
delta<-ft<ct
xx_numeric<-matrix(rnorm(nind*ndim_num),nind,ndim_num)
xx_numeric[1:10,1]<-NA
xx_numeric[11:20,2]<-NA
xx_numeric[21:30,3]<-NA
xx_numeric[31:40,4]<-NA
xx_numeric[41:50,5]<-NA
colnames(xx_numeric)<-paste0("X",1:ncol(xx_numeric))
xx_factor<-matrix(NA,nind,ndim_fac)
xx_factor[,1]<-sample(c("AA","BB","CC","DD",NA),nind,replace=TRUE)
xx_factor[,2]<-sample(c("apple","banana","banana","cherry",NA),nind,replace=TRUE)
xx_factor[,3]<-sample(c("good","medium","poor",NA),nind,replace=TRUE)
colnames(xx_factor)<-paste0("V",1:ncol(xx_factor))

library(dplyr)
library(grid)
library(gridtext)
library(survival)

a_survival_forest<-survival_forest(
  ot,delta,rep(1,nind),xx_numeric,xx_factor,
  significance=0.05,nboot=50)
length(a_survival_forest)

a_result<-predict_distance_forest(
  a_survival_forest,
  xx_numeric,
  xx_factor)
image(a_result$mean_distance)

set.seed(0)
nboot<-200
all_distance<-array(NA,c(nind,nind,nboot))
for(boot_idx in 1:nboot){
  cat(boot_idx)
  shuffle<-sample(1:nind,nind,replace=T)
  ot_boot<-ot[shuffle]
  delta_boot<-delta[shuffle]
  xx_numeric_boot<-xx_numeric[shuffle,]
  xx_factor_boot<-xx_factor[shuffle,]
  a_survival_tree<-survival_tree(
    time=ot_boot,
    event=delta_boot,
    matrix_numeric=xx_numeric_boot,
    matrix_factor=xx_factor_boot,
    significance=0.05)
  cat(" - ")
  # plot_survival_tree(a_survival_tree)
  # tree_to_table(a_survival_tree$survival_tree)
  all_distance[,,boot_idx]<-
    predict_distance(a_survival_tree,xx_numeric,xx_factor)$ind_distance
}

mean_distance<-apply(all_distance,c(1,2),mean,na.rm=T)
image(mean_distance)
idx<-apply(is.na(mean_distance),1,any)
mean_distance[idx,idx]

names(predict_distance(a_survival_tree,xx_numeric,xx_factor))

my_distance<-as.dist(mean_distance)
a_sammon_fit<-MASS::sammon(my_distance)

plot_survival_tree(a_survival_tree)

tree_to_table(a_survival_tree$survival_tree)
