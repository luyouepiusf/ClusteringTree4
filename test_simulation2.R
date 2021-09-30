library(survival)
set.seed(0)

nind<-600
x1<-rep(c(-1,1,0),each=nind/3)+rnorm(nind)/2
x2<-rep(c(0,0,sqrt(3)),each=nind/3)+rnorm(nind)/2

ft<-c(rexp(nind/3,1),
      rlnorm(nind/3,1,0.2),
      rweibull(nind/3,4,2))
ct<-rweibull(nind,4,4)
ot<-pmin(ft,ct)
delta<-ft<ct

pdf("data.pdf",width=6,height=5)
layout(rbind(c(1,1,1,2),
             c(1,1,1,3),
             c(1,1,1,4)))
par(mar=c(0,0,0,0),oma=c(5,5,5,5),cex=0.75)
plot(x1,x2,main="",col=adjustcolor(rep(c("red","purple","orange"),each=nind/3),alpha.f=0.3),pch=16)
xx<-seq(0,5,0.01)
plot(xx,1-pexp(xx,1),type="l",xlab="",ylab="",xaxt="n",yaxt="n",col="red")
plot(xx,1-plnorm(xx,1,0.2),type="l",xlab="",ylab="",xaxt="n",yaxt="n",col="purple")
plot(xx,1-pweibull(xx,4,2),type="l",xlab="",ylab="",xaxt="n",yaxt="n",col="orange")
axis(1)
mtext("x1                 ",side=1,outer=T,line=2.8)
mtext("x2",side=2,outer=T,line=2.8)
dev.off()

xx_numeric<-cbind(x1,x2)
colnames(xx_numeric)<-paste0("X",1:ncol(xx_numeric))
xx_factor<-matrix(NA,nind,0)

a_survival_tree<-survival_tree(
  time=ot,
  event=delta,
  matrix_numeric=xx_numeric,
  matrix_factor=xx_factor,
  significance=0.05)
plot_survival_tree(a_survival_tree)
predict_node(a_survival_tree,xx_numeric,xx_factor)

##########
# forest #
##########

set.seed(1)
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
  all_distance[,,boot_idx]<-
    predict_distance(a_survival_tree,xx_numeric,xx_factor)$ind_distance
}

mean_distance<-apply(all_distance,c(1,2),mean,na.rm=T)
a_dist<-dist(mean_distance)
a_cmdscale<-cmdscale(a_dist,k=2)

pdf("MDS.pdf",width=6,height=5)
plot(a_cmdscale[,1]+rnorm(nind,0,2),a_cmdscale[,2]+rnorm(nind,0,2),
     main="MDS",xlab="MDS1",ylab="MDS2",
     col=adjustcolor(rep(c("red","purple","orange"),each=nind/3),alpha.f=0.3),pch=16)
dev.off()
