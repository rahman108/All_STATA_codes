library(AlgDesign)
ffd<-gen.factorial(c(2,2,2), varNames=c("A","B","C"), factors="all")
ffd
write.csv(ffd, "Full Design1.csv")
set.seed(54321)
des<-optFederov(~.,ffd,4)
des
alt1<-des$design
alt1
alt2<-alt1
alt2
alt3<-alt2
alt3
alt1<-transform(alt1, r1=runif(4))
alt1
alt2<-transform(alt2, r1=runif(4))
alt2
alt3<-transform(alt3, r1=runif(4))
alt3
alt1_sort<-alt1[order(alt1$r1)]
alt1_sort
alt2_sort<-alt2[order(alt2$r1)]
alt2_sort
alt3_sort<-alt3[order(alt3$r1)]
alt3_sort