subdf<-subset(AWnimp, II)
N=nrow(subdf)
K=10
n=floor(N/K) 
S<-matrix(sample(1:N, K*n), K)

outcomes<-c("Ind","Cnj")
f=as.formula("Ind ~ PV.Time + II.sense + NI.object.actor + Med.actor + (1|Lemma)")

cl <- makeCluster(10)
clusterExport(cl, c("f","subdf","K","N","S","n","AWnimp","outcomes"))
models<- parLapply(cl,1:K, function(i) {library(lme4); glmer(f, data=subdf[-S[i,],], family=binomial, control=glmerControl(optimizer = "bobyqa"))})

sapply(1:K, function(i){
  predictedprobs<-predict(models[[i]], newdata=subdf[S[1, ],], allow.new.levels=T, type="response")
  predicted<-ifelse(predictedprobs>0.5, outcomes[1],outcomes[2])
  observed<-as.vector(ifelse(subdf[S[1, ],"Ind"]>0.5, outcomes[1],outcomes[2]))
  probs<-cbind(predictedprobs,1-predictedprobs)
  colnames(probs) <- outcomes
  model.statistics(observed,predicted,probs)[c("accuracy","R2.likelihood","tau.classification")]
})
stopCluster(cl)