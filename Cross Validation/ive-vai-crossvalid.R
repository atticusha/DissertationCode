data1<-subset(AWive, AI)
N=nrow(data1)
K=10
n=floor(N/K) 
S<-matrix(sample(1:N, K*n), K)
              
outcomes<-c("Ind","eCnj")
f=as.formula("Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + RdplW + NA.persons.actor + Pl.actor + NA.beast.of.burden.actor + actor.3 + actor.1 + actor.4 + (1|Lemma)")

cl <- makeCluster(10)
clusterExport(cl, c("f","data1","K","N","S","n","AWive","outcomes"))
models<- parLapply(cl,1:K, function(i) {library(lme4); glmer(f, data=data1[-S[i,],], family=binomial, control=glmerControl(optimizer = "bobyqa"))})
              
sapply(1:K, function(i){
  predictedprobs<-predict(models[[i]], newdata=data1[S[1, ],], allow.new.levels=T, type="response")
  predicted<-ifelse(predictedprobs>0.5, outcomes[1],outcomes[2])
  observed<-as.vector(ifelse(data1[S[1, ],"Ind"]>0.5, outcomes[1],outcomes[2]))
  probs<-cbind(predictedprobs,1-predictedprobs)
  colnames(probs) <- outcomes
  model.statistics(observed,predicted,probs)[c("accuracy","R2.likelihood","tau.classification")]
})