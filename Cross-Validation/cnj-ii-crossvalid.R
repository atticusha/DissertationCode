data1<-subset(AWCnj, II)
N=nrow(data1)
K=10
n=floor(N/K) 
S<-matrix(sample(1:N, K*n), K)

outcomes <- c("PV.e","PV.kaa","OtherCnj")

cl <- makeCluster(10)
clusterExport(cl, varlist=c("data1", "K", "S", "outcomes"))

parSapply(cl, 1:10, function(i) { library(lme4); library(polytomous);
  
  outcomes.logical <- cbind(data1[S[i,],"PV.e"], data1[S[i,],"PV.kaa"],
                            data1[S[i,],"OtherCnj"])
  
  outcomes.observed <- apply(outcomes.logical, 1, function(x)
    names(x)[which.max(x)])
  
  fpreds <- "II.sense + II.natural.land + II.weather + NI.object.actor +
(1 | Lemma)"
  
  outcomes.probs <- sapply(1:3, function(j) predict(glmer(formula =
                                                            as.formula(paste(c(outcomes[j], fpreds),collapse="~")), data =
                                                            data1[-S[i,],], family = binomial, control = glmerControl(optimizer =
                                                                                                                        "bobyqa")), type="response", newdata=data1[S[i,],], allow.new.levels=T))
  
  outcomes.probs <- t(apply(outcomes.probs, 1, function(x) x/sum(x)))
  
  colnames(outcomes.probs) <- outcomes
  
  outcomes.predicted <- apply(outcomes.probs, 1, function(x)
    names(x)[which.max(x)])
  
  model.statistics(outcomes.observed, outcomes.predicted,
                   outcomes.probs)[c("accuracy","R2.likelihood","tau.classification")]
  
})

stopCluster(cl)


