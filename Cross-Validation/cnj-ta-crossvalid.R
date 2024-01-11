subdf<-subset(AWCnj, TA)
N=nrow(subdf)
K=10
n=floor(N/K) 
S<-matrix(sample(1:N, K*n), K)

outcomes <- c("PV.e","PV.kaa","OtherCnj")

cl <- makeCluster(10)
clusterExport(cl, varlist=c("subdf", "K", "S", "outcomes"))

parSapply(cl, 1:10, function(i) { library(lme4); library(polytomous);
  
  outcomes.logical <- cbind(subdf[S[i,],"PV.e"], subdf[S[i,],"PV.kaa"],
                            subdf[S[i,],"OtherCnj"])
  
  outcomes.observed <- apply(outcomes.logical, 1, function(x)
    names(x)[which.max(x)])
  
  fpreds <- "PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal +
(1 | Lemma)"
  
  outcomes.probs <- sapply(1:3, function(j) predict(glmer(formula =
                                                            as.formula(paste(c(outcomes[j], fpreds),collapse="~")), data =
                                                            subdf[-S[i,],], family = binomial, control = glmerControl(optimizer =
                                                                                                                        "bobyqa")), type="response", newdata=subdf[S[i,],], allow.new.levels=T))
  
  outcomes.probs <- t(apply(outcomes.probs, 1, function(x) x/sum(x)))
  
  colnames(outcomes.probs) <- outcomes
  
  outcomes.predicted <- apply(outcomes.probs, 1, function(x)
    names(x)[which.max(x)])
  
  model.statistics(outcomes.observed, outcomes.predicted,
                   outcomes.probs)[c("accuracy","R2.likelihood","tau.classification")]
  
})

stopCluster(cl)