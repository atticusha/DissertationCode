subdf<-subset(AWCnj)

#Tokens
sapply(1:10, function(i) length(which(!(subdf$Lemma[S[i,]] %in% unique(subdf$Lemma[-S[i,]]))))/length(subdf$Lemma[S[i,]]))
#Types
sapply(1:10, function(i) length(setdiff(unique(subdf$Lemma[S[i,]]),unique(subdf$Lemma[-S[i,]])))/length(unique(subdf$Lemma[S[i,]])))
