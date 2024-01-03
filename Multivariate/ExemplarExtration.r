#Exemplar Extraction for all outcomes

##Probability Extraction
### Independent vs. ê-Conjunct
####Build new frames with probability
p.ii.AWive<-subset(AWive, II)
p.ii.AWive$p.val<-ii.ive.p.values.my[,1]

p.ai.AWive<-subset(AWive, AI)
p.ai.AWive$p.val<-ai.ive.p.values.my[,1]

p.ti.AWive<-subset(AWive, TI)
p.ti.AWive$p.val<-ti.ive.p.values.my[,1]

p.ta.AWive<-subset(AWive, TA)
p.ta.AWive$p.val<-ta.ive.p.values.my[,1]

####Add relevant variables from the predictive modelling
ex.ii.ive<-subset(p.ii.AWive, select = c(Lemma, Ind, PV.Time, Index, p.val))
ex.ai.ive<-subset(p.ai.AWive, select = c(Lemma, Ind, PV.Time, PV.Discourse, actor.3, actor.1, Index, p.val))
ex.ti.ive<-subset(p.ti.AWive, select = c(Lemma, Ind, PV.Discourse, TI.do, TI.money.count, NA.persons.actor, actor.3, actor.2, NI.place.goal, Px1Sg.goal, Index, p.val))
ex.ta.ive<-subset(p.ta.AWive, select = c(Lemma, Ind, TA.food, PV.Position, PV.Discourse, Obv.actor, PV.Move, PV.Time, actor.1, actor.2, Index, p.val))



### Independent vs. Conjunct
####Build new frames with probability
p.ii.AWnimp<-subset(AWnimp, II)
p.ii.AWnimp$p.val<-ii.indcnj.p.values.my[,1]

p.ai.AWnimp<-subset(AWnimp, AI)
p.ai.AWnimp$p.val<-ai.indcnj.p.values.my[,1]

p.ti.AWnimp<-subset(AWnimp, TI)
p.ti.AWnimp$p.val<-ti.indcnj.p.values.my[,1]

p.ta.AWnimp<-subset(AWnimp, TA)
p.ta.AWnimp$p.val<-ta.indcnj.p.values.my[,1]

####Add relevant variables from the predictive modelling
ex.ii.ivc<-subset(p.ii.AWnimp, select = c(Lemma, Ind, PV.Time, Index, p.val))
ex.ai.ivc<-subset(p.ai.AWnimp, select = c(Lemma, Ind, PV.Discourse, Obv.actor, Sg.actor, PV.Time, Index, p.val))
ex.ti.ivc<-subset(p.ti.AWnimp, select = c(Lemma, Ind, PV.Discourse, TI.money.count, NI.place.goal, NDI.Body.goal, actor.3, NI.nominal.goal, TI.do, actor.2, NA.persons.actor, Px1Sg.goal, Index, p.val))
ex.ta.ivc<-subset(p.ta.AWnimp, select = c(Lemma, Ind, TA.food, PV.Position, Obv.actor, PV.Move, PV.Time, Sg.actor, goal.2, NA.persons.goal, Obv.goal, actor.1, Index, p.val))


### Conjunct Type: ê-Conjunct
####Build new frames with probability
p.ii.AWCnj.e<-subset(AWCnj, II)
p.ii.AWCnj.e$p.val<-ii.eCnj.cnjtypes.p.values.my[,1]

p.ai.AWCnj.e<-subset(AWCnj, AI)
p.ai.AWCnj.e$p.val<-ai.eCnj.cnjtypes.p.values.my[,1]

p.ti.AWCnj.e<-subset(AWCnj, TI)
p.ti.AWCnj.e$p.val<-ti.eCnj.cnjtypes.p.values.my[,1]

p.ta.AWCnj.e<-subset(AWCnj, TA)
p.ta.AWCnj.e$p.val<-ta.eCnj.cnjtypes.p.values.my[,1]


### Conjunct Type: kâ-Conjunct
####Build new frames with probability
p.ii.AWCnj.kaa<-subset(AWCnj, II)
p.ii.AWCnj.kaa$p.val<-ii.kaaCnj.cnjtypes.p.values.my[,1]

p.ai.AWCnj.kaa<-subset(AWCnj, AI)
p.ai.AWCnj.kaa$p.val<-ai.kaaCnj.cnjtypes.p.values.my[,1]

p.ti.AWCnj.kaa<-subset(AWCnj, TI)
p.ti.AWCnj.kaa$p.val<-ti.kaaCnj.cnjtypes.p.values.my[,1]

## VTA build new frame with probability
p.ta.AWCnj.kaa<-subset(AWCnj, TA)
p.ta.AWCnj.kaa$p.val<-ta.kaaCnj.cnjtypes.p.values.my[,1]

### Conjunct Type: Other Conjunct
####Build new frames with probability
p.ii.AWCnj.other<-subset(AWCnj, II)
p.ii.AWCnj.other$p.val<-ii.OtherCnj.cnjtypes.p.values.my[,1]

p.ai.AWCnj.other<-subset(AWCnj, AI)
p.ai.AWCnj.other$p.val<-ai.OtherCnj.cnjtypes.p.values.my[,1]

p.ti.AWCnj.other<-subset(AWCnj, TI)
p.ti.AWCnj.other$p.val<-ti.OtherCnj.cnjtypes.p.values.my[,1]

p.ta.AWCnj.other<-subset(AWCnj, TA)
p.ta.AWCnj.other$p.val<-ta.OtherCnj.cnjtypes.p.values.my[,1]

###Add relevant variables from the predictive modelling
ex.ecnj.ii.cnjtype<-subset(p.ii.AWCnj.e, select = c(, Index, p.val))
ex.kaacnj.ii.cnjtype<-subset(p.ii.AWCnj.kaa, select = c(Lemma, Ind, II.weather, Index, p.val))
ex.other.ii.cnjtype<-subset(p.ii.AWCnj.other, select = c(, Index, p.val))

ex.ecnj.ai.cnjtype<-subset(p.ai.AWCnj.e, select = c(Lemma, Ind, actor.2, Sg.actor, actor.1, NDA.Relations.actor, actor.3, RdplW, PV.Discourse, PV.Position, Index, p.val))
ex.kaacnj.ai.cnjtype<-subset(p.ai.AWCnj.kaa, select = c(Lemma, Ind, Sg.actor, actor.1, actor.3, PV.Discourse, PV.Position, PV.WantCan, AI.cooking, NA.persons.actor, Pl.actor, Index, p.val))
ex.other.ai.cnjtype<-subset(p.ai.AWCnjs.other, select = c(Lemma, Ind, actor.2, NDA.Relations.actor, PV.WantCan, PV.Qual, AI.health, Index, p.val))

ex.ecnj.ti.cnjtype<-subset(p.ti.AWCnj.e, select = c(Lemma, Ind, actor.2, Prox.goal, PV.WantCan, TI.speech, NI.nominal.goal, Sg.goal, PV.Position, Index, p.val))
ex.kaacnj.ti.cnjtype<-subset(p.ti.AWCnj.kaa, select = c(Lemma, Ind, Prox.goal, PV.WantCan, TI.speech, PV.Position, NA.persons.actor, Index, p.val))
ex.other.ti.cnjtype<-subset(p.ti.AWCnj.other, select = c(Lemma, Ind, actor.2, NI.nominal.goal, PV.Position, NI.object.goal, Med.goal, Index, p.val))

ex.ecnj.ta.cnjtype<-subset(p.ta.AWCnj.e, select = c(Lemma, Ind, actor.2, Prox.goal, Sg.goal, actor.1, Px1Sg.goal, PV.Discourse, PV.Position, Index, p.val))
ex.kaacnj.ta.cnjtype<-subset(p.ta.AWCnj.kaa, select = c(Lemma, Ind, Prox.actor, Sg.actor, PV.Position, TA.cognitive, Index, p.val))
ex.other.ta.cnjtype<-subset(p.ta.AWCnj.other, select = c(Lemma, Ind, actor.1, PV.Discourse, TA.cognitive, Index, p.val))




##Make Logical Dataframes for Clustering
###Independent vs. ê-Conjunct
ex.ii.ive.logic<-subset(p.ii.AWive, select = c(Ind, PV.Time))
ex.ai.ive.logic<-subset(p.ai.AWive, select = c(Ind, PV.Time, PV.Discourse, actor.3, actor.1))
ex.ti.ive.logic<-subset(p.ti.AWive, select = c(Ind, PV.Discourse, TI.do, TI.money.count, NA.persons.actor, actor.3, actor.2, NI.place.goal, Px1Sg.goal))
ex.ta.ive.logic<-subset(p.ta.AWive, select = c(Ind, TA.food, PV.Position, PV.Discourse, Obv.actor, PV.Move, PV.Time, actor.1, actor.2))

###Independent vs. Conjunct
ex.ii.ivc.logic<-subset(p.ii.AWnimp, select = c(Ind, PV.Time))
ex.ai.ivc.logic<-subset(p.ai.AWnimp, select = c(Ind, PV.Discourse, Obv.actor, Sg.actor, PV.Time))
ex.ti.ivc.logic<-subset(p.ti.AWnimp, select = c(Ind, PV.Discourse, TI.money.count, NI.place.goal, NDI.Body.goal, actor.3, NI.nominal.goal, TI.do, actor.2, NA.persons.actor, Px1Sg.goal))
ex.ta.ivc.logic<-subset(p.ta.AWnimp, select = c(Ind, TA.food, PV.Position, Obv.actor, PV.Move, PV.Time, Sg.actor, goal.2, NA.persons.goal, Obv.goal, actor.1))


###Conjunct Type
####VII
ex.ecnj.ii.cnjtype.logic<-subset(p.ii.AWCnj.e, select = c(PV.e))
ex.kaacnj.ii.cnjtype.logic<-subset(p.ii.AWCnj.kaa, select = c(PV.kaa, II.weather))
ex.other.ii.cnjtype.logic<-subset(p.ii.AWCnj.other, select = c(OtherCnj))

####VAI
ex.ecnj.ai.cnjtype.logic<-subset(p.ai.AWCnj.e, select = c(PV.e, actor.2, Sg.actor, actor.1, NDA.Relations.actor, actor.3, RdplW, PV.Discourse, PV.Position))
ex.kaacnj.ai.cnjtype.logic<-subset(p.ai.AWCnj.kaa, select = c(PV.kaa, Sg.actor, actor.1, actor.3, PV.Discourse, PV.Position, PV.WantCan, AI.cooking, NA.persons.actor, Pl.actor))
ex.other.ai.cnjtype.logic<-subset(p.ai.AWCnj.other, select = c(OtherCnj, actor.2, NDA.Relations.actor, PV.WantCan, PV.Qual, AI.health))

####VTI
ex.ecnj.ti.cnjtype.logic<-subset(p.ti.AWCnj.e, select = c(PV.e, actor.2, Prox.goal, PV.WantCan, TI.speech, NI.nominal.goal, Sg.goal, PV.Position))
ex.kaacnj.ti.cnjtype.logic<-subset(p.ti.AWCnj.kaa, select = c(PV.kaa, Prox.goal, PV.WantCan, TI.speech, PV.Position, NA.persons.actor))
ex.other.ti.cnjtype.logic<-subset(p.ti.AWCnj.other, select = c(OtherCnj, actor.2, NI.nominal.goal, PV.Position, NI.object.goal, Med.goal))

####VTA
ex.ecnj.ta.cnjtype.logic<-subset(p.ta.AWCnj.e, select = c(PV.e, actor.2, Prox.goal, Sg.goal, actor.1, Px1Sg.goal, PV.Discourse, PV.Position))
ex.kaacnj.ta.cnjtype.logic<-subset(p.ta.AWCnj.kaa, select = c(PV.kaa, Prox.actor, Sg.actor, PV.Position, TA.cognitive))
ex.other.ta.cnjtype.logic<-subset(p.ta.AWCnj.other, select = c(OtherCnj, actor.1, PV.Discourse, TA.cognitive))






# DIST MATRICES========================================================
## IVE==================================================
#dist.ii.ive<-dist(ex.ii.ive.logic, method = "binary")
#dist.ai.ive<-dist(ex.ai.ive.logic, method = "binary")
#dist.ti.ive<-dist(ex.ti.ive.logic, method = "binary")
#dist.ta.ive<-dist(ex.ta.ive.logic, method = "binary")

## IVC==================================================
dist.ii.ivc<-dist(ex.ii.ivc.logic, method = "binary")
dist.ai.ivc<-dist(ex.ai.ivc.logic, method = "binary")
dist.ti.ivc<-dist(ex.ti.ivc.logic, method = "binary")
dist.ta.ivc<-dist(ex.ta.ivc.logic, method = "binary")

## CNJ TYPE==========================================
#dist.ii.ecnj.cnj<-dist(ex.ecnj.ii.cnjtype.logic, method = "binary")
#dist.ii.kaacnj.cnj<-dist(ex.kaacnj.ii.cnjtype.logic, method = "binary")
#dist.ii.other.cnj<-dist(ex.other.ii.cnjtype.logic, method = "binary")

#dist.ai.ecnj.cnj<-dist(ex.ecnj.ai.cnjtype.logic, method = "binary")
#dist.ai.kaacnj.cnj<-dist(ex.kaacnj.ai.cnjtype.logic, method = "binary")
#dist.ai.other.cnj<-dist(ex.other.ai.cnjtype.logic, method = "binary")

#dist.ti.ecnj.cnj<-dist(ex.ecnj.ti.cnjtype.logic, method = "binary")
#dist.ti.kaacnj.cnj<-dist(ex.kaacnj.ti.cnjtype.logic, method = "binary")
#dist.ti.other.cnj<-dist(ex.other.ti.cnjtype.logic, method = "binary")

#dist.ta.ecnj.cnj<-dist(ex.ecnj.ta.cnjtype.logic, method = "binary")
#dist.ta.kaacnj.cnj<-dist(ex.kaacnj.ta.cnjtype.logic, method = "binary")
#dist.ta.other.cnj<-dist(ex.other.ta.cnjtype.logic, method = "binary")




# CLUSTER================================
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend)

##Clustering

###Independent vs. ê-Conjunct
fviz_silhouette(hcut(ex.ii.ive.logic, k = 4, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.ai.ive.logic, k = 20, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.ti.ive.logic, k = 39, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.ta.ive.logic, k = 55, method = "ward.D2", hc_metric = "binary" ))


p.ii.AWive$cluster<-hcut(ex.ii.ive.logic, k = 4, method = "ward.D2" )$cluster
p.ai.AWive$cluster<-hcut(ex.ai.ive.logic, k = 20, method = "ward.D2" )$cluster
p.ti.AWive$cluster<-hcut(ex.ti.ive.logic, k = 39, method = "ward.D2" )$cluster
p.ta.AWive$cluster<-hcut(ex.ta.ive.logic, k = 55, method = "ward.D2" )$cluster

###Independent vs. Conjunct
fviz_silhouette(hcut(ex.ii.ivc.logic, k = 4, method = "ward.D2" ))
fviz_silhouette(hcut(ex.ai.ivc.logic, k = 17, method = "ward.D2"))
fviz_silhouette(hcut(ex.ti.ivc.logic, k = 77, method = "ward.D2" ))
fviz_silhouette(hcut(ex.ta.ivc.logic, k = 110, method = "ward.D2" ))

fviz_silhouette(hcut(ex.ii.ivc.logic, k = 4, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.ai.ivc.logic, k = 17, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.ti.ivc.logic, k = 77, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.ta.ivc.logic, k = 110, method = "ward.D2", hc_metric = "binary" ))


p.ii.AWnimp$cluster<-hcut(ex.ii.ivc.logic, k = 4, method = "ward.D2" )$cluster
p.ai.AWnimp$cluster<-hcut(ex.ai.ivc.logic, k = 17, method = "ward.D2" )$cluster
p.ti.AWnimp$cluster<-hcut(ex.ti.ivc.logic, k = 77, method = "ward.D2" )$cluster
p.ta.AWnimp$cluster<-hcut(ex.ta.ivc.logic, k = 110, method = "ward.D2" )$cluster


## CNJ TYPE CLUSTER================================

### ecnj ======================

fviz_silhouette(hcut(ex.ecnj.ii.cnjtype.logic, k = 2 , method = "ward.D2" ))
fviz_silhouette(hcut(ex.ecnj.ai.cnjtype.logic, k = 50, method = "ward.D2" ))
fviz_silhouette(hcut(ex.ecnj.ti.cnjtype.logic, k = 69, method = "ward.D2" ))
fviz_silhouette(hcut(ex.ecnj.ta.cnjtype.logic, k = 31, method = "ward.D2" ))

fviz_silhouette(hcut(ex.ecnj.ii.cnjtype.logic, k = 2 , method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.ecnj.ai.cnjtype.logic, k = 50, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.ecnj.ti.cnjtype.logic, k = 69, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.ecnj.ta.cnjtype.logic, k = 31, method = "ward.D2", hc_metric = "binary" ))


p.ii.AWCnj.e$cluster<-hcut(ex.ecnj.ii.cnjtype.logic, k = 2, method = "ward.D2" )$cluster
p.ai.AWCnj.e$cluster<-hcut(ex.ecnj.ai.cnjtype.logic, k = 50, method = "ward.D2" )$cluster
p.ti.AWCnj.e$cluster<-hcut(ex.ecnj.ti.cnjtype.logic, k = 69, method = "ward.D2" )$cluster
p.ta.AWCnj.e$cluster<-hcut(ex.ecnj.ta.cnjtype.logic, k = 31, method = "ward.D2" )$cluster

### kaacnj ======================

fviz_silhouette(hcut(ex.kaacnj.ii.cnjtype.logic, k = 2, method = "ward.D2" ))
fviz_silhouette(hcut(ex.kaacnj.ai.cnjtype.logic, k = 53, method = "ward.D2" ))
fviz_silhouette(hcut(ex.kaacnj.ti.cnjtype.logic, k = 22, method = "ward.D2" ))
fviz_silhouette(hcut(ex.kaacnj.ta.cnjtype.logic, k = 16, method = "ward.D2" ))

fviz_silhouette(hcut(ex.kaacnj.ii.cnjtype.logic, k = 4, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.kaacnj.ai.cnjtype.logic, k = 53, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.kaacnj.ti.cnjtype.logic, k = 22, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.kaacnj.ta.cnjtype.logic, k = 16, method = "ward.D2", hc_metric = "binary" ))


p.ii.AWCnj.kaa$cluster<-hcut(ex.kaacnj.ii.cnjtype.logic, k = 2, method = "ward.D2" )$cluster
p.ai.AWCnj.kaa$cluster<-hcut(ex.kaacnj.ai.cnjtype.logic, k = 53, method = "ward.D2" )$cluster
p.ti.AWCnj.kaa$cluster<-hcut(ex.kaacnj.ti.cnjtype.logic, k = 22, method = "ward.D2" )$cluster
p.ta.AWCnj.kaa$cluster<-hcut(ex.kaacnj.ta.cnjtype.logic, k = 16, method = "ward.D2" )$cluster

### othercnj ======================

fviz_silhouette(hcut(ex.other.ii.cnjtype.logic, k = 2, method = "ward.D2" ))
fviz_silhouette(hcut(ex.other.ai.cnjtype.logic, k = 13, method = "ward.D2" ))
fviz_silhouette(hcut(ex.other.ti.cnjtype.logic, k = 21, method = "ward.D2" ))
fviz_silhouette(hcut(ex.other.ta.cnjtype.logic, k = 11, method = "ward.D2"))


fviz_silhouette(hcut(ex.other.ii.cnjtype.logic, k = 2, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.other.ai.cnjtype.logic, k = 13, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.other.ti.cnjtype.logic, k = 21, method = "ward.D2", hc_metric = "binary" ))
fviz_silhouette(hcut(ex.other.ta.cnjtype.logic, k = 11, method = "ward.D2", hc_metric = "binary" ))


p.ii.AWCnj.other$cluster<-hcut(ex.other.ii.cnjtype.logic, k = 2, method = "ward.D2" )$cluster
p.ai.AWCnj.other$cluster<-hcut(ex.other.ai.cnjtype.logic, k = 13, method = "ward.D2" )$cluster
p.ti.AWCnj.other$cluster<-hcut(ex.other.ti.cnjtype.logic, k = 21, method = "ward.D2" )$cluster
p.ta.AWCnj.other$cluster<-hcut(ex.other.ta.cnjtype.logic, k = 11, method = "ward.D2" )$cluster

#if IND and p > 0.5, make new df

# EXEMPLAR MODELS
## IVE ============================
ii.awive.exemplar<-subset(p.ii.AWive, ( p.val > 0.5 & Ind==TRUE) | (p.val <= 0.5 & PV.e==TRUE) )
ai.awive.exemplar<-subset(p.ai.AWive, ( p.val > 0.5 & Ind==TRUE) | (p.val <= 0.5 & PV.e==TRUE) )
ti.awive.exemplar<-subset(p.ti.AWive, ( p.val > 0.5 & Ind==TRUE) | (p.val <= 0.5 & PV.e==TRUE) )
ta.awive.exemplar<-subset(p.ta.AWive, ( p.val > 0.5 & Ind==TRUE) | (p.val <= 0.5 & PV.e==TRUE) )

## IVC ============================
ii.awivc.exemplar<-subset(p.ii.AWnimp, ( p.val > 0.5 & Ind==TRUE) | (p.val <= 0.5 & Cnj==TRUE) )
ai.awivc.exemplar<-subset(p.ai.AWnimp, ( p.val > 0.5 & Ind==TRUE) | (p.val <= 0.5 & Cnj==TRUE) )
ti.awivc.exemplar<-subset(p.ti.AWnimp, ( p.val > 0.5 & Ind==TRUE) | (p.val <= 0.5 & Cnj==TRUE) )
ta.awivc.exemplar<-subset(p.ta.AWnimp, ( p.val > 0.5 & Ind==TRUE) | (p.val <= 0.5 & Cnj==TRUE) )

## CNJTYPE ============================
### ecnj
ii.cnjtype.e.exemplar<-subset(p.ii.AWCnj.e, ( p.val < 0.5 & PV.e==FALSE) | (p.val >= 0.5 & PV.e==TRUE) )
ai.cnjtype.e.exemplar<-subset(p.ai.AWCnj.e, ( p.val < 0.5 & PV.e==FALSE) | (p.val >= 0.5 & PV.e==TRUE) )
ti.cnjtype.e.exemplar<-subset(p.ti.AWCnj.e, ( p.val < 0.5 & PV.e==FALSE) | (p.val >= 0.5 & PV.e==TRUE) )
ta.cnjtype.e.exemplar<-subset(p.ta.AWCnj.e, ( p.val < 0.5 & PV.e==FALSE) | (p.val >= 0.5 & PV.e==TRUE) )

### kaacnj
ii.cnjtype.kaa.exemplar<-subset(p.ii.AWCnj.kaa, (  p.val < 0.5 & PV.kaa==FALSE ) | (p.val >= 0.5 & PV.kaa==TRUE) )
ai.cnjtype.kaa.exemplar<-subset(p.ai.AWCnj.kaa, (  p.val < 0.5 & PV.kaa==FALSE ) | (p.val >= 0.5 & PV.kaa==TRUE) )
ti.cnjtype.kaa.exemplar<-subset(p.ti.AWCnj.kaa, (  p.val < 0.5 & PV.kaa==FALSE ) | (p.val >= 0.5 & PV.kaa==TRUE) )
ta.cnjtype.kaa.exemplar<-subset(p.ta.AWCnj.kaa, (  p.val < 0.5 & PV.kaa==FALSE ) | (p.val >= 0.5 & PV.kaa==TRUE) )

### othercnj
ii.cnjtype.other.exemplar<-subset(p.ii.AWCnj.other, ( p.val < 0.5 & OtherCnj==FALSE) | (p.val >= 0.5 & OtherCnj==TRUE) )
ai.cnjtype.other.exemplar<-subset(p.ai.AWCnj.other, ( p.val < 0.5 & OtherCnj==FALSE) | (p.val >= 0.5 & OtherCnj==TRUE) )
ti.cnjtype.other.exemplar<-subset(p.ti.AWCnj.other, ( p.val < 0.5 & OtherCnj==FALSE) | (p.val >= 0.5 & OtherCnj==TRUE) )
ta.cnjtype.other.exemplar<-subset(p.ta.AWCnj.other, ( p.val < 0.5 & OtherCnj==FALSE) | (p.val >= 0.5 & OtherCnj==TRUE) )



#EXTRACT CLUSTER INDICIES


# SORT =====================
## IVE ===========================
write.csv(ii.awive.exemplar[with(ii.awive.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ii.awive.ex.csv")
write.csv(ai.awive.exemplar[with(ai.awive.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/tstai.awive.ex.csv")
write.csv(ti.awive.exemplar[with(ti.awive.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/tstti.awive.ex.csv")
write.csv(ta.awive.exemplar[with(ta.awive.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/tstta.awive.ex.csv")


## IVC ==========================
write.csv(ii.awivc.exemplar[with(ii.awivc.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ii.awivc.ex.csv")
write.csv(ai.awivc.exemplar[with(ai.awivc.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ai.awivc.ex.csv")
write.csv(ti.awivc.exemplar[with(ti.awivc.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ti.awivc.ex.csv")
write.csv(ta.awivc.exemplar[with(ta.awivc.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ta.awivc.ex.csv")

## CNJTYPE ================
### ecnj
write.csv(ii.cnjtype.e.exemplar[with(ii.cnjtype.e.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ii.cnj.e.ex.csv")
write.csv(ai.cnjtype.e.exemplar[with(ai.cnjtype.e.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ai.cnj.e.ex.csv")
write.csv(ti.cnjtype.e.exemplar[with(ti.cnjtype.e.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ti.cnj.e.ex.csv")
write.csv(ta.cnjtype.e.exemplar[with(ta.cnjtype.e.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ta.cnj.e.ex.csv")

### kaa
write.csv(ii.cnjtype.kaa.exemplar[with(ii.cnjtype.kaa.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/bonkii.cnj.kaa.ex.csv")
write.csv(ai.cnjtype.kaa.exemplar[with(ai.cnjtype.kaa.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ai.cnj.kaa.ex.csv")
write.csv(ti.cnjtype.kaa.exemplar[with(ti.cnjtype.kaa.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ti.cnj.kaa.ex.csv")
write.csv(ta.cnjtype.kaa.exemplar[with(ta.cnjtype.kaa.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ta.cnj.kaa.ex.csv")

### Other
write.csv(ii.cnjtype.other.exemplar[with(ii.cnjtype.other.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ii.cnj.other.ex.csv")
write.csv(ai.cnjtype.other.exemplar[with(ai.cnjtype.other.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ai.cnj.other.ex.csv")
write.csv(ti.cnjtype.other.exemplar[with(ti.cnjtype.other.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ti.cnj.other.ex.csv")
write.csv(ta.cnjtype.other.exemplar[with(ta.cnjtype.other.exemplar, order(cluster,p.val)),][myvars],"~/Desktop/bonk/ta.cnj.other.ex.csv")

