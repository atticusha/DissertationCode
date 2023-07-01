#Univariate Conjunct Type Alternation
##==========================================================================================================
## Install packages (polytomous must be installed from source)
install.packages("./polytomous_0.1.6.tar.gz", repos = NULL, type="source")
library(polytomous)

##==========================================================================================================
##Check to see if cochrane condition is satisfied (because this chi.sq table is 2x6, there is 1 instance of <5 observations allowed
cochran_Cnjtypes <- function(uniobjuni) {
  length(which(chisq.test(uniobjuni$posthoc$ctable)$expected<5))>1
}

### II ANALYSIS
####Paste variables the occure more than ten times
paste(paste(names(which(sort(colSums(subset(AWCnj, II)[grep("PV.[A-Z]", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("^II...*", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("Rdpl", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("^N.*[0-9]$", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("^auto.*", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("actor$", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("^actor", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("^goal", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, II)[grep("goal$", names(subset(AWCnj, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

####Create initial univarite analysis
II.Univariate.Cnjtypes.0 <- nominal(OrderType ~ PV.Time + PV.Qual + PV.Move + II.sense + II.natural.land + II.weather + RdplS +  auto.vii.1 + auto.ni.3.actor + auto.vai.6 + auto.vii.2 + auto.vii.5 + auto.vii.3 + auto.vii.4 + auto.vai.1 + auto.vai.9 + auto.ni.6.actor + Sg.actor + NI.object.actor + auto.ni.3.actor + Dem.actor + Pron.actor + Prox.actor + Med.actor + NI.nominal.actor + Pl.actor + auto.ni.6.actor + actor.3 + actor.4, data = subset(AWCnj, II))

#### Check cochrane condition; step 1) remove things that aren't about 1
for (i in II.Univariate.Cnjtypes.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}

#####remove less than .8
II.Univariate.Cnjtypes.1 <- nominal(OrderType ~ PV.Time + PV.Move + II.sense + II.natural.land + II.weather + RdplS +  auto.vii.1 + auto.ni.3.actor + auto.vai.6 + auto.vii.2 + auto.vii.5 + auto.vii.3 + auto.vii.4 + Sg.actor + NI.object.actor + auto.ni.3.actor + Dem.actor + Pron.actor + Prox.actor + Med.actor + NI.nominal.actor + actor.3 + actor.4, data = subset(AWCnj, II))

#### move on to step 2) make sure 0 of expected are <5
issues.ii.Cnjtypes<-lapply(II.Univariate.Cnjtypes.1$univariate,cochran_Cnjtypes)
II.Univariate.Cnjtypes <- nominal(OrderType ~ PV.Time + II.sense + II.natural.land + II.weather +  auto.vii.1 + auto.ni.3.actor + auto.vai.6 + auto.vii.2 + auto.vii.5 + auto.vii.3 + auto.vii.4 + Sg.actor + NI.object.actor + auto.ni.3.actor + Dem.actor + Pron.actor + Prox.actor + Med.actor + NI.nominal.actor + actor.3 + actor.4, data = subset(AWCnj, II))

####Write to file
sink(file = './issues.ii.Cnjtypes.txt')
print(issues.ii.Cnjtypes, max.print = NA)
sink(NULL)

sink(file = './IIUni-cnjtypes.txt')
print(summary(II.Univariate.Cnjtypes), max.print = NA)
sink(NULL)

####Make significance table
II.Uni.cnj.Sig<- subset(summary(II.Univariate.Cnjtypes)$sumry.table, alpha.X2 <0.05)

####Write to file
sink(file = './IIUniSig-cnjtypes.txt')
print(II.Uni.cnj.Sig, max.print = NA)
sink(NULL)

###AI ANALYSIS
####Paste variables the occure more than ten times
paste(paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("PV.[A-Z]", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("^AI...*", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("Rdpl", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("^N.*[0-9]$", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("^auto.*", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("actor$", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("^actor", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("^goal", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, AI)[grep("goal$", names(subset(AWCnj, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

####Create initial univarite analysis
AI.Univariate.Cnjtypes.0 <- nominal(OrderType ~ PV.Time + PV.Move + PV.Qual + PV.Discourse + PV.StartFin + PV.Position + PV.WantCan + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.plural + AI.money.count + AI.pray + AI.heat.fire + RdplW + RdplS +  auto.vai.5 + auto.vai.2 + auto.vai.6 + auto.vai.12 + auto.vai.11 + auto.na.5.actor + auto.vai.9 + auto.vai.3 + auto.vai.4 + auto.vai.13 + auto.nda.1.actor + auto.na.7.actor + auto.vai.8 + auto.vai.1 + auto.vai.7 + auto.vai.10 + auto.na.3.actor + auto.nda.3.actor + auto.na.1.actor + auto.na.2.actor + auto.na.4.actor + auto.na.6.actor + NA.persons.actor + auto.na.5.actor + Sg.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Dem.actor + Pers.actor + auto.nda.1.actor + Px1Sg.actor + auto.na.7.actor + Prox.actor + Incl.actor + Med.actor + NA.beast.of.burden.actor + auto.na.3.actor + auto.nda.3.actor + Obv.actor + auto.na.1.actor + auto.na.2.actor + NA.food.actor + Indef.actor + Px3Sg.actor + auto.na.4.actor + auto.na.6.actor + Px12Pl.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWCnj, AI))

####Check cochrane condition; step 1) remove things that aren't about 1
for (i in AI.Univariate.Cnjtypes.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}


####No issue, move on to step 2) make sure 0 of expected are <5
issues.ai.Cnjtypes<-lapply(AI.Univariate.Cnjtypes.0$univariate,cochran_Cnjtypes)
AI.Univariate.Cnjtypes <- nominal(OrderType ~  PV.Time + PV.Move + PV.Qual + PV.Discourse + PV.StartFin + PV.Position + PV.WantCan + AI.do + AI.state + AI.speech + AI.reflexive + AI.cooking + AI.health + AI.plural + AI.money.count + AI.pray + RdplW + RdplS +  auto.vai.5 + auto.vai.2 + auto.vai.6 + auto.vai.12 + auto.vai.11 + auto.na.5.actor + auto.vai.9 + auto.vai.3 + auto.vai.4 + auto.vai.13 + auto.nda.1.actor + auto.na.7.actor + auto.vai.8 + auto.vai.1 + auto.vai.7 + auto.vai.10 + auto.na.3.actor + auto.nda.3.actor + auto.na.1.actor + auto.na.2.actor + NA.persons.actor + auto.na.5.actor + Sg.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Dem.actor + Pers.actor + auto.nda.1.actor + Px1Sg.actor + auto.na.7.actor + Prox.actor + Incl.actor + Med.actor + NA.beast.of.burden.actor + auto.na.3.actor + auto.nda.3.actor + Obv.actor + auto.na.1.actor + auto.na.2.actor + NA.food.actor + auto.na.4.actor + auto.na.6.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWCnj, AI))

####Write to file
sink(file = './issues.ai.Cnjtypes.txt')
print(issues.ai.Cnjtypes, max.print = NA)
sink(NULL)

sink(file = './AIUni-cnjtypes.txt')
print(summary(AI.Univariate.Cnjtypes), max.print = NA)
sink(NULL)

####Make significance table
AI.Uni.cnj.Sig<- subset(summary(AI.Univariate.Cnjtypes)$sumry.table, alpha.X2 <0.05)

####Write to file
sink(file = './AIUniSig-cnjtypes.txt')
print(AI.Uni.cnj.Sig, max.print = NA)
sink(NULL)

###TI ANALYSIS
####Paste variables the occure more than ten times
paste(paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("PV.[A-Z]", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("^TI...*", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("Rdpl", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("^N.*[0-9]$", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("^auto.*", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("actor$", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("^actor", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("^goal", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TI)[grep("goal$", names(subset(AWCnj, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

####Create initial univarite analysis
TI.Univariate.Cnjtypes.0 <- nominal(OrderType ~  PV.Time + PV.Move + PV.Discourse + PV.WantCan + PV.Qual + PV.Position + PV.StartFin + TI.do + TI.cognitive + TI.speech + TI.money.count + RdplS + RdplW +  auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.vti.3 + auto.na.5.actor + auto.ni.9.goal + auto.ndi.1.goal + auto.ni.2.goal + auto.ni.10.goal + auto.ni.6.goal + auto.na.7.actor + auto.nda.1.actor + auto.ni.4.goal + auto.ni.7.goal + auto.ni.1.goal + auto.na.1.actor + auto.ni.8.goal + auto.ni.5.goal + auto.vti.6 + auto.vti.5 + NA.persons.actor + auto.na.5.actor + Sg.actor + Pron.actor + Pl.actor + Pers.actor + D.actor + NDA.Relations.actor + Dem.actor + auto.na.7.actor + auto.nda.1.actor + Px1Sg.actor + Prox.actor + auto.na.1.actor + Incl.actor + actor.3 + actor.1 + actor.2 + actor.4 + Sg.goal + NI.object.goal + auto.ni.3.goal + Dem.goal + Pron.goal + Pl.goal + Prox.goal + NI.nominal.goal + Med.goal + NI.natural.force.goal + auto.ni.9.goal + D.goal + auto.ndi.1.goal + auto.ni.2.goal + NDI.Body.goal + auto.ni.10.goal + NI.place.goal + auto.ni.6.goal + NI.nature.plants.goal + Px3Sg.goal + Der.Dim.goal + auto.ni.4.goal + auto.ni.7.goal + auto.ni.1.goal + auto.ni.8.goal + auto.ni.5.goal + Px3Pl.goal + Px12Pl.goal, data = subset(AWCnj, TI))

####Check cochrane condition; step 1) remove things that aren't about 1
for (i in TI.Univariate.Cnjtypes.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}


####No issue, move on to step 2) make sure 0 of expected are <5
issues.ti.Cnjtypes<-lapply(TI.Univariate.Cnjtypes.0$univariate,cochran_Cnjtypes)

####Create final univarite analysis
TI.Univariate.Cnjtypes <- nominal(OrderType ~ PV.Time + PV.Move + PV.Discourse + PV.WantCan + PV.Qual + PV.Position + TI.do + TI.cognitive + TI.speech + RdplS +  auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.vti.3 + auto.na.5.actor + auto.ni.9.goal + auto.ndi.1.goal + auto.ni.2.goal + auto.ni.10.goal + auto.ni.6.goal + NA.persons.actor + auto.na.5.actor + Sg.actor + Pron.actor + Pl.actor + Pers.actor + D.actor + NDA.Relations.actor + auto.na.7.actor + auto.nda.1.actor + auto.na.1.actor + actor.3 + actor.1 + actor.2 + Sg.goal + NI.object.goal + auto.ni.3.goal + Dem.goal + Pron.goal + Pl.goal + Prox.goal + NI.nominal.goal + Med.goal + NI.natural.force.goal + auto.ni.9.goal + D.goal + auto.ndi.1.goal + auto.ni.2.goal + NDI.Body.goal + auto.ni.10.goal + NI.place.goal + auto.ni.6.goal + NI.nature.plants.goal + Px3Sg.goal + auto.ni.4.goal + auto.ni.7.goal + auto.ni.1.goal + auto.ni.8.goal + auto.ni.5.goal, data = subset(AWCnj, TI))

###Write to file
sink(file = './issues.ti.Cnjtypes.txt')
print(issues.ti.Cnjtypes, max.print = NA)
sink(NULL)

sink(file = './TIUni-cnjtypes.txt')
print(summary(TI.Univariate.Cnjtypes), max.print = NA)
sink(NULL)

###Make significance table
TI.Uni.cnj.Sig<- subset(summary(TI.Univariate.Cnjtypes)$sumry.table, alpha.X2 <0.05)

###Write to file
sink(file = './TIUniSig-cnjtypes.txt')
print(TI.Uni.cnj.Sig, max.print = NA)
sink(NULL)

###TA ANALYSIS
####Paste variables the occur more than ten times
paste(paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("PV.[A-Z]", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("^TA...*", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("Rdpl", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("^N.*[0-9]$", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("^auto.*", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("actor$", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("^actor", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("^goal", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWCnj, TA)[grep("goal$", names(subset(AWCnj, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

####Create initial univarite analysis
TA.Univariate.Cnjtypes.0 <- nominal(OrderType ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.WantCan + PV.StartFin + TA.do + TA.cognitive + TA.speech + TA.food + TA.money.count + TA.allow + RdplS + RdplW +  auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + auto.nda.1.goal + auto.na.5.actor + auto.vta.2 + auto.na.7.goal + auto.na.2.goal + auto.na.3.goal + auto.nda.1.actor + auto.na.7.actor + auto.nda.3.goal + auto.vta.7 + auto.nda.3.actor + auto.na.1.actor + auto.na.6.goal + auto.na.1.goal + NA.persons.actor + Sg.actor + auto.na.5.actor + Pron.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + auto.nda.1.actor + Dem.actor + auto.na.7.actor + Obv.actor + Pl.actor + Prox.actor + Pers.actor + auto.nda.3.actor + auto.na.1.actor + Med.actor + NA.religious.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + goal.5 + NA.persons.goal + auto.na.5.goal + Sg.goal + Obv.goal + D.goal + NDA.Relations.goal + Pl.goal + Pron.goal + auto.nda.1.goal + Dem.goal + NA.beast.of.burden.goal + Px1Sg.goal + Prox.goal + auto.na.7.goal + auto.na.2.goal + ayisiyiniw.goal + auto.na.3.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + NA.food.goal + Px3Pl.goal + Med.goal + auto.na.6.goal + auto.na.1.goal + Px12Pl.goal + Incl.goal, data = subset(AWCnj, TA))

####Check cochrane condition; step 1) remove things that aren't about 1
for (i in TA.Univariate.Cnjtypes.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}

####No issue, move on to step 2) make sure 0 of expected are <5
issues.ta.Cnjtypes<-lapply(TA.Univariate.Cnjtypes.0$univariate,cochran_Cnjtypes)

####Create final univarite analysis
TA.Univariate.Cnjtypes <- nominal(OrderType ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.WantCan + PV.StartFin + TA.do + TA.cognitive + TA.speech + TA.food + TA.money.count + TA.allow + RdplS + RdplW +  auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.vta.4 + auto.nda.1.goal + auto.na.5.actor + auto.vta.2 + auto.na.7.goal + auto.na.2.goal + auto.na.3.goal + auto.nda.1.actor + auto.na.7.actor + auto.nda.3.goal + NA.persons.actor + Sg.actor + auto.na.5.actor + Pron.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + auto.nda.1.actor + Dem.actor + auto.na.7.actor + Obv.actor + Pl.actor + Prox.actor + Pers.actor + auto.nda.3.actor + auto.na.1.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + NA.persons.goal + auto.na.5.goal + Sg.goal + Obv.goal + D.goal + NDA.Relations.goal + Pl.goal + Pron.goal + auto.nda.1.goal + Dem.goal + NA.beast.of.burden.goal + Px1Sg.goal + Prox.goal + auto.na.7.goal + auto.na.2.goal + ayisiyiniw.goal + auto.na.3.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + auto.na.6.goal + auto.na.1.goal, data = subset(AWCnj, TA))

###Write to file
sink(file = './issues.ta.Cnjtypes.txt')
print(issues.ta.Cnjtypes, max.print = NA)
sink(NULL)

sink(file = './TAUni-cnjtypes.txt')
print(summary(TA.Univariate.Cnjtypes), max.print = NA)
sink(NULL)

###Make significance table
TA.Uni.cnj.Sig<- subset(summary(TA.Univariate.Cnjtypes)$sumry.table, alpha.X2 <0.05)

###Write to file
sink(file = './TAUniSig-cnjtypes.txt')
print(TA.Uni.cnj.Sig, max.print = NA)
sink(NULL)
