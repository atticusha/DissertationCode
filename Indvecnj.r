#Univariate Independent vs. ê-Conjunct Alternation
##==========================================================================================================
## Install packages (polytomous must be installed from source)
install.packages("./polytomous_0.1.6.tar.gz", repos = NULL, type="source")
library(polytomous)

##==========================================================================================================
##Check to see if cochrane condition is satisfied (because this chi.sq table is 2x2, there are 0 instances of <5 observations allowed
cochran_ive <- function(uniobjuni) {
  length(which(chisq.test(uniobjuni$posthoc$ctable)$expected<5))>0
}

##==========================================================================================================
## Ind v. eCnj

### II ANALYSIS
####Paste variables the occure more than ten times
paste(paste(names(which(sort(colSums(subset(AWive, II)[grep("PV.[A-Z]", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, II)[grep("^II...*", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, II)[grep("Rdpl", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, II)[grep("^N.*[0-9]$", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWive, II)[grep("^auto.*", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, II)[grep("actor$", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, II)[grep("^actor", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, II)[grep("^goal", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, II)[grep("goal$", names(subset(AWive, II)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

####Create initial univarite analysis
II.Univariate.ive.0 <- nominal(OrderType ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Position + II.sense + II.natural.land + II.weather + RdplS +  auto.vii.1 + auto.ni.3.actor + auto.vai.6 + auto.vii.2 + auto.vii.3 + auto.vii.4 + auto.vii.5 + auto.ni.6.actor + auto.vai.9 + auto.vai.1 + auto.ni.7.actor + Sg.actor + auto.ni.3.actor + NI.object.actor + Pron.actor + Dem.actor + Prox.actor + NI.nominal.actor + Pl.actor + Med.actor + auto.ni.6.actor + NI.place.actor + auto.ni.7.actor + actor.3 + actor.4, data = subset(AWive, II))

### Check cochrane condition; step 1) remove things that aren't about 1
for (i in II.Univariate.ive.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 0 of expected are <5
issues.ii.ive<-lapply(II.Univariate.ive.0$univariate,cochran_ive)

####Create final univarite analysis
II.Univariate.ive <- nominal(OrderType ~ PV.Time + PV.Move + PV.Qual + II.sense + II.natural.land + II.weather +  auto.vii.1 + auto.ni.3.actor + auto.vai.6 + auto.vii.2 + auto.vii.3 + auto.vii.4 + auto.vii.5 + auto.ni.6.actor + Sg.actor + auto.ni.3.actor + NI.object.actor + Pron.actor + Dem.actor + ôma.actor + Prox.actor + NI.nominal.actor + Pl.actor + Med.actor + auto.ni.6.actor + actor.3 + actor.4, data = subset(AWive, II))

####Write to file
sink(file = "./issues.ii.ive.txt")
print(issues.ii.ive, max.print = NA)
sink(NULL)

####Write to file
sink(file = "./IIUni-indve.txt")
print(summary(II.Univariate.ive), max.print = NA)
sink(NULL)
####Make significance table
II.Uni.ive.Sig<- subset(summary(II.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = "./IIUniSig-indve.txt")
print(II.Uni.ive.Sig, max.print = NA)
sink(NULL)

###AI ANALYSIS
####Paste variables the occure more than ten times
paste(paste(names(which(sort(colSums(subset(AWive, AI)[grep("PV.[A-Z]", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^AI...*", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("Rdpl", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^N.*[0-9]$", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^auto.*", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("actor$", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^actor", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("^goal", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, AI)[grep("goal$", names(subset(AWive, AI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

####Create initial univarite analysis
AI.Univariate.ive.0 <- nominal(OrderType ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.plural + AI.money.count + AI.pray + AI.heat.fire + RdplS + RdplW +  auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.vai.11 + auto.na.5.actor + auto.vai.9 + auto.vai.3 + auto.nda.1.actor + auto.vai.4 + auto.vai.13 + auto.na.7.actor + auto.vai.8 + auto.vai.1 + auto.nda.3.actor + auto.vai.7 + auto.vai.10 + auto.na.2.actor + auto.na.1.actor + auto.na.3.actor + auto.na.4.actor + auto.na.6.actor + NA.persons.actor + auto.na.5.actor + Sg.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + Pers.actor + auto.nda.1.actor + Dem.actor + auto.na.7.actor + Prox.actor + Incl.actor + Med.actor + auto.nda.3.actor + NA.beast.of.burden.actor + Obv.actor + auto.na.2.actor + auto.na.1.actor + NA.food.actor + auto.na.3.actor + Px3Sg.actor + auto.na.4.actor + Indef.actor + Px12Pl.actor + auto.na.6.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWive, AI))

####Check cochrane condition; step 1) remove things that aren't about 1
for (i in AI.Univariate.ive.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
####No issue, move on to step 2) make sure 0 of expected are <5
issues.ai.ive<-lapply(AI.Univariate.ive.0$univariate,cochran_ive)

####Create final univarite analysis
AI.Univariate.ive <- nominal(OrderType ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.plural + AI.money.count + AI.pray + AI.heat.fire + RdplS + RdplW +  auto.vai.2 + auto.vai.5 + auto.vai.6 + auto.vai.12 + auto.vai.11 + auto.na.5.actor + auto.vai.9 + auto.vai.3 + auto.nda.1.actor + auto.vai.4 + auto.vai.13 + auto.na.7.actor + auto.vai.8 + auto.vai.1 + auto.nda.3.actor + auto.vai.7 + auto.vai.10 + auto.na.2.actor + auto.na.1.actor + auto.na.3.actor + auto.na.4.actor + NA.persons.actor + auto.na.5.actor + Sg.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + Pers.actor + auto.nda.1.actor + Dem.actor + auto.na.7.actor + Prox.actor + Incl.actor + Med.actor + auto.nda.3.actor + NA.beast.of.burden.actor + Obv.actor + auto.na.2.actor + auto.na.1.actor + NA.food.actor + auto.na.3.actor + Px3Sg.actor + auto.na.4.actor + Indef.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWive, AI))

####Write to file
sink(file = "./issues.ai.ive.txt")
print(issues.ai.ive, max.print = NA)
sink(NULL)

####Write to file
sink(file = "./AIUni-indve.txt")
print(summary(AI.Univariate.ive), max.print = NA)
sink(NULL)
####Make significance table
AI.Uni.ive.Sig<- subset(summary(AI.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = "./AIUniSig-indve.txt")
print(AI.Uni.ive.Sig, max.print = NA)
sink(NULL)

###TI ANALYSIS
####Paste variables the occure more than ten times
paste(paste(names(which(sort(colSums(subset(AWive, TI)[grep("PV.[A-Z]", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^TI...*", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("Rdpl", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^N.*[0-9]$", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^auto.*", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("actor$", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^actor", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("^goal", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TI)[grep("goal$", names(subset(AWive, TI)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

####Create initial univarite analysis
TI.Univariate.ive.0 <- nominal(OrderType ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + PV.StartFin + TI.do + TI.cognitive + TI.speech + TI.money.count + RdplW + RdplS +  auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.vti.3 + auto.na.5.actor + auto.ni.9.goal + auto.ni.2.goal + auto.ni.10.goal + auto.ndi.1.goal + auto.ni.6.goal + auto.ni.7.goal + auto.na.7.actor + auto.nda.1.actor + auto.ni.1.goal + auto.ni.5.goal + auto.na.1.actor + auto.ni.8.goal + auto.ni.4.goal + auto.vti.6 + auto.vti.5 + NA.persons.actor + auto.na.5.actor + Pron.actor + Sg.actor + Pers.actor + Pl.actor + NDA.Relations.actor + Px1Sg.actor + auto.na.7.actor + auto.nda.1.actor + Dem.actor + Incl.actor + Prox.actor + auto.na.1.actor + actor.3 + actor.1 + actor.2 + Sg.goal + NI.object.goal + auto.ni.3.goal + Dem.goal + Pron.goal + Pl.goal + Prox.goal + Med.goal + NI.nominal.goal + NI.natural.force.goal + auto.ni.9.goal + auto.ni.2.goal + D.goal + auto.ni.10.goal + auto.ndi.1.goal + auto.ni.6.goal + auto.ni.7.goal + NDI.Body.goal + NI.nature.plants.goal + NI.place.goal + Px3Sg.goal + auto.ni.1.goal + Der.Dim.goal + auto.ni.5.goal + Px1Sg.goal + auto.ni.8.goal + auto.ni.4.goal, data = subset(AWive, TI))

for (i in TI.Univariate.ive.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
####No issue, move on to step 2) make sure 0 of expected are <5
issues.ti.ive<-lapply(TI.Univariate.ive.0$univariate,cochran_ive)

####Create final univarite analysis
TI.Univariate.ive <- nominal(OrderType ~ PV.Time + PV.Move + PV.Position + PV.Discourse + PV.Qual + PV.WantCan + PV.StartFin + TI.do + TI.cognitive + TI.speech + TI.money.count + RdplW + RdplS +  auto.vti.1 + auto.vti.4 + auto.ni.3.goal + auto.vti.2 + auto.vti.3 + auto.na.5.actor + auto.ni.9.goal + auto.ni.2.goal + auto.ni.10.goal + auto.ndi.1.goal + auto.ni.6.goal + auto.ni.7.goal + auto.na.7.actor + auto.nda.1.actor + auto.ni.1.goal + auto.ni.5.goal + auto.na.1.actor + auto.ni.8.goal + NA.persons.actor + auto.na.5.actor + Pron.actor + Sg.actor + Pers.actor + Pl.actor + NDA.Relations.actor + Px1Sg.actor + auto.na.7.actor + auto.nda.1.actor + Dem.actor + Incl.actor + Prox.actor + auto.na.1.actor + actor.3 + actor.1 + actor.2 + Sg.goal + NI.object.goal + auto.ni.3.goal + Dem.goal + Pron.goal + Pl.goal + Prox.goal + Med.goal + NI.nominal.goal + NI.natural.force.goal + auto.ni.9.goal + auto.ni.2.goal + D.goal + auto.ni.10.goal + auto.ndi.1.goal + auto.ni.6.goal + auto.ni.7.goal + NDI.Body.goal + NI.nature.plants.goal + NI.place.goal + Px3Sg.goal + auto.ni.1.goal + Der.Dim.goal + auto.ni.5.goal + Px1Sg.goal + auto.ni.8.goal + auto.ni.4.goal, data = subset(AWive, TI))

####Write to file
sink(file = "./issues.ti.ive.txt")
print(issues.ti.ive, max.print = NA)
sink(NULL)

####Write to file
sink(file = "./TIUni-indve.txt")
print(summary(TI.Univariate.ive), max.print = NA)
sink(NULL)

####Make significance table
TI.Uni.ive.Sig<- subset(summary(TI.Univariate.ive)$sumry.table, alpha.X2 <0.05)

sink(file = "./TIUniSig-indve.txt")
print(TI.Uni.ive.Sig, max.print = NA)
sink(NULL)

###TA ANALYSIS
####Paste variables the occur more than ten times
paste(paste(names(which(sort(colSums(subset(AWive, TA)[grep("PV.[A-Z]", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^TA...*", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("Rdpl", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^N.*[0-9]$", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "),
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^auto.*", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+",
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("actor$", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^actor", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("^goal", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), "+", 
      paste(names(which(sort(colSums(subset(AWive, TA)[grep("goal$", names(subset(AWive, TA)), value = TRUE)]), decr=T)>=10)), collapse =" + "), collapse =" + ")

####Create initial univarite analysis
TA.Univariate.ive.0 <- nominal(OrderType ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.WantCan + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + TA.allow + RdplS + RdplW +  auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.nda.1.goal + auto.vta.4 + auto.na.5.actor + auto.vta.2 + auto.na.7.goal + auto.na.2.goal + auto.na.3.goal + auto.nda.1.actor + auto.na.7.actor + auto.nda.3.goal + auto.nda.3.actor + auto.na.6.goal + auto.na.1.actor + auto.vta.7 + auto.na.1.goal + NA.persons.actor + Sg.actor + auto.na.5.actor + Pron.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + auto.nda.1.actor + Dem.actor + Pers.actor + auto.na.7.actor + Obv.actor + Pl.actor + Prox.actor + auto.nda.3.actor + Incl.actor + auto.na.1.actor + Med.actor + NA.religious.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + NA.persons.goal + Sg.goal + auto.na.5.goal + Obv.goal + D.goal + NDA.Relations.goal + Pl.goal + auto.nda.1.goal + Pron.goal + Px1Sg.goal + Dem.goal + NA.beast.of.burden.goal + Prox.goal + auto.na.7.goal + auto.na.2.goal + auto.na.3.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + Med.goal + NA.food.goal + auto.na.6.goal + Px3Pl.goal + Incl.goal + Px12Pl.goal + auto.na.1.goal, data = subset(AWive, TA))

for (i in TA.Univariate.ive.0$univariate)
{print(chisq.test(i$posthoc$ctable)$expected<0.8)}
### No issue, move on to step 2) make sure 0 of expected are <5
issues.ta.ive<-lapply(TA.Univariate.ive.0$univariate,cochran_ive)

####Create final univarite analysis
TA.Univariate.ive <- nominal(OrderType ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.WantCan + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + TA.allow + RdplS + RdplW +  auto.vta.6 + auto.vta.3 + auto.vta.5 + auto.vta.1 + auto.na.5.goal + auto.nda.1.goal + auto.vta.4 + auto.na.5.actor + auto.vta.2 + auto.na.7.goal + auto.na.2.goal + auto.na.3.goal + auto.nda.1.actor + auto.na.7.actor + auto.nda.3.goal + auto.nda.3.actor + auto.na.6.goal + auto.na.1.actor + auto.vta.7 + auto.na.1.goal + NA.persons.actor + Sg.actor + auto.na.5.actor + Pron.actor + D.actor + NDA.Relations.actor + Px1Sg.actor + auto.nda.1.actor + Dem.actor + Pers.actor + auto.na.7.actor + Obv.actor + Pl.actor + Prox.actor + auto.nda.3.actor + Incl.actor + auto.na.1.actor + Med.actor + NA.religious.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + NA.persons.goal + Sg.goal + auto.na.5.goal + Obv.goal + D.goal + NDA.Relations.goal + Pl.goal + auto.nda.1.goal + Pron.goal + Px1Sg.goal + Dem.goal + NA.beast.of.burden.goal + Prox.goal + auto.na.7.goal + auto.na.2.goal + auto.na.3.goal + Px3Sg.goal + Pers.goal + auto.nda.3.goal + Med.goal + NA.food.goal + auto.na.6.goal + Px3Pl.goal + Incl.goal + Px12Pl.goal + auto.na.1.goal, data = subset(AWive, TA))

####Write to file
sink(file = "./issues.ta.ive.txt")
print(issues.ta.ive, max.print = NA)
sink(NULL)

####Write to file
sink(file = "./TAUni-indve.txt")
print(summary(TA.Univariate.ive), max.print = NA)
sink(NULL)

####Make significance table
TA.Uni.ive.Sig<- subset(summary(TA.Univariate.ive)$sumry.table, alpha.X2 <0.05)

####Write to file
sink(file = "./TAUniSig-indve.txt")
print(TA.Uni.ive.Sig, max.print = NA)
sink(NULL)
