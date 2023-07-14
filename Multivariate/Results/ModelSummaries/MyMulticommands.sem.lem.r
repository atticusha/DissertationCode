# Model Commands Semantic Effects and Lemma
## Load Packages
library(lme4)

## Ind vs. Cnj
### VAI
ii.indcnj.sem.glmer <- glmer(Ind ~ PV.Time + II.sense + NI.object.actor + Med.actor + (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI
ai.indcnj.sem.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + NA.persons.actor + NA.beast.of.burden.actor + NA.food.actor + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.indcnj.sem.glmer <- glmer(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + NI.nominal.goal + NI.natural.force.goal + NDI.Body.goal + NI.place.goal + Der.Dim.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.indcnj.sem.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + NDA.Relations.actor + NA.persons.goal + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))


## Ind vs eCnj
### VII
ii.ive.sem.glmer <- glmer(Ind ~ PV.Time + II.sense + NI.object.actor + Dem.actor + (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI
ai.ive.sem.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + NA.persons.actor + NA.beast.of.burden.actor + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.ive.sem.glmer <- glmer(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + NI.natural.force.goal + NI.place.goal + Der.Dim.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.ive.sem.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + NDA.Relations.actor + NA.persons.goal + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))


## Cnj Type

### VII
ii.e.cnjtype.sem.glmer <- glmer(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI
ai.e.cnjtype.sem.glmer <- glmer(PV.e ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.e.cnjtype.sem.glmer <- glmer(PV.e ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.e.cnjtype.sem.glmer <- glmer(PV.e ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#-------------------------------------------------------
### VII
ii.kaa.cnjtype.sem.glmer <- glmer(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI
ai.kaa.cnjtype.sem.glmer <- glmer(PV.kaa ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.kaa.cnjtype.sem.glmer <- glmer(PV.kaa ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.kaa.cnjtype.sem.glmer <- glmer(PV.kaa ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#-------------------------------------------------------
### VII
ii.other.cnjtype.sem.glmer <- glmer(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI
ai.other.cnjtype.sem.glmer <- glmer(OtherCnj ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.other.cnjtype.sem.glmer <- glmer(OtherCnj ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.other.cnjtype.sem.glmer <- glmer(OtherCnj ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

