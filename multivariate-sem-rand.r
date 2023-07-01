#Multivariate Commands for all Alternations
##==========================================================================================================
###==========================================================================================================
###Independent vs ê-Conjunct
###==========================================================================================================
####VII
ii.ive.sem.glmer <- glmer(Ind ~ PV.Time + II.sense + NI.object.actor + Dem.actor + (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/ive/ii-ive-ModelStats.my.sem.txt')
round(print(summary(ii.ive.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.ive.sem.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + NA.persons.actor + NA.beast.of.burden.actor + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/ive/ai-ive-ModelStats.my.sem.txt')
round(print(summary(ai.ive.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.ive.sem.glmer <- glmer(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + NI.natural.force.goal + NI.place.goal + Der.Dim.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/ive/ti-ive-ModelStats.my.sem.txt')
round(print(summary(ti.ive.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.ive.sem.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + NDA.Relations.actor + NA.persons.goal + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/ive/ta-ive-ModelStats.my.sem.txt')
round(print(summary(ta.ive.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


###==========================================================================================================
###Independent vs Conjunct
###==========================================================================================================
####VII
ii.indcnj.sem.glmer <- glmer(Ind ~ PV.Time + II.sense + NI.object.actor + Med.actor + (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink( file = '~/ModelStatistics/sem/ivc/ii-ivc-ModelStats.my.sem.txt')
round(print(summary(ii.indcnj.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.indcnj.sem.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + NA.persons.actor + NA.beast.of.burden.actor + NA.food.actor + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/ivc/ai-ivc-ModelStats.my.sem.txt')
round(print(summary(ai.indcnj.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.indcnj.sem.glmer <- glmer(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + NI.nominal.goal + NI.natural.force.goal + NDI.Body.goal + NI.place.goal + Der.Dim.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/ivc/ti-ivc-ModelStats.my.sem.txt')
round(print(summary(ti.indcnj.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.indcnj.sem.glmer <- glmer(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + NDA.Relations.actor + NA.persons.goal + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/ivc/ta-ivc-ModelStats.my.sem.txt')
round(print(summary(ta.indcnj.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: ê-Conjunct
###==========================================================================================================
####VII
ii.e.cnjtype.sem.glmer <- glmer(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/ecnj/ii-enj-ModelStats.my.sem.txt')
round(print(summary(ii.e.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


####VAI
ai.e.cnjtype.sem.glmer <- glmer(PV.e ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/ecnj/ai-enj-ModelStats.my.sem.txt')
round(print(summary(ai.e.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.e.cnjtype.sem.glmer <- glmer(PV.e ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/ecnj/ti-enj-ModelStats.my.sem.txt')
round(print(summary(ti.e.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.e.cnjtype.sem.glmer <- glmer(PV.e ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/ecnj/ta-enj-ModelStats.my.sem.txt')
round(print(summary(ta.e.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: kâ-Conjunct
###==========================================================================================================
####VII
ii.kaa.cnjtype.sem.glmer <- glmer(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/kaacnj/ii-kaanj-ModelStats.my.sem.txt')
round(print(summary(ii.kaa.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


####VAI
ai.kaa.cnjtype.sem.glmer <- glmer(PV.kaa ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/kaacnj/ai-kaanj-ModelStats.my.sem.txt')
round(print(summary(ai.kaa.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.kaa.cnjtype.sem.glmer <- glmer(PV.kaa ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/kaacnj/ti-kaanj-ModelStats.my.sem.txt')
round(print(summary(ti.kaa.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.kaa.cnjtype.sem.glmer <- glmer(PV.kaa ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/kaacnj/ta-kaanj-ModelStats.my.sem.txt')
round(print(summary(ta.kaa.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: Other Conjunct
###==========================================================================================================
####VII
ii.other.cnjtype.sem.glmer <- glmer(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor + (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/other/ii-other-ModelStats.my.sem.txt')
round(print(summary(ii.other.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.other.cnjtype.sem.glmer <- glmer(OtherCnj ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/other/ai-other-ModelStats.my.sem.txt')
round(print(summary(ai.other.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.other.cnjtype.sem.glmer <- glmer(OtherCnj ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/other/ti-other-ModelStats.my.sem.txt')
round(print(summary(ti.other.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.other.cnjtype.sem.glmer <- glmer(OtherCnj ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/sem/cnjtype/other/ta-other-ModelStats.my.sem.txt')
round(print(summary(ta.other.cnjtype.sem.glmer)$coefficients, max.print = NA), 3)
sink(NULL)
