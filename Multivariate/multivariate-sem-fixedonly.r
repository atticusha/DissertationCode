#Multivariate Commands for all Alternations
##==========================================================================================================
###==========================================================================================================
###Independent vs ê-Conjunct
###==========================================================================================================
####VII
ii.ive.sem.fixed.glmer <- glm(Ind ~ PV.Time + II.sense + NI.object.actor + Dem.actor, data=subset(AWive, II), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/ive/ii-ive-ModelStats.my.sem.txt")
round(print(summary(ii.ive.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.ive.sem.fixed.glmer <- glm(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + NA.persons.actor + NA.beast.of.burden.actor, data=subset(AWive, AI), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/ive/ai-ive-ModelStats.my.sem.txt")
round(print(summary(ai.ive.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.ive.sem.fixed.glmer <- glm(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + NI.natural.force.goal + NI.place.goal + Der.Dim.goal, data=subset(AWive, TI), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/ive/ti-ive-ModelStats.my.sem.txt")
round(print(summary(ti.ive.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.ive.sem.fixed.glmer <- glm(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + NDA.Relations.actor + NA.persons.goal, data=subset(AWive, TA), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/ive/ta-ive-ModelStats.my.sem.txt")
round(print(summary(ta.ive.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


###==========================================================================================================
###Independent vs Conjunct
###==========================================================================================================
####VII
ii.indcnj.sem.fixed.glmer <- glm(Ind ~ PV.Time + II.sense + NI.object.actor + Med.actor, data=subset(AWnimp, II), family=binomial)

sink( file = "~/ModelStatistics/sem-fixed/ivc/ii-ivc-ModelStats.my.sem.txt")
round(print(summary(ii.indcnj.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.indcnj.sem.fixed.glmer <- glm(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + NA.persons.actor + NA.beast.of.burden.actor + NA.food.actor, data=subset(AWnimp, AI), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/ivc/ai-ivc-ModelStats.my.sem.txt")
round(print(summary(ai.indcnj.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.indcnj.sem.fixed.glmer <- glm(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + NI.nominal.goal + NI.natural.force.goal + NDI.Body.goal + NI.place.goal + Der.Dim.goal, data=subset(AWnimp, TI), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/ivc/ti-ivc-ModelStats.my.sem.txt")
round(print(summary(ti.indcnj.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.indcnj.sem.fixed.glmer <- glm(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + NDA.Relations.actor + NA.persons.goal, data=subset(AWnimp, TA), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/ivc/ta-ivc-ModelStats.my.sem.txt")
round(print(summary(ta.indcnj.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: ê-Conjunct
###==========================================================================================================
####VII
ii.e.cnjtype.sem.fixed.glmer <- glm(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/cnjtype/ecnj/ii-enj-ModelStats.my.sem.txt")
round(print(summary(ii.e.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.e.cnjtype.sem.fixed.glmer <- glm(PV.e ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor, data=subset(AWCnj, AI), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/cnjtype/ecnj/ai-enj-ModelStats.my.sem.txt")
round(print(summary(ai.e.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.e.cnjtype.sem.fixed.glmer <- glm(PV.e ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/cnjtype/ecnj/ti-enj-ModelStats.my.sem.txt")
round(print(summary(ti.e.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.e.cnjtype.sem.fixed.glmer <- glm(PV.e ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/cnjtype/ecnj/ta-enj-ModelStats.my.sem.txt")
round(print(summary(ta.e.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: kâ-Conjunct
###==========================================================================================================
####VII
ii.kaa.cnjtype.sem.fixed.glmer <- glm(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/cnjtype/kaacnj/ii-kaanj-ModelStats.my.sem.txt")
round(print(summary(ii.kaa.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.kaa.cnjtype.sem.fixed.glmer <- glm(PV.kaa ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor, data=subset(AWCnj, AI), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/cnjtype/kaacnj/ai-kaanj-ModelStats.my.sem.txt")
round(print(summary(ai.kaa.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.kaa.cnjtype.sem.fixed.glmer <- glm(PV.kaa ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/cnjtype/kaacnj/ti-kaanj-ModelStats.my.sem.txt")
round(print(summary(ti.kaa.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.kaa.cnjtype.sem.fixed.glmer <- glm(PV.kaa ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = "~/ModelStatistics/sem-fixed/cnjtype/kaacnj/ta-kaanj-ModelStats.my.sem.txt")
round(print(summary(ta.kaa.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: Other Conjunct
###==========================================================================================================
####VII
ii.other.cnjtype.sem.fixed.glmer <- glm(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

sink(file = "./ii-other-ModelStats.my.sem.txt")
round(print(summary(ii.other.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.other.cnjtype.sem.fixed.glmer <- glm(OtherCnj ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + NA.persons.actor + NDA.Relations.actor, data=subset(AWCnj, AI), family=binomial)

sink(file = "./ai-other-ModelStats.my.sem.txt")
round(print(summary(ai.other.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.other.cnjtype.sem.fixed.glmer <- glm(OtherCnj ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = "./ti-other-ModelStats.my.sem.txt")
round(print(summary(ti.other.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.other.cnjtype.sem.fixed.glmer <- glm(OtherCnj ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + Prox.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = "./ta-other-ModelStats.my.sem.txt")
round(print(summary(ta.other.cnjtype.sem.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)
