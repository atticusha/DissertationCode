#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glm.fixed <- glm(Ind ~ PV.Time + II.sense + NI.object.actor + Dem.actor, data=subset(AWive, II), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Ind vs. eCnj/ive-ii.sem.morph.txt')
round(print(summary(ii.ive.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.ive.glm.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + RdplW + NA.persons.actor + Pl.actor + NA.beast.of.burden.actor + actor.3 + actor.1 + actor.4, data=subset(AWive, AI), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Ind vs. eCnj/ive-ai.sem.morph.txt')
round(print(summary(ai.ive.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.glm.fixed <- glm(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + actor.3 + actor.2 + Pl.goal + NI.natural.force.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data=subset(AWive, TI), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Ind vs. eCnj/ive-ti.sem.morph.txt')
round(print(summary(ti.ive.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.glm.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + Sg.actor + NDA.Relations.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + Px3Sg.goal + Px3Pl.goal, data=subset(AWive, TA), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Ind vs. Cnj/ive-ta.sem.morph.txt')
round(print(summary(ta.ive.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glm.fixed <- glm(Ind ~ PV.Time + II.sense + NI.object.actor + Med.actor, data=subset(AWnimp, II), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Ind vs. Cnj/ivc-ii.sem.morph.txt')
round(print(summary(ii.indcnj.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.indcnj.glm.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NA.beast.of.burden.actor + NA.food.actor + actor.3 + actor.2 + actor.4, data=subset(AWnimp, AI), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Ind vs. Cnj/ivc-ai.sem.morph.txt')
round(print(summary(ai.indcnj.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.glm.fixed <- glm(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + actor.3 + actor.2 + Sg.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + NDI.Body.goal + Px3Sg.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data=subset(AWnimp, TI), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Ind vs. Cnj/ivc-ti.sem.morph.txt')
round(print(summary(ti.indcnj.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.glm.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + Sg.actor + NDA.Relations.actor + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + NA.persons.goal + Px3Pl.goal, data=subset(AWnimp, TA), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Ind vs. Cnj/ivc-ta.sem.morph.txt')
round(print(summary(ta.indcnj.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glm.fixed <- glm(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ii-e-cnj.sem.morph.txt')
round(print(summary(ii.e.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.e.cnjtype.glm.fixed <- glm(PV.e ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ai-e-cnj.sem.morph.txt')
round(print(summary(ai.e.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.glm.fixed <- glm(PV.e ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ti-e-cnj.sem.morph.txt')
round(print(summary(ti.e.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.glm.fixed <- glm(PV.e ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ta-e-cnj.sem.morph.txt')
round(print(summary(ta.e.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
ii.kaa.cnjtype.glm.fixed <- glm(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ii-kaa-cnj.sem.morph.txt')
round(print(summary(ii.kaa.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.kaa.cnjtype.glm.fixed <- glm(PV.kaa ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ai-kaa-cnj.sem.morph.txt')
round(print(summary(ai.kaa.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.glm.fixed <- glm(PV.kaa ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ti-kaa-cnj.sem.morph.txt')
round(print(summary(ti.kaa.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.glm.fixed <- glm(PV.kaa ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ta-kaa-cnj.sem.morph.txt')
round(print(summary(ta.kaa.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
ii.other.cnjtype.glm.fixed <- glm(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ii-other-cnj.sem.morph.txt')
round(print(summary(ii.other.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.other.cnjtype.glm.fixed <- glm(OtherCnj ~  PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ai-other-cnj.sem.morph.txt')
round(print(summary(ai.other.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.glm.fixed <- glm(OtherCnj ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ti-other-cnj.sem.morph.txt')
round(print(summary(ti.other.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.glm.fixed <- glm(OtherCnj ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = '/Users/atticusharrigan/Dres/Multi/Sem.Morph/Cnj Type/ta-other-cnj.sem.morph.txt')
round(print(summary(ta.other.cnjtype.glm.fixed)$coefficients, max.print = NA), 3)
sink(NULL)

