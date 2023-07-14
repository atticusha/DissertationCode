# Model Commands Semantic and Morphological Effects without Lemma
## Load Packages
library(lme4)

## Ind vs. cnj
### VII
ii.indcnj.glm.fixed <- glm(Ind ~ PV.Time + II.sense + NI.object.actor + Med.actor, data=subset(AWnimp, II), family=binomial)

### VAI
ai.indcnj.glm.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NA.beast.of.burden.actor + NA.food.actor + actor.3 + actor.2 + actor.4, data=subset(AWnimp, AI), family=binomial)

### VTI
ti.indcnj.glm.fixed <- glm(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + actor.3 + actor.2 + Sg.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + NDI.Body.goal + Px3Sg.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data=subset(AWnimp, TI), family=binomial)

### VTA
ta.indcnj.glm.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + Sg.actor + NDA.Relations.actor + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + NA.persons.goal + Px3Pl.goal, data=subset(AWnimp, TA), family=binomial)

## Ind vs eCnj
### VII
ii.ive.glm.fixed <- glm(Ind ~ PV.Time + II.sense + NI.object.actor + Dem.actor, data=subset(AWive, II), family=binomial)

### VAI
ai.ive.glm.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + RdplW + NA.persons.actor + Pl.actor + NA.beast.of.burden.actor + actor.3 + actor.1 + actor.4, data=subset(AWive, AI), family=binomial)

### VTI
ti.ive.glm.fixed <- glm(Ind ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + actor.3 + actor.2 + Pl.goal + NI.natural.force.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data=subset(AWive, TI), family=binomial)

### VTA
ta.ive.glm.fixed <- glm(Ind ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + Sg.actor + NDA.Relations.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + Px3Sg.goal + Px3Pl.goal, data=subset(AWive, TA), family=binomial)

## Cnj Type
### VII
ii.e.cnjtype.glm.fixed <- glm(PV.e ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

### VAI
ai.e.cnjtype.glm.fixed <- glm(PV.e ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

### VTI
ti.e.cnjtype.glm.fixed <- glm(PV.e ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

### VTA
ta.e.cnjtype.glm.fixed <- glm(PV.e ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal, data=subset(AWCnj, TA), family=binomial)
#-------------------------------------------------------
### VII
ii.kaa.cnjtype.glm.fixed <- glm(PV.kaa ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

### VAI
ai.kaa.cnjtype.glm.fixed <- glm(PV.kaa ~ PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

### VTI
ti.kaa.cnjtype.glm.fixed <- glm(PV.kaa ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

### VTA
ta.kaa.cnjtype.glm.fixed <- glm(PV.kaa ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal, data=subset(AWCnj, TA), family=binomial)
#-------------------------------------------------------
### VII
ii.other.cnjtype.glm.fixed <- glm(OtherCnj ~ II.sense + II.natural.land + II.weather + NI.object.actor, data=subset(AWCnj, II), family=binomial)

### VAI
ai.other.cnjtype.glm.fixed <- glm(OtherCnj ~  PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

### VTI
ti.other.cnjtype.glm.fixed <- glm(OtherCnj ~ PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

### VTA
ta.other.cnjtype.glm.fixed <- glm(OtherCnj ~ PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal, data=subset(AWCnj, TA), family=binomial)