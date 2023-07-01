#BiVariate Commands for all Alternations
##==========================================================================================================
## Install packages (polytomous must be installed from source)
install.packages("~/Downloads/polytomous_0.1.6.tar.gz", repos = NULL, type="source")
library(polytomous)

##==========================================================================================================
## Independent vs. ê-Conjunct Alternation

###VII
II.ive.Bivariate.man.initial <- nominal(. ~ PV.Time + II.sense + Sg.actor + NI.object.actor + Pron.actor + Dem.actor + Med.actor, data = subset(AWive, II))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(II.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
II.ive.Bivariate.man <- nominal(. ~ PV.Time + II.sense + NI.object.actor + Dem.actor, data = subset(AWive, II))

### VAI
AI.ive.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + RdplW + NA.persons.actor + Pl.actor + NA.beast.of.burden.actor + actor.3 + actor.1 + actor.4, data = subset(AWCnj, AI))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(AI.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
AI.ive.Bivariate.man <- nominal(. ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + RdplW + NA.persons.actor + Pl.actor + NA.beast.of.burden.actor + actor.3 + actor.1 + actor.4, data = subset(AWCnj, AI))

### VTI
TI.ive.Bivariate.man.initial <- nominal(. ~ PV.Discourse + TI.do + TI.cognitive + TI.money.count + NA.persons.actor + Pron.actor + Pers.actor + actor.3 + actor.1 + actor.2 + Pl.goal + NI.natural.force.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data = subset(AWive, TI))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(TI.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TI.ive.Bivariate.man <- nominal(. ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + actor.3 + actor.2 + Pl.goal + NI.natural.force.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data = subset(AWive, TI))

### VTA
TA.ive.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + Sg.actor + D.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + Px3Sg.goal + Px3Pl.goal, data = subset(AWive, TA))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(TA.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TA.ive.Bivariate.man <- nominal(. ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + Sg.actor + NDA.Relations.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + Px3Sg.goal + Px3Pl.goal, data = subset(AWive, TA))


##==========================================================================================================
## Independent vs. Conjunct Alternation

###VII
II.ivc.Bivariate.man.initial <- nominal(. ~ PV.Time + II.sense + NI.object.actor + Pron.actor + Dem.actor + Med.actor, data = subset(AWnimp, II))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(II.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
II.ivc.Bivariate.man <- nominal(. ~ PV.Time + II.sense + NI.object.actor + Med.actor, data = subset(AWnimp, II))

###VAI
AI.ivc.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + RdplW + NA.persons.actor + Sg.actor + Pron.actor + Pl.actor + Dem.actor + NA.beast.of.burden.actor + NA.food.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWnimp, AI))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(AI.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
AI.ivc.Bivariate.man <- nominal(. ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NA.beast.of.burden.actor + NA.food.actor + actor.3 + actor.2 + actor.4, data = subset(AWnimp, AI))

###VTI
TI.ivc.Bivariate.man.initial <- nominal(. ~ PV.Discourse + TI.do + TI.cognitive + TI.money.count + NA.persons.actor + Pron.actor + Pers.actor + actor.3 + actor.1 + actor.2 + Sg.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + D.goal + NDI.Body.goal + Px3Sg.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data = subset(AWnimp, TI))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(TI.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TI.ivc.Bivariate.man <- nominal(. ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + actor.3 + actor.2 + Sg.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + NDI.Body.goal + Px3Sg.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data = subset(AWnimp, TI))

###VTA
TA.ivc.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + Sg.actor + D.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + NA.persons.goal + Px3Pl.goal, data = subset(AWnimp, TA))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(TA.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TA.ivc.Bivariate.man <- nominal(. ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + Sg.actor + NDA.Relations.actor + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + NA.persons.goal + Px3Pl.goal, data = subset(AWnimp, TA))



##==========================================================================================================
## Conjunct Type Alternation

###VII
II.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + II.sense + II.natural.land + II.weather + Sg.actor + NI.object.actor, data = subset(AWCnj, II))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(II.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
II.ekaaother.Bivariate.man <- nominal(. ~ PV.Time + II.sense + II.natural.land + II.weather + NI.object.actor, data = subset(AWCnj, II))


### VAI
AI.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Dem.actor + Px1Sg.actor + Prox.actor + Med.actor + actor.3 + actor.1 + actor.2, data = subset(AWCnj, AI))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(AI.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
AI.ekaaother.Bivariate.man <- nominal(. ~ PV.Time + PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2, data = subset(AWCnj, AI))

### VTI
TI.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + Pron.actor + actor.3 + actor.1 + actor.2 + Sg.goal + NI.object.goal + Dem.goal + Pron.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data = subset(AWCnj, TI))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(TI.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TI.ekaaother.Bivariate.man <- nominal(. ~ PV.Time + PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data = subset(AWCnj, TI))

### VTA
TA.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.3 + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal, data = subset(AWCnj, TA))

####Remove items with over 0.5 uncertainty score and recreate a nominal model
subset(TA.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
TA.ekaaother.Bivariate.man <- nominal(. ~ PV.Time + PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal, data = subset(AWCnj, TA))
