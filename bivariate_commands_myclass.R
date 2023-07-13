# BiVariate Commands
## ind vs. eCnj
#=======================================================================
### VII
II.ive.Bivariate.man.initial <- nominal(. ~ PV.Time + II.sense + Sg.actor + NI.object.actor + Pron.actor + Dem.actor + Med.actor, data = subset(AWive, II))
subset(II.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

II.ive.Bivariate.man <- nominal(. ~ PV.Time + II.sense + NI.object.actor + Dem.actor, data = subset(AWive, II))
#### 

### VAI
AI.ive.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + RdplW + NA.persons.actor + Pl.actor + NA.beast.of.burden.actor + actor.3 + actor.1 + actor.4, data = subset(AWCnj, AI))
subset(AI.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)


AI.ive.Bivariate.man <- nominal(. ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.pray + RdplW + NA.persons.actor + Pl.actor + NA.beast.of.burden.actor + actor.3 + actor.1 + actor.4, data = subset(AWCnj, AI))

### VTI
TI.ive.Bivariate.man.initial <- nominal(. ~ PV.Discourse + TI.do + TI.cognitive + TI.money.count + NA.persons.actor + Pron.actor + Pers.actor + actor.3 + actor.1 + actor.2 + Pl.goal + NI.natural.force.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data = subset(AWive, TI))
subset(TI.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#category1    category2   N1   N2 N12     alpha.X2      alpha.G2          beta power effect.size
#15            TI.do TI.cognitive 1281 1008   0            0             0             0     1   0.8598999
#51 NA.persons.actor   Pron.actor  203  129 129            0 1.998491e-164 8.471817e-301     1   0.7844783
#52 NA.persons.actor   Pers.actor  203   95  95 1.70296e-239 3.479479e-116  4.66476e-215     1   0.6683759
#61       Pron.actor   Pers.actor  129   95  95            0 7.531925e-145             0     1   0.8520005
#78          actor.3      actor.1 1184 1043   0            0             0             0     1   0.8185091

TI.ive.Bivariate.man <- nominal(. ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + actor.3 + actor.2 + Pl.goal + NI.natural.force.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data = subset(AWive, TI))

### VTA
TA.ive.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + Sg.actor + D.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + Px3Sg.goal + Px3Pl.goal, data = subset(AWive, TA))
subset(TA.ive.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1           category2   N1   N2 N12 alpha.X2      alpha.G2 beta power
#177   D.actor NDA.Relations.actor   68   68  68        0 7.343737e-137    0     1
#199   actor.3              goal.3 1060 1185   0        0             0    0     1

TA.ive.Bivariate.man <- nominal(. ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + PV.StartFin + TA.speech + TA.cognitive + TA.do + TA.food + Sg.actor + NDA.Relations.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + NA.persons.goal + Px3Sg.goal + Px3Pl.goal, data = subset(AWive, TA))





#=======================================================================
## Ind vs Cnj as whole
#=======================================================================
### VII
II.ivc.Bivariate.man.initial <- nominal(. ~ PV.Time + II.sense + NI.object.actor + Pron.actor + Dem.actor + Med.actor, data = subset(AWnimp, II))
subset(II.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#NI.object.actor & Pron.actor & 145 & 58 & 58 \\
#NI.object.actor & Dem.actor & 145 & 57 & 57 \\
#Pron.actor & Dem.actor & 58 & 57 & 57 \\
#Pron.actor & Med.actor & 58 & 24 & 24 \\
#Dem.actor & Med.actor & 57 & 24 & 24 \\

II.ivc.Bivariate.man <- nominal(. ~ PV.Time + II.sense + NI.object.actor + Med.actor, data = subset(AWnimp, II))

### VAI
AI.ivc.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + RdplW + NA.persons.actor + Sg.actor + Pron.actor + Pl.actor + Dem.actor + NA.beast.of.burden.actor + NA.food.actor + actor.3 + actor.1 + actor.2 + actor.4, data = subset(AWnimp, AI))
subset(AI.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1  category2   N1   N2 N12 alpha.X2      alpha.G2 beta power
#247 NA.persons.actor Pron.actor  740  405 405        0             0    0     1
#249 NA.persons.actor  Dem.actor  740  201 201        0 3.366219e-202    0     1
#266       Pron.actor  Dem.actor  405  201 201        0 2.599352e-268    0     1
#295          actor.3    actor.1 3663 1836   0        0             0    0     1


AI.ivc.Bivariate.man <- nominal(. ~ PV.Time + PV.Move + PV.Qual + PV.StartFin + PV.Discourse + PV.Position + AI.do + AI.state + AI.speech + AI.cooking + AI.reflexive + AI.health + AI.pray + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NA.beast.of.burden.actor + NA.food.actor + actor.3 + actor.2 + actor.4, data = subset(AWnimp, AI))

### VTI
TI.ivc.Bivariate.man.initial <- nominal(. ~ PV.Discourse + TI.do + TI.cognitive + TI.money.count + NA.persons.actor + Pron.actor + Pers.actor + actor.3 + actor.1 + actor.2 + Sg.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + D.goal + NDI.Body.goal + Px3Sg.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data = subset(AWnimp, TI))
subset(TI.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#TI.do & TI.cognitive & 1641 & 1163 & 0 \\
#NA.persons.actor & Pron.actor & 261 & 158 & 158 \\
#NA.persons.actor & Pers.actor & 261 & 107 & 107 \\
#Pron.actor & Pers.actor & 158 & 107 & 107 \\
#actor.3 & actor.1 & 1514 & 1205 & 0 \\
#D.goal & NDI.Body.goal & 64 & 55 & 55 \\

TI.ivc.Bivariate.man <- nominal(. ~ PV.Discourse + TI.do + TI.money.count + NA.persons.actor + actor.3 + actor.2 + Sg.goal + Pl.goal + NI.nominal.goal + NI.natural.force.goal + NDI.Body.goal + Px3Sg.goal + NI.place.goal + Der.Dim.goal + Px1Sg.goal, data = subset(AWnimp, TI))
subset(TI.ivc.Bivariate.man$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

### VTA
TA.ivc.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + Sg.actor + D.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + NA.persons.goal + Px3Pl.goal, data = subset(AWnimp, TA))
subset(TA.ivc.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#category1           category2   N1   N2 N12 alpha.X2      alpha.G2 beta power
#187   D.actor NDA.Relations.actor   83   83  83        0 1.585164e-167    0     1
#206   actor.3              goal.3 1277 1510   0        0             0    0     1
TA.ivc.Bivariate.man <- nominal(. ~ PV.Time + PV.Move + PV.Discourse + PV.Qual + PV.Position + TA.speech + TA.cognitive + TA.do + TA.food + TA.money.count + NA.persons.actor + Sg.actor + NDA.Relations.actor + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + NA.persons.goal + Px3Pl.goal, data = subset(AWnimp, TA))



#=======================================================================
## eCnj vs kaa vs other
#=======================================================================
### VII
II.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + II.sense + II.natural.land + II.weather + Sg.actor + NI.object.actor, data = subset(AWCnj, II))
subset(II.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#category1       category2  N1  N2 N12     alpha.X2     alpha.G2         beta
#15  Sg.actor NI.object.actor 134 119 110 1.054056e-91 6.675665e-85 1.718082e-76


II.ekaaother.Bivariate.man <- nominal(. ~ PV.Time + II.sense + II.natural.land + II.weather + NI.object.actor, data = subset(AWCnj, II))


### VAI
AI.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pron.actor + Pl.actor + D.actor + NDA.Relations.actor + Dem.actor + Px1Sg.actor + Prox.actor + Med.actor + actor.3 + actor.1 + actor.2, data = subset(AWCnj, AI))
subset(AI.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#              category1           category2  N1  N2 N12
#155    NA.persons.actor          Pron.actor 545 292 292
#159    NA.persons.actor           Dem.actor 545 149 149
#180          Pron.actor           Dem.actor 292 149 149
#182          Pron.actor          Prox.actor 292  91  91
#183          Pron.actor           Med.actor 292  56  56
#196             D.actor NDA.Relations.actor 164 164 164
#198             D.actor         Px1Sg.actor 164 123 123
#205 NDA.Relations.actor         Px1Sg.actor 164 123 123
#212           Dem.actor          Prox.actor 149  91  91
#213           Dem.actor           Med.actor 149  56  56


AI.ekaaother.Bivariate.man <- nominal(. ~ PV.Time + PV.Qual + PV.Discourse + PV.Position + PV.WantCan + AI.do + AI.cooking + AI.health + RdplW + NA.persons.actor + Sg.actor + Pl.actor + NDA.Relations.actor + actor.3 + actor.1 + actor.2, data = subset(AWCnj, AI))

### VTI
TI.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + Pron.actor + actor.3 + actor.1 + actor.2 + Sg.goal + NI.object.goal + Dem.goal + Pron.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data = subset(AWCnj, TI))
subset(TI.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)
#            category1  category2   N1  N2 N12      alpha.X2      alpha.G2
# 64  NA.persons.actor Pron.actor  162  88  88 2.049022e-239 1.523563e-112
# 99           actor.3    actor.1 1195 693   0 1.608236e-293             0
# 139         Dem.goal  Pron.goal  216 216 216             0 3.975816e-305
# 140         Dem.goal  Prox.goal  216 122 122 6.716883e-247  1.70352e-140
# 142         Dem.goal   Med.goal  216  94  94  1.36427e-187 7.031619e-105
# 144        Pron.goal  Prox.goal  216 122 122 6.716883e-247  1.70352e-140
# 146        Pron.goal   Med.goal  216  94  94  1.36427e-187 7.031619e-105

TI.ekaaother.Bivariate.man <- nominal(. ~ PV.Time + PV.WantCan + PV.Position + TI.speech + NA.persons.actor + Sg.actor + actor.3 + actor.2 + Sg.goal + NI.object.goal + Prox.goal + NI.nominal.goal + Med.goal + D.goal, data = subset(AWCnj, TI))

### VTA
TA.ekaaother.Bivariate.man.initial <- nominal(. ~ PV.Time + PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.3 + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal, data = subset(AWCnj, TA))
subset(TA.ekaaother.Bivariate.man.initial$bivariate, uc.12 > 0.5 | uc.21 > 0.5)

#category1 category2  N1  N2 N12
#actor.3    goal.3 893 897   0


TA.ekaaother.Bivariate.man <- nominal(. ~ PV.Time + PV.Discourse + PV.Position + TA.cognitive + TA.speech + Prox.actor + actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + Prox.goal, data = subset(AWCnj, TA))
