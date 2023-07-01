#Multivariate Commands for all Alternations
##==========================================================================================================

###==========================================================================================================
###Independent vs ê-Conjunct
###==========================================================================================================
####VII: there were no morphological effects

####VAI
ai.ive.morph.glmer <- glmer(Ind ~ RdplW + Pl.actor + actor.3 + actor.1 + actor.4 + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/ive/ai-ModelStats.my.morph.txt')
round(print(summary(ai.ive.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.ive.morph.glmer <- glmer(Ind ~ actor.3 + actor.2 + Pl.goal + Px1Sg.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/ive/ti-ModelStats.my.morph.txt')
round(print(summary(ti.ive.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.ive.morph.glmer <- glmer(Ind ~ Sg.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + Px3Sg.goal + Px3Pl.goal + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/ive/ta-ModelStats.my.morph.txt')
round(print(summary(ta.ive.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


###==========================================================================================================
###Independent vs Conjunct
###==========================================================================================================
####VII: there were no morphological effects

####VAI
ai.indcnj.morph.glmer <- glmer(Ind ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.2 + actor.4 + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/ivc/ai-ModelStats.my.morph.txt')
round(print(summary(ai.indcnj.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.indcnj.morph.glmer <- glmer(Ind ~ actor.3 + actor.2 + Sg.goal + Pl.goal + Px3Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/ivc/ti-ModelStats.my.morph.txt')
round(print(summary(ti.indcnj.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.indcnj.morph.glmer <- glmer(Ind ~ Sg.actor + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + Px3Pl.goal + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/ivc/ta-ModelStats.my.morph.txt')
round(print(summary(ta.indcnj.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: ê-Conjunct
###==========================================================================================================
####VII: there were no morphological effects

####VAI
ai.e.cnjtype.morph.glmer <- glmer(PV.e ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/cnjtype/ecnj/ai-ecnj-ModelStats.my.morph.txt')
round(print(summary(ai.e.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.e.cnjtype.morph.glmer <- glmer(PV.e ~ Sg.actor + actor.3 + actor.2 + Sg.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/cnjtype/ecnj/ti-ecnj-ModelStats.my.morph.txt')
round(print(summary(ti.e.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.e.cnjtype.morph.glmer <- glmer(PV.e ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/cnjtype/ecnj/ta-ecnj-ModelStats.my.morph.txt')
round(print(summary(ta.e.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: kâ-Conjunct
###==========================================================================================================
####VII: there were no morphological effects

####VAI
ai.kaa.cnjtype.morph.glmer <- glmer(PV.kaa ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/cnjtype/kaacnj/ai-kaacnj-ModelStats.my.morph.txt')
round(print(summary(ai.kaa.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.kaa.cnjtype.morph.glmer <- glmer(PV.kaa ~ Sg.actor + actor.3 + actor.2 + Sg.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/cnjtype/kaacnj/ti-kaacnj-ModelStats.my.morph.txt')
round(print(summary(ti.kaa.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.kaa.cnjtype.morph.glmer <- glmer(PV.kaa ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/cnjtype/kaacnj/ta-kaacnj-ModelStats.my.morph.txt')
round(print(summary(ta.kaa.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: Other Conjunct
###==========================================================================================================
####VII: there were no morphological effects

####VAI
ai.other.cnjtype.morph.glmer <- glmer(OtherCnj ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/cnjtype/other/ai-other-ModelStats.my.morph.txt')
round(print(summary(ai.other.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.other.cnjtype.morph.glmer <- glmer(OtherCnj ~ Sg.actor + actor.3 + actor.2 + Sg.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/cnjtype/other/ti-other-ModelStats.my.morph.txt')
round(print(summary(ti.other.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.other.cnjtype.morph.glmer <- glmer(OtherCnj ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '~/ModelStatistics/morph/cnjtype/other/ta-other-ModelStats.my.morph.txt')
round(print(summary(ta.other.cnjtype.morph.glmer)$coefficients, max.print = NA), 3)
sink(NULL)
