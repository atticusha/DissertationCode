#Multivariate Commands for all Alternations
##==========================================================================================================

###==========================================================================================================
###Independent vs ê-Conjunct
###==========================================================================================================
### VII: There were no morphological effects

### VAI
ai.ive.morph.fixed.glmer <- glm(Ind ~ RdplW + Pl.actor + actor.3 + actor.1 + actor.4, data=subset(AWive, AI), family=binomial)

sink(file = "./ai-ModelStats.my.morph.txt")
round(print(summary(ai.ive.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.morph.fixed.glmer <- glm(Ind ~ actor.3 + actor.2 + Pl.goal + Px1Sg.goal, data=subset(AWive, TI), family=binomial)

sink(file = "./ti-ModelStats.my.morph.txt")
round(print(summary(ti.ive.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.morph.fixed.glmer <- glm(Ind ~ Sg.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + Px3Sg.goal + Px3Pl.goal, data=subset(AWive, TA), family=binomial)

sink(file = "./ta-ModelStats.my.morph.txt")
round(print(summary(ta.ive.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)


###==========================================================================================================
###Independent vs Conjunct
###==========================================================================================================
### VII: There were no morphological effects

### VAI
ai.indcnj.morph.fixed.glmer <- glm(Ind ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.2 + actor.4, data=subset(AWnimp, AI), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/ivc/ai-ModelStats.my.morph.txt")
round(print(summary(ai.indcnj.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.morph.fixed.glmer <- glm(Ind ~ actor.3 + actor.2 + Sg.goal + Pl.goal + Px3Sg.goal + Px1Sg.goal, data=subset(AWnimp, TI), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/ivc/ti-ModelStats.my.morph.txt")
round(print(summary(ti.indcnj.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.morph.fixed.glmer <- glm(Ind ~ Sg.actor + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + Px3Pl.goal, data=subset(AWnimp, TA), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/ivc/ta-ModelStats.my.morph.txt")
round(print(summary(ta.indcnj.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: ê-Conjunct
###==========================================================================================================
### VII: There were no morphological effects

### VAI
ai.e.cnjtype.morph.fixed.glmer <- glm(PV.e ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/cnjtype/ecnj/ai-ecnj-ModelStats.my.morph.txt")
round(print(summary(ai.e.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.morph.fixed.glmer <- glm(PV.e ~ Sg.actor + actor.3 + actor.2 + Sg.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/cnjtype/ecnj/ti-ecnj-ModelStats.my.morph.txt")
round(print(summary(ti.e.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.morph.fixed.glmer <- glm(PV.e ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/cnjtype/ecnj/ta-ecnj-ModelStats.my.morph.txt")
round(print(summary(ta.e.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: kâ-Conjunct
###==========================================================================================================
### VII: There were no morphological effects

### VAI
ai.kaa.cnjtype.morph.fixed.glmer <- glm(PV.kaa ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/cnjtype/kaacnj/ai-kaacnj-ModelStats.my.morph.txt")
round(print(summary(ai.kaa.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.morph.fixed.glmer <- glm(PV.kaa ~ Sg.actor + actor.3 + actor.2 + Sg.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/cnjtype/kaacnj/ti-kaacnj-ModelStats.my.morph.txt")
round(print(summary(ti.kaa.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.morph.fixed.glmer <- glm(PV.kaa ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/cnjtype/kaacnj/ta-kaacnj-ModelStats.my.morph.txt")
round(print(summary(ta.kaa.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type: Other Conjunct
###==========================================================================================================
### VII: There were no morphological effects

### VAI
ai.other.cnjtype.morph.fixed.glmer <- glm(OtherCnj ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2, data=subset(AWCnj, AI), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/cnjtype/other/ai-other-ModelStats.my.morph.txt")
round(print(summary(ai.other.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.morph.fixed.glmer <- glm(OtherCnj ~ Sg.actor + actor.3 + actor.2 + Sg.goal + D.goal, data=subset(AWCnj, TI), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/cnjtype/other/ti-other-ModelStats.my.morph.txt")
round(print(summary(ti.other.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.morph.fixed.glmer <- glm(OtherCnj ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal, data=subset(AWCnj, TA), family=binomial)

sink(file = "~/ModelStatistics/morph-fixed/cnjtype/other/ta-other-ModelStats.my.morph.txt")
round(print(summary(ta.other.cnjtype.morph.fixed.glmer)$coefficients, max.print = NA), 3)
sink(NULL)
