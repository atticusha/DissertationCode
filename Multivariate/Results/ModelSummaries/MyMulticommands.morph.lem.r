# Model Commands Morphological Effects and Lemma
## Load Packages
library(lme4)

## Ind vs. Cnj
### VII
#### ii.indcnj.morph.glmer <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#### No morphological Effects

### VAI
ai.indcnj.morph.glmer <- glmer(Ind ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.2 + actor.4 + (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.indcnj.morph.glmer <- glmer(Ind ~ actor.3 + actor.2 + Sg.goal + Pl.goal + Px3Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.indcnj.morph.glmer <- glmer(Ind ~ Sg.actor + actor.1 + actor.4 + goal.3 + goal.4 + goal.2 + Px3Pl.goal + (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

## Ind vs. eCnj
### VII
#### ii.ive.morph.glmer <- glmer(Ind ~ (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#### No morphological Effects

### VAI
ai.ive.morph.glmer <- glmer(Ind ~ RdplW + Pl.actor + actor.3 + actor.1 + actor.4 + (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.ive.morph.glmer <- glmer(Ind ~ actor.3 + actor.2 + Pl.goal + Px1Sg.goal + (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.ive.morph.glmer <- glmer(Ind ~ Sg.actor + actor.1 + actor.4 + actor.2 + goal.3 + goal.4 + Px3Sg.goal + Px3Pl.goal + (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

## Cnj Type
### VII
#### ii.e.cnjtype.morph.glmer <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#### No morphological Effects

### VAI
ai.e.cnjtype.morph.glmer <- glmer(PV.e ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.e.cnjtype.morph.glmer <- glmer(PV.e ~ Sg.actor + actor.3 + actor.2 + Sg.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.e.cnjtype.morph.glmer <- glmer(PV.e ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#-------------------------------------------------------
### VII
####ii.kaa.cnjtype.morph.glmer <- glmer(PV.kaa  ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#### No morphological Effects

### VAI
ai.kaa.cnjtype.morph.glmer <- glmer(PV.kaa ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.kaa.cnjtype.morph.glmer <- glmer(PV.kaa ~ Sg.actor + actor.3 + actor.2 + Sg.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.kaa.cnjtype.morph.glmer <- glmer(PV.kaa ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#-------------------------------------------------------
### VII
#### ii.other.cnjtype.morph.glmer <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#### No morphological Effects

### VAI
ai.other.cnjtype.morph.glmer <- glmer(OtherCnj ~ RdplW + Sg.actor + Pl.actor + actor.3 + actor.1 + actor.2 + (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.other.cnjtype.morph.glmer <- glmer(OtherCnj ~ Sg.actor + actor.3 + actor.2 + Sg.goal + D.goal + (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.other.cnjtype.morph.glmer <- glmer(OtherCnj ~ actor.1 + actor.2 + goal.3 + goal.4 + goal.1 + goal.2 + Sg.goal + Px1Sg.goal + (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))