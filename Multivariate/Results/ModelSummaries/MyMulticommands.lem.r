# Model Commands Lemma Only
## Load Packages
library(lme4)

## Ind vs. Cnj
### VII
ii.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI
ai.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

## Ind vs eCnj
### VII
ii.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI
ai.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

## Cnj Type
### VII
ii.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI
ai.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#-------------------------------------------------------
### VII
ii.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI
ai.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))
#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VAI
ai.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTI
ti.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

### VTA
ta.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))