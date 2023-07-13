#Ind vs eCnj
#============================================================================================================
### VII
ii.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Ind vs. eCnj/ive-ii.lem.txt')
round(print(summary(ii.ive.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Ind vs. eCnj/ive-ai.lem.txt')
round(print(summary(ai.ive.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Ind vs. eCnj/ive-ti.lem.txt')
round(print(summary(ti.ive.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Ind vs. eCnj/ive-ta.lem.txt')
round(print(summary(ta.ive.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)


#============================================================================================================
# Ind vs. cnj
#============================================================================================================
### VAI
ii.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Ind vs. Cnj/ivc-ii.lem.txt')
round(print(summary(ii.indcnj.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Ind vs. Cnj/ivc-ai.lem.txt')
round(print(summary(ai.indcnj.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Ind vs. Cnj/ivc-ti.lem.txt')
round(print(summary(ti.indcnj.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Ind vs. Cnj/ivc-ta.lem.txt')
round(print(summary(ta.indcnj.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

#============================================================================================================
# ecnj v kaacnj v other
#============================================================================================================
### VII
ii.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ii-e-cnj.lem.txt')
round(print(summary(ii.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ai-e-cnj.lem.txt')
round(print(summary(ai.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ti-e-cnj.lem.txt')
round(print(summary(ti.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ta-e-cnj.lem.txt')
round(print(summary(ta.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
### VII
ii.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ii-kaa-cnj.lem.txt')
round(print(summary(ii.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)


### VAI
ai.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ai-kaa-cnj.lem.txt')
round(print(summary(ai.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ti-kaa-cnj.lem.txt')
round(print(summary(ti.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ta-kaa-cnj.lem.txt')
round(print(summary(ta.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
### VII
ii.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ii-other-cnj.lem.txt')
round(print(summary(ii.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VAI
ai.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ai-other-cnj.lem.txt')
round(print(summary(ai.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTI
ti.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ti-other-cnj.lem.txt')
round(print(summary(ti.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

### VTA
ta.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = '/Users/atticusharrigan/Dres/Multi/Lem/Cnj Type/ta-other-cnj.lem.txt')
round(print(summary(ta.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

