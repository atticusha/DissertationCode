###==========================================================================================================
###Independent vs ê-Conjunct
###==========================================================================================================
####VII
ii.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ii.rand.txt')
round(print(summary(ii.ive.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ai.rand.txt')
round(print(summary(ai.ive.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ti.rand.txt')
round(print(summary(ti.ive.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.ive.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWive, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ive-ta.rand.txt')
round(print(summary(ta.ive.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)


###==========================================================================================================
###Independent vs Conjunct
###==========================================================================================================
####VII
ii.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ii.rand.txt')
round(print(summary(ii.indcnj.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ai.rand.txt')
round(print(summary(ai.indcnj.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ti.rand.txt')
round(print(summary(ti.indcnj.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.indcnj.glmer.rand <- glmer(Ind ~ (1|Lemma), data=subset(AWnimp, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/ivc-ta.rand.txt')
round(print(summary(ta.indcnj.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

###==========================================================================================================
###Conjunct Type 
###==========================================================================================================
####VII
ii.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ii.rand.txt')
round(print(summary(ii.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)


####VAI
ai.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ai.rand.txt')
round(print(summary(ai.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ti.rand.txt')
round(print(summary(ti.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.e.cnjtype.glmer.rand <- glmer(PV.e ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/e-cnj-ta.rand.txt')
round(print(summary(ta.e.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

#-------------------------------------------------------
####VII
ii.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ii.rand.txt')
round(print(summary(ii.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)


####VAI
ai.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ai.rand.txt')
round(print(summary(ai.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ti.rand.txt')
round(print(summary(ti.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.kaa.cnjtype.glmer.rand <- glmer(PV.kaa ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/kaa-cnj-ta.rand.txt')
round(print(summary(ta.kaa.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)
#-------------------------------------------------------
####VII
ii.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, II), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ii.rand.txt')
round(print(summary(ii.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VAI
ai.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, AI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ai.rand.txt')
round(print(summary(ai.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VTI
ti.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, TI), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ti.rand.txt')
round(print(summary(ti.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

####VTA
ta.other.cnjtype.glmer.rand <- glmer(OtherCnj ~ (1|Lemma), data=subset(AWCnj, TA), family=binomial, control=glmerControl(optimizer = "bobyqa"))

sink(file = 'SummaryTables/man/other-cnj-ta.rand.txt')
round(print(summary(ta.other.cnjtype.glmer.rand)$coefficients, max.print = NA), 3)
sink(NULL)

