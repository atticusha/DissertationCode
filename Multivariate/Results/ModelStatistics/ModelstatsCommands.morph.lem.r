# Model Statistics Morphological Effects and Lemma
## Load Packages
install.packages("./polytomous_0.1.6.tar.gz", repos = NULL, type="source")
library(polytomous)

## Ind v. Cnj
ii.indcnj.observed.my.morph <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my.morph <- ifelse(as.vector(fitted(ii.indcnj.morph.glmer))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my.morph,ii.indcnj.predicted.my.morph)
ii.indcnj.p.values.my.morph <- cbind(as.vector(fitted(ii.indcnj.morph.glmer)), 1-as.vector(fitted(ii.indcnj.morph.glmer)))
colnames(ii.indcnj.p.values.my.morph) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my.morph<- model.statistics(ii.indcnj.observed.my.morph, ii.indcnj.predicted.my.morph, ii.indcnj.p.values.my.morph)

ai.indcnj.observed.my.morph <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my.morph <- ifelse(as.vector(fitted(ai.indcnj.morph.glmer))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my.morph,ai.indcnj.predicted.my.morph)
ai.indcnj.p.values.my.morph <- cbind(as.vector(fitted(ai.indcnj.morph.glmer)), 1-as.vector(fitted(ai.indcnj.morph.glmer)))
colnames(ai.indcnj.p.values.my.morph) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my.morph<- model.statistics(ai.indcnj.observed.my.morph, ai.indcnj.predicted.my.morph, ai.indcnj.p.values.my.morph)

ti.indcnj.observed.my.morph <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my.morph <- ifelse(as.vector(fitted(ti.indcnj.morph.glmer))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my.morph,ti.indcnj.predicted.my.morph)
ti.indcnj.p.values.my.morph <- cbind(as.vector(fitted(ti.indcnj.morph.glmer)), 1-as.vector(fitted(ti.indcnj.morph.glmer)))
colnames(ti.indcnj.p.values.my.morph) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my.morph<- model.statistics(ti.indcnj.observed.my.morph, ti.indcnj.predicted.my.morph, ti.indcnj.p.values.my.morph)

ta.indcnj.observed.my.morph <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my.morph <- ifelse(as.vector(fitted(ta.indcnj.morph.glmer))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my.morph,ta.indcnj.predicted.my.morph)
ta.indcnj.p.values.my.morph <- cbind(as.vector(fitted(ta.indcnj.morph.glmer)), 1-as.vector(fitted(ta.indcnj.morph.glmer)))
colnames(ta.indcnj.p.values.my.morph) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my.morph<- model.statistics(ta.indcnj.observed.my.morph, ta.indcnj.predicted.my.morph, ta.indcnj.p.values.my.morph)

## Ind v. eCnj
ii.ive.observed.my.morph <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.morph <- ifelse(as.vector(fitted(ii.ive.morph.glmer))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.morph,ii.ive.predicted.my.morph)
ii.ive.p.values.my.morph <- cbind(as.vector(fitted(ii.ive.morph.glmer)), 1-as.vector(fitted(ii.ive.morph.glmer)))
colnames(ii.ive.p.values.my.morph) <- c("Ind","eCnj")
ii.ive.ModelStats.my.morph<- model.statistics(ii.ive.observed.my.morph, ii.ive.predicted.my.morph, ii.ive.p.values.my.morph)

ai.ive.observed.my.morph <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my.morph <- ifelse(as.vector(fitted(ai.ive.morph.glmer))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my.morph,ai.ive.predicted.my.morph)
ai.ive.p.values.my.morph <- cbind(as.vector(fitted(ai.ive.morph.glmer)), 1-as.vector(fitted(ai.ive.morph.glmer)))
colnames(ai.ive.p.values.my.morph) <- c("Ind","eCnj")
ai.ive.ModelStats.my.morph<- model.statistics(ai.ive.observed.my.morph, ai.ive.predicted.my.morph, ai.ive.p.values.my.morph)

ti.ive.observed.my.morph <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my.morph <- ifelse(as.vector(fitted(ti.ive.morph.glmer))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my.morph,ti.ive.predicted.my.morph)
ti.ive.p.values.my.morph <- cbind(as.vector(fitted(ti.ive.morph.glmer)), 1-as.vector(fitted(ti.ive.morph.glmer)))
colnames(ti.ive.p.values.my.morph) <- c("Ind","eCnj")
ti.ive.ModelStats.my.morph<- model.statistics(ti.ive.observed.my.morph, ti.ive.predicted.my.morph, ti.ive.p.values.my.morph)

ta.ive.observed.my.morph <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my.morph <- ifelse(as.vector(fitted(ta.ive.morph.glmer))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my.morph,ta.ive.predicted.my.morph)
ta.ive.p.values.my.morph <- cbind(as.vector(fitted(ta.ive.morph.glmer)), 1-as.vector(fitted(ta.ive.morph.glmer)))
colnames(ta.ive.p.values.my.morph) <- c("Ind","eCnj")
ta.ive.ModelStats.my.morph<- model.statistics(ta.ive.observed.my.morph, ta.ive.predicted.my.morph, ta.ive.p.values.my.morph)

## Cnj Type
ii.all.cnjtypes.observed.my.morph <- subset(AWCnj, II, OrderType, drop=TRUE)
### Create an empty vector to store the column names
ii.all.cnjtypes.p.values.my.morph <-cbind(fitted(ii.e.cnjtype.morph.glmer), fitted(ii.kaa.cnjtype.morph.glmer), fitted(ii.other.cnjtype.morph.glmer))
colnames(ii.all.cnjtypes.p.values.my.morph) <- c("eCnj","kaaCnj","Other")
ii.all.cnjtypes.predicted.my.morph <- vector(mode = "character", length = nrow(ii.all.cnjtypes.p.values.my.morph))
### Loop through each row of the ii.all.cnjtypes.p.values.my.fixed object
for (i in 1:nrow(ii.all.cnjtypes.p.values.my.morph)) {
  ### Find the index of the column with the highest value in this row
  max_col_index <- which.max(ii.all.cnjtypes.p.values.my.morph[i,])
  ### Store the column name in the ii.all.cnjtypes.predicted.my.fixed vector
  ii.all.cnjtypes.predicted.my.morph[i] <- colnames(ii.all.cnjtypes.p.values.my.morph)[max_col_index]
}
table(ii.all.cnjtypes.observed.my.morph,ii.all.cnjtypes.predicted.my.morph)
ii.all.cnjtypes.ModelStats.my.morph<- model.statistics(ii.all.cnjtypes.observed.my.morph, ii.all.cnjtypes.predicted.my.morph, ii.all.cnjtypes.p.values.my.morph)

ai.all.cnjtypes.observed.my.morph <- subset(AWCnj, AI, OrderType, drop=TRUE)
ai.all.cnjtypes.p.values.my.morph <-cbind(fitted(ai.e.cnjtype.morph.glmer), fitted(ai.kaa.cnjtype.morph.glmer), fitted(ai.other.cnjtype.morph.glmer))
colnames(ai.all.cnjtypes.p.values.my.morph) <- c("eCnj","kaaCnj","Other")
ai.all.cnjtypes.predicted.my.morph <- vector(mode = "character", length = nrow(ai.all.cnjtypes.p.values.my.morph))
for (i in 1:nrow(ai.all.cnjtypes.p.values.my.morph)) {
  max_col_index <- which.max(ai.all.cnjtypes.p.values.my.morph[i,])
  ai.all.cnjtypes.predicted.my.morph[i] <- colnames(ai.all.cnjtypes.p.values.my.morph)[max_col_index]
}
table(ai.all.cnjtypes.observed.my.morph,ai.all.cnjtypes.predicted.my.morph)
ai.all.cnjtypes.ModelStats.my.morph<- model.statistics(ai.all.cnjtypes.observed.my.morph, ai.all.cnjtypes.predicted.my.morph, ai.all.cnjtypes.p.values.my.morph)

ti.all.cnjtypes.observed.my.morph <- subset(AWCnj, TI, OrderType, drop=TRUE)
ti.all.cnjtypes.p.values.my.morph <-cbind(fitted(ti.e.cnjtype.morph.glmer), fitted(ti.kaa.cnjtype.morph.glmer), fitted(ti.other.cnjtype.morph.glmer))
colnames(ti.all.cnjtypes.p.values.my.morph) <- c("eCnj","kaaCnj","Other")
ti.all.cnjtypes.predicted.my.morph <- vector(mode = "character", length = nrow(ti.all.cnjtypes.p.values.my.morph))
for (i in 1:nrow(ti.all.cnjtypes.p.values.my.morph)) {
  max_col_index <- which.max(ti.all.cnjtypes.p.values.my.morph[i,])
  ti.all.cnjtypes.predicted.my.morph[i] <- colnames(ti.all.cnjtypes.p.values.my.morph)[max_col_index]
}
table(ti.all.cnjtypes.observed.my.morph,ti.all.cnjtypes.predicted.my.morph)
ti.all.cnjtypes.ModelStats.my.morph<- model.statistics(ti.all.cnjtypes.observed.my.morph, ti.all.cnjtypes.predicted.my.morph, ti.all.cnjtypes.p.values.my.morph)

ta.all.cnjtypes.observed.my.sem <- subset(AWCnj, TA, OrderType, drop=TRUE)
ta.all.cnjtypes.p.values.my.sem <-cbind(fitted(ta.e.cnjtype.sem.glmer), fitted(ta.kaa.cnjtype.sem.glmer), fitted(ta.other.cnjtype.sem.glmer))
colnames(ta.all.cnjtypes.p.values.my.sem) <- c("eCnj","kaaCnj","Other")
ta.all.cnjtypes.predicted.my.sem <- vector(mode = "character", length = nrow(ta.all.cnjtypes.p.values.my.sem))
for (i in 1:nrow(ta.all.cnjtypes.p.values.my.sem)) {
  max_col_index <- which.max(ta.all.cnjtypes.p.values.my.sem[i,])
  ta.all.cnjtypes.predicted.my.sem[i] <- colnames(ta.all.cnjtypes.p.values.my.sem)[max_col_index]
}
table(ta.all.cnjtypes.observed.my.sem,ta.all.cnjtypes.predicted.my.sem)
ta.all.cnjtypes.ModelStats.my.sem<- model.statistics(ta.all.cnjtypes.observed.my.sem, ta.all.cnjtypes.predicted.my.sem, ta.all.cnjtypes.p.values.my.sem)