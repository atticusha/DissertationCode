# Model Statistics Semantic Effects and Lemma
## Load Packages
install.packages("./polytomous_0.1.6.tar.gz", repos = NULL, type="source")
library(polytomous)

## Ind v. Cnj
ii.indcnj.observed.my.sem <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my.sem <- ifelse(as.vector(fitted(ii.indcnj.sem.glmer))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my.sem,ii.indcnj.predicted.my.sem)
ii.indcnj.p.values.my.sem <- cbind(as.vector(fitted(ii.indcnj.sem.glmer)), 1-as.vector(fitted(ii.indcnj.sem.glmer)))
colnames(ii.indcnj.p.values.my.sem) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my.sem<- model.statistics(ii.indcnj.observed.my.sem, ii.indcnj.predicted.my.sem, ii.indcnj.p.values.my.sem)

ai.indcnj.observed.my.sem <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my.sem <- ifelse(as.vector(fitted(ai.indcnj.sem.glmer))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my.sem,ai.indcnj.predicted.my.sem)
ai.indcnj.p.values.my.sem <- cbind(as.vector(fitted(ai.indcnj.sem.glmer)), 1-as.vector(fitted(ai.indcnj.sem.glmer)))
colnames(ai.indcnj.p.values.my.sem) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my.sem<- model.statistics(ai.indcnj.observed.my.sem, ai.indcnj.predicted.my.sem, ai.indcnj.p.values.my.sem)

ti.indcnj.observed.my.sem <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my.sem <- ifelse(as.vector(fitted(ti.indcnj.sem.glmer))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my.sem,ti.indcnj.predicted.my.sem)
ti.indcnj.p.values.my.sem <- cbind(as.vector(fitted(ti.indcnj.sem.glmer)), 1-as.vector(fitted(ti.indcnj.sem.glmer)))
colnames(ti.indcnj.p.values.my.sem) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my.sem<- model.statistics(ti.indcnj.observed.my.sem, ti.indcnj.predicted.my.sem, ti.indcnj.p.values.my.sem)

ta.indcnj.observed.my.sem <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my.sem <- ifelse(as.vector(fitted(ta.indcnj.sem.glmer))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my.sem,ta.indcnj.predicted.my.sem)
ta.indcnj.p.values.my.sem <- cbind(as.vector(fitted(ta.indcnj.sem.glmer)), 1-as.vector(fitted(ta.indcnj.sem.glmer)))
colnames(ta.indcnj.p.values.my.sem) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my.sem<- model.statistics(ta.indcnj.observed.my.sem, ta.indcnj.predicted.my.sem, ta.indcnj.p.values.my.sem)

## Ind v. eCnj
ii.ive.observed.my.sem <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.sem <- ifelse(as.vector(fitted(ii.ive.sem.glmer))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.sem,ii.ive.predicted.my.sem)
ii.ive.p.values.my.sem <- cbind(as.vector(fitted(ii.ive.sem.glmer)), 1-as.vector(fitted(ii.ive.sem.glmer)))
colnames(ii.ive.p.values.my.sem) <- c("Ind","eCnj")
ii.ive.ModelStats.my.sem<- model.statistics(ii.ive.observed.my.sem, ii.ive.predicted.my.sem, ii.ive.p.values.my.sem)

ai.ive.observed.my.sem <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my.sem <- ifelse(as.vector(fitted(ai.ive.sem.glmer))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my.sem,ai.ive.predicted.my.sem)
ai.ive.p.values.my.sem <- cbind(as.vector(fitted(ai.ive.sem.glmer)), 1-as.vector(fitted(ai.ive.sem.glmer)))
colnames(ai.ive.p.values.my.sem) <- c("Ind","eCnj")
ai.ive.ModelStats.my.sem<- model.statistics(ai.ive.observed.my.sem, ai.ive.predicted.my.sem, ai.ive.p.values.my.sem)

ti.ive.observed.my.sem <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my.sem <- ifelse(as.vector(fitted(ti.ive.sem.glmer))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my.sem,ti.ive.predicted.my.sem)
ti.ive.p.values.my.sem <- cbind(as.vector(fitted(ti.ive.sem.glmer)), 1-as.vector(fitted(ti.ive.sem.glmer)))
colnames(ti.ive.p.values.my.sem) <- c("Ind","eCnj")
ti.ive.ModelStats.my.sem<- model.statistics(ti.ive.observed.my.sem, ti.ive.predicted.my.sem, ti.ive.p.values.my.sem)

ta.ive.observed.my.sem <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my.sem <- ifelse(as.vector(fitted(ta.ive.sem.glmer))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my.sem,ta.ive.predicted.my.sem)
ta.ive.p.values.my.sem <- cbind(as.vector(fitted(ta.ive.sem.glmer)), 1-as.vector(fitted(ta.ive.sem.glmer)))
colnames(ta.ive.p.values.my.sem) <- c("Ind","eCnj")
ta.ive.ModelStats.my.sem<- model.statistics(ta.ive.observed.my.sem, ta.ive.predicted.my.sem, ta.ive.p.values.my.sem)


## Cnj Type
ii.all.cnjtypes.observed.my.sem <- subset(AWCnj, II, OrderType, drop=TRUE)
### Create an empty vector to store the column names
ii.all.cnjtypes.p.values.my.sem <-cbind(fitted(ii.e.cnjtype.sem.glmer), fitted(ii.kaa.cnjtype.sem.glmer), fitted(ii.other.cnjtype.sem.glmer))
colnames(ii.all.cnjtypes.p.values.my.sem) <- c("eCnj","kaaCnj","Other")
ii.all.cnjtypes.predicted.my.sem <- vector(mode = "character", length = nrow(ii.all.cnjtypes.p.values.my.sem))
### Loop through each row of the ii.all.cnjtypes.p.values.my.fixed object
for (i in 1:nrow(ii.all.cnjtypes.p.values.my.sem)) {
  ### Find the index of the column with the highest value in this row
  max_col_index <- which.max(ii.all.cnjtypes.p.values.my.sem[i,])
  ### Store the column name in the ii.all.cnjtypes.predicted.my.fixed vector
  ii.all.cnjtypes.predicted.my.sem[i] <- colnames(ii.all.cnjtypes.p.values.my.sem)[max_col_index]
}
table(ii.all.cnjtypes.observed.my.sem,ii.all.cnjtypes.predicted.my.sem)
ii.all.cnjtypes.ModelStats.my.sem<- model.statistics(ii.all.cnjtypes.observed.my.sem, ii.all.cnjtypes.predicted.my.sem, ii.all.cnjtypes.p.values.my.sem)


ai.all.cnjtypes.observed.my.sem <- subset(AWCnj, AI, OrderType, drop=TRUE)
ai.all.cnjtypes.p.values.my.sem <-cbind(fitted(ai.e.cnjtype.sem.glmer), fitted(ai.kaa.cnjtype.sem.glmer), fitted(ai.other.cnjtype.sem.glmer))
colnames(ai.all.cnjtypes.p.values.my.sem) <- c("eCnj","kaaCnj","Other")
ai.all.cnjtypes.predicted.my.sem <- vector(mode = "character", length = nrow(ai.all.cnjtypes.p.values.my.sem))
for (i in 1:nrow(ai.all.cnjtypes.p.values.my.sem)) {
  max_col_index <- which.max(ai.all.cnjtypes.p.values.my.sem[i,])
  ai.all.cnjtypes.predicted.my.sem[i] <- colnames(ai.all.cnjtypes.p.values.my.sem)[max_col_index]
}
table(ai.all.cnjtypes.observed.my.sem,ai.all.cnjtypes.predicted.my.sem)
ai.all.cnjtypes.ModelStats.my.sem<- model.statistics(ai.all.cnjtypes.observed.my.sem, ai.all.cnjtypes.predicted.my.sem, ai.all.cnjtypes.p.values.my.sem)

ti.all.cnjtypes.observed.my.sem <- subset(AWCnj, TI, OrderType, drop=TRUE)
ti.all.cnjtypes.p.values.my.sem <-cbind(fitted(ti.e.cnjtype.sem.glmer), fitted(ti.kaa.cnjtype.sem.glmer), fitted(ti.other.cnjtype.sem.glmer))
colnames(ti.all.cnjtypes.p.values.my.sem) <- c("eCnj","kaaCnj","Other")
ti.all.cnjtypes.predicted.my.sem <- vector(mode = "character", length = nrow(ti.all.cnjtypes.p.values.my.sem))
for (i in 1:nrow(ti.all.cnjtypes.p.values.my.sem)) {
  max_col_index <- which.max(ti.all.cnjtypes.p.values.my.sem[i,])
  ti.all.cnjtypes.predicted.my.sem[i] <- colnames(ti.all.cnjtypes.p.values.my.sem)[max_col_index]
}
table(ti.all.cnjtypes.observed.my.sem,ti.all.cnjtypes.predicted.my.sem)
ti.all.cnjtypes.ModelStats.my.sem<- model.statistics(ti.all.cnjtypes.observed.my.sem, ti.all.cnjtypes.predicted.my.sem, ti.all.cnjtypes.p.values.my.sem)

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