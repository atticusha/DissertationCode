# Model Statistics Mixed Effects
## Load Packages
install.packages("./polytomous_0.1.6.tar.gz", repos = NULL, type="source")
library(polytomous)

## Ind v. Cnj
ii.indcnj.observed.my <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my <- ifelse(as.vector(fitted(ii.indcnj.glmer))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my,ii.indcnj.predicted.my)
ii.indcnj.p.values.my <- cbind(as.vector(fitted(ii.indcnj.glmer)), 1-as.vector(fitted(ii.indcnj.glmer)))
colnames(ii.indcnj.p.values.my) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my<- model.statistics(ii.indcnj.observed.my, ii.indcnj.predicted.my, ii.indcnj.p.values.my)

ai.indcnj.observed.my <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my <- ifelse(as.vector(fitted(ai.indcnj.glmer))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my,ai.indcnj.predicted.my)
ai.indcnj.p.values.my <- cbind(as.vector(fitted(ai.indcnj.glmer)), 1-as.vector(fitted(ai.indcnj.glmer)))
colnames(ai.indcnj.p.values.my) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my<- model.statistics(ai.indcnj.observed.my, ai.indcnj.predicted.my, ai.indcnj.p.values.my)

ti.indcnj.observed.my <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my <- ifelse(as.vector(fitted(ti.indcnj.glmer))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my,ti.indcnj.predicted.my)
ti.indcnj.p.values.my <- cbind(as.vector(fitted(ti.indcnj.glmer)), 1-as.vector(fitted(ti.indcnj.glmer)))
colnames(ti.indcnj.p.values.my) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my<- model.statistics(ti.indcnj.observed.my, ti.indcnj.predicted.my, ti.indcnj.p.values.my)

ta.indcnj.observed.my <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my <- ifelse(as.vector(fitted(ta.indcnj.glmer))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my,ta.indcnj.predicted.my)
ta.indcnj.p.values.my <- cbind(as.vector(fitted(ta.indcnj.glmer)), 1-as.vector(fitted(ta.indcnj.glmer)))
colnames(ta.indcnj.p.values.my) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my<- model.statistics(ta.indcnj.observed.my, ta.indcnj.predicted.my, ta.indcnj.p.values.my)

## Ind v. eCnj
ii.ive.observed.my <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my <- ifelse(as.vector(fitted(ii.ive.glmer))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my,ii.ive.predicted.my)
ii.ive.p.values.my <- cbind(as.vector(fitted(ii.ive.glmer)), 1-as.vector(fitted(ii.ive.glmer)))
colnames(ii.ive.p.values.my) <- c("Ind","eCnj")
ii.ive.ModelStats.my<- model.statistics(ii.ive.observed.my, ii.ive.predicted.my, ii.ive.p.values.my)

ai.ive.observed.my <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my <- ifelse(as.vector(fitted(ai.ive.glmer))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my,ai.ive.predicted.my)
ai.ive.p.values.my <- cbind(as.vector(fitted(ai.ive.glmer)), 1-as.vector(fitted(ai.ive.glmer)))
colnames(ai.ive.p.values.my) <- c("Ind","eCnj")
ai.ive.ModelStats.my<- model.statistics(ai.ive.observed.my, ai.ive.predicted.my, ai.ive.p.values.my)

ti.ive.observed.my <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my <- ifelse(as.vector(fitted(ti.ive.glmer))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my,ti.ive.predicted.my)
ti.ive.p.values.my <- cbind(as.vector(fitted(ti.ive.glmer)), 1-as.vector(fitted(ti.ive.glmer)))
colnames(ti.ive.p.values.my) <- c("Ind","eCnj")
ti.ive.ModelStats.my<- model.statistics(ti.ive.observed.my, ti.ive.predicted.my, ti.ive.p.values.my)

ta.ive.observed.my <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my <- ifelse(as.vector(fitted(ta.ive.glmer))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my,ta.ive.predicted.my)
ta.ive.p.values.my <- cbind(as.vector(fitted(ta.ive.glmer)), 1-as.vector(fitted(ta.ive.glmer)))
colnames(ta.ive.p.values.my) <- c("Ind","eCnj")
ta.ive.ModelStats.my<- model.statistics(ta.ive.observed.my, ta.ive.predicted.my, ta.ive.p.values.my)

## Cnj Type
ii.all.cnjtypes.observed.my <- subset(AWCnj, II, OrderType, drop=TRUE)
### Create an empty vector to store the column names
ii.all.cnjtypes.p.values.my <-cbind(fitted(ii.e.cnjtype.glmer), fitted(ii.kaa.cnjtype.glmer), fitted(ii.other.cnjtype.glmer))
colnames(ii.all.cnjtypes.p.values.my) <- c("eCnj","kaaCnj","Other")
ii.all.cnjtypes.predicted.my <- vector(mode = "character", length = nrow(ii.all.cnjtypes.p.values.my))
### Loop through each row of the ii.all.cnjtypes.p.values.my object
for (i in 1:nrow(ii.all.cnjtypes.p.values.my)) {
  ### Find the index of the column with the highest value in this row
  max_col_index <- which.max(ii.all.cnjtypes.p.values.my[i,])
  ### Store the column name in the ii.all.cnjtypes.predicted.my vector
  ii.all.cnjtypes.predicted.my[i] <- colnames(ii.all.cnjtypes.p.values.my)[max_col_index]
}
table(ii.all.cnjtypes.observed.my,ii.all.cnjtypes.predicted.my)
ii.all.cnjtypes.ModelStats.my<- model.statistics(ii.all.cnjtypes.observed.my, ii.all.cnjtypes.predicted.my, ii.all.cnjtypes.p.values.my)

ai.all.cnjtypes.observed.my <- subset(AWCnj, AI, OrderType, drop=TRUE)
ai.all.cnjtypes.p.values.my <-cbind(fitted(ai.e.cnjtype.glmer), fitted(ai.kaa.cnjtype.glmer), fitted(ai.other.cnjtype.glmer))
colnames(ai.all.cnjtypes.p.values.my) <- c("eCnj","kaaCnj","Other")
ai.all.cnjtypes.predicted.my <- vector(mode = "character", length = nrow(ai.all.cnjtypes.p.values.my))
for (i in 1:nrow(ai.all.cnjtypes.p.values.my)) {
  max_col_index <- which.max(ai.all.cnjtypes.p.values.my[i,])
  ai.all.cnjtypes.predicted.my[i] <- colnames(ai.all.cnjtypes.p.values.my)[max_col_index]
}
table(ai.all.cnjtypes.observed.my,ai.all.cnjtypes.predicted.my)
ai.all.cnjtypes.ModelStats.my<- model.statistics(ai.all.cnjtypes.observed.my, ai.all.cnjtypes.predicted.my, ai.all.cnjtypes.p.values.my)

ti.all.cnjtypes.observed.my <- subset(AWCnj, TI, OrderType, drop=TRUE)
ti.all.cnjtypes.p.values.my <-cbind(fitted(ti.e.cnjtype.glmer), fitted(ti.kaa.cnjtype.glmer), fitted(ti.other.cnjtype.glmer))
colnames(ti.all.cnjtypes.p.values.my) <- c("eCnj","kaaCnj","Other")
ti.all.cnjtypes.predicted.my <- vector(mode = "character", length = nrow(ti.all.cnjtypes.p.values.my))
for (i in 1:nrow(ti.all.cnjtypes.p.values.my)) {
  max_col_index <- which.max(ti.all.cnjtypes.p.values.my[i,])
  ti.all.cnjtypes.predicted.my[i] <- colnames(ti.all.cnjtypes.p.values.my)[max_col_index]
}
table(ti.all.cnjtypes.observed.my,ti.all.cnjtypes.predicted.my)
ti.all.cnjtypes.ModelStats.my<- model.statistics(ti.all.cnjtypes.observed.my, ti.all.cnjtypes.predicted.my, ti.all.cnjtypes.p.values.my)

ta.all.cnjtypes.observed.my <- subset(AWCnj, TA, OrderType, drop=TRUE)
ta.all.cnjtypes.p.values.my <-cbind(fitted(ta.e.cnjtype.glmer), fitted(ta.kaa.cnjtype.glmer), fitted(ta.other.cnjtype.glmer))
colnames(ta.all.cnjtypes.p.values.my) <- c("eCnj","kaaCnj","Other")
ta.all.cnjtypes.predicted.my <- vector(mode = "character", length = nrow(ta.all.cnjtypes.p.values.my))
for (i in 1:nrow(ta.all.cnjtypes.p.values.my)) {
  max_col_index <- which.max(ta.all.cnjtypes.p.values.my[i,])
  ta.all.cnjtypes.predicted.my[i] <- colnames(ta.all.cnjtypes.p.values.my)[max_col_index]
}
table(ta.all.cnjtypes.observed.my,ta.all.cnjtypes.predicted.my)
ta.all.cnjtypes.ModelStats.my<- model.statistics(ta.all.cnjtypes.observed.my, ta.all.cnjtypes.predicted.my, ta.all.cnjtypes.p.values.my)
