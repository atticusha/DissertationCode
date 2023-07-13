#MODEL STATISTICS

## Ind v. Cnj
ii.indcnj.observed.my.rand <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my.rand <- ifelse(as.vector(fitted(ii.indcnj.glmer.rand))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my.rand,ii.indcnj.predicted.my.rand)
ii.indcnj.p.values.my.rand <- cbind(as.vector(fitted(ii.indcnj.glmer.rand)), 1-as.vector(fitted(ii.indcnj.glmer.rand)))
colnames(ii.indcnj.p.values.my.rand) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my.rand<- model.statistics(ii.indcnj.observed.my.rand, ii.indcnj.predicted.my.rand, ii.indcnj.p.values.my.rand)

ai.indcnj.observed.my.rand <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my.rand <- ifelse(as.vector(fitted(ai.indcnj.glmer.rand))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my.rand,ai.indcnj.predicted.my.rand)
ai.indcnj.p.values.my.rand <- cbind(as.vector(fitted(ai.indcnj.glmer.rand)), 1-as.vector(fitted(ai.indcnj.glmer.rand)))
colnames(ai.indcnj.p.values.my.rand) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my.rand<- model.statistics(ai.indcnj.observed.my.rand, ai.indcnj.predicted.my.rand, ai.indcnj.p.values.my.rand)

ti.indcnj.observed.my.rand <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my.rand <- ifelse(as.vector(fitted(ti.indcnj.glmer.rand))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my.rand,ti.indcnj.predicted.my.rand)
ti.indcnj.p.values.my.rand <- cbind(as.vector(fitted(ti.indcnj.glmer.rand)), 1-as.vector(fitted(ti.indcnj.glmer.rand)))
colnames(ti.indcnj.p.values.my.rand) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my.rand<- model.statistics(ti.indcnj.observed.my.rand, ti.indcnj.predicted.my.rand, ti.indcnj.p.values.my.rand)

ta.indcnj.observed.my.rand <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my.rand <- ifelse(as.vector(fitted(ta.indcnj.glmer.rand))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my.rand,ta.indcnj.predicted.my.rand)
ta.indcnj.p.values.my.rand <- cbind(as.vector(fitted(ta.indcnj.glmer.rand)), 1-as.vector(fitted(ta.indcnj.glmer.rand)))
colnames(ta.indcnj.p.values.my.rand) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my.rand<- model.statistics(ta.indcnj.observed.my.rand, ta.indcnj.predicted.my.rand, ta.indcnj.p.values.my.rand)

### Ind v. eCnj
ii.ive.observed.my.rand <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.rand <- ifelse(as.vector(fitted(ii.ive.glmer.rand))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.rand,ii.ive.predicted.my.rand)
ii.ive.p.values.my.rand <- cbind(as.vector(fitted(ii.ive.glmer.rand)), 1-as.vector(fitted(ii.ive.glmer.rand)))
colnames(ii.ive.p.values.my.rand) <- c("Ind","eCnj")
ii.ive.ModelStats.my.rand<- model.statistics(ii.ive.observed.my.rand, ii.ive.predicted.my.rand, ii.ive.p.values.my.rand)

ai.ive.observed.my.rand <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my.rand <- ifelse(as.vector(fitted(ai.ive.glmer.rand))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my.rand,ai.ive.predicted.my.rand)
ai.ive.p.values.my.rand <- cbind(as.vector(fitted(ai.ive.glmer.rand)), 1-as.vector(fitted(ai.ive.glmer.rand)))
colnames(ai.ive.p.values.my.rand) <- c("Ind","eCnj")
ai.ive.ModelStats.my.rand<- model.statistics(ai.ive.observed.my.rand, ai.ive.predicted.my.rand, ai.ive.p.values.my.rand)

ti.ive.observed.my.rand <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my.rand <- ifelse(as.vector(fitted(ti.ive.glmer.rand))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my.rand,ti.ive.predicted.my.rand)
ti.ive.p.values.my.rand <- cbind(as.vector(fitted(ti.ive.glmer.rand)), 1-as.vector(fitted(ti.ive.glmer.rand)))
colnames(ti.ive.p.values.my.rand) <- c("Ind","eCnj")
ti.ive.ModelStats.my.rand<- model.statistics(ti.ive.observed.my.rand, ti.ive.predicted.my.rand, ti.ive.p.values.my.rand)

ta.ive.observed.my.rand <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my.rand <- ifelse(as.vector(fitted(ta.ive.glmer.rand))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my.rand,ta.ive.predicted.my.rand)
ta.ive.p.values.my.rand <- cbind(as.vector(fitted(ta.ive.glmer.rand)), 1-as.vector(fitted(ta.ive.glmer.rand)))
colnames(ta.ive.p.values.my.rand) <- c("Ind","eCnj")
ta.ive.ModelStats.my.rand<- model.statistics(ta.ive.observed.my.rand, ta.ive.predicted.my.rand, ta.ive.p.values.my.rand)

## Cnj Type
ii.all.cnjtypes.observed.my.rand <- subset(AWCnj, II, OrderType, drop=TRUE)
# Create an empty vector to store the column names
ii.all.cnjtypes.p.values.my.rand <-cbind(fitted(ii.e.cnjtype.glmer.rand), fitted(ii.kaa.cnjtype.glmer.rand), fitted(ii.other.cnjtype.glmer.rand))
colnames(ii.all.cnjtypes.p.values.my.rand) <- c("eCnj","kaaCnj","Other")
ii.all.cnjtypes.predicted.my.rand <- vector(mode = "character", length = nrow(ii.all.cnjtypes.p.values.my.rand))
# Loop through each row of the ii.all.cnjtypes.p.values.my.rand object
for (i in 1:nrow(ii.all.cnjtypes.p.values.my.rand)) {
  # Find the index of the column with the highest value in this row
  max_col_index <- which.max(ii.all.cnjtypes.p.values.my.rand[i,])
  # Store the column name in the ii.all.cnjtypes.predicted.my.rand vector
  ii.all.cnjtypes.predicted.my.rand[i] <- colnames(ii.all.cnjtypes.p.values.my.rand)[max_col_index]
}
table(ii.all.cnjtypes.observed.my.rand,ii.all.cnjtypes.predicted.my.rand)
ii.all.cnjtypes.ModelStats.my.rand<- model.statistics(ii.all.cnjtypes.observed.my.rand, ii.all.cnjtypes.predicted.my.rand, ii.all.cnjtypes.p.values.my.rand)

ai.all.cnjtypes.observed.my.rand <- subset(AWCnj, AI, OrderType, drop=TRUE)
# Create an empty vector to store the column names
ai.all.cnjtypes.p.values.my.rand <-cbind(fitted(ai.e.cnjtype.glmer.rand), fitted(ai.kaa.cnjtype.glmer.rand), fitted(ai.other.cnjtype.glmer.rand))
colnames(ai.all.cnjtypes.p.values.my.rand) <- c("eCnj","kaaCnj","Other")
ai.all.cnjtypes.predicted.my.rand <- vector(mode = "character", length = nrow(ai.all.cnjtypes.p.values.my.rand))
# Loop through each row of the ai.all.cnjtypes.p.values.my.rand object
for (i in 1:nrow(ai.all.cnjtypes.p.values.my.rand)) {
  # Find the index of the column with the highest value in this row
  max_col_index <- which.max(ai.all.cnjtypes.p.values.my.rand[i,])
  # Store the column name in the ai.all.cnjtypes.predicted.my.rand vector
  ai.all.cnjtypes.predicted.my.rand[i] <- colnames(ai.all.cnjtypes.p.values.my.rand)[max_col_index]
}
table(ai.all.cnjtypes.observed.my.rand,ai.all.cnjtypes.predicted.my.rand)
ai.all.cnjtypes.ModelStats.my.rand<- model.statistics(ai.all.cnjtypes.observed.my.rand, ai.all.cnjtypes.predicted.my.rand, ai.all.cnjtypes.p.values.my.rand)

ti.all.cnjtypes.observed.my.rand <- subset(AWCnj, TI, OrderType, drop=TRUE)
# Create an empty vector to store the column names
ti.all.cnjtypes.p.values.my.rand <-cbind(fitted(ti.e.cnjtype.glmer.rand), fitted(ti.kaa.cnjtype.glmer.rand), fitted(ti.other.cnjtype.glmer.rand))
colnames(ti.all.cnjtypes.p.values.my.rand) <- c("eCnj","kaaCnj","Other")
ti.all.cnjtypes.predicted.my.rand <- vector(mode = "character", length = nrow(ti.all.cnjtypes.p.values.my.rand))
# Loop through each row of the ti.all.cnjtypes.p.values.my.rand object
for (i in 1:nrow(ti.all.cnjtypes.p.values.my.rand)) {
  # Find the index of the column with the highest value in this row
  max_col_index <- which.max(ti.all.cnjtypes.p.values.my.rand[i,])
  # Store the column name in the ti.all.cnjtypes.predicted.my.rand vector
  ti.all.cnjtypes.predicted.my.rand[i] <- colnames(ti.all.cnjtypes.p.values.my.rand)[max_col_index]
}
table(ti.all.cnjtypes.observed.my.rand,ti.all.cnjtypes.predicted.my.rand)
ti.all.cnjtypes.ModelStats.my.rand<- model.statistics(ti.all.cnjtypes.observed.my.rand, ti.all.cnjtypes.predicted.my.rand, ti.all.cnjtypes.p.values.my.rand)

ta.all.cnjtypes.observed.my.rand <- subset(AWCnj, TA, OrderType, drop=TRUE)
# Create an empty vector to store the column names
ta.all.cnjtypes.p.values.my.rand <-cbind(fitted(ta.e.cnjtype.glmer.rand), fitted(ta.kaa.cnjtype.glmer.rand), fitted(ta.other.cnjtype.glmer.rand))
colnames(ta.all.cnjtypes.p.values.my.rand) <- c("eCnj","kaaCnj","Other")
ta.all.cnjtypes.predicted.my.rand <- vector(mode = "character", length = nrow(ta.all.cnjtypes.p.values.my.rand))
# Loop through each row of the ta.all.cnjtypes.p.values.my.rand object
for (i in 1:nrow(ta.all.cnjtypes.p.values.my.rand)) {
  # Find the index of the column with the highest value in this row
  max_col_index <- which.max(ta.all.cnjtypes.p.values.my.rand[i,])
  # Store the column name in the ta.all.cnjtypes.predicted.my.rand vector
  ta.all.cnjtypes.predicted.my.rand[i] <- colnames(ta.all.cnjtypes.p.values.my.rand)[max_col_index]
}
table(ta.all.cnjtypes.observed.my.rand,ta.all.cnjtypes.predicted.my.rand)
ta.all.cnjtypes.ModelStats.my.rand<- model.statistics(ta.all.cnjtypes.observed.my.rand, ta.all.cnjtypes.predicted.my.rand, ta.all.cnjtypes.p.values.my.rand)



