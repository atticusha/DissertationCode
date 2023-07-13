#MODEL STATISTICS

### Ind v. eCnj
ii.ive.observed.my.fixed <- ifelse(subset(AWive, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.ive.predicted.my.fixed <- ifelse(as.vector(fitted(ii.ive.glm.fixed))>0.5, "Ind", "eCnj")
table(ii.ive.observed.my.fixed,ii.ive.predicted.my.fixed)
ii.ive.p.values.my.fixed <- cbind(as.vector(fitted(ii.ive.glm.fixed)), 1-as.vector(fitted(ii.ive.glm.fixed)))
colnames(ii.ive.p.values.my.fixed) <- c("Ind","eCnj")
ii.ive.ModelStats.my.fixed<- model.statistics(ii.ive.observed.my.fixed, ii.ive.predicted.my.fixed, ii.ive.p.values.my.fixed)

ai.ive.observed.my.fixed <- ifelse(subset(AWive, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.ive.predicted.my.fixed <- ifelse(as.vector(fitted(ai.ive.glm.fixed))>0.5, "Ind", "eCnj")
table(ai.ive.observed.my.fixed,ai.ive.predicted.my.fixed)
ai.ive.p.values.my.fixed <- cbind(as.vector(fitted(ai.ive.glm.fixed)), 1-as.vector(fitted(ai.ive.glm.fixed)))
colnames(ai.ive.p.values.my.fixed) <- c("Ind","eCnj")
ai.ive.ModelStats.my.fixed<- model.statistics(ai.ive.observed.my.fixed, ai.ive.predicted.my.fixed, ai.ive.p.values.my.fixed)

ti.ive.observed.my.fixed <- ifelse(subset(AWive, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.ive.predicted.my.fixed <- ifelse(as.vector(fitted(ti.ive.glm.fixed))>0.5, "Ind", "eCnj")
table(ti.ive.observed.my.fixed,ti.ive.predicted.my.fixed)
ti.ive.p.values.my.fixed <- cbind(as.vector(fitted(ti.ive.glm.fixed)), 1-as.vector(fitted(ti.ive.glm.fixed)))
colnames(ti.ive.p.values.my.fixed) <- c("Ind","eCnj")
ti.ive.ModelStats.my.fixed<- model.statistics(ti.ive.observed.my.fixed, ti.ive.predicted.my.fixed, ti.ive.p.values.my.fixed)

ta.ive.observed.my.fixed <- ifelse(subset(AWive, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.ive.predicted.my.fixed <- ifelse(as.vector(fitted(ta.ive.glm.fixed))>0.5, "Ind", "eCnj")
table(ta.ive.observed.my.fixed,ta.ive.predicted.my.fixed)
ta.ive.p.values.my.fixed <- cbind(as.vector(fitted(ta.ive.glm.fixed)), 1-as.vector(fitted(ta.ive.glm.fixed)))
colnames(ta.ive.p.values.my.fixed) <- c("Ind","eCnj")
ta.ive.ModelStats.my.fixed<- model.statistics(ta.ive.observed.my.fixed, ta.ive.predicted.my.fixed, ta.ive.p.values.my.fixed)




#================================================
### eCnj v. kaaCnj v. OtherCnj
#================================================

ii.all.cnjtypes.observed.my.fixed <- subset(AWCnj, II, OrderType, drop=TRUE)
# Create an empty vector to store the column names
ii.all.cnjtypes.p.values.my.fixed <-cbind(fitted(ii.e.cnjtype.glm.fixed), fitted(ii.kaa.cnjtype.glm.fixed), fitted(ii.other.cnjtype.glm.fixed))
colnames(ii.all.cnjtypes.p.values.my.fixed) <- c("eCnj","kaaCnj","Other")
ii.all.cnjtypes.predicted.my.fixed <- vector(mode = "character", length = nrow(ii.all.cnjtypes.p.values.my.fixed))
# Loop through each row of the ii.all.cnjtypes.p.values.my.fixed object
for (i in 1:nrow(ii.all.cnjtypes.p.values.my.fixed)) {
  # Find the index of the column with the highest value in this row
  max_col_index <- which.max(ii.all.cnjtypes.p.values.my.fixed[i,])
  # Store the column name in the ii.all.cnjtypes.predicted.my.fixed vector
  ii.all.cnjtypes.predicted.my.fixed[i] <- colnames(ii.all.cnjtypes.p.values.my.fixed)[max_col_index]
}
table(ii.all.cnjtypes.observed.my.fixed,ii.all.cnjtypes.predicted.my.fixed)
ii.all.cnjtypes.ModelStats.my.fixed<- model.statistics(ii.all.cnjtypes.observed.my.fixed, ii.all.cnjtypes.predicted.my.fixed, ii.all.cnjtypes.p.values.my.fixed)



ai.all.cnjtypes.observed.my.fixed <- subset(AWCnj, AI, OrderType, drop=TRUE)
# Create an empty vector to store the column names
ai.all.cnjtypes.p.values.my.fixed <-cbind(fitted(ai.e.cnjtype.glm.fixed), fitted(ai.kaa.cnjtype.glm.fixed), fitted(ai.other.cnjtype.glm.fixed))
colnames(ai.all.cnjtypes.p.values.my.fixed) <- c("eCnj","kaaCnj","Other")
ai.all.cnjtypes.predicted.my.fixed <- vector(mode = "character", length = nrow(ai.all.cnjtypes.p.values.my.fixed))
# Loop through each row of the ai.all.cnjtypes.p.values.my.fixed object
for (i in 1:nrow(ai.all.cnjtypes.p.values.my.fixed)) {
  # Find the index of the column with the highest value in this row
  max_col_index <- which.max(ai.all.cnjtypes.p.values.my.fixed[i,])
  # Store the column name in the ai.all.cnjtypes.predicted.my.fixed vector
  ai.all.cnjtypes.predicted.my.fixed[i] <- colnames(ai.all.cnjtypes.p.values.my.fixed)[max_col_index]
}
table(ai.all.cnjtypes.observed.my.fixed,ai.all.cnjtypes.predicted.my.fixed)
ai.all.cnjtypes.ModelStats.my.fixed<- model.statistics(ai.all.cnjtypes.observed.my.fixed, ai.all.cnjtypes.predicted.my.fixed, ai.all.cnjtypes.p.values.my.fixed)



ti.all.cnjtypes.observed.my.fixed <- subset(AWCnj, TI, OrderType, drop=TRUE)
# Create an empty vector to store the column names
ti.all.cnjtypes.p.values.my.fixed <-cbind(fitted(ti.e.cnjtype.glm.fixed), fitted(ti.kaa.cnjtype.glm.fixed), fitted(ti.other.cnjtype.glm.fixed))
colnames(ti.all.cnjtypes.p.values.my.fixed) <- c("eCnj","kaaCnj","Other")
ti.all.cnjtypes.predicted.my.fixed <- vector(mode = "character", length = nrow(ti.all.cnjtypes.p.values.my.fixed))
# Loop through each row of the ti.all.cnjtypes.p.values.my.fixed object
for (i in 1:nrow(ti.all.cnjtypes.p.values.my.fixed)) {
  # Find the index of the column with the highest value in this row
  max_col_index <- which.max(ti.all.cnjtypes.p.values.my.fixed[i,])
  # Store the column name in the ti.all.cnjtypes.predicted.my.fixed vector
  ti.all.cnjtypes.predicted.my.fixed[i] <- colnames(ti.all.cnjtypes.p.values.my.fixed)[max_col_index]
}
table(ti.all.cnjtypes.observed.my.fixed,ti.all.cnjtypes.predicted.my.fixed)
ti.all.cnjtypes.ModelStats.my.fixed<- model.statistics(ti.all.cnjtypes.observed.my.fixed, ti.all.cnjtypes.predicted.my.fixed, ti.all.cnjtypes.p.values.my.fixed)



ta.all.cnjtypes.observed.my.fixed <- subset(AWCnj, TA, OrderType, drop=TRUE)
# Create an empty vector to store the column names
ta.all.cnjtypes.p.values.my.fixed <-cbind(fitted(ta.e.cnjtype.glm.fixed), fitted(ta.kaa.cnjtype.glm.fixed), fitted(ta.other.cnjtype.glm.fixed))
colnames(ta.all.cnjtypes.p.values.my.fixed) <- c("eCnj","kaaCnj","Other")
ta.all.cnjtypes.predicted.my.fixed <- vector(mode = "character", length = nrow(ta.all.cnjtypes.p.values.my.fixed))
# Loop through each row of the ta.all.cnjtypes.p.values.my.fixed object
for (i in 1:nrow(ta.all.cnjtypes.p.values.my.fixed)) {
  # Find the index of the column with the highest value in this row
  max_col_index <- which.max(ta.all.cnjtypes.p.values.my.fixed[i,])
  # Store the column name in the ta.all.cnjtypes.predicted.my.fixed vector
  ta.all.cnjtypes.predicted.my.fixed[i] <- colnames(ta.all.cnjtypes.p.values.my.fixed)[max_col_index]
}
table(ta.all.cnjtypes.observed.my.fixed,ta.all.cnjtypes.predicted.my.fixed)
ta.all.cnjtypes.ModelStats.my.fixed<- model.statistics(ta.all.cnjtypes.observed.my.fixed, ta.all.cnjtypes.predicted.my.fixed, ta.all.cnjtypes.p.values.my.fixed)


#================================================
### Ind v. Cnj
#================================================

ii.indcnj.observed.my.fixed <- ifelse(subset(AWnimp, II, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ii.indcnj.predicted.my.fixed <- ifelse(as.vector(fitted(ii.indcnj.glm.fixed))>0.5, "Ind", "eCnj")
table(ii.indcnj.observed.my.fixed,ii.indcnj.predicted.my.fixed)
ii.indcnj.p.values.my.fixed <- cbind(as.vector(fitted(ii.indcnj.glm.fixed)), 1-as.vector(fitted(ii.indcnj.glm.fixed)))
colnames(ii.indcnj.p.values.my.fixed) <- c("Ind","eCnj")
ii.indcnj.ModelStats.my.fixed<- model.statistics(ii.indcnj.observed.my.fixed, ii.indcnj.predicted.my.fixed, ii.indcnj.p.values.my.fixed)

ai.indcnj.observed.my.fixed <- ifelse(subset(AWnimp, AI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ai.indcnj.predicted.my.fixed <- ifelse(as.vector(fitted(ai.indcnj.glm.fixed))>0.5, "Ind", "eCnj")
table(ai.indcnj.observed.my.fixed,ai.indcnj.predicted.my.fixed)
ai.indcnj.p.values.my.fixed <- cbind(as.vector(fitted(ai.indcnj.glm.fixed)), 1-as.vector(fitted(ai.indcnj.glm.fixed)))
colnames(ai.indcnj.p.values.my.fixed) <- c("Ind","eCnj")
ai.indcnj.ModelStats.my.fixed<- model.statistics(ai.indcnj.observed.my.fixed, ai.indcnj.predicted.my.fixed, ai.indcnj.p.values.my.fixed)

ti.indcnj.observed.my.fixed <- ifelse(subset(AWnimp, TI, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ti.indcnj.predicted.my.fixed <- ifelse(as.vector(fitted(ti.indcnj.glm.fixed))>0.5, "Ind", "eCnj")
table(ti.indcnj.observed.my.fixed,ti.indcnj.predicted.my.fixed)
ti.indcnj.p.values.my.fixed <- cbind(as.vector(fitted(ti.indcnj.glm.fixed)), 1-as.vector(fitted(ti.indcnj.glm.fixed)))
colnames(ti.indcnj.p.values.my.fixed) <- c("Ind","eCnj")
ti.indcnj.ModelStats.my.fixed<- model.statistics(ti.indcnj.observed.my.fixed, ti.indcnj.predicted.my.fixed, ti.indcnj.p.values.my.fixed)

ta.indcnj.observed.my.fixed <- ifelse(subset(AWnimp, TA, OrderType, drop=TRUE) == "Ind", "Ind", "eCnj")
ta.indcnj.predicted.my.fixed <- ifelse(as.vector(fitted(ta.indcnj.glm.fixed))>0.5, "Ind", "eCnj")
table(ta.indcnj.observed.my.fixed,ta.indcnj.predicted.my.fixed)
ta.indcnj.p.values.my.fixed <- cbind(as.vector(fitted(ta.indcnj.glm.fixed)), 1-as.vector(fitted(ta.indcnj.glm.fixed)))
colnames(ta.indcnj.p.values.my.fixed) <- c("Ind","eCnj")
ta.indcnj.ModelStats.my.fixed<- model.statistics(ta.indcnj.observed.my.fixed, ta.indcnj.predicted.my.fixed, ta.indcnj.p.values.my.fixed)


