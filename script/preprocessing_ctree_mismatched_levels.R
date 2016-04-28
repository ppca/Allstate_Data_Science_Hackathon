orig_train=read.csv("../data/train.csv", stringsAsFactors=FALSE, head=TRUE)

orig_test=read.csv("../data/test.csv", stringsAsFactors=FALSE, head=TRUE)

str(orig_train)

#mark test data label as -1:easier to separate training/test
orig_test$Response <- -1

merged <- rbind(orig_train, orig_test)

############### Data Pre-processing ###############

## create and drop some variables ##
merged$Diff_yrs=merged$CalendarYear - merged$ModelYear
merged=merged[,-c(2:3,5)] ##drop CalendarYear, ModelYear and Model

## convert some variables to be categorical ##
factor_names <- c(2:15,24,29)
merged[,factor_names] <- lapply(merged[,factor_names] , as.factor)
levels(merged$Make)[1] ="unknown"

## check btw-predictor correlations ##  
numeric_merged=merged[sapply(merged,is.numeric)] ##use only numerical variables to check correlations 

correlations=cor(numeric_merged)
corrplot(correlations,method="number", order = "original")

highCorr=findCorrelation(correlations,cutoff=0.8) ## cutoff can be adjusted 
highCorr_names= colnames(numeric_merged[,highCorr])  

merged=merged[,! names(merged) %in% highCorr_names]

## check variance of numerical variables ##
nearZeroVar_Col=nearZeroVar(numeric_merged)
nearZeroVar_Names=colnames(numeric_merged[,nearZeroVar_Col])
## convert those variables that has near zero variance to be categorical 
merged[,nearZeroVar_Names]= lapply(merged[,nearZeroVar_Names] , factor)

## find all mismatched levels in all categorical variables 
diff_Make_levels = setdiff(
  union( levels(droplevels(merged$Make[merged$Response!= -1],)), levels(droplevels(merged$Make[merged$Response== -1],)) ), 
  intersect( levels(droplevels(merged$Make[merged$Response!= -1],)), levels(droplevels(merged$Make[merged$Response== -1],)) )
)
diff_Make_levels

diff_Cat3_levels=setdiff(
  union( levels(droplevels(merged$Cat3[merged$Response!= -1],)), levels(droplevels(merged$Cat3[merged$Response== -1],)) ), 
  intersect( levels(droplevels(merged$Cat3[merged$Response!= -1],)), levels(droplevels(merged$Cat3[merged$Response== -1],)) )
)
diff_Cat3_levels

diff_Cat6_levels=setdiff(
  union( levels(droplevels(merged$Cat6[merged$Response!= -1],)), levels(droplevels(merged$Cat6[merged$Response== -1],)) ), 
  intersect( levels(droplevels(merged$Cat6[merged$Response!= -1],)), levels(droplevels(merged$Cat6[merged$Response== -1],)) )
)
diff_Cat6_levels

diff_OrdCat_levels=setdiff(
  union( levels(droplevels(merged$OrdCat[merged$Response!= -1],)), levels(droplevels(merged$OrdCat[merged$Response== -1],)) ), 
  intersect( levels(droplevels(merged$OrdCat[merged$Response!= -1],)), levels(droplevels(merged$OrdCat[merged$Response== -1],)) )
)
diff_OrdCat_levels

diff_NVVar1_levels=setdiff(
  union( levels(droplevels(merged$NVVar1[merged$Response!= -1],)), levels(droplevels(merged$NVVar1[merged$Response== -1],)) ), 
  intersect( levels(droplevels(merged$NVVar1[merged$Response!= -1],)), levels(droplevels(merged$NVVar1[merged$Response== -1],)) )
)
diff_NVVar1_levels

diff_NVVar2_levels=setdiff(
  union( levels(droplevels(merged$NVVar2[merged$Response!= -1],)), levels(droplevels(merged$NVVar2[merged$Response== -1],)) ), 
  intersect( levels(droplevels(merged$NVVar2[merged$Response!= -1],)), levels(droplevels(merged$NVVar2[merged$Response== -1],)) )
)
diff_NVVar2_levels

diff_NVVar3_levels=setdiff(
  union( levels(droplevels(merged$NVVar3[merged$Response!= -1],)), levels(droplevels(merged$NVVar3[merged$Response== -1],)) ), 
  intersect( levels(droplevels(merged$NVVar3[merged$Response!= -1],)), levels(droplevels(merged$NVVar3[merged$Response== -1],)) )
)
diff_NVVar3_levels

diff_NVVar4_levels=setdiff(
  union( levels(droplevels(merged$NVVar4[merged$Response!= -1],)), levels(droplevels(merged$NVVar4[merged$Response== -1],)) ), 
  intersect( levels(droplevels(merged$NVVar4[merged$Response!= -1],)), levels(droplevels(merged$NVVar4[merged$Response== -1],)) )
)
diff_NVVar4_levels

### Deal with Make ###
## Group mismatched levels of Make to the matched levels ##
imputed_data=merged[(merged$Make %in%  diff_Make_levels), ] 
imputed_data$Make = droplevels(imputed_data$Make)

new_train=merged[! merged$RowID %in% imputed_data$RowID,]
new_train$Make= droplevels(new_train$Make)

## Impute Vehicle's Make in test set using decision tree model 
ctree_Make = ctree(Make ~ ., data=new_train[,-c(1,20:25)]) ## delete ID and non-vehicle variables 
imputed_data$Pred_Make = predict(ctree_Make, imputed_data[,-c(1,20:25)])

## Recombine the data
#imputed_data$Make=imputed_data$Pred_Make
merged_newMake=merged
merged_newMake$Make[ merged_newMake$RowID %in% imputed_data$RowID]= imputed_data$Pred_Make
merged_newMake$Make= droplevels(merged_newMake$Make)

# this is incorrect: merged_new1=rbind(new_train,imputed_data[,-27])


### Deal with Cat3 ###
## Group mismatched levels of Cat3 to the matched levels ##
imputed_data2=merged_newMake[(merged_newMake$Cat3 %in%  diff_Cat3_levels), ] 
imputed_data2$Cat3 = droplevels(imputed_data2$Cat3)

new_train2=merged_newMake[! merged_newMake$RowID %in% imputed_data2$RowID,]
new_train2$Cat3= droplevels(new_train2$Cat3)

## Impute Cat3 in test set using decision tree model 
ctree_Cat3= ctree(Cat3 ~ ., data=new_train2[,-c(1,20:25)]) ## delete ID and non-vehicle variables 
imputed_data2$Pred_Cat3 = predict(ctree_Cat3, imputed_data2[,-c(1,20:25)])

## Recombine the data

merged_newCat3=merged_newMake
merged_newCat3$Cat3[ merged_newCat3$RowID %in% imputed_data2$RowID]= imputed_data2$Pred_Cat3
merged_newCat3$Cat3= droplevels(merged_newCat3$Cat3)

### Deal with Cat6 ###
## Group mismatched levels of Cat6 to the matched levels ##
imputed_data3=merged_newCat3[(merged_newCat3$Cat6 %in%  diff_Cat6_levels), ] 
imputed_data3$Cat6 = droplevels(imputed_data3$Cat6)

new_train3=merged_newCat3[! merged_newCat3$RowID %in% imputed_data3$RowID,]
new_train3$Cat6= droplevels(new_train3$Cat6)

## Impute Cat6 in test set using decision tree model 
ctree_Cat6= ctree(Cat6 ~ ., data=new_train3[,-c(1,20:25)]) ## delete ID, non-vehicle variables and Response  
imputed_data3$Pred_Cat6 = predict(ctree_Cat6, imputed_data3[,-c(1,20:25)])

## Recombine the data

merged_newCat6=merged_newCat3
merged_newCat6$Cat6[ merged_newCat6$RowID %in% imputed_data3$RowID]= imputed_data3$Pred_Cat6
merged_newCat6$Cat6= droplevels(merged_newCat6$Cat6)


### Deal with OrdCat ###
## Group mismatched levels of OrdCat to the matched levels ##
imputed_data4=merged_newCat6[(merged_newCat6$OrdCat %in%  diff_OrdCat_levels), ] 
imputed_data4$OrdCat = droplevels(imputed_data4$OrdCat)

new_train4=merged_newCat6[! merged_newCat6$RowID %in% imputed_data4$RowID,]
new_train4$OrdCat= droplevels(new_train4$OrdCat)

## Impute OrdCat in test set using decision tree model 
ctree_OrdCat= ctree(OrdCat ~ ., data=new_train4[,-c(1,20:25)]) ## delete ID, non-vehicle variables and Response  
imputed_data4$Pred_OrdCat = predict(ctree_OrdCat, imputed_data4[,-c(1,20:25)])

## Recombine the data

merged_newOrd=merged_newCat6
merged_newOrd$OrdCat[ merged_newOrd$RowID %in% imputed_data4$RowID]= imputed_data4$Pred_OrdCat
merged_newOrd$OrdCat= droplevels(merged_newOrd$OrdCat)

### Deal with NVVar1 ###
## Group mismatched levels of NVVar1 to the matched levels ##
imputed_data5=merged_newOrd[(merged_newOrd$NVVar1 %in%  diff_NVVar1_levels), ] 
imputed_data5$NVVar1 = droplevels(imputed_data5$NVVar1)

new_train5=merged_newOrd[! merged_newOrd$RowID %in% imputed_data5$RowID,]
new_train5$NVVar1= droplevels(new_train5$NVVar1)

## Impute NVVar1 in test set using decision tree model 
ctree_NVVar1= ctree(NVVar1 ~ ., data=new_train5[,c(20:24)]) ## delete ID, vehicle variables and Response  
imputed_data5$Pred_NVVar1 = predict(ctree_NVVar1, imputed_data5[,c(20:24)])

## Recombine the data
merged_newNVVar1=merged_newOrd
merged_newNVVar1$NVVar1[ merged_newNVVar1$RowID %in% imputed_data5$RowID]= imputed_data5$Pred_NVVar1
merged_newNVVar1$NVVar1= droplevels(merged_newNVVar1$NVVar1)

### Deal with NVVar2 ###
## Group mismatched levels of NVVar2 to the matched levels ##
imputed_data6=merged_newNVVar1[(merged_newNVVar1$NVVar2 %in%  diff_NVVar2_levels), ] 
imputed_data6$NVVar2 = droplevels(imputed_data6$NVVar2)

new_train6=merged_newNVVar1[! merged_newNVVar1$RowID %in% imputed_data6$RowID,]
new_train6$NVVar2= droplevels(new_train6$NVVar2)

## Impute NVVar2 in test set using decision tree model 
ctree_NVVar2= ctree(NVVar2 ~ ., data=new_train6[,c(20:24)]) ## delete ID, vehicle variables and Response  
imputed_data6$Pred_NVVar2 = predict(ctree_NVVar2, imputed_data6[,c(20:24)])

## Recombine the data
merged_newNVVar2=merged_newNVVar1
merged_newNVVar2$NVVar2[ merged_newNVVar2$RowID %in% imputed_data6$RowID]= imputed_data6$Pred_NVVar2
merged_newNVVar2$NVVar2= droplevels(merged_newNVVar2$NVVar2)

### Deal with NVVar3 ###
## Group mismatched levels of NVVar3 to the matched levels ##
imputed_data7=merged_newNVVar2[(merged_newNVVar2$NVVar3 %in%  diff_NVVar3_levels), ] 
imputed_data7$NVVar3 = droplevels(imputed_data7$NVVar3)

new_train7=merged_newNVVar2[! merged_newNVVar2$RowID %in% imputed_data7$RowID,]
new_train7$NVVar3= droplevels(new_train7$NVVar3)

## Impute NVVar3 in test set using decision tree model 
ctree_NVVar3= ctree(NVVar3 ~ ., data=new_train7[,c(20:24)]) ## delete ID, vehicle variables and Response  
imputed_data7$Pred_NVVar3 = predict(ctree_NVVar3, imputed_data7[,c(20:24)])

## Recombine the data
merged_newNVVar3=merged_newNVVar2
merged_newNVVar3$NVVar3[ merged_newNVVar2$RowID %in% imputed_data7$RowID]= imputed_data7$Pred_NVVar3
merged_newNVVar3$NVVar3= droplevels(merged_newNVVar3$NVVar3)

### Deal with NVVar4 ###
## Group mismatched levels of NVVar4 to the matched levels ##
imputed_data8=merged_newNVVar3[(merged_newNVVar3$NVVar4 %in%  diff_NVVar4_levels), ] 
imputed_data8$NVVar4 = droplevels(imputed_data8$NVVar4)

new_train8=merged_newNVVar3[! merged_newNVVar3$RowID %in% imputed_data8$RowID,]
new_train8$NVVar4= droplevels(new_train8$NVVar4)

## Impute NVVar4 in test set using decision tree model 
ctree_NVVar4 = ctree(NVVar4 ~ ., data=new_train8[,c(20:24)]) ## delete ID, vehicle variables and Response  
imputed_data8$Pred_NVVar4 = predict(ctree_NVVar4, imputed_data8[,c(20:24)])

## Recombine the data
merged_newNVVar4=merged_newNVVar3
merged_newNVVar4$NVVar4[ merged_newNVVar2$RowID %in% imputed_data8$RowID]= imputed_data8$Pred_NVVar4
merged_newNVVar4$NVVar4= droplevels(merged_newNVVar4$NVVar4)

## final data 
merged_final=merged
merged_final$Pred_Make=merged_newNVVar4$Make
merged_final$Pred_Cat3=merged_newNVVar4$Cat3
merged_final$Pred_Cat6=merged_newNVVar4$Cat6
merged_final$Pred_OrdCat=merged_newNVVar4$OrdCat
merged_final$Pred_NVVar1=merged_newNVVar4$NVVar1
merged_final$Pred_NVVar2=merged_newNVVar4$NVVar2
merged_final$Pred_NVVar3=merged_newNVVar4$NVVar3
merged_final$Pred_NVVar4=merged_newNVVar4$NVVar4

write.csv(merged_final, file = "/data/imputed_data.csv", 
          row.names = FALSE)