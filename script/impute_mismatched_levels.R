
#******read in imputed data******#
df_all = read_csv('data/imputed_data.csv')
dt_all = data.table(df_all)
dt_train = dt_all[Response!=-1,,]
dt_test = dt_all[Response==-1,,]

#********read in original data********#
df_train_original = read_csv('data/train.csv')
df_test_original = read_csv('data/test.csv')
labels = df_train_original[,c('RowID','Response')]
df_test_original$Response = -1
df_all_original = rbind(df_train_original, df_test_original)

#*****prepare for imputing mismatched levels
#get intersection of categories in both train & test
makes=intersect(dt_test$Make,dt_train$Make)
Cat3s=intersect(dt_test$Cat3,dt_train$Cat3)
Cat6s=intersect(dt_test$Cat6,dt_train$Cat6)
OrdCats=intersect(dt_test$OrdCat,dt_train$OrdCat)
NVVar1s=intersect(dt_test$NVVar1,dt_train$NVVar1)
NVVar2s=intersect(dt_test$NVVar2,dt_train$NVVar2)
NVVar3s=intersect(dt_test$NVVar3,dt_train$NVVar3)
NVVar4s=intersect(dt_test$NVVar4,dt_train$NVVar4)

#***********fill in mismatched levels using predicted values**********#
# get new dataset:df_all_1
df_all=data.frame(dt_all)
df_all_1=data.frame(dt_all)[,1:26]
df_all_1[dt_all$Make %nin% makes,'Make']=df_all[dt_all$Make %nin% makes, 'Pred_Make']
df_all_1[dt_all$Cat3 %nin% Cat3s,'Cat3']=df_all[dt_all$Cat3 %nin% Cat3s, 'Pred_Cat3']
df_all_1[dt_all$Cat6 %nin% Cat6s,'Cat6']=df_all[dt_all$Cat6 %nin% Cat6s, 'Pred_Cat6']
df_all_1[dt_all$OrdCat %nin% OrdCats,'OrdCat']=df_all[dt_all$OrdCat %nin% OrdCats, 'Pred_OrdCat']
df_all_1[dt_all$NVVar1 %nin% NVVar1s,'NVVar1']=df_all[dt_all$NVVar1 %nin% NVVar1s, 'Pred_NVVar1']
df_all_1[dt_all$NVVar2 %nin% NVVar2s,'NVVar2']=df_all[dt_all$NVVar2 %nin% NVVar2s, 'Pred_NVVar2']
df_all_1[dt_all$NVVar3 %nin% NVVar3s,'NVVar3']=df_all[dt_all$NVVar3 %nin% NVVar3s, 'Pred_NVVar3']
df_all_1[dt_all$NVVar4 %nin% NVVar4s,'NVVar4']=df_all[dt_all$NVVar4 %nin% NVVar4s, 'Pred_NVVar4']

df_all_1['Var1']=df_all_original$Var1
df_all_1['Var3']=df_all_original$Var3
df_all_1['Var4']=df_all_original$Var4
df_all_1['Var6']=df_all_original$Var6

write_csv(df_all_1,'/data/df_all_1.csv')

