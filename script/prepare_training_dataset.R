#*************read in df_all_1 where all mismatched levels are imputed
df_all_1=read_csv('/data/df_all_1.csv')

#possibly categorical variables
cont_to_cat=c('NVVar1','NVVar2','NVVar3','NVVar4')

#ordered categorical variables
ordered=c('OrdCat')

#***********one-hot-encoding********************************************#
ohe_feats = c('Make','Cat1','Cat2','Cat3','Cat4','Cat5','Cat6','Cat7','Cat8','Cat9','Cat10','Cat11','Cat12','NVCat','OrdCat')
df_all_ohe_feats=dummy.data.frame(df_all_1[,ohe_feats],names=ohe_feats,sep='_')
#**********continuous variables***************************************#
num_feats = c('RowID','Diff_yrs','Var1','Var2','Var3','Var4','Var5','Var6','Var7','Var8')
df_all_cont_feats = df_all_1[,num_feats]

##drop Var1,Var2,Var3
var_delete=c('Var2','Var1','Var3')
for (var in var_delete){
  df_all_cont_feats[var]=NULL
}
#**********cont_to_cat***************************************#
#the model with NVVars as continuous works better than as categorical
df_all_cont_to_cat=df_all_1[,cont_to_cat] 

#***********bind numerical and categorical features together*****************#
df_all_2=cbind(df_all_cont_feats,df_all_ohe_feats,df_all_cont_to_cat) #delete redundant RowID columns
remove(df_all_cont_feats)
remove(df_all_ohe_feats)
remove(df_all_cont_to_cat)

write_csv('/data/df_all_2.csv')