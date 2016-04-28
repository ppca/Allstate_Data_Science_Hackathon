#*********************read in relevant data*******************#
dt_all = read_csv('/data/imputed_data.csv')
df_all_2 = read_csv('data/df_all_2.csv')

#split train/test
df_train = df_all_2[dt_all$Response!=-1,]
df_test = df_all_2[dt_all$Response==-1,]
X_train = as.matrix(df_train)[,c(-1)]
X_test = as.matrix(df_test)[,c(-1)]

df_cv_train = df_train[df_train_original$CalendarYear!=2007,]
df_cv_test = df_train[df_train_original$CalendarYear==2007,]

X_cv_train = as.matrix(df_cv_train[,c(-1)])
X_cv_test = as.matrix(df_cv_test[,c(-1)])

##change into xgboost matrix
dX_train <- xgb.DMatrix(X_train, label = labels$Response)
dX_test <- xgb.DMatrix(X_test)

ll=labels[df_train_original$CalendarYear!=2007,]$Response
ll_cv_test = labels[df_train_original$CalendarYear==2007,]$Response
dX_cv_train <- xgb.DMatrix(X_cv_train,label=ll)
dX_cv_test <- xgb.DMatrix(X_cv_test)

#***********set up xgboost parameters********************#

#this set of parameters is optimal by cross-validation
param <- list("objective" = "binary:logistic",
              "eval_metric" = 'logloss', 
              "eta" = 0.02,
              'n_estimators'=4000,
              "max_depth" = 3,
              "subsample" = 0.5,
              "colsample_bytree" = 0.75,
              "min_child_weight" = 2,
              "nthread" = 4
)

#****************Run Cross Valication on whole training data***************#
set.seed(1)
cv.nround = 2000
bst.cv = xgb.cv(param = param,
                data = dX_train, 
                nfold = 5,
                nrounds=cv.nround,
                stratified = TRUE,
                maximize = TRUE
                
)
#min:0.578775

##***************use holdout set for validation*********************
set.seed(1)
xgb_val = xgboost(data = dX_cv_train, 
                  param=param,
                  nround = which.min(bst.cv$test.logloss.mean),  
                  prediction = TRUE,
                  maximize = TRUE,
                  nthread = 4
)

#look at validation set results
pred_val = predict(xgb_val,dX_cv_test) # a reference for final result
logloss_val = LogLossBinary(ll_cv_test, pred_val, eps = 1e-15)

#check feature importance 
X_val_imp <- xgb.importance(colnames(df_all_2)[c(-1)], model=xgb_val)
xgb.plot.importance(X_val_imp)


#***************train boosting mode on whole training set****************#
set.seed(1)
xgb_1 = xgboost(data = dX_train, 
                param=param,
                nround = which.min(bst.cv$test.logloss.mean), 
                eval_metric = "logloss",
                prediction = TRUE,
                maximize = TRUE,
                stratified = TRUE,
                nthread = 4
)

#check feature importance: this is pretty much the same as using only part of training data
X_all_imp <- xgb.importance(colnames(df_all_2)[-1], model=xgb_1)
xgb.plot.importance(X_all_imp)


#************write result to a csv file for submission*****************#
result=data.frame('RowID'=df_test$RowID,'ProbabilityOfResponse' = predict(xgb_1,dX_test))
write_csv(result,'submissions/submission005.csv')
