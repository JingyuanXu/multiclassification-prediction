# install.packages(c(caret,ranger,gbm,caretEnsemble))
library(caret)
library(caretEnsemble)
library(xgboost)
train_data = read.csv(file.choose(), header = T)
train_label =
  read.csv(file.choose(), header = T)
test_data = read.csv(file.choose(), header =
                       T)
# str(train_data)
# str(train_label)
train_label$damage_grade[train_label$damage_grade==1]='One'
train_label$damage_grade[train_label$damage_grade==2]='Two'
train_label$damage_grade[train_label$damage_grade==3]='Three'
train_label$damage_grade=as.factor(train_label$damage_grade)
train=cbind(train_data,train_label)
summary(train) 
dim(train)
###for xgboost only
train_boost=train[,1:39]
train_boost_target=train$damage_grade
train_matrix=model.matrix(~.-1, data=train_boost)
test_martrix=model.matrix(~.-1, data=test_data)
#remove dulipate build_id
#  train=train[,-40]
# 5 times 10 folds cv
set.seed(4321)
fitControl=trainControl(
  method='cv',number=10,classProbs = T,#returnResamp ='none',
  allowParallel = T,verboseIter=T,summaryFunction=multiClassSummary
)
 # use gbm  and ranger model to predict
## define the grid search for gbm, ranger and xgboost
 gbm_grid=expand.grid(n.trees=seq(10,200,10),
                      interaction.depth=seq(1,10,1),
                      shrinkage=seq(0.1,1,0.1),
                      n.minobsinnode=seq(1,20,1))
ranger_grid=expand.grid(mtry=c(4:8),
                        splitrule=c('gini','extratrees'),
                        min.node.size=c(1:10))
xgb_grid <- expand.grid(
  nrounds=seq(50,500,50),
  max_depth = seq(2,10,2),
  eta = seq(0.0, 0.5,0.1),
  gamma = seq(0.,1,0.1),
  colsample_bytree = seq(0.2,0.8,0.2),
  subsample = seq(0,1,0.25),
  min_child_weight = seq(0,1,0.25)
  )


###build the model

gbm_model = train(damage_grade~.,
                  data=train, 
                  trControl=fitControl,
                  tuneGrid=gbm_grid,
                  method="gbm",
                  metric='logLoss')
rf_model=train(damage_grade~.,
               data=train,
               trControl=fitControl,
               tuneGrid=ranger_grid,
               method='ranger',
               metric='logLoss')
xgb_model=train(x=train_matrix,
                y=train_boost_target,
                trControl=fitControl,
                #tuneGrid=xgb_grid,
                method='xgbTree',
                metric='logLoss')
####predict with the model
pre_gbm=predict(gbm_model,test_data)
pre_ranger=predict(rf_model,test_data)
pre_xgb=predict(xgb_model,test_martrix)
pre_xgb
####output the result
result_gbm=cbind(test_data[,1],pre_gbm)
result_ranger=cbind(test_data[,1],pre_ranger)
result_xgb=cbind(test_data[,1],pre_xgb) 
 
colnames(result_gbm)=c('buildong_id','damage_grade')
colnames(result_ranger)=c('buildong_id','damage_grade')
colnames(result_xgb)=c('buildong_id','damage_grade')
write.csv(result_gbm,'gbm1.csv',sep="",col.names = T,row.names = F,quote=F)
write.csv(result_ranger,'ranger1.csv',sep="",col.names = T,row.names = F,quote=F)
write.csv(result_xgb,'xgb1.csv',sep="",col.names = T,row.names = F,quote=F)
 
 varImp(xgb_model)

 


# evaluate with F1 score
 
# 
# y = ... # factor of positive / negative cases
# predictions = ... # factor of predictions
# 
# precision = posPredValue(predictions, y, positive="1")
# recall = sensitivity(predictions, y, positive="1")
# 
# F1 = (2 * precision * recall) / (precision + recall)
 
 
 
# test= prcomp(train,scale=T)
# prcomp(USArrests,scale=T) 
