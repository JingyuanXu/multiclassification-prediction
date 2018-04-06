# install.packages(c(caret,ranger,gbm,caretEnsemble))
library(caret)
library(caretEnsemble)
library(xgboost)
library(parallel)
library(doMC)
library(gbm)
registerDoMC(cores = 8)
# prediction type: multiclass classification
# methods  used:gbm  boosting  ranger  bagging  and xgboost boosting
# metric used: F1 score
# cross validation used: repeatedCV  5 times 10 folds
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
  method='repeatedcv',
  number=5,
  classProbs = T,
  #returnResamp ='none',
  repeats = 3,
  allowParallel = T,
  verboseIter=T,
  summaryFunction=multiClassSummary
)
 # use gbm  and ranger model to predict
## define the grid search for gbm, ranger and xgboost
gbm_grid=expand.grid( n.trees=seq(50,400,50),
                      interaction.depth=seq(1,7,2),
                      shrinkage=seq(.05, .25,.05) ,
                      n.minobsinnode=seq(5,20,5))
ranger_grid=expand.grid(mtry=c(4:8),
                        splitrule=c('gini','extratrees'),
                        min.node.size=seq(1,10,2))
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
                  data=train[,-c(22:39)], 
                  trControl=fitControl,
                  #tuneGrid=gbm_grid,
                  method="gbm",
                  metric='Mean_F1'
                  )
  
gbmImp=varImp(gbm_model,scale=F)
 plot(gbmImp)
####AFTER CV   ADD ONE FEATURE fro gbm method according to varImp
 train$PCA=train$geo_level_1_id*961.15+train$geo_level_2_id*145.15+train$geo_level_3_id*76.64+train$age*116.94+train$has_superstructure_mud_mortar_stone*170.46+train$has_superstructure_cement_mortar_brick*98.03
str(train)
 rf_model=train(damage_grade~.,
               data=train,
               trControl=fitControl,
              # tuneGrid=ranger_grid,
               method='ranger',
               metric='Mean_F1'
               )
# 5 fold The final values used for the model were mtry = 60, splitrule = gini and min.node.size = 1.  best score is 0.5930097
# 5ytimes 10 fold The final values used for the model were mtry = 31, splitrule = gini and min.node.size = 1. best score is 0.5969575 

xgb_model=train(x=train_matrix,
                y=train_boost_target,
                trControl=fitControl,
                #tuneGrid=xgb_grid,
                method='xgbTree',
                metric='Mean_F1',
                verbose=T)
####fix the parameters and get the final model
finalControl <- trainControl(method = "none", classProbs = TRUE)
gbm_final=train(
  damage_grade~.,
  data=train[,-c(22:39)], 
  trControl=finalControl,
  tuneGrid=data.frame(
    .n.trees = 150, 
    .interaction.depth = 3, 
    .shrinkage = 0.1 , 
    .n.minobsinnode = 10.
  ),
  method="gbm",
  metric='Mean_F1',
  verbose=T
)
set.seed(4321)
 
rf_final=train(damage_grade~.,
               data=train,
               trControl=finalControl,
               tuneGrid=expand.grid(.mtry=31,
                                    .splitrule = 'gini' ,  
                                    .min.node.size = 1.),
               method='ranger',
               metric='Mean_F1',
               verbose=T)
xgb_final=train(x=train_matrix,
                y=train_boost_target,
                trControl=finalControl,
                #tuneGrid=data.frame(),
                method='xgbTree',
                metric='Mean_F1',
                verbose=T)

####predict with the model
test_data$PCA=test_data$geo_level_1_id*961.15+test_data$geo_level_2_id*145.15+test_data$geo_level_3_id*76.64+test_data$age*116.94+test_data$has_superstructure_mud_mortar_stone*170.46+test_data$has_superstructure_cement_mortar_brick*98.03
dim(test_data)
dim(train)
pre_gbm=predict(gbm_final,test_data)
pre_ranger=predict(rf_final,test_data)
pre_xgb=predict(xgb_final,test_martrix)

####output the result
result_gbm=cbind(test_data[,1],pre_gbm)
result_ranger=cbind(test_data[,1],pre_ranger)
result_xgb=cbind(test_data[,1],pre_xgb) 
 
### format the result 
colnames(result_gbm)=c('building_id','damage_grade')
colnames(result_ranger)=c('building_id','damage_grade')
colnames(result_xgb)=c('building_id','damage_grade')
result_ranger=data.frame(result_ranger)
table(result_ranger$damage_grade)
write.csv(result_gbm,'gbm3.csv',sep="",col.names = T,row.names = F,quote=F)
write.csv(result_ranger,'ranger2.csv',sep="",col.names = T,row.names = F,quote=F)
write.csv(result_xgb,'xgb1.csv',sep="",col.names = T,row.names = F,quote=F)
 