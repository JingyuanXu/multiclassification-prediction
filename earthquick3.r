library(caret)
library(ranger)
library(xgboost)
library(gbm)
train_data=read.csv(file.choose(),header = T,stringsAsFactors = F)
# train_label=read.csv(file.choose(),header=T,stringsAsFactors = F )
test_data=read.csv(file.choose(),header=T,stringsAsFactors = F)
train_num_data$damage_grade=as.factor(train_num_data$damage_grade)
summary(train_data)
table(test_data$roof_type)
table(train_data$land_surface_condition)
table(train_data$legal_ownership_status)
table(test_data$plan_configuration)
table(train_data$position)
table(train_data$other_floor_type)
table(train_data$ground_floor_type)
table(train_data$foundation_type)
###process the category factor
trans_data= function (data){
  data$land_surface_condition[data$land_surface_condition=='2f15']=1
  data$land_surface_condition[data$land_surface_condition=='808e']=2
  data$land_surface_condition[data$land_surface_condition=='d502']=3
  
  data$legal_ownership_status[data$legal_ownership_status=='ab03']=1
  data$legal_ownership_status[data$legal_ownership_status=='bb5f']=2
  data$legal_ownership_status[data$legal_ownership_status=='c8e1']=3
  data$legal_ownership_status[data$legal_ownership_status=='cae1']=4
  
  data$plan_configuration[data$plan_configuration=='1442']=1
  data$plan_configuration[data$plan_configuration=='3fee']=2
  data$plan_configuration[data$plan_configuration=='448']=3
  data$plan_configuration[data$plan_configuration=='0448']=3
  data$plan_configuration[data$plan_configuration=='6.00E+81']=4
  data$plan_configuration[data$plan_configuration=='6e81']=4
  data$plan_configuration[data$plan_configuration=='84cf']=5
  data$plan_configuration[data$plan_configuration=='8e3f']=6
  data$plan_configuration[data$plan_configuration=='a779']=7
  data$plan_configuration[data$plan_configuration=='cb88']=8
  data$plan_configuration[data$plan_configuration=='d2d9']=9
  
  data$position[data$position=='1787']=1
  data$position[data$position=='3356']=2
  data$position[data$position=='bcab']=3
  data$position[data$position=='bfba']=4
  
  data$other_floor_type[data$other_floor_type=='441a']=2
  data$other_floor_type[data$other_floor_type=='67f9']=1
  data$other_floor_type[data$other_floor_type=='9eb0']=3
  data$other_floor_type[data$other_floor_type=='f962']=4
  
  data$ground_floor_type[data$ground_floor_type=='467b']=1
  data$ground_floor_type[data$ground_floor_type=='b1b4']=3
  data$ground_floor_type[data$ground_floor_type=='b440']=4
  data$ground_floor_type[data$ground_floor_type=='bb5f']=2
  data$ground_floor_type[data$ground_floor_type=='e26c']=5
  
  data$foundation_type[data$foundation_type=='337f']=3
  data$foundation_type[data$foundation_type=='467b']=1
  data$foundation_type[data$foundation_type=='6c3e']=4
  data$foundation_type[data$foundation_type=='858b']=5
  data$foundation_type[data$foundation_type=='bb5f']=2
  
  data$roof_type[data$roof_type=='67f9']=1
  data$roof_type[data$roof_type=='7.00E+76']=2
  data$roof_type[data$roof_type=='7e76']=2
  data$roof_type[data$roof_type=='e0e2']=3
  data$land_surface_condition=as.factor(data$land_surface_condition)
  data$foundation_type=as.factor(data$foundation_type)
  data$roof_type=as.factor(data$roof_type)
  data$ground_floor_type=as.factor(data$ground_floor_type)
  data$other_floor_type=as.factor(data$other_floor_type)
  data$position=as.factor(data$position)
  data$plan_configuration=as.factor(data$plan_configuration)
  data$legal_ownership_status=as.factor(data$legal_ownership_status)
 return(data) 
}
 train_num_data=trans_data(train_data)
 test_num_data=trans_data(test_data)

 ##config the model
 set.seed(4321)
 fitControl=trainControl(
   method='CV',
   number=5,
   classProbs = F,
   #returnResamp ='none',
   #repeats = 3,
   allowParallel = T,
   verboseIter=T,
   summaryFunction=multiClassSummary
 )
 gbm_grid=expand.grid( n.trees=seq(100,300,50),
                       interaction.depth=seq(1,7,2),
                       shrinkage=seq(.05, .25,.05) ,
                       n.minobsinnode=seq(5,20,5))
 gbm_model = train(damage_grade~.,
                   data=train_num_data, 
                   trControl=fitControl,
                   tuneGrid=gbm_grid,
                   #preProcess='pca',
                   method="gbm",
                   metric='Mean_F1'
 )
 finalControl=trainControl(method='none',classProbs = F)
 gbm_num_final=train(damage_grade~.,
       data=train_num_data, 
       trControl=finalControl,
       tuneGrid=expand.grid(
         .n.trees=800,
         .interaction.depth = 7,
         .shrinkage = 0.1,
         .n.minobsinnode = 5
       ),
       method="gbm",
       metric='Mean_F1'
 )
gbm_num_predict=predict(gbm_num_final,test_num_data)
gbm_num_result=cbind(test_num_data[,1],gbm_num_predict)
 
colnames(gbm_num_result)=c('building_id','damage_grade') 
write.csv(gbm_num_result,"gbm_num_result_3.csv",row.names=F,quote=F,sep="")
