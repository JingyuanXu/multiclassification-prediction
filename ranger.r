library(ranger)
library(gbm)
library(caret)
ranger_train=read.csv(file.choose(),header = T,stringsAsFactors = F)
ranger_test=read.csv(file.choose(),header = T,stringsAsFactors = F)
###after summary  make sure clear to all vars and no NA values
summary(train_data)
###feature engineering
### define the function to process the category factors
trans_data= function (data){
  
  ###  change string into num  called label encoding!!!!
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
  ### feature engineering 
  data$areaLevel[data$area<12]=1
  data$areaLevel[data$area>=12&data$area<40]=2
  data$areaLevel[data$area>=40&data$area<60]=3
  data$areaLevel[data$area>=60&data$area<96]=4
  data$areaLevel[data$area>=96]=5
  
  data$ageLevel[data$age<30]=1
  data$ageLevel[data$age>=30&data$age<100]=2
  data$ageLevel[data$age>=100&data$age<200]=3
  data$ageLevel[data$age>=200&data$age<990]=4
  data$ageLevel[data$age>=990]=5
  # 
  data$heightLevel[data$height<=1]=1
  data$heightLevel[data$height>=2&data$height<=7]=2
  data$heightLevel[data$height>=8&data$height<=12]=3
  data$heightLevel[data$height>=13]=5
  # 
  data$antiLevel[data$has_superstructure_timber==1|data$has_superstructure_bamboo==1]=1
  data$antiLevel[data$has_superstructure_cement_mortar_stone==1|data$has_superstructure_cement_mortar_brick==1]=2
  data$antiLevel[data$has_superstructure_mud_mortar_brick==1|data$has_superstructure_mud_mortar_stone==1]=3
  data$antiLevel[data$has_superstructure_adobe_mud==1|data$has_superstructure_stone_flag==1]=4
  data$antiLevel[data$has_superstructure_timber==0&data$has_superstructure_bamboo==0&data$has_superstructure_cement_mortar_stone==0&data$has_superstructure_cement_mortar_brick==0&data$has_superstructure_mud_mortar_brick==0&data$has_superstructure_mud_mortar_stone==0&data$has_superstructure_adobe_mud==0&data$has_superstructure_stone_flag==0]=5
  
  ###convert geo id into  fators
  # data$geo_level_1_id=as.factor(data$geo_level_1_id)
  # data$geo_level_2_id=as.factor(data$geo_level_2_id)
  # data$geo_level_3_id=as.factor(data$geo_level_3_id)
  
  data$land_surface_condition=as.factor(data$land_surface_condition)
  data$foundation_type=as.factor(data$foundation_type)
  data$roof_type=as.factor(data$roof_type)
  data$ground_floor_type=as.factor(data$ground_floor_type)
  data$other_floor_type=as.factor(data$other_floor_type)
  data$position=as.factor(data$position)
  data$plan_configuration=as.factor(data$plan_configuration)
  data$legal_ownership_status=as.factor(data$legal_ownership_status)
  ### convert into factors
  data$areaLevel=as.factor(data$areaLevel)
  data$ageLevel=as.factor(data$ageLevel)
  data$heightLevel=as.factor(data$heightLevel)
  data$antiLevel=as.factor(data$antiLevel) 
  
  return(data) 
}
### apply the function to both data sets
ranger_train_factor=trans_data(ranger_train)
ranger_test_factor=trans_data(ranger_test)
##combine the damage_grade to the train
ranger_train_factor$damage_grade=as.factor(ranger_train$damage_grade)
 
###with feature analysis with all feature add levels
#  with feature mtry = 20,      num.trees = 4500 for 31.67  but this one with 0.6916 score  which is higher than  31.64(0.6901)
#  with feature and pca index  mtry = 20,      num.trees = 4500 for 31.64
str(ranger_train_factor)
### pca analysis does not work 
# ranger_train_factor_pca=ranger_train_factor[,-c(1,30:39)]
###add pca value for final attempt
 
###build the model
ranger_feature_gini=ranger(damage_grade~.,ranger_train_factor[,-c(1)],mtry=20,importance='impurity',num.trees = 5000,splitrule = 'gini',min.node.size = 1,num.threads = 8,seed=4321)
plot(ranger_feature_gini$variable.importance)
###apply the model to predict
ranger_feature_pred=predict(ranger_feature_gini,ranger_test_factor)
###prepare the output dataset
ranger_feature_result=cbind(ranger_test_factor[,1],ranger_feature_pred$predictions)
colnames(ranger_feature_result)=c("building_id",'damage_grade')
write.csv(ranger_feature_result,"ranger_31.7.csv",row.names=F,quote=F,sep='')
