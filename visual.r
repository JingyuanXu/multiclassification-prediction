library(caret)
library(ggplot2)
head(train)
ggplot(train,aes(x=train$area,y=train$damage_grade))+
  geom_density(aes(fill=train$damage_grade),alpha=0.4)

df <- data.frame(ab.class = c(rep("A", 200), rep("B", 200)),
                 val = c(rnorm(200, 0, 1), rnorm(200, 1, 1)))
ggplot(df, aes(x = val, group = ab.class)) +
  geom_density(aes(fill = ab.class), alpha = 0.4)

ggplot(train, aes(x = area, group = has_secondary_use)) +
  geom_density(aes(fill = has_secondary_use), alpha = 0.4)
ggplot(train, aes(x = train$count_floors_pre_eq , group = train$damage_grade
                  )) +
  geom_density(aes(fill = train$damage_grade), alpha = 0.4)+xlim(0,20)

ggplot(train, aes(x =log(count_families*age*height*count_floors_pre_eq/area+1), group = train$damage_grade)) +
  geom_density(aes(fill = train$damage_grade), alpha = 0.4)+xlim(0, 8)
 
ggplot(train,mapping=aes(x=has_secondary_use,y=..count..,fill=damage_grade))+
  geom_bar(stat='count',position = position_dodge())
train$has_secondary_use


attach(train)
val=count_families*age*height*count_floors_pre_eq/area
test=data.frame(val,damage_grade)
detach(train)
boxplot(val~damage_grade,test)
sample=train[490,]

######### feature engineering 
train_data$area
hist(train_data$area,data=train_data,xlim = c(0,150))
table(train_data$area)
### area   5 levels
###   <12  12-40  40-60  61-96  96<
train_data$areaLevel[train_data$area<12]=1
train_data$areaLevel[train_data$area>=12&train_data$area<40]=2
train_data$areaLevel[train_data$area>=40&train_data$area<60]=3
train_data$areaLevel[train_data$area>=60&train_data$area<96]=4
train_data$areaLevel[train_data$area>=96]=5
table(train_data$areaLevel)


hist(train_data$age,data=train_data,xlim = c(0,150))
table(train_data$ageLevel)
### age   4 levels
###   <30  35-100  100-200   990<


train_data$ageLevel[train_data$age<30]=1
train_data$ageLevel[train_data$age>=30&train_data$age<100]=2
train_data$ageLevel[train_data$age>=100&train_data$age<200]=3
train_data$ageLevel[train_data$age>=200&train_data$age<990]=4
train_data$ageLevel[train_data$age>=990]=5

hist(train_data$height,data=train_data,xlim = c(0,30))
table(train_data$height)
### height   4 levels
###  1    2-7 8-12      13<

train_data$heightLevel[train_data$height<=1]=1
train_data$heightLevel[train_data$height>=2&train_data$height<=7]=2
train_data$heightLevel[train_data$height>=8&train_data$height<=12]=3
train_data$heightLevel[train_data$height>=13]=5
table(train_data$heightLevel)

### anti quke
# has_superstructure_timber 1
# has_superstructure_bamboo  1  
# has_superstructure_cement_mortar_stone 2
# has_superstructure_cement_mortar_brick 2
# has_superstructure_mud_mortar_brick 3
# has_superstructure_mud_mortar_stone 3
# has_superstructure_adobe_mud 4
# has_superstructure_stone_flag 4
train_data$antiLevel[train_data$has_superstructure_timber==1|train_data$has_superstructure_bamboo==1]=1
train_data$antiLevel[train_data$has_superstructure_cement_mortar_stone==1|train_data$has_superstructure_cement_mortar_brick==1]=2
train_data$antiLevel[train_data$has_superstructure_mud_mortar_brick==1|train_data$has_superstructure_mud_mortar_stone==1]=3
train_data$antiLevel[train_data$has_superstructure_adobe_mud==1|train_data$has_superstructure_stone_flag==1]=4
 
table(train_data$antiLevel)
 
typeof(train_data$geo_level_1_id)













