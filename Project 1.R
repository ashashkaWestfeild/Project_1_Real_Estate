## see the FEATURES & Determine which is the TARGET : Price
## see rest of the features => Determine what to change
## See MISSING values, Do all the necessary CHANGES and IMPUTATION
## Finally you can TIDYMODELS
## Build the MODEL => 80/20
## VALIDATE on 20
## Final score on test and upload on website
## (NOTE: This problem can be easily solved using Random Forest/GBM)


library(tidyverse)
library(tidymodels)
library(car)

## load the data
path <- r'{C:\Users\91835\Dropbox\PC\Courses\EDVANCER_EDUVENTURES\R\PROJECT\Project1- Real Estate\}'

housing_train <- read.csv(paste0(path,'housing_train.csv'))
housing_test <- read.csv(paste0(path,'housing_test.csv'))

setdiff(names(housing_train), names(housing_test))
# OUTCOME => "Price"

head(housing_train$Price)
# It is continuous

##------------DATA-DICTIONARY------------

# Suburb : categorical => Which suburb the property is located in 
# Address : categorical => short address
# Rooms : numeric => Number of Rooms
# Type : categorical => type of the property
# Price : numeric => This is the target variable, price of the property 
# Method : categorical => method for selling 
# SellerG : categorical => Name of the seller 
# Distance : numeric => distance from the city center
# Postcode : categorical => postcode of the property
# Bedroom2 : Numeric => numbers of secondary bedrooms (this is different from rooms)
# Bathroom : numeric => number of bathrooms
# Car : numeric => number of parking spaces
# Landsize : numeric => landsize
# BuildingArea : numeric => buildup area
# YearBuilt : numeric => year of building 
# CouncilArea : numeric => council area to which the property belongs
##------------------------
df <- housing_train

head(df)
View(df)
glimpse(df)

# run unique() on all columns

sum(is.na(df$Suburb)) # 0
unique(df$Suburb) # 142 unique values
sum(is.na(df$Address)) # 0
sum(is.na(df$Rooms)) # 0
unique_rooms <- sort(unique(df$Rooms))
table(df$Rooms)
sum(is.na(df$Type)) # 0
unique_type <- unique(df$Type) # 3 unique type
unique_method <- unique(df$Method) # 5 unique methods
unique(df$SellerG) # 182 unique sellers
table(df$Distance)
sum(is.na(df$Distance)) # 0
sum(is.na(df$Postcode)) # 0
sum(is.na(df$Bedroom2)) # 1559
unique(df$Bedroom2)
sum(is.na(df$Bathroom)) # 1559
sum(is.na(df$Car)) # 1559
sum(is.na(df$Landsize)) # 1564
sum(is.na(df$BuildingArea)) # 4209
sum(is.na(df$YearBuilt)) # 3717
unique_CouncilArea <- unique(df$CouncilArea)
sum(grepl("\\W", df$CouncilArea)) # 1607
postcode <- sort(unique(df$Postcode))

##-----------------------
# Get unique postcode for all CouncilArea(do it for all Council area)
sort(unique(df[df$CouncilArea == "Hume", c('Postcode')]))

# function to restore CouncilArea according to Postcode
replace_CouncilArea <- function(x,y){
  df[df['Postcode'] == x, c('CouncilArea')] <<-  y
  return(df)
}

mapply(replace_CouncilArea,c(3043,3044,3046,3055,3056,3057,3058,3060,3068),"Moreland")
mapply(replace_CouncilArea, c(3058,3070,3071,3072,3073,3078,3083),"Darebin")
mapply(replace_CouncilArea, c(3015,3016,3018,3025),"Hobsons Bay")
mapply(replace_CouncilArea, c(3101,3102,3103,3104,3122,3123,3124,3126,3146,3147),"Boroondara")
mapply(replace_CouncilArea, c(3145,3161,3162,3163,3165,3184,3185,3187,3204),"Glen Eira")
mapply(replace_CouncilArea, c(3125,3127,3128),"Whitehorse")
mapply(replace_CouncilArea, c(3141,3142,3143,3144,3145,3146,3181),"Stonnington")
mapply(replace_CouncilArea, c(3011,3012,3013,3019,3032),"Maribyrnong")
mapply(replace_CouncilArea, c(3072,3079,3081,3084,3085,3087),"Banyule")
mapply(replace_CouncilArea, c(3186,3187,3188),"Bayside")
mapply(replace_CouncilArea, c(3054,3065,3066,3067,3068,3078,3121),"Yarra")
mapply(replace_CouncilArea, c(3031,3032,3033,3034,3039,3040,3041,3042),"Moonee Valley")
mapply(replace_CouncilArea, c(3020,3021,3033,3042),"Brimbank")
mapply(replace_CouncilArea, c(3105,3107,3108),"Manningham")
mapply(replace_CouncilArea, c(3181,3182,3183,3184,3185,3205,3206,3207),"Port Phillip")
mapply(replace_CouncilArea, c(3000,3002,3003,3006,3008,3031,3051,3052,3053,3141,3207),"Melbourne")
mapply(replace_CouncilArea, c(3125,3147,3148,3166,3167),"Monash")
mapply(replace_CouncilArea, c(3167,3188,3189),"Kingston")
mapply(replace_CouncilArea, c(3047,3061,3067),"Hume")

sum(is.na(df$CouncilArea))

# function to get road type
road_type <- function(address){
  address <- unlist(strsplit(address," "))
  address <- address[length(address)]
  return(address)
}

df['RoadType'] <- mapply(road_type,df$Address)
View(df)
unique(df$RoadType) # 57 unique values

housing_test['RoadType'] <- mapply(road_type,housing_test$Address)
View(housing_test)

# Test for co-relation between features (You can use this to show business which features are related but use VIF to remove multi-colinearity)
cor.test(df$Rooms,df$Bedroom2) # cor = 0.9203091
cor.test(df$Rooms,df$Bathroom) # cor = 0.5819089
cor.test(df$Rooms,df$Car) # cor = 0.4087185
cor.test(df$Rooms,df$Landsize) # cor = 0.08312395 

# problem: Landsize is 0 for valid number of Rooms = c(1,2,3,4,5,6)
unique(df[df$Landsize == 0 & !(is.na(df$Landsize)), c('Rooms')]) 
# 2  3  1  4  6  5
mean_landsize <- df %>% select(Rooms,Landsize) %>% 
  filter(Landsize != 0) %>% group_by(Rooms) %>% 
  summarise(round(mean(Landsize)))

df[df$Rooms==1 & df$Landsize==0 & !(is.na(df$Landsize)), c('Landsize')] = mean_landsize[1,2]
df[df$Rooms==2 & df$Landsize==0 & !(is.na(df$Landsize)), c('Landsize')] = mean_landsize[2,2]
df[df$Rooms==3 & df$Landsize==0 & !(is.na(df$Landsize)), c('Landsize')] = mean_landsize[3,2]
df[df$Rooms==4 & df$Landsize==0 & !(is.na(df$Landsize)), c('Landsize')] = mean_landsize[4,2]
df[df$Rooms==5 & df$Landsize==0 & !(is.na(df$Landsize)), c('Landsize')] = mean_landsize[5,2]
df[df$Rooms==6 & df$Landsize==0 & !(is.na(df$Landsize)), c('Landsize')] = mean_landsize[6,2]

# After modeling we observe that this 5 RoadType have significant impact on OUTCOME. Therefore changing in both df and housing_test
imp_roadtype <- c('RoadType_Cct','RoadType_Cir','RoadType_Cl','RoadType_Crescent','RoadType_Mews')
df$RoadType_Cct <- (df$RoadType=='Cct')*1
df$RoadType_Cir <- (df$RoadType=='Cir')*1
df$RoadType_Cl <- (df$RoadType=='Cl')*1
df$RoadType_Crescent <- (df$RoadType=='Crescent')*1
df$RoadType_Mews <- (df$RoadType=='Mews')*1
df$RoadType_other <- (!(df$RoadType_Cct+df$RoadType_Cir+df$RoadType_Cl+df$RoadType_Crescent+df$RoadType_Mews))*1

housing_test$RoadType_Cct <- (housing_test$RoadType=='Cct')*1
housing_test$RoadType_Cir <- (housing_test$RoadType=='Cir')*1
housing_test$RoadType_Cl <- (housing_test$RoadType=='Cl')*1
housing_test$RoadType_Crescent <- (housing_test$RoadType=='Crescent')*1
housing_test$RoadType_Mews <- (housing_test$RoadType=='Mews')*1
housing_test$RoadType_other <- (!(housing_test$RoadType_Cct+housing_test$RoadType_Cir+housing_test$RoadType_Cl+housing_test$RoadType_Crescent+housing_test$RoadType_Mews))*1

View(df)
View(housing_test)

# handling df$SellerG as it is converting it into factors, which giving ERROR while predicting 
seller <- data.frame('prop' = prop.table(table(df$SellerG)),
                     'count' = table(df$SellerG))
seller <- seller[,c('prop.Var1','prop.Freq','count.Freq')]
View(seller)

# dummy condition 
sum(seller$count.Freq < 100) # this will remove 165 outof 182 features creating 18 extra features, threshold = 0.013 for Freq < 100

# creating 2 dummy one will have all the significant seller's and other will have all the insignificant seller's
# insignificant <- c('ASL','Besser','Blue','Buxton/Advantage','Buxton/Find','Caine','Castran','CASTRAN','Century','David','Direct','Dixon','Domain','Elite','Fletchers/One','Galldon','Garvey','Geoff','Ham','Hamilton','hockingstuart/Advantage','hockingstuart/Village','J','Joseph','Kay','Kelly','LJ','Lucas','Marshall','Matthew','Meadows','Morrison','Nick','O\'Donoghues','One','R&H','Red','Rodney','Scott','Sotheby\'s','Weast','Walsh')
 
df$Suburb <- as.character(df$Suburb)
df$SellerG <- as.character(df$SellerG)

## Making MODEL-------------------------------------

df_pipe <- recipe(Price ~ ., data = df) %>%
  update_role(BuildingArea, YearBuilt, Address, RoadType, new_role = "drop_vars") %>%
  update_role(SellerG, Type, Method, CouncilArea,new_role = "to_dummies") %>%
  update_role(Bedroom2, Bathroom, Car, Landsize, new_role = "NA_to_median") %>%
  step_rm(has_role("drop_vars")) %>%
  step_other(has_role('to_dummies'), threshold = 0.013, other = '_other_') %>%
  step_dummy(has_role("to_dummies")) %>%
  step_impute_median(has_role("NA_to_median"),-all_outcomes())

# dmy <- recipe(Price ~ ., data = df) %>% 
#   step_other(SellerG, threshold = 0.013, other = '_other_') %>% 
#   step_dummy(SellerG) %>% prep()
# View(bake(dmy, new_data = df))

# prep()---------------
df_pipe <- prep(df_pipe)

# bake()-------------
train <- bake(df_pipe, new_data = NULL)
test <- bake(df_pipe, new_data = housing_test)
head(train)
head(test)

# SPLIT THE DATA => 80/20---------------

set.seed(3)
s=sample(1:nrow(train),0.8*nrow(train)) # selecting 80% of data to TRAIN

t1=train[s,] # train data -> 80% 
t2=train[-s,] # Test data -> 20%

## MODEL BUILDING------------

# Iteration 1
model1 <- lm(Price ~ ., data = t1)
sort(vif(model1), decreasing = TRUE)

alias(model1)

# Iteration 2
model1 <- lm(Price ~ . -Distance -Postcode -RoadType_other -CouncilArea_Bayside -CouncilArea_Boroondara, data = t1)
sort(vif(model1), decreasing = TRUE)

alias(model1)

# Iteration 3
model1 <- lm(Price ~ . -Distance -Postcode -RoadType_other -CouncilArea_Bayside -CouncilArea_Boroondara -CouncilArea_Brimbank -CouncilArea_Darebin -CouncilArea_Glen.Eira -CouncilArea_Hobsons.Bay -CouncilArea_Kingston, data = t1)
sort(vif(model1), decreasing = TRUE)

alias(model1)

# Iteration 4
model1 <- lm(Price ~ . -Distance -Postcode -RoadType_other -CouncilArea_Bayside -CouncilArea_Boroondara -CouncilArea_Brimbank -CouncilArea_Darebin -CouncilArea_Glen.Eira -CouncilArea_Hobsons.Bay -CouncilArea_Kingston -CouncilArea_Manningham -CouncilArea_Maribyrnong -CouncilArea_Melbourne -CouncilArea_Monash -CouncilArea_Moonee.Valley, data = t1)
sort(vif(model1), decreasing = TRUE)

alias(model1)

# Iteration 4
model1 <- lm(Price ~ . -Distance -Postcode -RoadType_other -CouncilArea_Bayside -CouncilArea_Boroondara -CouncilArea_Brimbank -CouncilArea_Darebin -CouncilArea_Glen.Eira -CouncilArea_Hobsons.Bay -CouncilArea_Kingston -CouncilArea_Manningham -CouncilArea_Maribyrnong -CouncilArea_Melbourne -CouncilArea_Monash -CouncilArea_Moonee.Valley -CouncilArea_Moreland -CouncilArea_Port.Phillip -CouncilArea_Stonnington -CouncilArea_Whitehorse -CouncilArea_Yarra -CouncilArea_X_other_, data = t1)

sort(vif(model1), decreasing = TRUE) # Suburb  193.467952

# Iteration 5
model1 <- lm(Price ~ . -Distance -Postcode -RoadType_other -CouncilArea_Bayside -CouncilArea_Boroondara -CouncilArea_Brimbank -CouncilArea_Darebin -CouncilArea_Glen.Eira -CouncilArea_Hobsons.Bay -CouncilArea_Kingston -CouncilArea_Manningham -CouncilArea_Maribyrnong -CouncilArea_Melbourne -CouncilArea_Monash -CouncilArea_Moonee.Valley -CouncilArea_Moreland -CouncilArea_Port.Phillip -CouncilArea_Stonnington -CouncilArea_Whitehorse -CouncilArea_Yarra -CouncilArea_X_other_ -Suburb, data = t1)

sort(vif(model1), decreasing = TRUE) # Rooms  3.556051


summary(model1)

model1 <- stats::step(model1)

summary(model1)

# R-squared:  0.5103, Adjusted R-squared:  0.508   
# Error between R-squared & Adjusted R-squared: 0.4507153%
# F-statistic: (p-value < 2.2e-16) <0.05
# It means X have some effect on Y

formula(model1)
# Price ~ Rooms + Bedroom2 + Bathroom + Landsize + RoadType_Cct + RoadType_Cir + RoadType_Cl + RoadType_Crescent + RoadType_Mews + Type_t + Type_u + Method_S + Method_VB + Method_X_other_ + SellerG_Biggin + SellerG_Buxton + SellerG_Fletchers + SellerG_Gary + SellerG_Greg + SellerG_hockingstuart + SellerG_Jellis + SellerG_Marshall + SellerG_Miles + SellerG_Nelson + SellerG_Ray + SellerG_RT + SellerG_Woodards + SellerG_X_other_

model1 <- lm(Price ~ Rooms + Bathroom + Landsize + RoadType_Cct + RoadType_Cir + RoadType_Cl + Type_t + Type_u + Method_S + Method_VB + SellerG_Biggin + SellerG_Buxton + SellerG_Fletchers + SellerG_Gary + SellerG_Greg + SellerG_hockingstuart + SellerG_Jellis + SellerG_Marshall + SellerG_Miles + SellerG_Nelson + SellerG_Ray + SellerG_RT + SellerG_Woodards + SellerG_X_other_, data = t1)

summary(model1) # -Bedroom2 -RoadType_Crescent -RoadType_Crescent -Method_X_other_
# R-squared:  0.5094,	Adjusted R-squared:  0.5075 
# F-statistic: (p-value: < 0.00000000000000022)


## Projecting LINEAR MODEL on our TEST data t2------------

t2.pred <- predict(model1, newdata = t2)

## MODEL VALIDATION-----------------

options(scipen=999)
errors_t2 <- t2$Price-t2.pred
errors_t1 <- t1$Price-predict(model1,t1)

## calculating RMSE & MAE for test
rmse_t2 <- errors_t2**2 %>% mean() %>% sqrt() # 440573.4
mae_t2 <- mean(abs(errors_t2)) # 308896.7

## calculating RMSE & MAE for train
rmse_t1 <- errors_t1**2 %>% mean() %>% sqrt() # 468507.6
mae_t1 <- mean(abs(errors_t1)) # 314314.7

(rmse_t1-rmse_t2)*100/rmse_t1 # 5.96239%
(mae_t1-mae_t2)*100/mae_t2 # 1.723766%

## Visuals of Assumptions
plot(model1,1)
plot(model1,2)
plot(model1,3)
plot(model1,4)

## FINAL MODEL------------------------


final_model1 <- lm(Price ~ Rooms + Bathroom + Landsize + RoadType_Cct + RoadType_Cir + RoadType_Cl + Type_t + Type_u + Method_S + Method_VB + SellerG_Biggin + SellerG_Buxton + SellerG_Fletchers + SellerG_Gary + SellerG_Greg + SellerG_hockingstuart + SellerG_Jellis + SellerG_Marshall + SellerG_Miles + SellerG_Nelson + SellerG_Ray + SellerG_RT + SellerG_Woodards + SellerG_X_other_, data = train)

sort(vif(final_model1), decreasing = TRUE)

final_model1 <- stats::step(final_model1)

summary(final_model1)
# R-squared:  0.5053,	Adjusted R-squared:  0.5037 
# F-statistic: (p-value: < 2.2e-16)

# Plotting Price against each feature in final_model1
nm <- names(final_model1$coefficients)
nm <- nm[-1]

pairs(train[,c('Price',nm)])

# checking rmse
errors_train <- train$Price-predict(final_model1, train)

rmse_train <- errors_train**2 %>% mean() %>% sqrt # 462789.4

## Predicting on test--------

test.pred <- predict(final_model1, newdata=test)

write.csv(test.pred,paste0(path,'submission1.csv'), row.names = FALSE)

## Diagnostic Plots(Assumptions Plot)-----------

plot(final_model1,1)
plot(final_model1,2)
plot(final_model1,3)
plot(final_model1,4)

durbinWatsonTest(final_model1)
# as p-value = 0.376 > 0.05 => Fail to Reject the NULL HYPOTHESIS and conclude that the residuals in this regression model are not autocorrelated



##------------------------------
## In DIAGNOSTIC PLOTS(plots for assumption), If they are not valid on assumptions that means data has NON-LINEARITY.
## Especially observing HOMOSCEDASTICITYY and RESIDUALS ARE NORMALLY DISTRIBUTED OR NOT
## My model is CONSISTENT, but it's PREDICTIVE POWER is bad(This can also be known from Adjusted R-squared, if it is small then predictive power is bad).
## To INCREASE it's predictive power use NON-LINEAR Technique's
## 1) Random Forest
## 2) Gradient Boosting
##------------------------------
## what if I add regularization(ridge/lasso) to this model?
## The purpose of REGULARIZATION is to decrease OVERFITTING.
## Adding regularization will not help predictive power, it only balances your data and increase STABILITY of the model.
## Use RIDGE for OUTLIER sensitive data 
##------------------------------
## To verify my model, we calculate score = 212467/rmse_test
## Score should be greater than 0.51
## Score(for train) = 212467/rmse_train = 212467/462789.4 = 0.46
## 0.46<0.51 => BAD PREDICTION
##------------------------------

#### USING RANDOM FOREST
library(rpart.plot)
library(ranger)

## Selecting hyperparameters
rf_model1 <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  mode = 'regression',
  engine = 'ranger'
)

## folds
k_fold <- vfold_cv(train, v = 10)

## Making grid
model1_grid <- grid_regular(mtry(c(5,25)), trees(c(100,500)), min_n(c(2,10)), levels = 5)
dim(model1_grid) # 125

## Tuning grid

tune_model1 <- tune_grid(
  rf_model1,
  Price~.,
  resamples = k_fold,
  grid = model1_grid,
  metrics = metric_set(rmse),
  control = control_grid(verbose = TRUE)
)

autoplot(tune_model1)+theme_light()

tune_model1 %>% show_best()

final_rf_model1 <- rf_model1 %>% 
  set_engine('ranger', importance = 'permutation') %>% 
  finalize_model(select_best(tune_model1,'rmse')) %>% 
  fit(Price~., data = train)

## VIP

final_rf_model1 %>% 
  vip(geom='col', aesthetics=list(fill='blue', aplha=0.8)) + 
  scale_y_continuous(expand = c(0,0))

saveRDS(final_rf_model1,file=paste0(path,'final_rf_model1.RDS'))
# readRDS(file=paste0(path,'final_rf_model1.RDS'))

## predictions

t1_pred <- predict(final_rf_model1, new_data=t1)[,1]
t2_pred <- predict(final_rf_model1, new_data=t2)[,1]
train_pred <- predict(final_rf_model1, new_data=train) #%>% select(.pred)
test_pred <- predict(final_rf_model1, new_data=test) #%>% select(.pred)

write.csv(test_pred,paste0(path,'submission2.csv'), row.names = FALSE)

error_t1 <- unlist(t1$Price-t1_pred)
error_t2 <- unlist(t2$Price-t2_pred)

rmse_t1 <- error_t1**2 %>% mean() %>% sqrt # 198307.3
rmse_t2 <- error_t2**2 %>% mean() %>% sqrt # 188819.4

error_train <- as.numeric(unlist(train$Price-train_pred))
rmse_train <- error_train**2 %>% mean() %>% sqrt # 196445.4

# 212467/rmse_train = 1.081558

## Why rmse_train keeps changing in RF whenever you run again and again?
## 1) Because rows and columns are randomized at node
## 2) 2nd reason is BOOSTRAPPING



