# Project_1_Real_Estate

Price of a property is one of the most important decision criterion when people buy homes. Real state firms need to be consistent in their pricing in order 
to attract buyers. Having a predictive model for the same will be great tool to have, which in turn can also be used to tweak development of properties, 
putting more emphasis on qualities which increase the value of the property.

We have given you two datasets, housing_train.csv and housing_test.csv, You need to use data housing_train to build predictive model for response variable 
"Price". Housing_test data contains all other factors except "Price", you need to predict that using the model that you developed.

First I stared with linear model as dependent variable "Price" is continous. 
But, after doing DIAGNOSTIC PLOTS we understand that data has NON-LINEARITY.
Therefore, made the final_rf_model1.RDS using RANDOM FOREST.

I have uploaded housing_train.csv, housing_test.csv, final_rf_model1.RDS files in this repository. 
I have uploaded my predicted values in "Predicted value of Price for training_test.csv"
