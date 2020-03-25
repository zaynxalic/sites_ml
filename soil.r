#createDataPartition

#add data limit
memory.limit(10000000000)
library(caret)

library(psych)
library (rattle)

#load the soil data
soil <- read.csv(file = "hr_lr_labm.csv")

labmCode <- soil$labm_code

#factorize the soil
#labmNum <- factor(c(labmCode))

labrValue <- soil$labr_value

#load the "h_soil_water_stat" non-null column
#fill in the NULL value as NA value
soil$h_soil_water_stat[soil$h_soil_water_stat == "NULL"] = NA

#get the null value row
naValuerow <- which(is.na(soil$h_soil_water_stat))

validsoilSample <- soil[-naValuerow,]

invalidsoilSample <- soil[naValuerow,]

#set random seed
set.seed(222)
  
train_index <- createDataPartition( y = validsoilSample$h_soil_water_stat,
                                    p = 0.1,
                                    list = FALSE,
                                    times = 1)

train_data <- (validsoilSample[c('labm_code','labr_value','h_soil_water_stat')])[train_index,]

test_data <- (validsoilSample[c('labm_code','labr_value','h_soil_water_stat')])[-train_index,]

fit_Control <- trainControl(method = "cv", number = 10,savePredictions = TRUE)

#svm machine learning
svm_model <- train(h_soil_water_stat ~ .,
                   data = train_data,
                   method = 'svmLinear',
                   na.action = na.omit,
                   trControl = fit_Control)


