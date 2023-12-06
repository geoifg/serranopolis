## Library
install.packages(c("raster", "sf", "caret"), dependencies=TRUE)
library(raster)
library(sf)
library(caret)

## Command to read all raster files in geotiff format in a given directory
variables <- list.files(path="D:/Documents/IFG/TCC/ALESSANDRA/DATA2", 
                        pattern=".tif$", full.names=TRUE)

variables

## Stack command stacks raster data into one file.
# Here, we will call the stacked raster data 'xvars'.
xvars <- stack(variables) 

xvars

## With the 'st_read' command, we read the vector file in gpkg 
# format containing sample points for training.
# Note: The file must be of POINT type (not MULTIPOINT).
samples <- st_read("D:/Documents/IFG/TCC/ALESSANDRA/DATA2/Points_scenario1.gpkg")

## To plot the points
plot(samples)     

## For installing the 'tibble' library
install.packages("tibble")
library(tibble)

## For installing the 'dplyr' library (simplified)
install.packages("dplyr")
library(dplyr)       # for data manipulation

# or complete 'dplyr'
install.packages("tidyverse")

## Extract pixel values from raster for each point and keep attribute table values.
# We use the 'tibble' and 'dplyr' libraries for simplicity.
sample_xvars <- raster::extract(xvars, samples) %>% 
  tibble::as_tibble() %>% 
  dplyr::bind_cols(samples, .)
sample_xvars

head(sample_xvars)

## Note: Initially run without the 'factor' command until the ROC curve.
## Then, go back and run again, now transforming the samples into a factor.

## Declaring the response variable as a categorical data
sample_xvars$SitioX <- as.factor(sample_xvars$SitioX)    
##sample_xvars$Soil <- as.factor(sample_xvars$Soil) 
##sample_xvars$Geology <- as.factor(sample_xvars$Geology) 

sample_xvars$Sitio <- NULL
str(sample_xvars)

## Since the algorithm does not work with raster or vector,
# we will convert the data into a data frame.
xvars_df <- as.data.frame(sample_xvars)

xvars_df$geom <- NULL
str(xvars_df)
dim(xvars_df)

## Boxplot graphics
library(ggplot2)
local_relief <- ggplot(xvars_df) +
  aes(x = SitioX, y = Amplitude_17) +
  geom_violin(width=1.2) +
  theme(text = element_text(size = 28)) +
  geom_boxplot(shape = "circle", fill = "#112446", alpha=0.2) +
  theme_bw()

slope <- ggplot(xvars_df) +
  aes(x = SitioX, y = Slope_p) +
  geom_violin(width=1.2) +
  theme(text = element_text(size = 28)) +
  geom_boxplot(shape = "circle", fill = "#112446", alpha=0.2) +
  theme_bw()

VRM <- ggplot(xvars_df) +
  aes(x = SitioX, y = VRM) +
  geom_violin(width=1.2) +
  theme(text = element_text(size = 28)) +
  geom_boxplot(shape = "circle", fill = "#112446", alpha=0.2) +
  theme_bw()

aspect <- ggplot(xvars_df) +
  aes(x = SitioX, y = Aspect) +
  geom_violin(width=1.2) +
  theme(text = element_text(size = 28)) +
  geom_boxplot(shape = "circle", fill = "#112446", alpha=0.2) +
  theme_bw()

Sky_View_Factor <- ggplot(xvars_df) +
  aes(x = SitioX, y = Sky_View_Factor) +
  geom_violin(width=1.2) +
  theme(text = element_text(size = 28)) +
  geom_boxplot(shape = "circle", fill = "#112446", alpha=0.2) +
  theme_bw()

Depression <- ggplot(xvars_df) +
  aes(x = SitioX, y = Depression) +
  geom_violin(width=1.2) +
  theme(text = element_text(size = 28)) +
  geom_boxplot(shape = "circle", fill = "#112446", alpha=0.2) +
  theme_bw()

twi <- ggplot(xvars_df) +
  aes(x = SitioX, y = SAGA_TWI) +
  geom_violin(width=1.2) +
  theme(text = element_text(size = 28)) +
  geom_boxplot(shape = "circle", fill = "#112446", alpha=0.2) +
  theme_bw()

MRRTF <- ggplot(xvars_df) +
  aes(x = SitioX, y = MRRTF) +
  geom_violin(width=1.2) +
  theme(text = element_text(size = 28)) +
  geom_boxplot(shape = "circle", fill = "#112446", alpha=0.2) +
  theme_bw()

library(ggpubr)
ggarrange(slope, VRM, aspect, local_relief, Sky_View_Factor, Depression, twi, MRRTF + rremove("x.text"), 
          labels = c("a)", "b)", "c)", "d)", "e)", "f", "g)", "h)"),
          ncol = 3, nrow = 3)


## Working with packages
# Installing the "corrplot" package
install.packages("corrplot")

# Calling the "corrplot" package
library(corrplot)

## First, create the correlation matrix.
M <- cor(xvars_df[2:19])        #==>Specify the total number of variables here
M
corrplot(M, method = "ellipse")

# Another representation
corrplot(M, method = "number", tl.cex = 0.5, number.cex = .4, tl.col = 'black')

# Another representation
corrplot(M, method = "ellipse", type = "upper", tl.cex = 0.7, tl.col = 'black')

# Another representation
corrplot.mixed(M, lower = "number", number.cex = .45, 
               upper = "ellipse", tl.pos = "lt",
               tl.cex = 0.5, tl.col = 'black')

# Another representation
corrplot.mixed(M, lower = "number", lower.col = "black",
               number.cex = .45, upper = "ellipse", 
               tl.pos = "lt", tl.cex = 0.5, tl.col = 'black')

## Split the training and validation samples
# In this case, we will use 70% of the samples for training and 30% for validation.
# The split between 70 and 30% will be based on the class data.
set.seed(114)         #<<==Specify the total number of samples here
xvars_df2 <- as.vector(createDataPartition(xvars_df$SitioX, list=FALSE, p=0.7))
training_class <- xvars_df[xvars_df2,]
validation_class <- xvars_df[-xvars_df2,]

## Check the number of training samples
nrow(training_class)
str(training_class)

## Check the number of validation samples
nrow(validation_class)
head(training_class)


## Now, to work with the probability of occurrence
# The 'trainControl' command will determine the classifier approach 
# Note: In this case, the CV method = 5-fold cross-validation)
# The 'classProbs' and 'summaryFunction' commands work with text-type variables only
control <- trainControl(method = "cv",     
                        number = 5,       
                        classProbs = TRUE,
                        savePredictions = TRUE,
                        summaryFunction = twoClassSummary)


## Building the C5.0 model
library(C50)
set.seed(114)
model_c50 <- train(SitioX ~ .,  data = training_class, method = "C5.0",
                   trControl=control, metric = 'ROC')

print(model_c50)




## Building the RF model
set.seed(114)
model_RF <- train(SitioX  ~ .,  data = training_class, method = "rf",
                  trControl=control, metric = 'ROC')

print(model_RF)

## Building the XGBoost model
library(xgboost)
set.seed(100)
model_xgboost <- train(SitioX  ~ .,  data = training_class, method = "xgbTree",
                       trControl=control, metric = 'ROC')
print(model_xgboost)

model_xgboost$modelInfo

model_xgboost$finalModel

xgb_imp <- xgboost::xgb.importance(model_xgboost$finalModel$feature_names,model=model_xgboost$finalModel)

head(xgb_imp)

lattice::dotplot(Feature ~ Importance*100,
                 data = xgb_imp, xlim=c(-10, 110))

xgb.plot.importance(xgb_imp)

c50_imp <- C50::C5imp(model_c50$finalModel, metric = "usage")

head(c50_imp)

lattice::dotplot(c50_imp)

## Building the GBM model
set.seed(100)
model_gbm <- train(SitioX  ~ .,  data = training_class, method = "gbm",
                   trControl=control, metric = 'ROC')

print(model_gbm)



## Plot comparing the overall models for incidents
comparison <- resamples(list(C5.0=model_c50, RF=model_RF, XGBoost=model_xgboost, GBM=model_gbm))


## Boxplots of validations
bwplot(comparison)

summary(comparison)


## Getting the importance of variables
# For the varImp option, type = 2 (default) is used for the mean of the mean Gini decrease 'MeanDecreaseGini' 
# based on the Gini impurity used for calculating decision tree nodes.
# Alternatively, you can set type = 1, then the calculated measure is the mean decrease in accuracy.
# RF Algorithm

plot(varImp(model_c50,type=2))
plot(varImp(model_RF,type=2))


# XGBoost Importance
# Incorrect way
plot(varImp(model_xgboost,type=2))

# Correct way
xgb_imp <- xgboost::xgb.importance(model_xgboost$finalModel$feature_names,model=model_xgboost$finalModel)

head(xgb_imp)

lattice::dotplot(Feature ~ Importance*100,
                 data = xgb_imp, xlim=c(-10, 110))

xgb.plot.importance(xgb_imp)

library(C50)
C50_imp <- C5imp(model_c50$finalModel, metric = "splits", pct = TRUE)
colnames(C50_imp) <- c(Features, Overall)

C50_imp2 <- as.data.frame(C50_imp)

summary(C50_imp2)

lattice::dotplot(Overall,
                 data = xgb_imp, xlim=c(-10, 110))


library(tidyr)
C50_imp2 %>% separate(Overall,
                      c("Feature", "Importance"))

separate(
  C50_imp2,
  Overall,
  into = c("Spl_1", "Spl_2"),
  sep = "[^[:alnum:]]+",
  remove = FALSE
)


str(C50_imp2)


## Importance for individual classes
importanceC50 <- varImp(model_c50)
plot(importanceC50)

importanceRF <- varImp(model_RF)
plot(importanceRF, top = 15)

importanceXGB <- varImp(model_xgboost)
plot(importanceXGB)

install.packages("MLmetrics")
library("MLmetrics")
## C5.0 Model
# Confusion matrix with independent validation data
valid_C50 <- predict(model_c50, validation_class)
C50_table <- table(validation_class$SitioX, valid_C50)

colnames(C50_table) = c("X0", "X1")
C50_table

# Validation C5.0 statistics
library(caret)
confusionMatrix(C50_table)
F1_Score(valid_C50, validation_class$SitioX)
Area_Under_Curve(valid_C50, validation_class$SitioX)


## GBM Model
# Confusion matrix with independent validation data
valid_gbm <- predict(model_gbm, validation_class)
gbm_table <- table(validation_class$SitioX, valid_gbm)

colnames(gbm_table) = c("X0", "X1")
gbm_table

# Validation GBM statistics
library(caret)
confusionMatrix(gbm_table)
F1_Score(valid_gbm, validation_class$SitioX)
Area_Under_Curve(valid_gbm, validation_class$SitioX)



## RF Model
# Confusion matrix with independent validation data
valid_rf <- predict(model_RF, validation_class)
RF_table <- table(validation_class$SitioX, valid_rf)

colnames(RF_table) = c("X0", "X1")
RF_table

# Validation RF statistics
confusionMatrix(valid_rf, validation_class$SitioX)
F1_Score(valid_rf, validation_class$SitioX)
## xgboost model
# Confusion matrix with independent validation data
valid_xgboost <- predict(model_xgboost, validation_class)
xgboost_table <- table(validation_class$SitioX, valid_xgboost)

colnames(xgboost_table) = c("X0", "X1")
xgboost_table

# Validation xgboost statistics
confusionMatrix(xgboost_table)
F1_Score(valid_xgboost, validation_class$SitioX)

###### Note: Run again from the 'Declaring the response variable as a categorical data' command, transforming it into a factor 
## Calculate ROC and AUC
# Obtaining the ROC curve from validation data
library(ROCR)
library(pROC)

roc_c50 <- roc(validation_class$SitioX,
               predict(model_c50, validation_class, type = "prob")[,1],
               levels = rev(levels(validation_class$SitioX)))
roc_c50

# Receiver Operating Characteristic (ROC) curve for the Gradient Boosting Machine (GBM) model
roc_gbm <- roc(validacao_class$SitioX,
               predict(modelo_gbm, validacao_class, type = "prob")[,1],
               levels = rev(levels(validacao_class$SitioX)))
roc_gbm

# Receiver Operating Characteristic (ROC) curve for the Random Forest (RF) model
roc_rf <- roc(validacao_class$SitioX,
              predict(modelo_RF, validacao_class, type = "prob")[,1],
              levels = rev(levels(validacao_class$SitioX)))
roc_rf

# Receiver Operating Characteristic (ROC) curve for the XGBoost model
roc_xgboost <- roc(validacao_class$SitioX,
                   predict(modelo_xgboost, validacao_class, type = "prob")[,1],
                   levels = rev(levels(validacao_class$SitioX)))
roc_xgboost

# Obtaining the Area Under the Curve (AUC) values
auc(roc_c50)
auc(roc_rf)
auc(roc_xgboost)

# Generating the ROC curve plot
roc_rose <- plot(roc_c50, print.auc = TRUE, col = "blue", print.auc.y = .4, add = TRUE)
roc_rose <- plot(roc_rf, xlim=c(1,0), ylim=c(0,1), print.auc = TRUE, col = "green", print.auc.y = .5)
roc_rose <- plot(roc_xgboost, print.auc = TRUE, col = "orange", print.auc.y = .3, add = TRUE)

# Creating the model in raster file
c50_pred <- predict(xvars, modelo_c50, filename="D:/Documents/IFG/TCC/ALESSANDRA/TCC_RESULTS/SCENARIO1/c50.tif", type="prob", 
                    index=2, na.rm=TRUE, progress="window", overwrite=TRUE)

rf_pred <- predict(xvars, modelo_RF, filename="C:/TCC_RESULTS/SCENARIO1/rf.tif", type="prob", 
                   index=2, na.rm=TRUE, progress="window", overwrite=TRUE)

xgboost_pred <- predict(xvars, modelo_xgboost, filename="C:/TCC_RESULTS/SCENARIO1/xgboost.tif", type="prob", 
                        index=2, na.rm=TRUE, progress="window", overwrite=TRUE)

# Plotting the model in R
plot(c50_pred)
plot(rf_pred) 
plot(xgboost_pred)

# Confusion matrix and model statistics with validation samples
# Applying the model to the validation data
C50pred <- as.factor(predict(modelo_c50, validacao_class, type="raw"))

# Creating a confusion matrix (table) between the validation data and the predicted values by the model
C50_tabela <- table(validacao_class$SitioX, C50pred)

# Calculating the confusion matrix with statistical metrics
confusionMatrix(C50_tabela)

# Applying the model to the validation data
RFpred <- as.factor(predict(modelo_RF, validacao_class, type="raw"))

# Creating a confusion matrix (table) between the validation data and the predicted values by the model
RF_tabela <- table(validacao_class$SitioX, RFpred)

# Calculating the confusion matrix with statistical metrics
confusionMatrix(RF_tabela)

# Applying the model to the validation data
XGBoostpred <- as.factor(predict(modelo_xgboost, validacao_class, type="raw"))

# Creating a confusion matrix (table) between the validation data and the predicted values by the model
XGBoost_tabela <- table(validacao_class$SitioX, XGBoostpred)

# Calculating the confusion matrix with statistical metrics
confusionMatrix(XGBoost_tabela)
