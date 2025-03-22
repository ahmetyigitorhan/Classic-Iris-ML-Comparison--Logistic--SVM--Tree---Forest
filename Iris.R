View(iris)
# Column Names
names(iris)
# Number of rows
nrow(iris)
iris
library(mice)
# Checking the missing values
md.pattern(iris)

# Data Types of columns
str(iris)

# Summary Statistics
summary(iris)

#Visualization of the Data
par(mfrow=c(2,2))
boxplot(iris[,1:4],
        main='Iris - Data Distribution',
        names=colnames(iris[,1:4]),
        col=rainbow(4))
sapply(names(iris)[1:4],FUN=function(x){boxplot(iris[[x]] ~ iris$Species,
                                                 main=x,
                                                 col=rainbow(4),
                                                 ylab='Values',
                                                 xlab='Species')})
sapply(names(iris)[1:4],FUN=function(x){hist(iris[[x]],
                                             main=x,
                                             ylab='Y-axis',
                                             xlab='X-axis')})
## Checking the Outliers
library(rstatix)
# for setosa
irisSetosa <- subset(iris,iris$Species=='setosa',select=)
identify_outliers(irisSetosa ,Sepal.Length)
identify_outliers(irisSetosa,Sepal.Width)
identify_outliers(irisSetosa,Petal.Length)
out <- identify_outliers(irisSetosa,Petal.Width) # 1 extreme outliers
miss <- out[which(out$is.extreme==TRUE),]$Petal.Width
irisSetosa <- irisSetosa[-which(irisSetosa$Petal.Width >= miss),]
nrow(irisSetosa)
# for versicolor
irisVersicolor <- subset(iris,iris$Species=='versicolor',select=)
identify_outliers(irisVersicolor,Sepal.Length)
identify_outliers(irisVersicolor,Sepal.Width)
identify_outliers(irisVersicolor,Petal.Length)
identify_outliers(irisVersicolor,Petal.Width)
# for virginica
irisVirginica <- subset(iris,iris$Species=='virginica',select=)
identify_outliers(irisVirginica,Sepal.Length)
identify_outliers(irisVirginica,Sepal.Width)
identify_outliers(irisVirginica,Petal.Length)
identify_outliers(irisVirginica,Petal.Width)

iris <- rbind(irisSetosa,irisVersicolor,irisVirginica)
View(iris)

### Anova Test 
# Normality Check of Sepal.Width
shapiro.test(iris$Sepal.Width) # p.value = 0.08 -> normally distributed 
# Homogeneity of Variances of Sepal Width
bartlett.test(iris$Sepal.Width ~ iris$Species) # p.value = 0.3157 -> so there is homogeneity of variances.
# Anova 
anov <- aov(iris$Sepal.Width ~ iris$Species )
anov
summary(anov) # p.value = 2e-16 .So reject the H0  at least one group mean is different 
# TukeyHSD
TukeyHSD(anov) # all p.value <= 0.05 . So there are significant difference in three group means
#Based on the boxplot, the other three independent variables show noticeable differences in their means, which may indicate statistical significance.

### Correlation Test
#Pearson Correlation
cor_matrix <- cor(iris[ , 1:4],method='pearson')
library(corrplot)
install.packages('ggcorrplot')
library(ggcorrplot)
corrplot(cor_matrix,method='color',addCoef.col = 'black',
         tl.col='black',number.cex = 0.8,col=colorRampPalette(c('blue','white','red'))(200))

#### Model Part ####
set.seed(155)
irisSetosaIn <- sample(1:nrow(irisSetosa),size=0.8*nrow(irisSetosa))
set.seed(155)
irisVersicolorIn <- sample(1:nrow(irisVersicolor),size=0.8*nrow(irisVersicolor))
set.seed(155)
irisVirginicaIn <- sample(1:nrow(irisVirginica),size=0.8*nrow(irisVirginica))

TrainSetosa <- irisSetosa[irisSetosaIn,]
TrainVersicolor <- irisVersicolor[irisVersicolorIn,]
TrainVirginica <- irisVirginica[irisVirginicaIn,]

TrainSet <- rbind(TrainSetosa,TrainVersicolor,TrainVirginica)
table(TrainSet$Species)

TestSetosa <- irisSetosa[-irisSetosaIn,]
TestVersicolor <- irisVersicolor[-irisVersicolorIn,]
TestVirginica <- irisVirginica[-irisVirginicaIn,]

TestSet <- rbind(TestSetosa,TestVersicolor,TestVirginica)
table(TestSet$Species)

# Creating Model with Multinomail Logistic Regression
library(e1071)
library(tidyverse)
library(nnet)
library(caret)

ModelLog <- multinom(Species ~ . , data=TrainSet)
ModelLog
summary(ModelLog)
ModelLog$fitted.values
ModelLog$decay

# Creating Model with Support Vector Machine 
install.packages('GGally')
library(GGally)
ggpairs(iris, aes(color = Species))

ModelSvm1 <- svm(Species ~ . , data=TrainSet, kernel = 'linear')
ModelSvm2 <- svm(Species ~ . , data=TrainSet, kernel = 'radial')

summary(ModelSvm1)
summary(ModelSvm2)

# Creating Model with Decision Trees 
library(rpart)
library(rattle)
ModelDec<- rpart(Species ~ . , data=TrainSet , method='class',
                  parms=list(split='gini'))
ModelDec
fancyRpartPlot(ModelDec)
summary(ModelDec)

# Creating Model with Random Forest
library(randomForest)
ModelRF <- randomForest(Species ~ . , data=TrainSet , ntree=500)
ModelRF

#### Prediction Part ####

# Predcition with Multinomial Logistic Regression
caret::varImp(ModelLog)

predModelLog <- predict(ModelLog,TestSet)
predModelLog

#Prediction with Support Vector Machine

predModelSvm1 <- predict(ModelSvm1,TestSet)
predModelSvm2 <- predict(ModelSvm2,TestSet)

# Prediction with Decision Trees 
predModelDec <- predict(ModelDec,TestSet,type='class')

# Prediction with Random Forest
predModelRF <- predict(ModelRF,TestSet)

#### Comparison Part ####

# Multinomial 
caret::confusionMatrix(predModelLog,TestSet$Species) # Accuracy = 1
# SVM
caret::confusionMatrix(predModelSvm1,TestSet$Species) # Accuracy = 1
caret::confusionMatrix(predModelSvm2,TestSet$Species) # Accuracy = 0.9667
# Decision Tree
confusionMatrix(predModelDec,TestSet$Species) # Accuracy = 0.93
# Random Forest
confusionMatrix(predModelRF,TestSet$Species) # Accuracy = 0.93

# Among all models tested, both the Multinomial Logistic Regression and 
# the linear SVM achieved perfect classification accuracy, demonstrating 
# their effectiveness for this specific dataset. However, all models 
# performed reasonably well, indicating that the Iris dataset is highly 
# separable with a variety of machine learning techniques."