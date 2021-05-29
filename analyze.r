#Export data from excel files
Untitled
library("readxl")
Att_Data_set <- read_excel("D:\\Data Scientist Training Program\\R
Programming\\Projects\\Attribute DataSet.xlsx")
Dress_Sales<- read_excel("D:\\Data Scientist Training Program\\R
Programming\\Projects\\Dress Sales.xlsx")
#Understanding the structure
str(Att_Data_set)
#Data Cleaning to make a structured data set
Att_Data_set$Size[Att_Data_set$Size == "small"] <- "S"
Att_Data_set$Price[Att_Data_set$Price == "very-high"] <- "High"
Att_Data_set$Price[Att_Data_set$Price == "Average"] <- "Medium"
Att_Data_set$Season[Att_Data_set$Season == "Automn"] <- "Autumn"
Att_Data_set$SleeveLength[Att_Data_set$SleeveLength == "capsleeves"] <-
  "cap-sleeves"
Att_Data_set$SleeveLength[Att_Data_set$SleeveLength == "halfsleeve"] <- "half"
Att_Data_set$SleeveLength[Att_Data_set$SleeveLength == "sleeevless"] <- "sleeveless"
Att_Data_set$SleeveLength[Att_Data_set$SleeveLength == "sleevless"] <- "sleeveless"
Att_Data_set$SleeveLength[Att_Data_set$SleeveLength == "sleveless"] <- "sleeveless"
Att_Data_set$SleeveLength[Att_Data_set$SleeveLength == "threequater"] <-
  "threequarter"
Att_Data_set$SleeveLength[Att_Data_set$SleeveLength == "thressqatar"] <-
  "threequarter"
Att_Data_set$SleeveLength[Att_Data_set$SleeveLength == "urndowncollor"] <-
  "turndowncollor"
Att_Data_set$waiseline[Att_Data_set$waiseline == "modal"] <- "model" Att_Data_set$waiseline[Att_Data_set$waiseline == "sill"] <- "silk" Att_Data_set$FabricType[Att_Data_set$FabricType == "flannael"] <- "flannel" Att_Data_set$FabricType[Att_Data_set$FabricType == "knitting"] <- "knitted" Att_Data_set$FabricType[Att_Data_set$FabricType == "sattin"] <- "satin" Att_Data_set$FabricType[Att_Data_set$FabricType == "wollen"] <- "woolen" Att_Data_set$`Pattern Type`[Att_Data_set$`Pattern Type` == "leapord"] <- "leopard"
#Remove na,#N/A,blank,NULL,null data from the dataset
Att_Data_set[!(is.na(Att_Data_set$Price) | Att_Data_set$Price =="" |
                 Att_Data_set$Price =="null" | Att_Data_set$Price =="NULL" | Att_Data_set$Price
               =="#N/A"), ]
Page 1

Untitled
str(Att_Data_set)
anyNA(Att_Data_set)
Att_Data_set<-na.omit(Att_Data_set)
anyNA(Att_Data_set)
is.na(x=Att_Data_set)
rowSums(x = is.na(x = Att_Data_set))
Att_Data_set <- apply(Att_Data_set,2,toupper)
# Convert Dress Sales Character rows into num to get Average sales
Dress_Sales$`41617`<-as.numeric(Dress_Sales$`41617`)
Dress_Sales$`14/9/2013`<-as.numeric(Dress_Sales$`14/9/2013`)
Dress_Sales$`16/9/2013`<-as.numeric(Dress_Sales$`16/9/2013`)
Dress_Sales$`18/9/2013`<-as.numeric(Dress_Sales$`18/9/2013`)
Dress_Sales$`20/9/2013`<-as.numeric(Dress_Sales$`20/9/2013`)
Dress_Sales$`22/9/2013`<-as.numeric(Dress_Sales$`22/9/2013`)
#calculating Average sales in new column as dress sales per dress iD
Dress_Sales <- transform(Dress_Sales, Avg_sales_P_ID = rowMeans(Dress_Sales[,-1],
                                                                na.rm = TRUE))
# Append Average Dress Sales column into Attribute Data table
m1 <- merge(x = Att_Data_set, y = Dress_Sales[,c(1,25)], by.x = "Dress_ID", by.y = "Dress_ID")
str(m1)
Page 2

Untitled
# recommendation/Style/Price/Size/Season/Neckline/
# Sleevelength/waiseline/Material/Fabrictype/Decoration/
# PatternType convert into factor
m1$Recommendation <- as.factor(m1$Recommendation)
m1$Style<-as.factor(m1$Style)
m1$Price<-as.factor(m1$Price)
m1$Size<-as.factor(m1$Size)
m1$Season<-as.factor(m1$Season)
m1$NeckLine<-as.factor(m1$NeckLine)
m1$SleeveLength<-as.factor(m1$SleeveLength)
m1$waiseline<-as.factor(m1$waiseline)
m1$Material<-as.factor(m1$Material)
m1$FabricType<-as.factor(m1$FabricType)
m1$Decoration<-as.factor(m1$Decoration)
m1$`Pattern Type`<-as.factor(m1$`Pattern Type`)
#Removing NULL values from the dataset if any
str(m1)
anyNA(m1)
m1<-na.omit(m1)
anyNA(m1)
# Split data into 70% in training and 20% in testing
library(cranly)
library(caTools)
set.seed(100)
Page 3

Untitled
spl_data <- sample.split(m1$Recommendation, SplitRatio=0.70)
spl_data
table(spl_data)
train_data <- subset(m1,spl_data ==T)
nrow(train_data)
test_data <- subset(m1,spl_data == F)
nrow(test_data)
# run logistic regression model
names(m1)
library(arm)
LRModel <- bayesglm(Recommendation ~ Style + Price + Rating + Size + Season + NeckLine + SleeveLength + waiseline + Material + FabricType +
                      Decoration + `Pattern Type` + Avg_sales_P_ID
                    ,  data = train_data, family= binomial)
summary(LRModel)
# to remove unimportant variables and take only important variables in model step(LRModel)
LRModel1 <- glm(formula = Recommendation ~ Season + Avg_sales_P_ID,  data =
                  train_data, family= binomial)
summary(LRModel1)
# Test model for Predict response
probabilities <- predict(LRModel1, test_data, type = "response")
probabilities
predtest_class <- ifelse(probabilities>0.5,1,0)
cm <- table(test_data$Recommendation,predtest_class)
Page 4

Untitled
#Check accuracy of your model
accuracy <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy
#Plot model data on graph
library(ROCR)
ROCRpred = prediction(probabilities, test_data$Recommendation)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf)
###### End of Logistic Regression ######
# Loading package
library(e1071)
library(caTools)
library(caret)
# -----Naive B Model---------
set.seed(12345)
classifier_cl <- naiveBayes(Recommendation ~ ., data = train_data)
classifier_cl
# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_data)
# Confusion Matrix
cm <- table(test_data$Recommendation, y_pred)
cm
#Check accuracy
accuracy <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy
#=========== Naive Bayes Ends ==================#
# ============ KNN ================#
Page 5

#install.packages("class")
#detach("ROCR")
library(class)
Untitled
prc_test_pred <- knn(train = train_data, test = test_data,cl =
                       train_data$Recommendation, k=5)
# Confusion Matrix
cm <- table(test_data$Recommendation, prc_test_pred)
cm
#Check accuracy
accuracy <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy
# ============== KNN done=================#
# =========== SVM ===================#
library(e1071)
svmfit = svm(Recommendation ~ ., data = Att_Data_set, kernel = "linear",type = 'C-classification')
y_pred = predict(svmfit, newdata = test_data)
# Confusion Matrix
cm <- table(test_data$Recommendation, y_pred)
cm
accuracy <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy
#run same model with the overall dataset
#to check model actual accuracy
y_pred1 = predict(svmfit, newdata = m1)
cm <- table(m1$Recommendation, y_pred1)
cm
Page 6

final_data <- cbind(m1,y_pred1)
accuracy <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy
# =============End Project ===========================================
Untitled
Page 7
