#Applied Machine Learning Assignment 
#Student's TP Number : TP067418
#Student's Name : Lee Zhi Ming
#Please run the codes from top to bottom*#

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Import Data#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getwd()
data = read.csv('/Users/zm/documents/airlinedata.csv')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data Exploration#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
str(data)
View(summary(data))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data Cleaning#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
data= data%>%select(-'X', -'id')

##Transforming categorical variables to factor class
str(data)
summary(data)
data = data%>%
  mutate(Gender = factor(Gender),
         Customer.Type = factor(Customer.Type),
         Type.of.Travel = factor(Type.of.Travel),
         Class = factor(Class),
         Inflight.wifi.service = factor(Inflight.wifi.service),
         Departure.Arrival.time.convenient = factor(Departure.Arrival.time.convenient),
         Ease.of.Online.booking = factor(Ease.of.Online.booking),
         Gate.location = factor(Gate.location),
         Food.and.drink = factor(Food.and.drink),
         Online.boarding = factor(Online.boarding),
         Seat.comfort = factor(Seat.comfort),
         Inflight.entertainment = factor(Inflight.entertainment),
         On.board.service = factor(On.board.service),
         Leg.room.service = factor(Leg.room.service),
         Baggage.handling = factor(Baggage.handling),
         Checkin.service = factor(Checkin.service),
         Inflight.service = factor(Inflight.service),
         Cleanliness = factor(Cleanliness))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Recoding categorical variables into numbers
data = data%>%mutate(satisfaction = factor(satisfaction, levels=c('satisfied','neutral or dissatisfied'), 
                                           labels=c("0", "1")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Correlation Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~
#correlation analysis data preparation
library(reshape2)
matrix_data = data.matrix(data)
View(head(matrix_data))
realmatrix=cor(na.omit(matrix_data))
realmatrix
cormat <- round(cor(matrix_data),2)
View(head(cormat))
melted_cormat <- melt(realmatrix)
head(melted_cormat)

#correlation heatmap
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = 'white')+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  geom_text(aes(Var2, Var1, label = sprintf(value, fmt = '%#.2f') ), color = "black", size = 3.8) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  theme(axis.text.y = element_text(size = 12))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Finding missing values~~~~~~~~~~~~~~~~~~~~~
library(DataExplorer)
plot_missing(data)
colSums(is.na(data))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Imputing missing values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data = data %>%
  mutate(Arrival.Delay.in.Minutes= coalesce(Arrival.Delay.in.Minutes ,Departure.Delay.in.Minutes))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Removal of highly correlated features~~~~~~~~~~~~~~~~~~~~~
data= data%>%select(-'Arrival.Delay.in.Minutes')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EDA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1. Barplot of target variable 
satisfaction_plot = as.data.frame(table(data$satisfaction))
print(satisfaction_plot)
names(satisfaction_plot) = c('col1', 'col2')

satisfaction_plot %>% 
  count(Satisfaction = factor(col1), Frequency = col2) %>% 
  mutate(Percentage = prop.table(Frequency)) %>% 
  ggplot(aes(x = Satisfaction, y = Percentage, fill = Frequency, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') + 
  geom_text( vjust = 0) + 
  scale_y_continuous(labels = scales::percent)

#2. Barplot of customer type vs satisfaction level
ggplot(data, aes(x = Customer.Type, fill = satisfaction)) +
  geom_bar(stat='Count', position='dodge') +
  labs(x = 'Type of Customer')+
  guides(fill = guide_legend(title.position="top", title ="Satisfaction") ) + 
  scale_fill_brewer(palette = "Set1", labels = c("0(Satisfied)", "1(Neutral/Dissastisfied)"))

#3. Barplot of travel class vs satisfaction level
ggplot(data, aes(x = Class, fill = satisfaction)) +
  geom_bar(stat='Count', position='dodge') +
  labs(x = 'Travel Class')+
  guides(fill = guide_legend(title.position="top", title ="Satisfaction") ) + 
  scale_fill_brewer(palette = "Set1", labels = c("0(Satisfied)", "1(Neutral/Dissastisfied)"))

#4. Barplot of Type of Travel vs satisfaction level
ggplot(data, aes(x = Type.of.Travel, fill = satisfaction)) +
  geom_bar(stat='Count', position='dodge') +
  labs(x = 'Type of Travel', y = 'No. Of Passengers' )+
  guides(fill = guide_legend(title.position="top", title ="Satisfaction") ) + 
  scale_fill_brewer(palette = "Set1", labels = c("0(Satisfied)", "1(Neutral/Dissastisfied)"))+
  coord_flip()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data Partition~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#dividing data set into train and test
library(caTools)
set.seed(1234)
split = sample.split(data$satisfaction, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

#check proportion of target variable 
prop.table(table(data$satisfaction))
prop.table(table(training_set$satisfaction))
prop.table(table(test_set$satisfaction))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Building SVM Model~~~~~~~WARNING : BUILDING THIS MODEL WILL TAKE TIME~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(kernlab)
library(e1071)
library(caret)
classifier = ksvm(satisfaction~., data = training_set, kernel = "vanilladot")
classifier

#predicting the model with training data
predict_train = predict(classifier, training_set)

#evaluating model prediction with confusion matrix
cm_vanilladot = table(predict_train, training_set$satisfaction)
confusionMatrix(cm_vanilladot)

#predicting the model with test data
predict_test = predict(classifier, test_set)

#evaluating model prediction with confusion matrix
cm = table(predict_test, test_set$satisfaction)
confusionMatrix(cm)

#building svm model with rbf kernel 
classifier_rbf = ksvm(satisfaction~., data = training_set,
                  kernel = 'rbfdot')  
classifier_rbf

#predicting the model with training data
predict_train_rbf = predict(classifier_rbf, training_set)

#evaluating model prediction with confusion matrix
cm_train_rbf = table(predict_train_rbf, training_set$satisfaction)
confusionMatrix(cm_train_rbf)

#predicting the model with test data
predict_test_rbf = predict(classifier_rbf, test_set)

#evaluating model prediction with confusion matrix
cm_rbf = table(predict_test_rbf, test_set$satisfaction)
confusionMatrix(cm_rbf)

#building svm model with tanhdot
classifier_tanhdot = ksvm(satisfaction~., data = training_set,
                      kernel = 'tanhdot')  
classifier_tanhdot

#predicting the model with training data
predict_train_tanhdot = predict(classifier_tanhdot, training_set)

#evaluating model prediction with confusion matrix
cm_tanhdot_train = table(predict_train_tanhdot, training_set$satisfaction)
confusionMatrix(cm_tanhdot_train)

#predicting the model with test data
predict_test_tanhdot = predict(classifier_tanhdot, test_set)

#evaluating model prediction with confusion matrix
cm_tanhdot = table(predict_test_tanhdot, test_set$satisfaction)
confusionMatrix(cm_tanhdot)

#building svm model with polydot
classifier_polydot = ksvm(satisfaction~., data = training_set,
                          kernel = 'polydot')  
classifier_polydot

#predicting the model with training data
predict_train_polydot = predict(classifier_polydot, training_set)

#evaluating model prediction with confusion matrix
cm_polydot_train = table(predict_train_polydot, training_set$satisfaction)
confusionMatrix(cm_polydot_train)

#predicting the model with test data
predict_test_polydot = predict(classifier_polydot, test_set)

#evaluating model prediction with confusion matrix
cm_polydot = table(predict_test_polydot, test_set$satisfaction)
confusionMatrix(cm_polydot)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Tuning SVM Model~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Model Tuning 1 ~~~~~~~~~~~~~~~~~~~~~
#building svm model with rbf kernel + increase Cost parameter to 3
classifier_rbf_cost3= ksvm(satisfaction~., data = training_set,
                      kernel = 'rbfdot', C = 3)  
classifier_rbf_cost3

#Tuned Model Testing
#predicting the model with training data
predict_train_rbf_cost3 = predict(classifier_rbf_cost3, training_set)

#evaluating model prediction with confusion matrix
cm_train_rbf_cost3 = table(predict_train_rbf_cost3, training_set$satisfaction)
confusionMatrix(cm_train_rbf_cost3)

#predicting the model with test data
predict_test_rbf_cost3 = predict(classifier_rbf_cost3, test_set)

#evaluating model prediction with confusion matrix
cm_rbf_cost3 = table(predict_test_rbf_cost3, test_set$satisfaction)
confusionMatrix(cm_rbf_cost3)

#ROC Curve + AUC Score 
library(pROC)
roc(training_set$satisfaction, classifier_rbf_cost3$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

roc_svm_test = roc(training_set$satisfaction, predictor =as.numeric(classifier_rbf_cost3))
plot(roc_svm_test, add = TRUE,col = "red", print.auc=TRUE, print.auc.x = 0.5, print.auc.y = 0.3)
legend(0.3, 0.2, legend = c("test-svm"), lty = c(1), col = c("blue"))

pred_ROCR <- prediction(training_set$satisfaction, classifier_rbf_cost3)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Model Building~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(randomForest)
library(caret)
set.seed(1234)  
randomforest_classifier = randomForest(satisfaction ~ ., data= training_set, 
                                       ntree = 400,         
                                       replace = TRUE,      
                                       sampsize = 200,     
                                       nodesize = 5,       
                                       importance = TRUE,  
                                       proximity = FALSE,    
                                       norm.votes = TRUE,   
                                       keep.forest = TRUE,  
                                       keep.inbag = TRUE)
randomforest_classifier

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Error analysis and variable significance~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(randomforest_classifier, type="l", main="error rates vs. size of forest (in trees)")

legend("top", colnames(randomforest_classifier$err.rate), col=1:3,cex=0.5,fill=1:3)

#Feature Importance 
varImpPlot(randomforest_classifier, cex=.8, main="Feature Importance In Random Forest Classifier")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Model Testing~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Evaluating Trainining Set 
prediction_train_randomforest=  predict(randomforest_classifier, training_set)
prediction_train_randomforest

cm_training_randomforest= confusionMatrix(prediction_train_randomforest, training_set$satisfaction)
cm_training_randomforest

#Evaluating Test Set
set.seed(1234)
prediction_test_randomforest =  predict(randomforest_classifier, test_set)
prediction_test_randomforest 

cm_test_randomforest = confusionMatrix(prediction_test_randomforest , test_set$satisfaction)
cm_test_randomforest

#ROC Curve + AUC Score 
library(pROC)
roc(training_set$satisfaction, randomforest_classifier$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Normalizing Data Before Building ANN~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#normalizing the data
normalize = function(x) {
  return((x- min(x))/ (max(x) - min(x)))
}

#convert the all variables to numeric in order to be normalized
training_set_converted = training_set%>%mutate(Gender = as.numeric(Gender),
                                     Customer.Type =as.numeric(Customer.Type),
                                     Type.of.Travel = as.numeric(Type.of.Travel),
                                     Class = as.numeric(Class),
                                     Inflight.wifi.service = as.numeric(Inflight.wifi.service),
                                     Departure.Arrival.time.convenient = as.numeric(Departure.Arrival.time.convenient),
                                     Ease.of.Online.booking = as.numeric(Ease.of.Online.booking),
                                     Gate.location = as.numeric(Gate.location),
                                     Food.and.drink = as.numeric(Food.and.drink),
                                     Online.boarding = as.numeric(Online.boarding),
                                     Seat.comfort = as.numeric(Seat.comfort),
                                     Inflight.entertainment = as.numeric(Inflight.entertainment),
                                     On.board.service = as.numeric(On.board.service),
                                     Leg.room.service = as.numeric(Leg.room.service),
                                     Baggage.handling = as.numeric(Baggage.handling),
                                     Checkin.service = as.numeric(Checkin.service),
                                     Inflight.service = as.numeric(Inflight.service),
                                     Cleanliness = as.numeric(Cleanliness),
                                     satisfaction = as.numeric(satisfaction))

test_set_converted = test_set%>%mutate(Gender = as.numeric(Gender),
                                               Customer.Type =as.numeric(Customer.Type),
                                               Type.of.Travel = as.numeric(Type.of.Travel),
                                               Class = as.numeric(Class),
                                               Inflight.wifi.service = as.numeric(Inflight.wifi.service),
                                               Departure.Arrival.time.convenient = as.numeric(Departure.Arrival.time.convenient),
                                               Ease.of.Online.booking = as.numeric(Ease.of.Online.booking),
                                               Gate.location = as.numeric(Gate.location),
                                               Food.and.drink = as.numeric(Food.and.drink),
                                               Online.boarding = as.numeric(Online.boarding),
                                               Seat.comfort = as.numeric(Seat.comfort),
                                               Inflight.entertainment = as.numeric(Inflight.entertainment),
                                               On.board.service = as.numeric(On.board.service),
                                               Leg.room.service = as.numeric(Leg.room.service),
                                               Baggage.handling = as.numeric(Baggage.handling),
                                               Checkin.service = as.numeric(Checkin.service),
                                               Inflight.service = as.numeric(Inflight.service),
                                               Cleanliness = as.numeric(Cleanliness),
                                               satisfaction = as.numeric(satisfaction))

#normalize training and test 
training_set_normalized =as.data.frame(lapply(training_set_converted[,c(1:22)], normalize))
str(training_set_normalized)

test_set_normalized =as.data.frame(lapply(test_set_converted[,c(1:22)], normalize))
str(test_set_normalized)

#convert "satisfaction" back to factor class
training_set_normalized$satisfaction = as.factor(training_set_normalized$satisfaction)
test_set_normalized$satisfaction = as.factor(test_set_normalized$satisfaction)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Building ANN using H2o package~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#building model with h2o package
install.packages('h2o')
library(h2o)
library(caret)

#Start a local cluster with 6GB RAM and all the cores"
h2o.init(nthreads = -1, max_mem_size = '6G')


#converting training and test set into H2o dataframe
train = as.h2o(training_set_normalized)
test = as.h2o(test_set_normalized)
str(train)
train
#take a look at the contents of the h2oframe(train and test set)
h2o.describe(train)
h2o.describe(test)
View(train)


#building neural network using H2o package, ReClu function
# 1 layers with 5 neurons  and 100 iterations (epoch), 
n = h2o.deeplearning(x = 1:21,
                     y = 'satisfaction',
                     training_frame = train,
                     standardize = FALSE,
                     activation = 'RectifierWithDropout',
                     hidden = 5,
                     seed = 123,
                     epochs = 100)


#compute variable importance and performance
h2o.varimp_plot(n,num_of_features = 20)

#evaluate the performance of the model  
h2o.performance(n)

#testing the model using train set 
pred = h2o.predict(object = n, 
                   newdata = train
)

#convert from h2o frame into dataframe
predtrain = as.data.frame(pred)
trainindf = as.data.frame(train)
View(predtrain)
View(trainindf)

cm_train_nn = confusionMatrix(table(predtrain$predict, trainindf$satisfaction))
cm_train_nn


#testing the model using test set 
pred_test = h2o.predict(object = n, 
                   newdata = test
)

predtest = as.data.frame(pred_test)
testindf = as.data.frame(test)
View(predtest)
View(testindf)

cm_test_nn = confusionMatrix(table(predtest$predict, testindf$satisfaction))
cm_test_nn

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Model Tuning~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#IMPROVING THE MODEL , ReClu function
#2 layers with 20 neurons each , 3 fold cross validation and 1000 iterations (epoch)
nimproved = h2o.deeplearning(x = 1:22,
                             y = 'satisfaction',
                             training_frame = train,
                             standardize = FALSE,
                             nfolds = 3,
                             activation = 'RectifierWithDropout',
                             hidden = c(20, 20),
                             seed = 123,
                             epochs = 1000)


#IMPROVED MODEL TESTINGGG
#compute variable importance and performance
h2o.varimp_plot(nimproved,num_of_features = 20)

#evaluate the performance of the model  
h2o.performance(nimproved)


#testing the model using train set 
pred_nnimproved_trainingset = h2o.predict(object = nimproved, 
                   newdata = train
)


pred_train_nnimproved = as.data.frame(pred_nnimproved_trainingset)
trainindf = as.data.frame(train)
View(predtrain)
View(trainindf)

cm_train_nnimproved = confusionMatrix(table(pred_train_nnimproved$predict, trainindf$satisfaction))
cm_train_nnimproved 

#explore predictions in test set 
pred_nnimproved_testset= h2o.predict(object = nimproved, 
                   newdata = test
)
pred_test_nnimproved = as.data.frame(pred_nnimproved_testset)
testindf = as.data.frame(test)
View(predindf)
View(testindf)

cm_test_nnimproved  = confusionMatrix(table(pred_test_nnimproved$predict, testindf$satisfaction))
cm_test_nnimproved 

h2o.auc(nimproved)
h2o.auc(n)

h2o.shutdown()
Y
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


