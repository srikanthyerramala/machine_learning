rm(list = ls(all = TRUE))
# library(doParallel)
# c1<-makeCluster(3)
# registerDoParallel(c1)
setwd("~/Downloads")
my_data = read.csv("TextClassification_Data.csv")
### Removing unnecessary fields #####
my_data$previous_appointment <- NULL
my_data$ID <- NULL
my_data$fileid <- NULL


text <- my_data$DATA

###### Using regex to extract text between RTF strips #######
reg_ind <- gregexpr("(\\s)(.*)[\\](.*?)",text)
text_1 <- regmatches(as.character(text),reg_ind)
text_2 <- lapply(text_1,FUN=function(x){paste(x,collapse ="")})
text_3 <- gsub("\\\\","",text_2)
text_4 <- gsub(".*\\}","",text_3)
text_5 <- gsub("xx+","",text_4)
text_6 <- gsub("Pt","Patient",text_5)
text_6 <- gsub("Appt","Appointment",text_6)
text_6 <- gsub("pt","patient",text_6)
text_6 <- gsub("appt","appointment",text_6)

rm(text_1,text_2,text_3,text_4,text_5)
rm(text)

my_data$DATA_NEW <- text_6

data <- data.frame(SUMMARY = my_data$SUMMARY,DATA = my_data$DATA_NEW)


data$DATA <- paste(data$SUMMARY,data$DATA,sep = " ")
data$SUMMARY <- NULL
data$sub_categories <- my_data$sub_categories
data$sub_categories <- gsub("mEDICATION RELATED","MEDICATION RELATED",data$sub_categories)


#write.csv(data,"/Users/apple/Downloads/data.csv")

###### Creating Corpus ######

library(tm)
data_corpus <- Corpus(VectorSource(data$DATA))

# examine the  corpus
print(data_corpus)

inspect(data_corpus[1:3])

# clean up the corpus using tm_map()

corpus_clean <- tm_map(data_corpus, content_transformer(tolower))
inspect(corpus_clean[1:3])
rm(data_corpus)

corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, c(stopwords("english")))
corpus_clean <- tm_map(corpus_clean, removePunctuation)
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus_clean <- tm_map(corpus_clean, toSpace, "/")
corpus_clean <- tm_map(corpus_clean, toSpace, "@")
corpus_clean <- tm_map(corpus_clean, toSpace, "\\|")
corpus_clean <- tm_map(corpus_clean, toSpace, "-")
View(corpus_clean)

corpus_clean <- tm_map(corpus_clean, stripWhitespace)
corpus_clean <- tm_map(corpus_clean,stemDocument, language ="english")
#str(corpus_clean)

#corpus_clean <- tm_map(corpus_clean, PlainTextDocument) 
# library(wordnet)
# lapply(corpus_clean,function(x){
#   x.filter <- getTermFilter("ExactMatchFilter", x, TRUE)
# terms <- getIndexTerms("NOUN", 1, x.filter)
# sapply(corpus_clean, getLemma)
# })
#library(rJava)
#library(RWeka) #enable ngrams
# Onegram <- NGramTokenizer(corpus_clean, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
# Bigram <- NGramTokenizer(corpus_clean, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
# Trigram <- NGramTokenizer(corpus_clean, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))
# 
# # Exploratory Analysis Results
# # converting tokens of n-grams into tables
# 
# Tab_onegram <- data.frame(table(Onegram))
# Tab_bigram <- data.frame(table(Bigram))
# Tab_trigram <- data.frame(table(Trigram))




# create a document-term sparse matrix

# using weight tf
data_dtm <- DocumentTermMatrix(corpus_clean, control=list(weighting = function(x) weightTf(x)))
data_dtm <- removeSparseTerms(data_dtm,0.995)
dim(data_dtm)

# using weight tfidf

 data_dt <- as.matrix(data_dt)

data_dtm <- DocumentTermMatrix(corpus_clean, control=list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
data_dtm <- removeSparseTerms(data_dtm,0.995)
dim(data_dtm)

# using binary model

data_dt <- DocumentTermMatrix(corpus_clean)
data_dtm <- removeSparseTerms(data_dtm,0.995)
dim(data_dtm)




colnames(data_dtm)
?findAssocs
findAssocs(data_dtm, "refer", corlimit=0.2)



# svdM = svd(data_dtm)
# s= diag(svdM$d)
# u = svdM$u
# v = svdM$v
# vt = t(v)
# 
# eigenval = svdM$d
# e_sqare_energy = (eigenval/sum(eigenval))*100
# cumsum(e_sqare_energy)
# 
# svd <- svd(data_dtm,nu=1370,nv=1370)
# S <- diag(svd$d[1:1370])
# dim(data_dtm)
# data_dtm <- svd$u %*% S %*% t(svd$v)


###### Dividing data into train, Validation and Test dataset #####

library(caTools)

set.seed(1234)
Sample<-sample.split(data$sub_categories,SplitRatio=0.7) 
head(Sample,10)
length(Sample)
data_train<-subset(as.matrix(data_dtm),Sample==T)
test <-subset(as.matrix(data_dtm),Sample==F)

Eval_set <- subset(data,Sample==F)
head(Eval_set,10)
#str(Eval_set)
nrow(Eval_set)

EvalSample<-sample.split(Eval_set$sub_categories,SplitRatio=0.5)
data_val<-subset(as.matrix(test),EvalSample==T)
data_test<-subset(as.matrix(test),EvalSample==F)
nrow(data_train)
nrow(data_val)
nrow(data_test)

#unique(Val)

raw_train<-subset(data$sub_categories,Sample==T) 
raw_train <- as.factor(raw_train)
#raw_train <- as.data.frame(raw_train)

raw_Test<-subset(data$sub_categories,Sample==F)

raw_val <- subset(raw_Test,EvalSample==T)
raw_val <- as.factor(raw_val)
#raw_val <- as.data.frame(raw_val)

raw_test <- subset(raw_Test,EvalSample==F)
raw_test <- as.factor(raw_test)
#raw_test <- as.data.frame(raw_test)

grep("JUNK",Eval_set$sub_categories)

str(raw_train)
str(raw_val)
str(raw_test)


# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}


# apply() convert_counts() to columns of train/test data

data_train <- apply(data_train, MARGIN = 2, convert_counts)
data_train <- as.data.frame(data_train)
data_val <- apply(data_val, MARGIN = 2, convert_counts)
data_val <- as.data.frame(data_val)
data_test <- apply(data_test, MARGIN = 2, convert_counts)
data_test <- as.data.frame(data_test)



##  Training C50 model on the data ----
library(e1071)
library(C50)
C50 <- C5.0(data_train, raw_train)
summary(C50)

##  Evaluating model performance ----
data_trainC50_pred <- predict(C50, data_train)
str(data_trainC50_pred)
conftrain.C50<-table(raw_train, data_trainC50_pred)

acctrain.train.C50 <- sum(diag(conftrain.C50))/nrow(data_train) * 100
acctrain.train.C50

data_valC50_pred <- predict(C50, data_val)
str(data_valC50_pred)
conf.val.C50<-table(raw_val, data_valC50_pred)

accuracy.val.C50 <- sum(diag(conf.val.C50))/nrow(data_val) * 100
accuracy.val.C50 #63.56494

n = sum(conf.val.C50) # number of instances
nc = nrow(conf.val.C50) # number of classes
diag = diag(conf.val.C50) # number of correctly classified instances per class 
rowsums = apply(conf.val.C50, 1, sum) # number of instances per class
colsums = apply(conf.val.C50, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
C50_Metrics <- data.frame(precision, recall, f1) 
write.csv(C50_Metrics,"/Users/apple/Downloads/C50_Metrics.csv")



################### Boosted Decision Tree ################

C50_B <- C5.0(data_train, raw_train,trials = 10)
summary(C50_B)

##  Evaluating model performance
data_trainC50B_pred <- predict(C50_B, data_train)
str(data_train_pred)
conftrain.C50_B<-table(raw_train, data_trainC50B_pred)

acctrain.train.C50_B <- sum(diag(conftrain.C50_B))/nrow(data_train) * 100
acctrain.train.C50_B

data_valC50B_pred <- predict(C50_B, data_val)

conf.val.C50_B<-table(raw_val, data_valC50B_pred)

accuracy.val.C50_B <- sum(diag(conf.val.C50_B))/nrow(data_val) * 100
accuracy.val.C50_B # 66.68218

n = sum(conf.val.C50_B) # number of instances
nc = nrow(conf.val.C50_B) # number of classes
diag = diag(conf.val.C50_B) # number of correctly classified instances per class 
rowsums = apply(conf.val.C50_B, 1, sum) # number of instances per class
colsums = apply(conf.val.C50_B, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes


precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
C50B_Metrics <- data.frame(precision, recall, f1) 
write.csv(C50B_Metrics,"/Users/apple/Downloads/C50B_Metrics.csv")

######################  Rpart #################

library(rpart)

str(data_train)
Rp_train <- as.data.frame(data_train)
Rp_train$sub_categories <- raw_train
str(Rp_train)
Rp_val <- as.data.frame(data_val)
Rp_val$sub_categories <- raw_val

rpart_model<-rpart(sub_categories~.,data = Rp_train,method = "class")

printcp(rpart_model)

rpart_Final<-rpart(sub_categories~.,data = Rp_train,cp=0.01,method = "class")

plotcp(rpart_Final)

##  Evaluating model performance

predTrainRpart<-as.vector(predict(rpart_Final,data_train,type="class"))

conf.Tr.Rpart<-table(Rp_train$sub_categories,predTrainRpart)
accuracy.Rp_train <- sum(diag(conf.Tr.Rpart))/sum(conf.Tr.Rpart) * 100
accuracy.Rp_train

predValRpart<-predict(rpart_Final,data_val,type="class")

conf.val.Rpart<-table(raw_val,predValRpart)
accuracy.Rp_val <- sum(diag(conf.val.Rpart))/sum(conf.val.Rpart) * 100
accuracy.Rp_val ## 50.3317

n = sum(conf.val.Rpart) # number of instances
nc = nrow(conf.val.Rpart) # number of classes
diag = diag(conf.val.Rpart) # number of correctly classified instances per class 
rowsums = apply(conf.val.Rpart, 1, sum) # number of instances per class
colsums = apply(conf.val.Rpart, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
Rpart_Metrics <- data.frame(precision, recall, f1) 
write.csv(Rpart_Metrics,"/Users/apple/Downloads/C50_Metrics.csv")


#################   Random Forest   ##############

library(randomForest)
# rf_train <- as.data.frame(data_train)
# rownames(rf_train) <- c(1:nrow(rf_train))
# rf_train1 <- data.frame(apply(rf_train, 2, as.factor))
# rf_val <- as.data.frame(data_val)
# rownames(rf_val) <- c(1:nrow(rf_val))
#rf_val1 <- data.frame(apply(rf_val, 2, as.factor))
#tuneRF(rf_train1,raw_train,ntreeTry = 100 ,stepFactor=0.5)
RfModel <- randomForest(data_train,raw_train, ntree = 100 )

summary(RfModel)
##  Evaluating model performance

Rfdata_train_pred <- predict(RfModel, data_train)
conf.Rf_train<-table(raw_train, Rfdata_train_pred)
#write.csv(conf.Rf_train,"/Users/apple/Downloads/conf.Rf_train.csv")

accuracy.Rf_train <- sum(diag(conf.Rf_train))/sum(conf.Rf_train) * 100
accuracy.Rf_train # 

data_valRf_pred <- predict(RfModel, data_val)
conf.val.Rf<-table(data_valRf_pred,raw_val)
accuracy.Rf_val <- sum(diag(conf.val.Rf))/sum(conf.val.Rf) * 100
accuracy.Rf_val  # 68.60235

n = sum(conf.val.Rf) # number of instances
nc = nrow(conf.val.Rf) # number of classes
diag = diag(conf.val.Rf) # number of correctly classified instances per class 
rowsums = apply(conf.val.Rf, 1, sum) # number of instances per class
colsums = apply(conf.val.Rf, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
RF_Metrics <- data.frame(precision, recall, f1) 
write.csv(RF_Metrics,"/Users/apple/Downloads/RF_Metrics.csv")

#############  naive bayes  ###############
library(e1071)

model<-naiveBayes(data_train,raw_train)

summary(model)

##  Evaluating model performance

predictTrainNB<-predict(model,data_train)

confTNB<-table(raw_train,predictTrainNB)
acc_train_NB <- sum(diag(confTNB))/sum(confTNB) * 100
acc_train_NB


predictvalNB<-predict(model,data_val)

conf.val.NB<-table(as.factor(raw_val),predictvalNB)

acc_val_NB <- sum(diag(conf.val.NB))/sum(conf.val.NB) * 100
acc_val_NB #52.66236

n = sum(conf.val.NB) # number of instances
nc = nrow(conf.val.NB) # number of classes
diag = diag(conf.val.NB) # number of correctly classified instances per class 
rowsums = apply(conf.val.NB, 1, sum) # number of instances per class
colsums = apply(conf.val.NB, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
NB_Metrics <- data.frame(precision, recall, f1) 
write.csv(NB_Metrics,"/Users/apple/Downloads/NB_Metrics.csv")


############ SVM ##############
library(e1071)

sum(is.na(data_train))

str(data_train)
svm_train <- as.data.frame(data_train)
svm_train$sub_categories <- raw_train
str(svm_train)
svm_val <- as.data.frame(data_val)
#svm_val$sub_categories <- raw_val

svm_test <- as.data.frame(data_test)
#table(svm_train$sub_categories,raw_train)
#svm_test$sub_categories <- raw_test

tunedsvm <- tune.svm(sub_categories~.,type = "C-classification",kernel = "polynomial", cost = 10^(1:2), gamma = (10^(-6:-1)),data = svm_train)

svm_model <- svm(sub_categories~.,data = svm_train,type = "C-classification",kernel = "polynomial", cost = 100, gamma = 0.01 )

predTrainsvm<-predict(svm_model,data_train,type="class")

conf.Tr.svm<-table(svm_train$sub_categories,predTrainsvm)
accuracy.svm_train <- sum(diag(conf.Tr.svm))/sum(conf.Tr.svm) * 100
accuracy.svm_train ##83.17039

predValsvm<-predict(svm_model,data_val,type="class")

conf.val.svm<-table(predValsvm,raw_val)
accuracy.svm_val <- sum(diag(conf.val.svm))/sum(conf.val.svm) * 100
accuracy.svm_val ## 69.03293

predtestsvm<-predict(svm_model,data_test,type="class")

conf.test.svm<-table(predtestsvm,raw_test)
accuracy.svm_test <- sum(diag(conf.test.svm))/sum(conf.test.svm) * 100
accuracy.svm_test ## 68.49761

barplot(SVM_Metrics$recall,names.arg = row.names(SVM_Metrics),las=2,ylab = "Recall")


n = sum(conf.val.svm) # number of instances
nc = nrow(conf.val.svm) # number of classes
diag = diag(conf.val.svm) # number of correctly classified instances per class 
rowsums = apply(conf.val.svm, 1, sum) # number of instances per class
colsums = apply(conf.val.svm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

#write.csv(conf.test.svm,"/Users/apple/Downloads/conf.test.svm.csv")


precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
SVM_Metrics <- data.frame(precision, recall, f1) 
write.csv(SVM_Metrics,"/Users/apple/Downloads/SVM_Metrics.csv")

############ Visio #################

RF_Metrics
Rpart_Metrics
NB_Metrics
C50_Metrics
C50B_Metrics

library(ggplot2)

par(bg = 'white')

barplot(c(RF_Metrics$recall,NB_Metrics$recall,C50_Metrics$recall,C50B_Metrics$recall),names.arg = c(row.names(RF_Metrics),row.names(NB_Metrics),row.names(C50_Metrics),row.names(C50B_Metrics)))
barplot(RF_Metrics$recall,names.arg = row.names(RF_Metrics),las=2,ylab = "Recall")
barplot(NB_Metrics$recall,names.arg = row.names(NB_Metrics),las=2,ylab = "Recall")
barplot(C50_Metrics$recall,names.arg = row.names(C50_Metrics),las=2,ylab = "Recall")
barplot(C50B_Metrics$recall,names.arg = row.names(C50B_Metrics),las=2,ylab = "Recall")
barplot(SVM_Metrics$recall,names.arg = row.names(SVM_Metrics),las=2,ylab = "Recall")

Recall_All <- data.frame(C50_Metrics$recall,NB_Metrics$recall,SVM_Metrics$recall,RF_Metrics$recall)

colnames(Recall_All) <- c("C5.0","NB","SVM","RF")

rownames(Recall_All) <- rownames(C50_Metrics)

write.csv(Recall_All,"/Users/apple/Downloads/Recall_All.csv")

Recall_All <- round(Recall_All,2)


barplot(c(accuracy.val.C50,accuracy.Rp_val,acc_val_NB,accuracy.Rf_val,69.03293 ),names.arg = c("DT","Rpart","NB","RF","SVM"),las=2,xlab = 'Models',ylab = "accuracy",col = rainbow(20),space=1)



############ Random Forest Model on Test dataset ##########
rf_test <- as.data.frame(data_test)
rownames(rf_test) <- c(1:nrow(rf_test))
rf_test1 <- data.frame(apply(rf_test, 2, as.factor))

##### Prediction on test data using Term frequency method #####

data_testRf_pred_Tf <- predict(RfModel, data_test)
conf.test.Rf_Tf<-table(raw_test, data_testRf_pred_Tf)
conf.test.Rf_Tf
accuracy.Rf_test_tf <- sum(diag(conf.test.Rf_Tf))/sum(conf.test.Rf_Tf) * 100
accuracy.Rf_test_tf  # 69.19586
#tfidf 68.96311

n = sum(conf.test.Rf_Tf) # number of instances
nc = nrow(conf.test.Rf_Tf) # number of classes
diag = diag(conf.test.Rf_Tf) # number of correctly classified instances per class 
rowsums = apply(conf.test.Rf_Tf, 1, sum) # number of instances per class
colsums = apply(conf.test.Rf_Tf, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

##### Prediction on test data using TfIdf method #####

data_testRf_pred_tfidf <- predict(RfModel, data_test)
conf.test.Rf_Tfidf<-table(raw_test, data_testRf_pred_tfidf)
write.csv(conf.test.Rf_Tfidf,"/Users/apple/Downloads/conf.matrix_tftdf.csv")
conf.test.Rf_Tfidf
accuracy.Rf_test_tfidf <- sum(diag(conf.test.Rf_Tfidf))/sum(conf.test.Rf_Tfidf) * 100
accuracy.Rf_test_tfidf # 68.98638

###### Prediction on test data using Boolean method #####

#grep("JUNK",raw_test)

data_testRf_pred <- predict(RfModel, data_test)
conf.test.Rf<-table(raw_test, data_testRf_pred)
conf.test.Rf

write.csv(conf.test.Rf,"/Users/apple/Downloads/conf.matrix.csv")
accuracy.Rf_test <- sum(diag(conf.test.Rf))/sum(conf.test.Rf) * 100
accuracy.Rf_test # 68.76527


n = sum(conf.test.Rf) # number of instances
nc = nrow(conf.test.Rf) # number of classes
diag = diag(conf.test.Rf) # number of correctly classified instances per class 
rowsums = apply(conf.test.Rf, 1, sum) # number of instances per class
colsums = apply(conf.test.Rf, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
RF_Test_Metrics <- data.frame(precision, recall, f1) 
barplot(RF_Test_Metrics$recall,names.arg = row.names(RF_Test_Metrics),las=2,ylab = "Recall")

barplot(c(accuracy.Rf_test,accuracy.Rf_test_tfidf,accuracy.Rf_test_tf),names.arg = c("Binary","TF","TFIDF"),xlab = 'Random Forest',ylab = "Accuracy",col = rainbow(20),space=1)


write.csv(RF_Test_Metrics,"/Users/apple/Downloads/RF_Test_Metrics.csv")
write.csv(RF_Test_Metrics_Tf,"/Users/apple/Downloads/RF_Test_Metrics_Tf.csv")
write.csv(RF_Test_Metrics_Tfidf,"/Users/apple/Downloads/RF_Test_Metrics_TfIdf.csv")
