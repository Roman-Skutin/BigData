install.packages("RClickhouse")
# DB connection
library("RClickhouse")
p <- DBI::dbConnect(RClickhouse::clickhouse(), host="",port=9000, password = "")
irisData<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header=FALSE)
names(irisData)<-c("SepalLength", "SepalWidth", "PetalLength", "PetalWidth", "Species")
DBI::dbSendQuery(p, "use db_6010")
DBI::dbWriteTable(p, "iris", irisData)
d <- DBI::dbGetQuery(p, "SELECT * from iris")
print(d)
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
dataset <- d # create a link
dataset$Class <- factor(c("Iris-setosa","Iris-versicolor","Iris-virginica"))
summary(dataset)
dim(dataset) # measurements
sapply(dataset, class) # list of types of each attribute
head(dataset) # display
levels(dataset$Class) #  classifier levels
# ??ani??aaaeaiea aeaia e??ena
percentage <- prop.table(table(dataset$Class)) * 100
cbind(freq = table(dataset$Class), percentage = percentage)
# break the data into variables and response
x <- dataset[,1:4]
y <- dataset[,5]
# display
par(mfrow = c(1,4))
for(i in 1:4){
  boxplot(x[,i], main = names(iris)[i])
}
# interaction within data
#featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")
# analysis
control <- trainControl(method="cv", number=10)
metric <- "Accuracy" # controlled mark
# linear algorithms
set.seed(13)
fit.lda <- train(Class~., data=dataset, method="lda", metric=metric, trControl=control)
# nonlinear algorithms
set.seed(13)
fit.cart <- train(Class~., data=dataset, method="rpart", metric=metric, trControl=control)
set.seed (13)
fit.knn <- train(Class~., data=dataset, method="knn", metric=metric, trControl=control)
# complex algorithms
set.seed(13)
fit.svm <- train(Class~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# RandomForest
set.seed(13)
fit.rf <- train(Class~., data=dataset, method="rf", metric=metric, trControl=control)
# estimates for each algorithm
results <- resamples(list(lda =fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results)

print(fit.cart)
# cut off some of the data for validation
validation_index <- createDataPartition(dataset$Class, p =0.80, list=FALSE) #80%
# choose 20% for validation
validation <- dataset[-validation_index,]
# 80% initial data
dataset <- dataset[validation_index,]
predictions <- predict(fit.cart, validation)
confusionMatrix(predictions, validation$Class)