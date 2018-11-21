dataset <- read.csv('snsdata.csv')
table(dataset$bible)
table(dataset$basketball)
table(dataset$sexy)
table(dataset$rock)
table(dataset$dress)
table(dataset$death)
table(dataset$drugs)
summary(dataset$drugs)
summary(log(dataset$drugs))

summary(dataset$age)
summary(dataset$friends)
table(dataset$gender)
table(dataset$gradyear)

standardization <- function(x) {
  return ((x - mean(x)) / sd(x))
}

dataset <-  na.omit(dataset)
dataset$gender <- ifelse(dataset$gender=='M', 1, 0)
dataset_norm <- as.data.frame(lapply(dataset, standardization))
model <- kmeans(dataset_norm, centers = 4)
