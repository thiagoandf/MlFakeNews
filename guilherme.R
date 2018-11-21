data = read.csv('./snsdata.csv', header = TRUE)
sapply(data, levels)
sapply(data, class)
levels(factor(data$gender))

# Replace M and F for 1 and 0
data$gender <- ifelse(gender=='M', 1, 0)
View(data)