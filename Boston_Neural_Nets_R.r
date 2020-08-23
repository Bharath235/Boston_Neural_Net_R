library(MASS)

head(Boston)

str(Boston)

any(is.na(Boston))

data <- Boston

maxs <- apply(data,2,max)
maxs

mins <- apply(data,2,min)
mins

scaled.data <- scale(data, center=mins, scale = maxs-mins)
scaled <- as.data.frame(scaled.data)
head(scaled)

library(caTools)
sample <- sample.split(scaled$medv, SplitRatio=0.7)
train <- subset(scaled, sample==T)
test <- subset(scaled, sample==F)

library(neuralnet)

nn <- neuralnet(medv~., data=train, hidden=c(5,3), linear.output=T)

#n <- names(train)
#f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

#nn <- neuralnet(f, data=train, hidden=c(5,3), linear.output=T)

plot(nn)

predicted.nn.values <- compute(nn, test[1:13])
str(predicted.nn.values)

true.predictions <- predicted.nn.values$net.result * (max(data$medv)-min(data$medv))+min(data$medv)

test.r <- (test$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn <- sum((test.r - true.predictions)^2)/nrow(test)
MSE.nn

error.df <- data.frame(test.r, true.predictions)
head(error.df)

library(ggplot2)

ggplot(error.df, aes(test.r, true.predictions)) + geom_point() + stat_smooth()


