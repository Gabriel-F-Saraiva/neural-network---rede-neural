library("MASS")
library("rpart")
set.seed(0)

data <- Boston

head(data)

data[is.na(data)==TRUE]

## Train-test split

train_test_split_index <- 0.8*nrow(data)

train <- data.frame(data[1:train_test_split_index,])
test <- data.frame(data[(train_test_split_index+1):nrow(data),])

#CART
fit_tree <- rpart(medv~.,method = "anova",data = train)
tree_predict <- predict(fit_tree,test)
mse_tree <- mean((tree_predict-test$medv)^2)

# Neural network 
set.seed(0)

max_data <- apply(data, 2, max)
min_data <- apply(data, 2, min)

# min max scale
# Formula: (x-min_x)/(max_x - min_x)
set.seed(0)
scaled <- scale(data, center = min_data, scale = max_data - min_data)

index <- sample(1:nrow(data),round(0.80*nrow(data)))
train_data <- as.data.frame(scaled[index,])
test_data <- as.data.frame(scaled[-index,])

library(neuralnet)

nn <- neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat, 
                data = train_data,
                hidden = c(5,4,3,2),
                linear.output = T)
plot(nn)

pr.nn <- compute(nn,test[,1:13])
pr.nn$net.result
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_data$medv)*(max(data$medv)-min(data$medv))+min(data$medv)  

MSE_nn <- mean((pr.nn_ - test.r)^2)


#### Exercicio 1
library(ISLR)
library(neuralnet)
data <- College

private <- ifelse(data$Private== 'Yes', 1, 0)

# normalizar os dados
data <- data[,2:18]
max_data <- apply(data, 2, max)
min_data <- apply(data, 2, min)
scaled <- data.frame(scale(data,center = min_data, scale = max_data - min_data))

scaled$Private <- private





