
set.seed(10)

n <- 200

x <- rnorm(n, 5, 1)

# true relationship
y <- 2*x + rnorm(n, 0, 1)

dat <- data.frame(
  y,
  x
)

for(i in 1:80){
  dat[[paste0("z",i)]] <- rnorm(n, 5, 1)
}

# Training and testing data 
train_id <- sample(1:n, n/2)

train <- dat[train_id, ]
test  <- dat[-train_id, ]

# Simple model
m_simple <- lm(y ~ x, data = train)
summary(m_simple)

# Prediction error (training)
pred_train_simple <- predict(m_simple, train)
mean((train$y - pred_train_simple)^2)

# Prediction error (testing)
pred_test_simple <- predict(m_simple, test)
mean((test$y - pred_test_simple)^2)

# Overfit model
m_overfit <- lm(y ~ .,
                data = train)
summary(m_overfit)

# Training error
pred_train_overfit <- predict(m_overfit, train)
mean((train$y - pred_train_overfit)^2)

# Testing error
pred_test_overfit <- predict(m_overfit, test)
mean((test$y - pred_test_overfit)^2)

# Plot the predicted relationship between the simple model and the overfit model

# Simple model
plot(train$y ~ train$x)
lines(pred_train_simple ~ train$x)

# Overfit model
plot(train$y ~ train$x)
lines(pred_train_overfit ~ train$x)

