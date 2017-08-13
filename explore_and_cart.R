library(dplyr)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
library(corrplot)
library(ggplot2)
library(readr)
library(e1071)

hr = read_csv('Hr_comma_sep.csv')

# transform char to factor
df = hr %>% 
  mutate(left = factor(left, labels = c('Remain', 'Left')), 
         salary = ordered(salary, levels = c('low', 'medium', 'high')))

# split train-test
splt = sample.split(df$left, SplitRatio = 0.7)
train = subset(df, splt == T)
test = subset(df, splt == F)



# explore with graphs
names(df)

# a distribution of employees' years of service
ggplot(data = df, aes(x = time_spend_company, fill = left)) +
  geom_histogram(binwidth = 1, alpha = 0.5, col = 'white', position = 'identity') +
  scale_x_continuous(breaks = seq(0,11))
ggsave('plot1.png', units = 'mm', height = 150, width = 150)

# average_montly_hours
ggplot(data = df, aes(x = average_montly_hours, fill = left)) +
  geom_histogram(binwidth = 10, alpha = 0.5, col = 'white', position = 'identity')
ggsave('plot2.png', units = 'mm', height = 150, width = 150)

# average_montly_hours by sales and salary
ggplot(data = df , aes(average_montly_hours)) +
  geom_density(fill = "pink") +
  facet_grid(sales ~ salary) +
  theme(axis.text.y = element_blank())
ggsave('plot3.png', units = 'mm', height = 200, width = 150)

# last_evaluation vs satisfaction_level
ggplot(data = df , aes(x = last_evaluation, y = satisfaction_level)) +
  geom_point(alpha = 0.1, col = 'orange', position = position_jitter()) +
  facet_wrap(~left)
ggsave('plot4.png', units = 'mm', height = 120, width = 200)

# correlation
cor(df[,1:6])
corrplot(corr = cor(df[,1:6]))


# CART with cv
tr.control = trainControl(method='cv', number=10)
cp.grid = expand.grid(.cp = seq(0,0.002,0.0001))
tr = train(left ~., data=train, method = 'rpart', trControl = tr.control, tuneGrid = cp.grid)
tr #-> cp = 0.0013
model = rpart(left ~ ., data = train, method = 'class', cp=0.0013)
prp(model)
pred = predict(model, newdata = test, type = 'prob')

# ROC
predROCR = prediction(pred[,2], as.numeric(test$left) - 1)
perfROCR = performance(predROCR, 'tpr', 'fpr')
plot(perfROCR) 

# AUC
as.numeric(performance(predROCR, 'auc')@y.values) #-> 0.9754466
