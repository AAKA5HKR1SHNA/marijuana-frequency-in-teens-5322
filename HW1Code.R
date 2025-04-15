#Load libraries
library(tidyverse)
library(tree)
library(randomForest)
library(gbm)

#Load data
load('youth_data.Rdata')

#Create training and test data sets

set.seed(2)
drug <- df %>% filter(MRJMDAYS!=5) %>% select(IRSEX, NEWRACE2, HEALTH2, EDUSCHLGO, INCOME, GOVTPROG, POVERTY3, PDEN10, COUTYP4, IRMJAGE, IRMJFY, IRMJFM, MRJYDAYS, MRJMDAYS)
train <- sample(1:nrow(drug), 500)
drug.test <- drug[-train,]
drug.train <- drug[train,]

#_____________________________________________

#Among youths who report having using marijuana in the last month,
#what factors affect the frequency of their usage?

# binary - Is their tendency to use marijuana correlated to them receiving government aid?
# classification - What demographic factors affect the category of marijuana use they fall under (in the last 30 days)?
# regression - irmjfy - What affects number of days they use marijuana in a year?

#_____________________________________________

#Binary classification (has or has not received govt. aid)

#Creating a basic tree

govt.tree <- tree(GOVTPROG ~ MRJYDAYS + MRJMDAYS + IRMJFY + IRMJAGE, data = drug, subset = train)

summary(govt.tree)

plot(govt.tree)
text(govt.tree, pretty = 1)

#Performing cross-validation to determine optimal tree size

govt.cv <- cv.tree(govt.tree)
names(govt.cv)

#Based on this, our optimal tree has four nodes.
#However, our existing tree has less nodes. In the interest of full transparency, I'm not sure what this means, so I will leave it as is for now.

#Finding test error of model:

govt.pred <- predict(govt.tree, drug.test, type = 'class')
confmat <- table(predicted = govt.pred, actual = drug.test[['GOVTPROG']])

confmat

1 - ((confmat[1,1] + confmat[2,2])/ sum(confmat))

#Our test error is roughly 0.341, which is lower than our training error.

#_______________________________________________

#Multi-class classification (predicting marijuana usage category in the last month)

#Creating a boosting model

set.seed(2)
boost.mj <- gbm(MRJMDAYS ~ . -IRMJFM -IRMJFY -IRMJAGE -MRJYDAYS, data = drug.train, distribution = "multinomial", n.trees = 1000, interaction.depth = 4, shrinkage = 0.05)

summary(boost.mj)

#From this, we can tell the most influential factors for the creation of the model were race and overall health.

#______________________________________________

#Regression (predicting number of days of marijuana use in the last year)

#Creating a bagging (randomForest) model

mj.bag <- randomForest(IRMJFY ~ . -IRMJFM -IRMJFY -IRMJAGE -MRJYDAYS, data = drug.train, mtry = 3, importance = TRUE)
mj.bag

#Trying different values of mtry resulted in choosing mtry = 3 as it explained the most variance in the data.

#Looking at accuracy of model.

mj.pred <- predict(mj.bag, drug.test)

#Mean squared error
mean((drug.test$IRMJFY - mj.pred)^2)

#From this, we can tell that the test error is extremely high.

#_____________________________________________

