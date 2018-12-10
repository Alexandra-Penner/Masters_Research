# Analysis for NFL project

library(MLmetrics)
library(stargazer)
library(ggplot2)
library(gbm)
library(mlr)


# read data
gamedf <- read.csv("gamedata.csv")

# remove identifiers
gamedf <- gamedf[,-c(1,2,17)]

# remove NA
# 2 instances were missing the win output for an unknown reason
gamedf <- gamedf[!is.na(gamedf$win),]


set.seed(12345)
t <- sample(c(1:nrow(gamedf)), nrow(gamedf)/5, replace = F)

test <- gamedf[t,]
train <- gamedf[-t,]

model <- glm(win~., family = binomial(link="logit"), data = train)

summary(model)

pred <- predict(model, test, type = "response")
pred <- ifelse(pred<0.5, 0, 1)
truth <- as.vector(test$win)


F1_Score(truth, pred)

# tables and charts
stargazer(model, font.size = "scriptsize")

ggplot(train, aes(x=HomeFieldpos, y=win)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 

ggplot(train, aes(x=HomePointsPerTrip, y=win)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 


# Boosting version
set.seed(8674309)
# make target var a factor
train$win <- as.factor(train$win)
test$win <-as.factor(test$win)

# task
MLdata <- makeClassifTask(id = "gbmtask", data = train, target = "win")

# CV folds
CVfolds <- makeResampleDesc(method = "CV", iters = 5L)

# tuning strategy
tuneMethod <- makeTuneControlRandom(maxit = 250L)

# Learners
gbmLearner <- makeLearner("classif.gbm", distribution = "adaboost")


# hyper parameters
gbmParam <- makeParamSet(makeIntegerParam("n.trees",lower=10L,upper=1000L),
                         makeIntegerParam("interaction.depth",lower=3L,upper=10L),
                         makeIntegerParam("n.minobsinnode",lower=5L,upper=20L),
                         makeNumericParam("shrinkage", lower=0.001, upper=0.1))

GBMtune <- tuneParams(learner = gbmLearner,
                      task = MLdata,
                      resampling = CVfolds,
                      par.set = gbmParam,
                      control = tuneMethod)

TunedGBM <- setHyperPars(learner=gbmLearner, par.vals = GBMtune$x)

# Verify performance on cross validated sample sets
resample(learner = TunedGBM,
         task = MLdata,
         resampling = CVfolds,
         measures=list(f1,acc,fpr,fnr))

# Train the final model
finalModel <- train(learner = TunedGBM, task = MLdata)

FinalGBM <- gbm(win~., 
                  distribution = "adaboost",
                  data = train,
                  n.trees=630, 
                  interaction.depth=5, 
                  n.minobsinnode=7, 
                  shrinkage=0.0139)

# Predictio 
prediction <- predict(finalModel, newdata = test)

# Performance
print(performance(prediction, measures = list(f1,acc,fpr,fnr)))

summary.gbm(FinalGBM)
plot.gbm(FinalGBM, i.var = 12, type="link")


