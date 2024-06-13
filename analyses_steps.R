#!/usr/bin/env Rscript

## LIBRARIES
library(caret)
library(glmnet)
library(ISLR)
library(MASS)

## 1. LOAD THE DATA
the.data <- read.csv('data/monodta.txt',header=TRUE,sep=" ")
the.data <- tibble::as_tibble(the.data)
str(the.data)

## SELECT ONLY THE COLUMNS WITH PRS
the.data.prs <- the.data[,c(2:23)]
head(the.data.prs)
str(the.data.prs)

## SPLIT DATA
smp_size <- floor(0.80 * nrow(the.data.prs))
set.seed(2000)
train_ind <- sample(seq_len(nrow(the.data.prs)), size = smp_size)
the.data.train <- the.data.prs[train_ind, ]
the.data.test <- the.data.prs[-train_ind, ]

## TRAINING
cv_5 = trainControl(method = "cv", number = 5)
hit_elnet = train(
    mono ~ ., data = the.data.train,
    method = "glmnet",
    trControl = cv_5
)

hit_elnet

hit_elnet_int = train(
    mono ~ . ^ 2, data = the.data.train,
    method = "glmnet",
    trControl = cv_5,
    tuneLength = 10
)

get_best_result = function(caret_fit) {
    best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
    best_result = caret_fit$results[best, ]
    rownames(best_result) = NULL
    best_result
}

get_best_result(hit_elnet_int)

## TESTING
set.seed(2000)
X = model.matrix(mono ~ . ^ 2, the.data.test)[, -1]
y = the.data.test$mono

fit_lasso_cv = cv.glmnet(X, y, alpha = 1)
sqrt(fit_lasso_cv$cvm[fit_lasso_cv$lambda == fit_lasso_cv$lambda.min]) # CV-RMSE minimum

coef(fit_lasso_cv)

sum(coef(fit_lasso_cv) != 0)

sum(coef(fit_lasso_cv) == 0)


#####
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = TRUE)


en <- train(mono~.,
            the.data.train,
            method='glmnet',
            tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                  lambda = seq(0.0001,0.2,length=5)),
            trControl=custom)
en

mean(en$resample$RMSE)

plot(en, main = "Elastic Net Regression")

plot(varImp(en,scale=TRUE))

## the.data <- as.data.frame(the.data)
## the.data.pheno <- the.data[,c(1:2)]
## head(the.data.pheno)
## the.data.cov <- the.data[,c(1,24:25)]
## head(the.data.cov)
## the.data.pca <- the.data[,c(1,26:33)]
## head(the.data.pca)
