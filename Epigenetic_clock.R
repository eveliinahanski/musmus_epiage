set.seed(3648)
training_data
model0 <- cv.glmnet(y = training_data[,22], x = as.matrix(training_data[,30:102]), alpha = 1, nfolds = nrow(training_data), grouped = FALSE) # y is chronological age in days, x is cpg sites to be included.
model1 <- glmnet(y = training_data[,22], x = as.matrix(training_data[,30:102]), lambda = model0$lambda.min)
weights <- data.frame(coef.name = dimnames(coef(model1))[[1]], coef.value = matrix(coef(model1)))
all_cpgs <- as.list(weights$coef.name)
col.num <- which(colnames(training_data)%in%all_cpgs)
x <- training_data[,sort(c(col.num))]
x <- as.matrix(x)
predicted <- predict(model1, newx = x)
training_data$Epigenetic_age_days_Clock1 <- predicted