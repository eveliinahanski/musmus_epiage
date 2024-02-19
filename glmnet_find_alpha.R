# Find optimal alpha for CLOCK 1 (one that is used only for lab mice), using code from this tutorial: https://rpubs.com/jmkelly91/881590 Running through it 10 times.

# Create an empty dataframe to store results
final_results <- data.frame()

# Loop through 10 runs
for (run in 1:10) {
  # Set a unique and random seed for each run
  set.seed(sample.int(1e6, 1))
  
  colnames(training_data)
  search <- training_data[,c(30:102)] # cpg to be included
  rownames(search) <- 1:50
  pred <- as.matrix(search)
  n <- 50
  dv <- (rowSums(pred[1:n, 1:10]) + 
           0.8 * rowSums(pred[1:n, 11:20]) + 
           0.6 * rowSums(pred[1:n, 21:30]) + 
           0.4 * rowSums(pred[1:n, 31:40]) +
           0.2 * rowSums(pred[1:n, 41:50]) + rnorm(n))
  pred <- scale(pred) 
  
  # Train-test Split
  search <- na.omit(search)
  train_rows <- sample(nrow(search), .7*50, replace = FALSE)
  pred.train <- pred[train_rows,]
  dv.train <- dv[train_rows]
  pred.test <- pred[-train_rows,]
  dv.test <- dv[-train_rows]
  
  # Ridge
  ridge <- cv.glmnet(x=pred.train, y=dv.train, type.measure="mse", 
                     alpha=0, family="gaussian", nlambda=200)
  ridge.predicted <- predict(ridge, ridge$lambda.1se, new=pred.test)
  ridge_mse <- mean((dv.test - ridge.predicted)^2)
  
  # Lasso
  lasso <- cv.glmnet(x=pred.train, y=dv.train, type.measure="mse", 
                     alpha=1, family="gaussian", nlambda=200)
  lasso.predicted <- predict(lasso, lasso$lambda.1se, new=pred.test)
  lasso_mse <- mean((dv.test - lasso.predicted)^2)
  
  # Elastic Net
  models <- list()
  for (i in 0:20) {
    name <- paste0("alpha", i/20)
    models[[name]] <- cv.glmnet(pred.train, dv.train, type.measure="mse", alpha=i/20, 
                                family="gaussian")
  }
  
  elastic_net_results <- data.frame()
  for (i in 0:20) {
    name <- paste0("alpha", i/20)
    predicted <- predict(models[[name]], s=models[[name]]$lambda.1se, newx=pred.test)
    mse <- mean((dv.test - predicted)^2)
    temp <- data.frame(alpha=i/20, mse=mse, name=name)
    elastic_net_results <- rbind(elastic_net_results, temp)
  }
  
  # Combine results for the current run
  run_results <- data.frame(Run = run, Ridge = ridge_mse, Lasso = lasso_mse, elastic_net_results)
  
  # Append the results to the final dataframe
  final_results <- rbind(final_results, run_results)
}
