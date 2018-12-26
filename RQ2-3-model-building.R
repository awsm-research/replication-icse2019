library(Rnalytica)
library(randomForest)
library(doParallel)
registerDoParallel(cores=12)
  
o <- foreach(project=listDataset[listDataset$corpus == "jira",]$system, .combine=cbind) %dopar% {
# for(project in listDataset[listDataset$corpus == "jira",]$system){
  df <- NULL
  print(project)
  Data <- loadDefectDataset(project,"jira")
  data <- Data$data
  indep <- Data$indep
  indep <- AutoSpearman(data[,indep], indep)

  for(i in 1:100){
    #Generate a bootstrap sample with replacement
    set.seed(i)
    indices <- sample(nrow(data), replace = TRUE)
    
    #Generate training dataset using a bootstrap sample
    train_data <- data[indices,]
    
    #Generate testing dataset (instances not included in the bootstrap sample)
    test_data <- data[-unique(indices),]
    
    f.noisy <- as.formula(paste( "HeuBug", '~', paste(indep, collapse = "+")))
    glm.noisy <- glm(f.noisy, data = train_data, family = binomial)
    rfc.noisy <- randomForest(f.noisy, data = train_data)
    
    f.noisy <- as.formula(paste( "HeuBugCount", '~', paste(indep, collapse = "+")))
    lm.noisy <- lm(f.noisy, data = train_data)
    rfr.noisy <- randomForest(f.noisy, data = train_data)
    
    f.clean <- as.formula(paste( "RealBug", '~', paste(indep, collapse = "+")))
    glm.clean <- glm(f.clean, data = train_data, family = binomial)
    rfc.clean <- randomForest(f.clean, data = train_data)
    
    f.clean <- as.formula(paste( "RealBugCount", '~', paste(indep, collapse = "+")))
    lm.clean <- lm(f.clean, data = train_data)
    rfr.clean <- randomForest(f.clean, data = train_data)
    
    glm.noisy <- predict(glm.noisy, test_data[,indep], type = "response")
    glm.clean <- predict(glm.clean, test_data[,indep], type = "response")
    
    lm.noisy <- predict(lm.noisy, test_data[,indep])
    lm.clean <- predict(lm.clean, test_data[,indep])
    
    rfc.noisy <- predict(rfc.noisy, test_data[,indep], type = "prob")[,"TRUE"]
    rfc.clean <- predict(rfc.clean, test_data[,indep], type = "prob")[,"TRUE"]
    
    rfr.noisy <- predict(rfr.noisy, newdata=test_data[,indep])
    rfr.clean <- predict(rfr.clean, newdata=test_data[,indep])
    
    df <- rbind(df,
                 cbind(project = project, 
                   boot_id = i,
                   glm.noisy = glm.noisy,
                   glm.clean = glm.clean,
                   lm.noisy = lm.noisy,
                   lm.clean = lm.clean,
                   rfc.noisy = rfc.noisy,
                   rfc.clean = rfc.clean,
                   rfr.noisy = rfr.noisy,
                   rfr.clean = rfr.clean,
                   oracle.binary = as.character(test_data$RealBug),
                   oracle.number = test_data$RealBugCount,
                   LOC = test_data$CountLineCode,
                   filename = as.character(test_data$File)
                 ))
  }
  saveRDS(df, file=paste0("models/",project,".Rds"))
  return(project)
}
