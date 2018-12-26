library(ggplot2)
library(Rnalytica)
library(plyr)
library(effsize)

rq2.sanitycheck <- NULL
rq2 <- NULL
rq3 <- NULL
rq3.ranking <- NULL
projects <- listDataset[listDataset$corpus == "jira",]$system

for(project in projects){
  print(project)
  data <- data.frame(readRDS(file=paste0("models/",project,".Rds")))
  
  col <- c("boot_id","glm.noisy","glm.clean","lm.noisy","lm.clean",
           "rfc.noisy","rfc.clean","rfr.noisy","rfr.clean","oracle.number","LOC")
  data[,col] <- lapply(data[,col],function(x) as.numeric(as.character(x)))
  data$oracle.binary <- as.logical(as.character(data$oracle.binary))

  perf <- NULL
  for(boot_id in 1:100){
    result <- data[data$boot_id==boot_id,]
    
    # accuracy for each model
    models <- c("glm.noisy","glm.clean","rfc.noisy","rfc.clean")
    for(model in models){
      result.sorted <- result[order(-result[,model]),]
      result.sorted$cum_loc <- cumsum(result.sorted$LOC)
      
      perf <- rbind(perf, c(model = model, c(performance.calculation(result$oracle.binary, result[,model]))))
    
      # compute p@20, r@20
      result.sorted <- result[order(-result[,model]),]
      result.sorted$cum_loc <- cumsum(result.sorted$LOC)
      
      totalbugs <- sum(result.sorted$oracle.binary == 1)
      top20 <- result.sorted[result.sorted$cum_loc < max(result.sorted$cum_loc)*0.2,]
      
      p20 <- ifelse(nrow(top20)==0,0,nrow(top20[top20$oracle.binary == 1,])/nrow(top20))
      r20 <- nrow(top20[top20$oracle.binary == 1,])/totalbugs

      rq3.ranking <- rbind(rq3.ranking, c(project=project, model = model, p20=p20, r20=r20))
    }
  }
  
  perf <- data.frame(perf)
  perf[,2:16] <- lapply(perf[,2:16],function(x) as.numeric(as.character(x)))
  perf[is.na(perf)] <- 0
  # rq2.sanitycheck - sanity check
  models <- c("glm.noisy","glm.clean","rfc.noisy","rfc.clean")
  for(model in models){
    for(measure in c("AUC","Precision","Recall","Fmeasure")){
      rq2.sanitycheck <- rbind(rq2.sanitycheck, c(project=project, model=model, measure=measure, value=mean(perf[perf$model == model,measure])))  
    }
  }
  # rq2 - performance difference
  for(measure in c("AUC","Precision","Recall","Fmeasure")){
    rq2 <- rbind(rq2, c(project=project, classifier="GLM", measure=measure, value=mean(perf[perf$model == "glm.clean",measure]-perf[perf$model == "glm.noisy",measure])))
    rq2 <- rbind(rq2, c(project=project, classifier="RF", measure=measure, value=mean(perf[perf$model == "rfc.clean",measure]-perf[perf$model == "rfc.noisy",measure])))
  }
}

rq3.ranking <- data.frame(rq3.ranking)
rq3.ranking[,3:4] <- lapply(rq3.ranking[,3:4],function(x) as.numeric(as.character(x)))
saveRDS(rq3.ranking, file="results/ranking-values-defect-classification.rds")

rq2.sanitycheck <- data.frame(rq2.sanitycheck)
rq2.sanitycheck$value <- as.numeric(as.character(rq2.sanitycheck$value))
saveRDS(rq2.sanitycheck, file="results/performance-values-defect-classification.rds")

########### Compute Performance Difference ###########

# rq3 - performance difference for ranking measures
rq3 <- NULL
for(project in projects){
  rq3 <- rbind(rq3, cbind(project=project, classifier="GLM", measure="p20", 
                      value=mean(rq3.ranking[rq3.ranking$project == project & rq3.ranking$model=="glm.clean",]$p20-rq3.ranking[rq3.ranking$project == project & rq3.ranking$model=="glm.noisy",]$p20) ))
  rq3 <- rbind(rq3, cbind(project=project, classifier="GLM", measure="r20", 
                      value=mean(rq3.ranking[rq3.ranking$project == project & rq3.ranking$model=="glm.clean",]$r20-rq3.ranking[rq3.ranking$project == project & rq3.ranking$model=="glm.noisy",]$r20) ))
  
  rq3 <- rbind(rq3, cbind(project=project, classifier="RF", measure="p20", 
                          value=mean(rq3.ranking[rq3.ranking$project == project & rq3.ranking$model=="rfc.clean",]$p20-rq3.ranking[rq3.ranking$project == project & rq3.ranking$model=="rfc.noisy",]$p20) ))
  rq3 <- rbind(rq3, cbind(project=project, classifier="RF", measure="r20", 
                          value=mean(rq3.ranking[rq3.ranking$project == project & rq3.ranking$model=="rfc.clean",]$r20-rq3.ranking[rq3.ranking$project == project & rq3.ranking$model=="rfc.noisy",]$r20) ))
}

rq2 <- data.frame(rq2)
rq2$value <- as.numeric(as.character(rq2$value))
saveRDS(rq2, file="results/performance-diff-defect-classification.rds")

rq3 <- data.frame(rq3)
rq3$value <- as.numeric(as.character(rq3$value))
saveRDS(rq3, file="results/ranking-diff-defect-classification.rds")


########### Analyze Results ###########

# rq2.sanitycheck
rq2.sanitycheck <- readRDS(file="results/performance-values-defect-classification.rds")
levels(rq2.sanitycheck$model) <- c("GLM_real","GLM_heu","RFC_real","RFC_heu")
rq2.sanitycheck$measure <- factor(rq2.sanitycheck$measure, levels=c("AUC","Precision","Recall","Fmeasure"))
ggplot(rq2.sanitycheck[rq2.sanitycheck$measure %in% c("AUC","Precision","Recall","Fmeasure"),], aes(x=measure, y=value, fill=model)) + geom_boxplot() + ylab("") + xlab("") + theme_bw() + theme(legend.position="top", legend.title=element_blank()) + scale_y_continuous(limit=c(0,1), breaks=0:5*0.2) + scale_fill_brewer()
ggsave(file="figures/figure6-b.pdf", width=4, height=3.6)


for(classifier in c("GLM","RFC")){
  for(measure in c("AUC","Precision","Recall","Fmeasure")){
    heu <- paste0(classifier,"_heu")
    real <- paste0(classifier,"_real")
    
    p <- wilcox.test(rq2.sanitycheck[rq2.sanitycheck$model==heu & rq2.sanitycheck$measure==measure ,]$value, rq2.sanitycheck[rq2.sanitycheck$model==real & rq2.sanitycheck$measure==measure ,]$value, paired = TRUE)$p.value
    estimate <- cliff.delta(rq2.sanitycheck[rq2.sanitycheck$model==heu & rq2.sanitycheck$measure==measure ,]$value, rq2.sanitycheck[rq2.sanitycheck$model==real & rq2.sanitycheck$measure==measure ,]$value, paired = TRUE)$estimate
    magnitude <- as.character(cliff.delta(rq2.sanitycheck[rq2.sanitycheck$model==heu & rq2.sanitycheck$measure==measure ,]$value, rq2.sanitycheck[rq2.sanitycheck$model==real & rq2.sanitycheck$measure==measure ,]$value, paired = TRUE)$magnitude)
    print(paste(classifier, measure, round(p, digits=2), round(estimate, digits=2), magnitude))
  }
}

rq3.ranking <- readRDS("results/ranking-values-defect-classification.rds")
rq3.ranking$p20 <- as.numeric(as.character(rq3.ranking$p20))
rq3.ranking$r20 <- as.numeric(as.character(rq3.ranking$r20))

for(classifier in c("glm","rfc")){
  for(measure in c("p20","r20")){
    heu <- paste0(classifier,".noisy")
    real <- paste0(classifier,".clean")
    p <- wilcox.test(rq3.ranking[rq3.ranking$model==heu,measure], rq3.ranking[rq3.ranking$model==real,measure], paired = TRUE)$p.value
    cliff <- cliff.delta(rq3.ranking[rq3.ranking$model==heu,measure], rq3.ranking[rq3.ranking$model==real,measure], paired = TRUE)
    print(paste(classifier, measure, round(p, digits=2), round(cliff$estimate, digits=2), as.character(cliff$magnitude)))
  }
}



# rq2
rq2 <- readRDS(file="results/performance-diff-defect-classification.rds")
rq2$measure <- factor(rq2$measure, levels=c("AUC","Precision","Recall","Fmeasure"))
ggplot(rq2[rq2$measure %in% c("AUC","Precision","Recall","Fmeasure"),], aes(x=measure, y=value, fill=classifier)) + geom_boxplot() + coord_cartesian(ylim=c(-0.4,0.4)) + scale_y_continuous(breaks=-4:4*0.1) + ylab("") + xlab("") + theme_bw() + theme(legend.position="top", legend.title=element_blank()) + scale_fill_brewer() + geom_hline(yintercept=0, color="red", linetype="dashed")
ggsave(file="figures/figure7-b.pdf", width=5, height=5)

summary(rq2[rq2$classifier=="GLM" & rq2$measure == "AUC","value"])
summary(rq2[rq2$classifier=="RF" & rq2$measure == "AUC","value"])

summary(rq2[rq2$classifier=="GLM" & rq2$measure == "Fmeasure","value"])
summary(rq2[rq2$classifier=="RF" & rq2$measure == "Fmeasure","value"])


# rq3
rq3 <- readRDS(file="results/ranking-diff-defect-classification.rds")
levels(rq3$measure) <- c("P@20%LOC","R@20%LOC")
ggplot(rq3[rq3$measure %in% c("P@20%LOC","R@20%LOC"),], aes(x=measure, y=value, fill=classifier)) + geom_boxplot() + coord_cartesian(ylim=c(-0.3,0.4))  + ylab("") + xlab("") + theme_bw() + theme(legend.position="top", legend.title=element_blank()) + scale_fill_brewer() + geom_hline(yintercept=0, color="red", linetype="dashed")
ggsave(file="figures/figure9-b.pdf", width=3.2, height=3.2)

summary(rq3[rq3$classifier=="GLM" & rq3$measure == "P@20%LOC","value"])
summary(rq3[rq3$classifier=="RF" & rq3$measure == "P@20%LOC","value"])

summary(rq3[rq3$classifier=="GLM" & rq3$measure == "R@20%LOC","value"])
summary(rq3[rq3$classifier=="RF" & rq3$measure == "R@20%LOC","value"])

