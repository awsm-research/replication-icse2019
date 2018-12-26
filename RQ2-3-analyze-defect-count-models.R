library(ggplot2)
library(Rnalytica)
library(effsize)
library(gridExtra)
library(grid)
library(Hmisc)
### rq2.sanitycheck / rq2 ###
rq2.sanitycheck <- NULL
rq2 <- NULL
rq3 <- NULL
rq.ranking <- NULL
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
    models <- c("lm.noisy","lm.clean","rfr.noisy","rfr.clean")
    for(model in models){
      mae <- mean(abs(result$oracle.number - result[,model]))
      mdae <- median(abs(result$oracle.number - result[,model]))
      spearman <- as.numeric(spearman(result$oracle.number,result[,model]))
      guess <- sample(min(unique(result$oracle.number)):max(unique(result$oracle.number)), length(result$oracle.number), replace=TRUE)
      mae_guess <- mean(abs(result$oracle.number - guess))
      sa <- (1-(mae/mae_guess))*100
      perf <- rbind(perf, c(model = model, mae=mae, mdae=mdae, spearman=spearman, sa=sa))
    }
    
    # compute p@20, r@20, IFA, effort
    for(model in models){
      result.sorted <- result[order(-result[,model]),]
      result.sorted$cum_loc <- cumsum(result.sorted$LOC)
      
      totalbugs <- sum(result.sorted$oracle.binary == 1)
      top20 <- result.sorted[result.sorted$cum_loc < max(result.sorted$cum_loc)*0.2,]
      
      p20 <- ifelse(nrow(top20)==0,0,nrow(top20[top20$oracle.binary == 1,])/nrow(top20))
      r20 <- nrow(top20[top20$oracle.binary == 1,])/totalbugs

      rq.ranking <- rbind(rq.ranking, c(project=project, model = model, p20=p20, r20=r20))
    }
  }

  perf <- data.frame(perf)
  perf[,2:5] <- lapply(perf[,2:5],function(x) as.numeric(as.character(x)))
  
  # rq2.sanitycheck - sanity check
  models <- c("lm.noisy","lm.clean","rfr.noisy","rfr.clean")
  for(model in models){
    for(measure in c("mae","mdae", "spearman","sa")){
      rq2.sanitycheck <- rbind(rq2.sanitycheck, c(project=project, model=model, measure=measure, value=mean(perf[perf$model == model,measure])))  
    }
  }
  # rq2 - difference for each bootstrap sample
  for(measure in c("mae","mdae", "spearman","sa")){
    rq2 <- rbind(rq2, c(project=project, classifier="LM", measure=measure, value=mean(perf[perf$model == "lm.clean",measure]-perf[perf$model == "lm.noisy",measure])))
    rq2 <- rbind(rq2, c(project=project, classifier="RF", measure=measure, value=mean(perf[perf$model == "rfr.clean",measure]-perf[perf$model == "rfr.noisy",measure])))
  }
}

rq.ranking <- data.frame(rq.ranking)
rq.ranking[,3:4] <- lapply(rq.ranking[,3:4],function(x) as.numeric(as.character(x)))
saveRDS(rq.ranking, file="results/ranking-values-defect-count.rds")

# rq3 - performance difference
rq3 <- NULL
for(project in projects){
  rq3 <- rbind(rq3, cbind(project=project, classifier="LM", measure="p20", 
                          value=mean(rq.ranking[rq.ranking$project == project & rq.ranking$model=="lm.clean",]$p20-rq.ranking[rq.ranking$project == project & rq.ranking$model=="lm.noisy",]$p20) ))
  rq3 <- rbind(rq3, cbind(project=project, classifier="LM", measure="r20", 
                          value=mean(rq.ranking[rq.ranking$project == project & rq.ranking$model=="lm.clean",]$r20-rq.ranking[rq.ranking$project == project & rq.ranking$model=="lm.noisy",]$r20) ))
  rq3 <- rbind(rq3, cbind(project=project, classifier="RF", measure="p20", 
                          value=mean(rq.ranking[rq.ranking$project == project & rq.ranking$model=="rfr.clean",]$p20-rq.ranking[rq.ranking$project == project & rq.ranking$model=="rfr.noisy",]$p20) ))
  rq3 <- rbind(rq3, cbind(project=project, classifier="RF", measure="r20", 
                          value=mean(rq.ranking[rq.ranking$project == project & rq.ranking$model=="rfr.clean",]$r20-rq.ranking[rq.ranking$project == project & rq.ranking$model=="rfr.noisy",]$r20) ))
}

rq2.sanitycheck <- data.frame(rq2.sanitycheck)
rq2.sanitycheck$value <- as.numeric(as.character(rq2.sanitycheck$value))
saveRDS(rq2.sanitycheck, file="results/performance-values-defect-count.rds")

rq2 <- data.frame(rq2)
rq2$value <- as.numeric(as.character(rq2$value))
saveRDS(rq2, file="results/performance-diff-defect-count.rds")

rq3 <- data.frame(rq3)
rq3$value <- as.numeric(as.character(rq3$value))
saveRDS(rq3, file="results/ranking-diff-defect-count.rds")


################################################################################

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right", "top")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "top" = arrangeGrob(legend,
                                         do.call(arrangeGrob, gl),
                                         ncol = 1,
                                         heights = unit.c(lheight, unit(1, "npc") - lheight)),
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  # return gtable invisibly
  invisible(combined)
}



#######################################################
# rq2.sanitycheck
rq2.sanitycheck <- readRDS(file="results/performance-values-defect-count.rds")

levels(rq2.sanitycheck$model) <- c("LM_real","LM_heu","RFR_real","RFR_heu")
levels(rq2.sanitycheck$measure) <- c("MAE","MdAE","SA","Spearman")

g1 <- ggplot(rq2.sanitycheck[rq2.sanitycheck$measure %in% c("MAE"),], aes(x=measure, y=value, fill=model)) + geom_boxplot() + ylab("") + xlab("") + theme_bw() + theme(legend.position="top", legend.title=element_blank()) + scale_y_continuous(limit=c(0,1), breaks=0:5*0.2) + scale_fill_brewer()
g3 <- ggplot(rq2.sanitycheck[rq2.sanitycheck$measure %in% c("SA"),], aes(x=measure, y=value, fill=model)) + geom_boxplot() + ylab("") + xlab("") + theme_bw() + theme(legend.position="top", legend.title=element_blank()) + scale_y_continuous(limit=c(75,100), breaks=c(75,80,85,90,95,100)) + theme(legend.position = "none") + scale_fill_brewer()

pdf(file="figures/figure6-a.pdf", width=3, height=4)
grid_arrange_shared_legend(g1, g3, ncol = 2, nrow = 1, position="top")
dev.off()

for(classifier in c("LM","RFR")){
  for(measure in c("MAE","SA")){
    heu <- paste0(classifier,"_heu")
    real <- paste0(classifier,"_real")
    
    p <- wilcox.test(rq2.sanitycheck[rq2.sanitycheck$model==heu & rq2.sanitycheck$measure==measure ,]$value, rq2.sanitycheck[rq2.sanitycheck$model==real & rq2.sanitycheck$measure==measure ,]$value, paired = TRUE)$p.value
    estimate <- cliff.delta(rq2.sanitycheck[rq2.sanitycheck$model==heu & rq2.sanitycheck$measure==measure ,]$value, rq2.sanitycheck[rq2.sanitycheck$model==real & rq2.sanitycheck$measure==measure ,]$value, paired = TRUE)$estimate
    magnitude <- as.character(cliff.delta(rq2.sanitycheck[rq2.sanitycheck$model==heu & rq2.sanitycheck$measure==measure ,]$value, rq2.sanitycheck[rq2.sanitycheck$model==real & rq2.sanitycheck$measure==measure ,]$value, paired = TRUE)$magnitude)
    print(paste(classifier, measure, round(p, digits=2), round(estimate, digits=2), magnitude))
  }
}


rq.ranking <- readRDS("results/ranking-values-defect-count.rds")
for(classifier in c("lm","rfr")){
  for(measure in c("p20","r20")){
    heu <- paste0(classifier,".noisy")
    real <- paste0(classifier,".clean")
    p <- wilcox.test(rq.ranking[rq.ranking$model==heu,measure], rq.ranking[rq.ranking$model==real,measure], paired = TRUE)$p.value
    cliff <- cliff.delta(rq.ranking[rq.ranking$model==heu,measure], rq.ranking[rq.ranking$model==real,measure], paired = TRUE)
    print(paste(classifier, measure, round(p, digits=2), round(cliff$estimate, digits=2), as.character(cliff$magnitude)))
  }
}


#######################################################
# rq2
rq2 <- readRDS(file="results/performance-diff-defect-count.rds")
levels(rq2$measure) <- c("MAE","MdAE","SA","Spearman")
levels(rq2$classifier) <- c("LM","RF")
g1 <- ggplot(rq2[rq2$measure %in% c("MAE"),], aes(x=measure, y=value, fill=classifier)) + geom_boxplot() + coord_cartesian(ylim=c(-.4,0.4))  + scale_fill_brewer() + theme_bw() + ylab("") + xlab("")  + geom_hline(yintercept=0, color="red", linetype="dashed") + theme(legend.position="top", legend.title=element_blank())
g2 <- ggplot(rq2[rq2$measure %in% c("SA"),], aes(x=measure, y=value, fill=classifier)) + geom_boxplot() + coord_cartesian(ylim=c(-10,10))  + scale_fill_brewer() + theme_bw() + ylab("") + xlab("") + geom_hline(yintercept=0, color="red", linetype="dashed")  + theme(legend.position="top", legend.title=element_blank())
pdf(file="figures/figure7-a.pdf", width=3, height=4.5)
grid_arrange_shared_legend(g1, g2, ncol = 2, nrow = 1, position="top")
dev.off()

summary(rq2[rq2$classifier=="LM" & rq2$measure == "MAE","value"])
summary(rq2[rq2$classifier=="RF" & rq2$measure == "MAE","value"])

summary(rq2[rq2$classifier=="LM" & rq2$measure == "SA","value"])
summary(rq2[rq2$classifier=="RF" & rq2$measure == "SA","value"])

#######################################################
# rq3
rq3 <- readRDS(file="results/ranking-diff-defect-count.rds")
rq3 <- rbind(rq3, rq2[rq2$measure=="Spearman",])
rq3$measure <- factor(rq3$measure)
levels(rq3$measure) <- c("P@20%LOC","R@20%LOC","Spearman")
ggplot(rq3, aes(x=measure, y=value, fill=classifier)) + geom_boxplot() + coord_cartesian(ylim=c(-0.3,0.4))  + ylab("") + xlab("") + theme_bw() + theme(legend.position="top", legend.title=element_blank()) + scale_fill_brewer() + geom_hline(yintercept=0, color="red", linetype="dashed")
ggsave(file="figures/figure9-a.pdf", width=3.2, height=3.2)

summary(rq3[rq3$classifier=="LM" & rq3$measure == "P@20%LOC","value"])
summary(rq3[rq3$classifier=="RF" & rq3$measure == "P@20%LOC","value"])

summary(rq3[rq3$classifier=="LM" & rq3$measure == "R@20%LOC","value"])
summary(rq3[rq3$classifier=="RF" & rq3$measure == "R@20%LOC","value"])

summary(rq3[rq3$classifier=="LM" & rq3$measure == "Spearman","value"])
summary(rq3[rq3$classifier=="RF" & rq3$measure == "Spearman","value"])
