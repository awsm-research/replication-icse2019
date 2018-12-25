library(reshape)
library(ggplot2)
# library(Rnalytica)

df <- NULL
diff <- NULL

for(project in listDataset[listDataset$corpus == "jira",]$system){
  print(project)
  Data <- loadDefectDataset(project,"jira")
  data <- Data$data

  # Bug(H) <=> Bug(R)
  pert.bug.diff = data[data$HeuBug == TRUE,]$HeuBugCount-data[data$HeuBug == TRUE,]$RealBugCount
  bug.miss = length(pert.bug.diff[pert.bug.diff<0])/length(pert.bug.diff) # H=1 <less< R=2, missing
  bug.equal = length(pert.bug.diff[pert.bug.diff==0])/length(pert.bug.diff) # H=1 == R=1, no diff
  bug.false = length(pert.bug.diff[pert.bug.diff>0])/length(pert.bug.diff) # H=2 >higher> R=1, false
  pert.bug.diff = length(pert.bug.diff[pert.bug.diff!=0])/length(pert.bug.diff)
  
  # Clean(H) <=> Clean(R)
  pert.clean.diff = data[data$HeuBug == FALSE,]$HeuBugCount-data[data$HeuBug == FALSE,]$RealBugCount
  clean.miss = length(pert.clean.diff[pert.clean.diff<0])/length(pert.clean.diff) # H=0, R=1, missing
  clean.false = length(pert.clean.diff[pert.clean.diff>0])/length(pert.clean.diff) # H=1, R=2, false
  pert.clean.diff = length(pert.clean.diff[pert.clean.diff!=0])/length(pert.clean.diff)
  
  mignitude <- table(abs(data[data$RealBug == TRUE,]$RealBugCount-data[data$RealBug == FALSE,]$HeuBugCount))
  mignitude <- mignitude[names(mignitude)!=0]
  mignitude <- mignitude/sum(mignitude)
  names(mignitude)[as.numeric(names(mignitude)) >= 10] <- "10+"
  
  diff <- rbind(diff, cbind(diff=names(mignitude),value=mignitude))
  
  # Percentage of defective modules in heuristic defect datasets are actually mislabelled
  MislabelledDefective <- table(data$HeuBug,data$RealBug)[2,1]/nrow(data[data$HeuBug == TRUE,])
  
  # Percentage of clean modules in heuristics defect datasets are actually mislabelled
  MislabelledClean <- table(data$HeuBug,data$RealBug)[1,2]/nrow(data[data$HeuBug == FALSE,])
  
  df <- rbind(df, cbind(project=project,
                        LessDefectCount=bug.miss,
                        SameDefectCount=bug.equal,
                        HigherDefectCount=bug.false,
                        MissingClean=clean.miss,
                        FalseClean=clean.false,
                        Defective=pert.bug.diff, 
                        Clean=pert.clean.diff,
                        MislabelledDefective=MislabelledDefective, 
                        MislabelledClean=MislabelledClean))
}

df <- data.frame(df)
df[,2:10] <- lapply(df[,2:10],function(x) as.numeric(as.character(x)))

diff <- data.frame(diff)
diff$value <- as.numeric(as.character(diff$value))
diff$diff <- factor(diff$diff, c(1:9,"10+"))

# Plotting

chart <-melt(df[,c("project","LessDefectCount","SameDefectCount","HigherDefectCount")])
chart$project <- factor(chart$project, levels=as.character(df[order(df$HigherDefectCount),]$project))
ggplot() + geom_bar(data = chart, aes(x=project, y=value*100, fill=variable), stat="identity") + theme_bw() + ylab("Percentage") + xlab("") + theme( legend.position="top", legend.title=element_blank()) + scale_fill_brewer() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("paper/figures/figure5.pdf", width=6, height=5)


ggplot(melt(df[,c("Defective","Clean")]), aes(x=variable,y=value*100)) + geom_boxplot() + xlab("") + ylab("Percentage") + theme_bw() + scale_y_continuous(limits = c(0,100), breaks=0:5*20) 
ggsave("paper/figures/figure4-a.pdf", width=3, height=3)

ggplot(melt(df[,c("MislabelledDefective","MislabelledClean")]), aes(x=variable,y=value*100)) + geom_boxplot()  + xlab("") + ylab("Percentage") + theme_bw() + scale_y_continuous(limits = c(0,100), breaks=0:5*20)
ggsave("paper/figures/figure4-b.pdf", width=3, height=3)

ggplot(diff, aes(x=diff, y=value*100)) + geom_boxplot() + xlab("Magnitude of the difference") + ylab("Percentage of defective modules") + theme_bw() + scale_y_continuous(limits = c(0,80), breaks=0:5*20)
ggsave("paper/figures/figure8.pdf", width=7, height=2.7)


###### Generate Report ######

print("RQ1-a:")
rq1a.1 <- summary(df$Defective)
print(paste0("At the median,", floor(rq1a.1["Median"]*100),"% of defective modules in heuristic defect datasets have different defect counts from those in realistic defect datasets"))
print(paste0("Figure 4a shows that ",floor(rq1a.1["Min."]*100),"%-",floor(rq1a.1["Max."]*100),"% of defective modules in heuristic defect datasets have different defect counts from those in realistic defect datasets."))

rq1a.2 <- summary(chart[chart$variable=="HigherDefectCount",]$value)
print(paste0("We find that ",floor(rq1a.2["Min."]*100),"%-",floor(rq1a.2["Max."]*100),"% of defective modules in heuristic defect datasets have a higher defect count than realistic defect datasets."))

rq1a.3 <- summary(chart[chart$variable=="LessDefectCount",]$value)
print(paste0("We find that ",floor(rq1a.3["Min."]*100),"%-",floor(rq1a.3["Max."]*100),"% f defective modules in heuristic defect datasets have a lower defect count in realistic defect datasets."))

rq1a.4 <- summary(df$Clean)
print(paste0("at the median, ", floor(rq1a.4["Median"]*100),"% of clean modules in heuristic defect datasets have different defect counts from those in realistic defect datasets."))
print(paste0("Figure 4a shows that ",floor(rq1a.4["Min."]*100),"%-",floor(rq1a.4["Max."]*100),"% of defective modules in heuristic defect datasets have different defect counts from those in realistic defect datasets."))

print("RQ1-b:")
rq1b.1 <- summary(df$MislabelledDefective)
rq1b.2 <- summary(df$MislabelledClean)
print(paste0("At the median, ", floor(rq1b.1["Median"]*100),"% of defective modules and ",floor(rq1b.2["Median"]*100),"% of the clean modules in heuristic defect datasets are mislabelled."))
print(paste0("Figure 4b shows that ",floor(rq1b.1["Min."]*100),"%-",floor(rq1b.1["Max."]*100),"% of defective modules in heuristic defect datasets are actually clean in realistic defect datasets."))
print(paste0("Figure 4b shows that ",floor(rq1b.2["Min."]*100),"%-",floor(rq1b.2["Max."]*100),"% of clean modules in heuristic defect datasets are actually defective in realistic defect datasets."))