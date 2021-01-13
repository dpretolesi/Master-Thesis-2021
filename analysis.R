set.seed(42)

library(dplyr)
library(chron)
library(ggplot2)
library(GGally)

data <- read.csv("Experiment 1 - Sheet1.csv")
data_2 <- read.csv("Experiment 2 - Sheet1.csv")

tlx <- read.csv("tlx.csv")
tlx_2 <- read.csv("tlx_p.csv")

#convert time in appropriate format
data$Time <- chron(times=data$Time)
data_2$Time <- chron(times=data_2$Time)


###Descriptive Stats###

##Age A##
AvgAge <- data %>% summarise(mean = mean(Age), sd = sd(Age), median = median(Age), n= n())
# M=24.8 SD=2.39

##Age B##
AvgAge_2 <- data_2 %>% summarise(mean = mean(Age), sd = sd(Age), median = median(Age), n= n())
# M=31.7 SD=10.52

##Errors A##
TotalErr <- data %>% group_by(Condition) %>% summarise(mean = mean(Total.Errors), 
                                                       sd = sd(Total.Errors), median = median(Total.Errors),
                                                       quantile= quantile(Total.Errors, 0.75), n= n())
# Paper(M=6.4, SD=5.59, Mdn = 5, 75% = 6), VR(M=5.0, SD=2.54, Mdn = 5, 75% = 7)

InstallErr <- data %>% group_by(Condition) %>% summarise(mean = mean(Install_Error), 
                                                         sd = sd(Install_Error), median = median(Install_Error),
                                                         quantile= quantile(Install_Error, 0.75), n= n())
# Paper(M=5.4 SD=4.72, Mdn = 5, 75%=6), VR(M=4.6 SD=2.07, Mdn = 5, 75%=6)

SelecErr <- data %>% group_by(Condition) %>% summarise(mean = mean(Select_Error), 
                                                       sd = sd(Select_Error), median = median(Select_Error),
                                                       quantile= quantile(Select_Error, 0.75), n= n())
# Paper(M=1.0 SD=1.22, Mdn=1, 75%=1), VR(M=0.4 SD=0.54, Mdn=0, 75%=1)

##Errors B##
TotalErr_2 <- data_2 %>% group_by(Condition) %>% summarise(mean = mean(Total.Errors), 
                                                           sd = sd(Total.Errors), median = median(Total.Errors),
                                                           quantile= quantile(Total.Errors, 0.75), n= n())
# Paper(M=2.2 SD=1.30, Mdn=2, 75%=3), VR(M=4.6 SD=2.88, Mdn=3, 75%=6)

InstallErr_2 <- data_2 %>% group_by(Condition) %>% summarise(mean = mean(Install_Error), 
                                                             sd = sd(Install_Error), median = median(Install_Error),
                                                             quantile= quantile(Install_Error, 0.75), n= n())
# Paper(M=1.6 SD=0.54, Mdn=2, 75%=2), VR(M=3.8 SD=1.92, Mdn=3, 75%=4)

SelecErr_2 <- data_2 %>% group_by(Condition) %>% summarise(mean = mean(Select_Error), 
                                                           sd = sd(Select_Error), median = median(Select_Error),
                                                           quantile= quantile(Select_Error, 0.75), n= n())
# Paper(M=0.6 SD=0.89, Mdn=0, 75%=1), VR(M=0.2 SD=0.44, Mdn=0,75, 75%=0)

SkipErr_2 <- data_2 %>% group_by(Condition) %>% summarise(mean = mean(Skip_Error), 
                                                          sd = sd(Skip_Error), median = median(Skip_Error),
                                                          quantile= quantile(Skip_Error, 0.75), n= n())
# Paper(M=0 SD=0, Mdn=0, 75%=0), VR(M=0.6 SD=0.89,Mdn=0, 75%=1)

##Time##
AvgTime <- data %>% group_by(Condition) %>% summarise(mean = mean(Time), sd = sd(Time), n=n())
# Paper = 00:39:09   VR = 00:49:19 

AvgTime_2 <- data_2 %>% group_by(Condition) %>% summarise(mean = mean(Time), sd = sd(Time), n=n())
# Paper = 00:12:31   VR = 00:31:41 


###TLX_A###

Avg_TLX <- data %>% group_by(Condition) %>% summarise(mean = mean(TLX), sd = sd(TLX), median = median(TLX), n= n())
# Paper(M=32.534 SD=16.702103, Mdn=30.67), VR(M=42.534 SD=6.854213, Mdn=42.67)

Avg_Effort <- tlx %>% group_by(experimentID) %>% summarise(mean = mean(Effort), 
                                                           sd = sd(Effort), median = median(Effort),
                                                           n= n())
# Paper(M=84 SD=90.99, Mdn=60), VR(M=170 SD=63.24, Mdn=150)

Avg_Frustr <- tlx %>% group_by(experimentID) %>% summarise(mean = mean(Frustration.Level), 
                                                           sd = sd(Frustration.Level), 
                                                           median = median(Frustration.Level), n= n())
# Paper(M=48 SD=107.33, Mdn=0), VR(M=48 SD=107.33, Mdn=0)

Avg_Mental <- tlx %>% group_by(experimentID) %>% summarise(mean = mean(Mental.Demand), 
                                                            sd = sd(Mental.Demand), median = median(Mental.Demand), n= n())
# Paper(M=104 SD=71.27, Mdn=120), VR(M=174 SD=145.01, Mdn=120)

Avg_Perf <- tlx %>% group_by(experimentID) %>% summarise(mean = mean(Performance), 
                                                           sd = sd(Performance), median = median(Performance), n= n())
# Paper(M=114 SD=35.77, Mdn=120), VR(M=28 SD=43.81, Mdn=0)

Avg_Phy <- tlx %>% group_by(experimentID) %>% summarise(mean = mean(Physical.Demand), 
                                                        sd = sd(Physical.Demand), median = median(Physical.Demand), n= n())
# Paper(M=10 SD=14.14, Mdn=0), VR(M=44 SD=26.07, Mdn=60)

Avg_Temp <- tlx %>% group_by(experimentID) %>% summarise(mean = mean(Temporal.Demand), 
                                                        sd = sd(Temporal.Demand), median = median(Temporal.Demand), n= n())
# Paper(M=128 SD=124.57, Mdn=120), VR(M=174 SD=121.77, Mdn=120)


###TLX_B###

Avg_TLX_2 <- data_2 %>% group_by(Condition) %>% summarise(mean = mean(TLX), sd = sd(TLX), median = median(TLX), n= n())
# Paper(M=32.932 SD=20.35, Mdn=26.67), VR(M=51.60 SD=18.08, Mdn=52.67)

Avg_Effort_2 <- tlx_2 %>% group_by(experimentID) %>% summarise(mean = mean(Effort), sd = sd(Effort), median = median(Effort), n= n())
# Paper(M=88 SD=100.34, Mdn=30), VR(M=206 SD=180.49, Mdn=140)

Avg_Frustr_2 <- tlx_2 %>% group_by(experimentID) %>% summarise(mean = mean(Frustration.Level), 
                                                           sd = sd(Frustration.Level), median = median(Frustration.Level), n= n())
# Paper(M=0 SD=0, Mdn=0), VR(M=48 SD=58.19, Mdn=60)

Avg_Mental_2 <- tlx_2 %>% group_by(experimentID) %>% summarise(mean = mean(Mental.Demand), 
                                                           sd = sd(Mental.Demand), median = median(Mental.Demand), n= n())
# Paper(M=178 SD=143.94, Mdn=80), VR(M=206 SD=136.67, Mdn=240)

Avg_Perf_2 <- tlx_2 %>% group_by(experimentID) %>% summarise(mean = mean(Performance), 
                                                         sd = sd(Performance), median = median(Performance), n= n())
# Paper(M=30 SD=44.72, Mdn=0), VR(M=134 SD=128.37, Mdn=120)

Avg_Phy_2 <- tlx_2 %>% group_by(experimentID) %>% summarise(mean = mean(Physical.Demand), 
                                                        sd = sd(Physical.Demand), median = median(Physical.Demand), n= n())
# Paper(M=38 SD=49.19, Mdn=30), VR(M=80 SD=61.64, Mdn=60)

Avg_Temp_2 <- tlx_2 %>% group_by(experimentID) %>% summarise(mean = mean(Temporal.Demand), 
                                                         sd = sd(Temporal.Demand), median = median(Temporal.Demand), n= n())
# Paper(M=160 SD=118.11, Mdn=150), VR(M=100 SD=154.11. Mdn=0)


### Indipendent Sample t-test A ###

##Time##
Time_test<-t.test(Time ~ Condition, var.equal = TRUE, data=data)

##Errors##
TotErr_test <- t.test(Total.Errors ~ Condition, var.equal = TRUE, data=data)

InstallErr_test <- t.test(Install_Error ~ Condition, var.equal = TRUE, data=data)

SelectErr_test <- t.test(Select_Error ~ Condition, var.equal = TRUE, data=data)

##TLX##
TLX_test <- t.test(TLX ~ Condition, var.equal = TRUE, data=data)

Effort_test <- t.test(data=tlx, Effort ~ experimentID, var.equal = TRUE)

Frustr_test <- t.test(data=tlx, Frustration.Level ~ experimentID, var.equal = TRUE)

Mental_test <- t.test(data=tlx, Mental.Demand ~ experimentID, var.equal = TRUE)

Perf_test <- t.test(data=tlx, Performance ~ experimentID, var.equal = TRUE)

Phy_test <- t.test(data=tlx, Physical.Demand ~ experimentID, var.equal = TRUE)

Temp_test <- t.test(data=tlx, Temporal.Demand ~ experimentID, var.equal = TRUE)


### Indipendent Sample t-test B ###

##Time##
Time_test_2 <-t.test(Time ~ Condition, var.equal = TRUE, data=data_2)

##Errors##
TotErr_test_2 <- t.test(Total.Errors ~ Condition, var.equal = TRUE, data=data_2)

InstallErr_test_2 <- t.test(Install_Error ~ Condition, var.equal = TRUE, data=data_2)

SelectErr_test_2 <- t.test(Select_Error ~ Condition, var.equal = TRUE, data=data_2)

##TLX##
TLX_test_2 <- t.test(TLX ~ Condition, var.equal = TRUE, data=data_2)

Effort_test_2 <- t.test(data=tlx_2, Effort ~ experimentID, var.equal = TRUE)

Frustr_test_2 <- t.test(data=tlx_2, Frustration.Level ~ experimentID, var.equal = TRUE)

Mental_test_2 <- t.test(data=tlx_2, Mental.Demand ~ experimentID, var.equal = TRUE)

Perf_test_2 <- t.test(data=tlx_2, Performance ~ experimentID, var.equal = TRUE)

Phy_test_2 <- t.test(data=tlx_2, Physical.Demand ~ experimentID, var.equal = TRUE)

Temp_test_2 <- t.test(data=tlx_2, Temporal.Demand ~ experimentID, var.equal = TRUE)


###Visualizations A###

my_cols <- c("#00AFBB", "#E7B800")

##Scatter Plot##

plot(Total.Errors ~ Time, data= data, xlab = "Time", ylab = "Errors",col=my_cols[tlx$experimentID], pch=19)
title(main = "Scatter plot Average Errors with best-fit line", font.main = 4)
abline(lm(Total.Errors ~ Time, data= data), col= "red")
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols,pch=19)

plot(Install_Error ~ Time, data= data, xlab = "Time", ylab = "Installation Errors",col=my_cols[tlx$experimentID], pch=19)
title(main = "Scatter plot Installation Errors with best-fit line", font.main = 4)
abline(lm(Install_Error ~ Time, data= data), col= "red")
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols,pch=19)

plot(Select_Error ~ Time, data= data, xlab = "Time", ylab = "Selection Errors",col=my_cols[tlx$experimentID], pch=19)
title(main = "Scatter plot Selection Errors with best-fit line", font.main = 4)
abline(lm(Select_Error ~ Time, data= data), col= "red")
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols,pch=19)

#Grouped Bar Plot##

ggplot(data, aes(x=Condition, weight=Total.Errors, fill=Condition))+ geom_bar()+
  labs(title="Number of Total Errors per condition", x="Condition", y="Total Errors")

ggplot(data, aes(x=Condition, weight=Install_Error, fill=Condition))+ geom_bar()+
  labs(title="Number of Installation Errors per condition", x="Condition", y="Install Errors")

ggplot(data, aes(x=Condition, weight=Select_Error, fill=Condition))+ geom_bar()+
  labs(title="Number of Selection Errors per condition", x="Condition", y="Selection Errors")

##Spearman Correlation Heatmap## 
ggcorr(tlx[,7:12], method = c("all.obs", "spearman"), hjust = .85, size = 4, layout.exp=1, name="Corr")

###Spearman Correlation Heatmap per condition###
ggcorr(paper_tlx_a[,5:10], method = c("all.obs", "spearman"), hjust = .85, size = 4, layout.exp=1, name="Corr")
ggcorr(vr_tlx_a[,5:10], method = c("all.obs", "spearman"), hjust = .85, size = 4, layout.exp=1, name="Corr")


###Visualizations B###

my_cols_2 <- c("#FC4E07", "#07b5fc")

##Scatter Plot##

plot(Total.Errors ~ Time, data= data_2, xlab = "Time", ylab = "Errors",col=my_cols_2[tlx_2$experimentID], pch=19)
title(main = "Scatter plot Averge Errors with best-fit line", font.main = 4)
abline(lm(Total.Errors ~ Time, data= data), col= "red")
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols_2,pch=19)

plot(Install_Error ~ Time, data= data_2, xlab = "Time", ylab = "Installation Errors",col=my_cols_2[tlx_2$experimentID], pch=19)
title(main = "Scatter plot Installation Errors with best-fit line", font.main = 4)
abline(lm(Install_Error ~ Time, data= data_2), col= "red")
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols_2,pch=19)

plot(Select_Error ~ Time, data= data_2, xlab = "Time", ylab = "Selection Errors",col=my_cols_2[tlx_2$experimentID], pch=19)
title(main = "Scatter plot Selection Errors with best-fit line", font.main = 4)
abline(lm(Select_Error ~ Time, data= data_2), col= "red")
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols_2,pch=19)

plot(Skip_Error ~ Time, data= data_2, xlab = "Time", ylab = "Skip Errors",col=my_cols_2[tlx_2$experimentID], pch=19)
title(main = "Scatter plot Skip Errors with best-fit line", font.main = 4)
abline(lm(Skip_Error ~ Time, data= data_2), col= "red")
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols_2,pch=19)

##Grouped Barplot##

ggplot(data_2, aes(x=Condition, weight=Total.Errors, fill=Condition))+ geom_bar()+
  labs(title="Number of Total Errors per condition", x="Condition", y="Total Errors")

ggplot(data_2, aes(x=Condition, weight=Install_Error, fill=Condition))+ geom_bar()+
  labs(title="Number of Installation Errors per condition", x="Condition", y="Install Errors")

ggplot(data_2, aes(x=Condition, weight=Select_Error, fill=Condition))+ geom_bar()+
  labs(title="Number of Selection Errors per condition", x="Condition", y="Selection Errors")

ggplot(data_2, aes(x=Condition, weight=Skip_Error, fill=Condition))+ geom_bar()+
  labs(title="Number of Skip Errors per condition", x="Condition", y="Skip Errors")

##Scatter Plots##

plot(Total.Errors ~ Age, data = extra,  xlab = "Age", ylab = "Total Errors",col=my_cols_2[extra$Condition], pch=19)
title(main = "Total Errors by Age with best-fit line", font.main = 3)
abline(lm(Total.Errors ~ Age, data= extra), col= "red")
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols_2,pch=19)

plot(TLX ~ Age, data = extra,  xlab = "Age", ylab = "TLX",col=my_cols_2[extra$Condition], pch=19)
title(main = "TLX by Age with best-fit line", font.main = 3)
abline(lm(TLX ~ Age, data= extra), col= "red")
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols_2,pch=19)

plot(Total.Errors ~ Age, data = data_2,  xlab = "Age", ylab = "Total Errors",col=my_cols_2[extra$Condition], pch=19)
title(main = "Total Errors by Age at the end of data collection", font.main = 3)
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols_2,pch=19)

plot(TLX ~ Age, data = data_2,  xlab = "Age", ylab = "TLX",col=my_cols_2[extra$Condition], pch=19)
title(main = "TLX by Age at the end of data collection", font.main = 3)
legend(locator(),c("Paper","VR"),cex=.8,col=my_cols_2,pch=19)

##Spearman Correlation Heatmap##

ggcorr(tlx_2[,9:14], method = c("all.obs", "spearman"), hjust = .85, size = 4, layout.exp=1, name="Corr")

###Spearman Correlation Heatmap per condition###
ggcorr(paper_tlx_b[,5:10], method = c("all.obs", "spearman"), hjust = .85, size = 4, layout.exp=1, name="Corr")
ggcorr(vr_tlx_b[,5:10], method = c("all.obs", "spearman"), hjust = .85, size = 4, layout.exp=1, name="Corr")

