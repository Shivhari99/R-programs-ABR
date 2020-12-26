library(ggplot2)
library(Lahman)
library(MASS)
weld_info <- read.csv(file.choose())
#Step 1: 
head(weld_info)
tail(weld_info)
summary(weld_info)
colnames(weld_info)
colnames(weld_info)<- c("weld_type_id","weld_type","number_of_welds","inspected_welds_number","failed_welds_number","work_proportion","inspection_rate")
colnames(weld_info)
head(weld_info)
ggplot(weld_info, aes(x= work_proportion, y=weld_type_id , color= work_proportion))+geom_point()
plot(weld_info, aes(x= weld_type, color = work_proportion))

#Step 2 Beta Distribution:
num_weld_info <- as.numeric(as.character(weld_info$weld_type_id))
num_Weld_1 <- as.numeric(as.character(weld_info$inspected_welds_number))
num_weld_2 <- as.numeric(as.character(weld_info$failed_welds_number))
dbeta(num_weld_info, shape1 = num_Weld_1, shape2 = num_weld_2, ncp = 1, log= T)
num_Weld_1
plot(weld_info$weld_type_id)
dbeta(num_weld_info, shape1 = num_Weld_1,shape2 = num_weld_2, log = F, ncp = 0)
pbeta(num_weld_info, shape1 = num_Weld_1, shape2 = num_weld_2, log = F)
betad<-dbeta(weld_info$weld_type_id, shape1 = weld_info$inspected_welds_number, shape2 = weld_info$failed_welds_number, ncp = 0, log = FALSE)
betad
plot(betad)
betap
plot(betap)
ggplot(weld_info, aes(x= inspection_rate, y= failed_welds_number, color= inspected_welds_number))+ geom_point()
ggplot(weld_info, aes(x= inspection_rate, y= weld_type , color= inspected_welds_number))+ geom_point()
plot(weld_info, aes(x= weld_type))
a<- weld_info$failed_welds_number
a
b<- weld_info$inspected_welds_number
b
cbind(weld_info$weld_type_id,weld_info$number_of_welds,weld_info$weld_type,a,b, betad, betap)

#Step 3:
w1<- read.csv(file.choose(new = TRUE),sep = ',')
summary(w1)
length(w1$weld.type)
mean(w1$number.of.welds)
mean(w1$number.of.inspected.welds)
error <- qt(0.975,df=length(w1$number.of.welds)-1)*sd(w1$number.of.failed.welds)/sqrt(length(w1$inspection.rate))
error
lower <- mean(w1$number.of.welds)-error
lower
upper<- mean(w1$number.of.welds)+error
upper
betaq <- qbeta(weld_info$work_proportion, shape1 = weld_info$inspected_welds_number, shape2 = weld_info$failed_welds_number, lower.tail = TRUE, log.p = FALSE)
betaq
Lower_limit<-qbeta(0.025, shape1 = weld_info$inspected_welds_number, shape2 = weld_info$failed_welds_number, lower.tail = TRUE, log.p = FALSE)
Lower_limit
Upper_limit <-qbeta(0.975, shape1 = weld_info$inspected_welds_number, shape2 = weld_info$failed_welds_number, lower.tail = TRUE, log.p = FALSE)
Upper_limit
beta_distr <- cbind(a,b)
beta_distr
cbind(weld_info$weld_type_id,weld_info$weld_type,beta_distr, Upper_limit, Lower_limit)
plot(betaq)
#Step 4: 
repair_rate <- rbeta(10000, a,b)
repair_rate
new_data <- cbind(weld_info$weld_type_id,weld_info$weld_type, weld_info$inspected_welds_number, weld_info$failed_welds_number, weld_info$inspection_rate,repair_rate)
new_data
rbind(weld_info,repair_rate)
summary(weld_info)
hist(x=repair_rate)
ggplot(weld_info, aes(x= inspection_rate))+geom_histogram()

#Step 5:
ggplot(data = weld_info, aes(x= inspection_rate, y= number_of_welds))+geom_boxplot()
n <- 100 
SampleValuesBox <- lapply(seq(nrow(beta)),function(i) rbeta(n, beta$a[i], beta$b[i]))
SampleValuesBoxPlot <- data.frame(weld_type_id = rep(weld_info$weld_type_id, each = n),yvalue = unlist(SampleValuesBox))

ggplot(SampleValuesBox, aes(x = factor(weld.type.ID), y = yvalue)) + geom_boxplot()+labs(x="Weld Type ID", y="repair rate")

#Step 6 (Bonus Question):
#1. RMSE calculation:
install.packages("Metrics")
library(Metrics)
rmse(weld_info$inspection_rate, weld_info$failed_welds_number)
rmse(Lower_limit,Upper_limit)
#Confidence Interval:
error <- qt(0.975,df=length(w1$number.of.welds)-1)*sd(w1$number.of.failed.welds)/sqrt(length(w1$inspection.rate))
error
left <- mean(w1$number.of.welds)-error
left
right <- mean(w1$number.of.welds)+error
right
