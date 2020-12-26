wines_data <- read.csv(file.choose())
head(wines_data)
tail(wines_data)
install.packages("ggfortify")
library(forecast)
library()
library(ggplot2)
library(leaps)
library(reshape2)
library(MASS)
library(ggcorrplot)
library(plotmo)
library(univOutl)
is.na(wines_data)
summary(wines_data)
range(wines_data, na.rm = FALSE)
dim(wines_data)
sapply(wines_data, class)
ggplot(data = wines_data , aes(x = chem1, color = cultivar))+geom_histogram()
principal_comp<-prcomp(wines_data, scale = FALSE)
principal_comp
#outliers detection:
outlier_values <- boxplot.stats(wines_data$chem1)$out
boxplot(wines_data$cultivar, main= "chem1", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

outlier_values <- boxplot.stats(wines_data$chem2)$out
boxplot(wines_data$cultivar, main= "chem2", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

outlier_values <- boxplot.stats(wines_data$chem3)$out
boxplot(wines_data$cultivar, main= "chem3", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

outlier_values <- boxplot.stats(wines_data$chem4)$out
boxplot(wines_data$cultivar, main= "chem4", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

outlier_values <- boxplot.stats(wines_data$chem5)$out
boxplot(wines_data$cultivar, main= "chem5", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

outlier_values <- boxplot.stats(wines_data$chem6)$out
boxplot(wines_data$cultivar, main= "chem6", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

outlier_values <- boxplot.stats(wines_data$chem7)$out
boxplot(wines_data$cultivar, main= "chem7", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

outlier_values <- boxplot.stats(wines_data$cultivar)$out
boxplot(wines_data$chem8, main= "cultivar", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#mahalnobis distance:
mean<- colMeans(wines_data)
mean

medians <- colMedians(wines_data, na.rm = FALSE, hasNA = FALSE, keep.names = TRUE)
medians

covariance <- cov(wines_data)
covariance
D2<- mahalanobis(wines_data,mean,covariance)
D2

#scatterplot for pca
pca_res<-prcomp(wines_data, scale. = TRUE)
pca_res
install.packages("ggfortify")
plot(pca_res)
ggplot(data = wines_data, aes(x = chem1, y= chem2, color=cultivar))+geom_point()
ggplot(data = wines_data, aes(x = chem3, y= chem4, color=cultivar))+geom_point()
ggplot(data = wines_data, aes(x = chem5, y= chem6, color=cultivar))+geom_point()


