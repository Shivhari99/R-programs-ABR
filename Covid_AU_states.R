covid_stats <- read.csv(file.choose())
covid_stats

head(covid_stats)
tail(covid_stats)
summary(covid_stats)
colnames<- c("date","state","state_abbrev","confirmed","confirmed_cum","deaths","deaths_cum","tests","tests_cum","positives","positives_cum","recoevered","recovered_cum")
head(covid_stats)
tail(covid_stats)
library(ggplot2)
ggplot(data = covid_stats,aes(x= confirmed,y= deaths,color= state))+geom_point()+geom_line()
ggplot(data = covid_stats,aes(x= tests,y= positives,color= state))+geom_point()+geom_line()
ggplot(data = covid_stats,aes(x= deaths_cum,color= state))+ geom_bar()
ggplot(data = covid_stats, aes(x= positives,y=recovered,color= state )) + geom_point()+geom_line()

