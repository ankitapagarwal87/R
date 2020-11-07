setwd("E:\\Simplilearn\\Data Science with R\\Project\\Internet")
rm(list=ls())

#Reading Internet Data

mydata = read.csv("Internet_Dataset.csv")

#1.	The team wants to analyze each variable of the data collected through data summarization to get a basic understanding of the dataset and to prepare for further analysis.

summary(mydata)

#2.	As mentioned earlier, a unique page view represents the number of sessions during which that page was viewed one or more times. A visit counts all instances, no matter how many times the same visitor may have been to your site. So, the team needs to know whether the unique page view value depends on visits.

#To check the correlation between Visits and Unique Page Views

cor(mydata$Uniquepageviews,mydata$Visits)
plot(mydata$Uniquepageviews,mydata$Visits, col = "blue",xlab="Visits",ylab="Unique Page Views",cex=0.6,
     main="Visits vs UniquePageViews",cex.main=0.8,cex.axis=0.6 )

#Or we can run a annova test to check the dependency between Visits and Unique Page Views

av <- aov(Uniquepageviews ~ as.factor(Visits), data = mydata)
summary(av)

#3.	Find out the probable factors from the dataset, which could affect the exits. Exit Page Analysis is usually required to get an idea about why a user leaves the website for a session and moves on to another one. Please keep in mind that exits should not be confused with bounces.

Exit<-aov(Exits~., data=mydata)
summary(Exit)

#4.	Every site wants to increase the time on page for a visitor. This increases the chances of the visitor understanding the site content better and hence there are more chances of a transaction taking place. Find the variables which possibly have an effect on the time on page.

Timespent<-aov(Timeinpage~., data=mydata)
summary(Timespent)

#5.	A high bounce rate is a cause of alarm for websites which depend on visitor engagement. Help the team in determining the factors that are impacting the bounce.

mydata$Bounces=mydata$Bounces*0.01

model<-glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data = mydata,family = "binomial")
summary(model)

