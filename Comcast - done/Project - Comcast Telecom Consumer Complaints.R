#Reading Comcast Data and loading libraries

rm(list=ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(plyr)
library(zoo)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(gridExtra)

setwd("E:\\Simplilearn\\Data Science with R\\Project\\Comcast")

comcast<- read.csv("Comcast Telecom Complaints data.csv")
head(comcast)
str(comcast)

sum(is.na(comcast))

# Cleaning the date

comcast$Date<- dmy(comcast$Date)

#Provide the trend chart for the number of complaints at monthly 
#and daily granularity levels.

#Daily Trend

a = aggregate(comcast$Date, by=list(comcast$Date), FUN=length)
str(a)

ggplot(a, aes(Group.1, x, group = 1)) + 
  geom_line() +
  xlab("Date") + 
  ylab("Number of Complaints")

#Monthly Trend

month<- as.yearmon(comcast$Date, format = "%d-%m-%Y")
b<-cbind(comcast,month)

c = aggregate(b$month, by=list(b$month), FUN=length)
str(c)

ggplot(c, aes(Group.1, x, group = 1)) + 
  geom_line() +
  xlab("Date") + 
  ylab("Number of Complaints")

#Provide a table with the frequency of complaint types.
#Which complaint types are maximum i.e., around internet, 
#network issues, or across any other domains.

complaint_list = data.frame(comcast$Customer.Complaint)
colnames(complaint_list)=c("Complaint")

corpus= Corpus(VectorSource(complaint_list$Complaint))

#Text Cleaning

corpus <- tm_map(corpus,content_transformer(tolower)) #Converting all text into lower case

corpus<- tm_map(corpus,removeNumbers) #Remove Numbers

corpus = tm_map(corpus,removeWords,stopwords(kind="en"))#Removing common stop words

corpus = tm_map(corpus,removePunctuation)#Remove Punctuation

corpus = tm_map(corpus,stripWhitespace)#Removing white spaces

corpus= tm_map(corpus,removeWords,c("get","took","can","can","comcast"))#Remove additional words

#Create Term Document Matrix (TDM)

tdm = TermDocumentMatrix(corpus)
m=as.matrix(tdm)
v=sort(rowSums(m),decreasing = T)

#List with Frequency of Compliant Types

d=data.frame(word=names(v),freq=v)

#word cloud

set.seed(2)

wordcloud(d$word,d$freq,random.order=F,min.freq = 5, max.words=1000, rot.per=0.2, colors=brewer.pal(5, "Dark2"), scale=c(4,0.2))
title(main = "Complaint Types - Word Cloud",font.main=1,cex.main=1.5)

# Complaint Type Processing as seen from word cloud

internet_tickets<- contains(comcast$Customer.Complaint,match = 'internet',ignore.case = T)
billing_tickets<- contains(comcast$Customer.Complaint,match = 'bill',ignore.case = T)
data_tickets<- contains(comcast$Customer.Complaint,match = 'data',ignore.case = T)
service_tickets<- contains(comcast$Customer.Complaint,match = 'service',ignore.case = T)
speed_tickets<- contains(comcast$Customer.Complaint,match = 'speed',ignore.case = T)
charges_tickets<- contains(comcast$Customer.Complaint,match = 'charge',ignore.case = T)

comcast$ComplaintType[internet_tickets]<- "Internet"
comcast$ComplaintType[billing_tickets]<- "Billing"
comcast$ComplaintType[data_tickets]<- "data"
comcast$ComplaintType[service_tickets]<- "service"
comcast$ComplaintType[speed_tickets]<- "speed"
comcast$ComplaintType[charges_tickets]<- "charges"

comcast$ComplaintType[-c(internet_tickets,
                              billing_tickets,data_tickets,service_tickets,speed_tickets,charges_tickets)]<- "Others"

table(comcast$ComplaintType)

#Create a new categorical variable with value as Open and Closed. 
#Open & Pending is to be categorized as Open and Closed & Solved 
#is to be categorized as Closed.

comcast$Status_New<-revalue(comcast$Status, c(Pending = "Open", Solved = "Closed"))
head(comcast)

#Provide state wise status of complaints in a stacked bar chart.
#Use the categorized variable from Q3. Provide insights on:

ggplot(comcast, aes(y = State)) + geom_bar(aes(fill = Status_New))

#Which state has the maximum complaints

count_complaints = aggregate(comcast$Status_New, by=list(comcast$State), FUN=length)
df <-count_complaints[order(-count_complaints$x),]
head(df,5)

#Which state has the highest percentage of unresolved complaints

resolved_data<- subset(comcast, Status_New == "Closed")
unresolved_data<- subset(comcast, Status_New == "Open")

resolved = aggregate(resolved_data$Status_New, by=list(resolved_data$State), FUN=length)
unresolved = aggregate(unresolved_data$Status_New, by=list(unresolved_data$State), FUN=length)

per = merge(resolved, unresolved, by = "Group.1")

per$per = (per$x.y/(per$x.x+per$x.y)*100)

high_unresolved = aggregate(per$per, by=list(per$Group.1), FUN=max)
high_unresolved[(high_unresolved$x == max(high_unresolved$x)),]
df1 <-high_unresolved[order(-high_unresolved$x),]
head(df1,5)

#Provide the percentage of complaints resolved till date, 
#which were received through the Internet and customer care calls.

resolved1 = aggregate(resolved_data$Status_New, by=list(resolved_data$Received.Via), FUN=length)
unresolved1 = aggregate(unresolved_data$Status_New, by=list(unresolved_data$Received.Via), FUN=length)

per1 = merge(resolved1, unresolved1, by = "Group.1")

per1$per = (per1$x.x/(per1$x.x+per1$x.y)*100)
per1

df2 <- table(comcast$Received.Via, comcast$Status_New)
df2 <- cbind(df2, Total = rowSums(df2))
df2

# Pie Chart with Percentages
slices <- c(864, 255)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Received Via Call") 

# Pie Chart with Percentages
slices <- c(843, 262)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Received Via Internet") 



