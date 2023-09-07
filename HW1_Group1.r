setwd("C:/Users/ajite/OneDrive/Documents/Courses/Adv Stats/HW1")
#Question 1

Question1<- read.csv("workload.csv")

df<- subset(Question1,select=c(Home.workload))

df$Home.workloadlabel<- factor(df$Home.workload,
                               levels=c(1,2,3,4,5,6),
                               labels=c("Mom: Full-time; Dad: Full-time",
                                        "Mom: Part-time; Dad: Full-time",
                                        "Mom: Not employed; Dad: Full-time",
                                        "Mom: Full-time; Dad: Part-time or not employed",
                                        "Mom: Not employed; Dad: Not employed",
                                        "Other"))
#Display working status categories
levels(df$Home.workloadlabel)

#install.packages("dplyr") #one time installation
#library("dplyr")

n=nrow(df); n #total number of observations

Freq<- df %>%
  group_by(Home.workloadlabel) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq


#----------Frequency & Relative Frequency Distributions of Home.workload--------------
options(digits=4)
Rel.Freq<- df %>%
  group_by(Home.workloadlabel) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)) %>%
  mutate('Relative Frequency'=Frequency/n); Rel.Freq

#----------Creating a Bar Chart-------------------------------------------------

'Set the plot margin sizes in the following order: bottom,left,top,and right'
par(mar=c(5,5,5,5)) 


count<- table(df$Home.workload); count
barplot(count,
        main="Home Workload",
        xlab="Working Status",
        ylab="Frequency",
        ylim=c(0,500),
        names.args = c(1,2,3,4,5,6),
        border="red",col="lightblue",
)

#Creating a Pie Chart
piepercent<- round(count/n*100,1); piepercent
pie(piepercent,
    labels=paste(piepercent,"%",sep=""),
    main="Home Workload (Percent)",
    col=rainbow(length(count)))

legend("topright",
       c("Mom: Full-time; Dad: Full-time",
         "Mom: Part-time; Dad: Full-time",
         "Mom: Not employed; Dad: Full-time",
         "Mom: Full-time; Dad: Part-time or not employed",
         "Mom: Not employed; Dad: Not employed",
         "Other"),
       cex=1,
       fill=rainbow(length(piepercent))
)

#___________ QUESTION 2____________#

Question2<- read.csv("MBA.csv") #Load up GSS2018.csv data

#Add a new feature to the data frame:degree labels
Question2$Degreelabel <- factor(Question2$Degree,
                                levels=c(1,2,3,4),
                                labels=c("BA",
                                         "B.Eng",
                                         "BBA",
                                         "Other")) 

levels(Question2$Degreelabel)
#--------------------------------------------

##Cross-Classification Table of Frequencies
n=nrow(Question2); n #Total number of observations
addmargins(table(Question2$Degreelabel,Question2$University));

##Row-relative Frequencies
table(Question2$Degreelabel,Question2$University) %>% 
  prop.table(margin=1) %>%
  round(2)

##Column-relative Frequencies
table(Question2$Degreelabel,Question2$University) %>% 
  prop.table(margin=2) %>%
  round(2)

##Overall-relative Frequencies (also called joint probability distribution)
table(Question2$Degreelabel,Question2$University) %>% 
  prop.table() %>%  
  round(2)

#Side-by-Side Bar Chart
count<- table(Question2$Degreelabel,Question2$University); count
barplot(count,
        main="Two-dimentional Bar Chart",
        xlab="University",
        ylab="Degree",
        col=c("darkblue","red","green","yellow"),
        cex=1.2,cex.axis=1.2,cex.lab=1.2,
        legend.text=rownames(count),beside=T, 
        args.legend = list(x = "topright", horiz = T, cex = 0.5))


rm(list=ls()) #Clear up the Working Environment 


#___________ QUESTION 3____________#

rm(list=ls())
GSS<- read.csv("GSS2018.csv")
hist(GSS$EDUC,
     breaks=10, #Define different Number of Breaks. 
     freq=T,#freq=F --> hist of rel freq,comment out ylim,ylab,and labels.
     main="Histogram of Ameriacans Education in 2018",
     xlab="Education Level",
     ylab="Frequency",
     labels=T,#label the values
     xlim=c(0,20),
     ylim=c(0,800),
     col="red",
     border="black",
     cex=1.2,cex.lab=1.2,cex.axis=1.2,
     las=1,)#rotate the value of y-axis

boxplot(GSS$EDUC, na.rm=T, 
        main = "Box Plot of Education",
        xlab="Education",
        col="maroon",
        horizontal=T)
summary(GSS$EDUC)

#___________ QUESTION 4____________#

rm(list=ls())
inflation <- read.csv("inflation.csv")
myts1<- ts(inflation$France ,
          start=c(1990,1),#1st month of 1976
          end=c(2014,4), #12th month of 2016
          frequency=4)
myts2<- ts(inflation$Germany ,
          start=c(1990,1),#1st month of 1976
          end=c(2014,4), #12th month of 2016
          frequency=4) 
require(graphics)
ts.plot(myts1,myts2,
        main="Time Series Graph: Inflation",
        ylab="Inflation Rate",
        lwd=1.5,
        col=c("red","blue"),
        lty=c(1:2))
legend('topright',legend=c("France","Germany"),col=c("red","blue"),
       lwd=2,lty=1:2,bty="n") 



#___________ QUESTION 5____________#

rm(list=ls())
GSS<- read.csv("GSS2018.csv")
summary(GSS$INCOME)
avg = mean(GSS$INCOME, na.rm=T)
median(GSS$INCOME, na.rm=T)
boxplot(GSS$INCOME, na.rm=T, 
        main = "Box Plot of Income",
        xlab="Income",
        col="maroon",
        horizontal=T)
range = max(GSS$INCOME, na.rm=T) - min(GSS$INCOME, na.rm=T); range
std = sd(GSS$INCOME, na.rm=T)
cov = std/avg;cov
Q1 = quantile(GSS$INCOME, na.rm=T, probs=c(.25)); Q1
Q3 = quantile(GSS$INCOME, na.rm=T, probs=c(.75)); Q3
IQR = Q3 - Q1; IQR
plot(GSS$EDUC,GSS$INCOME,
     main="Scatter Plot",
     ylab="Income",
     xlab="Education Level",
     col="purple",
     pch=19,
     cex=1.2, cex.lab=1.2, cex.axis=1.2)
cor(GSS$EDUC,GSS$INCOME,use = 'pairwise')
