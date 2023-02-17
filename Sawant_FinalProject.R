#Final Project
#Project: Introduction to Analytics
#Author : Rushikesh Sawant

print("Rushikesh Sawant")
#Clean Variables at the start of the program
rm(list=ls())
#clean the plots screen
dev.off()

#INSTALL the Necessary Package
#install.packages('magrittr')
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('plyr')
#install.packages('tidyverse')
#install.packages('ggplot2')
#install.packages('maps')
#install.packages('mapproj')
#install.packages("sqldf")
#install.packages("plotly")
#install.packages("plotrix")
#Loading the Necessary Packages####
library(ggplot2)
library(maps)
library(mapproj)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(sqldf)
library(plotly)
library(plotrix)

#Import Data and cleaning data AND Summarize the data####
EqData  <- read.csv("EqData.csv", header=TRUE)
EqData<-EqData[,-c(4,8,12,13,14,15,16,17,18,19,20,24,25,26,32,38,41,43,45,47,49)]
summary(EqData)

#Visualization to check whether earthquakes are predomi,nant around tectonic plate edges####
largeEq <-sqldf("select * from EqData where country = 'USA' and Magnitude > 1.0")%>%    #segregating based on country and magnitude
  mutate(hover = paste0(location_name, "\nYear:", year , "\nMagnitude:", Magnitude))    #Adding a hover column with necessary details

##Porperties of the background Map####
geo_properties <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  showsubunits = FALSE,
  landcolor = toRGB("grey10"),
  showlakes = TRUE,
  lakecolor = toRGB('lightblue')
)
##Scatter plot based on lat and long and adding the hover function ####
eq.fig = plot_geo(largeEq,lat = ~latitude , lon = ~longitude,
                  marker = list(size = 4, color = "#EE4B2B", opacity = 0.5))  %>%
  add_markers(text = ~hover,
              hoverinfo = 'text') %>%
  config(displayModeBar = FALSE) %>%
  layout(geo = geo_properties,
         title = "Earthquakes in USA between 1920 - 2020",
         font = list(family = "DM Sans"))

eq.fig  #Scatter plot on USA Map


#Determing if there is corelation between Magnitude and Toll of death and total Damage####
largeEqmag <-sqldf("select * from EqData where Magnitude > 8.0 ORDER BY Magnitude desc limit 18")
largeEqtd <-sqldf("select total_deaths from largeEqmag")

##segregating magnitude,totaldeaths,damageMillions####
xy.pop<-largeEqmag$Magnitude
xx.pop<-largeEqtd$total_deaths/1000
xz.pop<-largeEqmag$Damage....Millions./100
x<- as.data.frame(xz.pop)
mcol<-plotrix::color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-plotrix::color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)

##ButterFly plot of magnitude and total deaths####
agelabels<-largeEqmag$country
par(mar=plotrix::pyramid.plot(xy.pop,xx.pop, labels=agelabels,top.labels = c("Magnitude","Country","Deaths/1000"),
                              main="Magnitude VS Number of Deaths",lxcol=mcol,rxcol=fcol,unit = "",
                              gap=45,show.values=TRUE))

##ButterFly Plot of magnitude and Damage in Millions####
par(mar=plotrix::pyramid.plot(xy.pop,xz.pop, labels=agelabels,top.labels = c("Magnitude","Country","Damages"),
                                       main="Magnitude VS Damages in 100 Millions",lxcol=mcol,rxcol=fcol,unit = "",
                                       gap=40,show.values=TRUE))

#Pie Chart depicting the top 10 countries facing earthquakes####
tmp <-  as.data.frame(table(EqData$country))
class(tmp$NOE)
colnames(tmp)<-c("Country","NOE")
df <- arrange(tmp,desc(NOE))
df <- sqldf("select * from df limit 10")
pie(df$NOE,df$Country,main = "Top 10 Regions affected by Earthquake",col = rainbow(length(df$NOE)))

#Pareto Chart new columns####
df <- mutate(df,cumcounts=cumsum(NOE))
df <- mutate(df,relfreq=(NOE/sum(NOE))*100)
df <- mutate(df,cumfreq=cumsum(relfreq))

#A pareto Chart with top 10 Countries####
Cpareto <- barplot(df$NOE,names.arg=df$Country,space=0.15,width=0.9,border=NA,axes=FALSE
              ,main="Countries Pareto",ylab="Cummulative Count",ylim=c(1,8.05*max(df$NOE,na.rm = TRUE))
              ,cex.axis=0.5,las=2,col="red") 
Cpareto + lines(df$cumcounts,type='b',cex=0.6,pch=19,col='cyan') + axis(2,at=c(0,df$cumcounts),col.axis = "grey10",col.ticks = "grey10",cex.axis=0.8) +
  axis(side =4,at=c(0,df$cumcounts),tick= TRUE,line=NA,col="cyan4",col.axis="cyan4",cex.axis=0.7,las=2,labels=paste0(round(c(0,df$cumfreq),digits=0),'%'))+ box()


#Clean Variables at the start of the program
rm(list=ls())
#clean the plots screen
dev.off()

