#This R-script looks at the OMX development over time. Data is from netfonds.se

library(readxl)
library(ggplot2)
library(dplyr)

od <- read_excel("C:Projects/trading/omx30hist.xls", sheet = 1) #Read in omx daily values .xls

head(od)
tail(od)

#Create Boxplot per Year

t <- mean(od$close) #Avg daily close value

od$year <- factor(od$year) #Year as factor, needed for the x label to be correct in Boxplot

g <- ggplot(od, aes(x = year, y = close, group = year))
g <- g + geom_boxplot()
g <- g + geom_hline(yintercept = t, linetype = 2, color = 'red', alpha = 0.5) #adds t as line of hline for y axis!
g <- g + scale_x_discrete(breaks = 
                            c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017"),
                          labels = c("04","05","06","07","08","09","10","11","12","13","14","15", "16", "17"))
g <- g + theme_minimal()
g <- g + ggtitle("Boxplot of omx30 daily close value per year")
plot(g)

#Similar plot with Months instead, to spot which month is usually most volatile
od$month <- factor(od$month)

g <- ggplot(od, aes(x = month, y = close, group = month))
g <- g + geom_boxplot() #Comment out this to get rid of the observations dots in the boxes
g <- g + geom_hline(yintercept = t, linetype = 2, color = 'red', alpha = 0.5) #adds target line. hline for y axis!
g <- g + theme_minimal()
g <- g + ggtitle("Boxplot of omx30 daily close value per month")
plot(g)

#Same plot as above with day number of the month on x-axis, to see which days of the month is usually most volatile
od$day <- factor(od$day)

g <- ggplot(od, aes(x = day, y = close, group = day)) #we actually dont need group = day here
g <- g + geom_boxplot() #Comment out this to get rid of the observations dots in the boxes
#g <- g + geom_jitter(alpha = .25)
g <- g + geom_hline(yintercept = t, linetype = 2, color = 'red', alpha = 0.5) #adds target line. hline for y axis!
g <- g + theme_minimal()
#g <- g + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())#Hide vertical gridlines
g <- g + ggtitle("Boxplot of omx30 daily close value per day of month")
plot(g)

#Only subtle changes as expected between days over the years

#Creating Small multiples of boxplots over the years

od$quote_date <- factor(od$quote_date)

g <- ggplot(od, aes(x = month, y = close)) #Needed to remove group = year here
g <- g + geom_boxplot()
g <- g + geom_hline(yintercept = t, linetype = 2, color = 'red', alpha = 0.5) #adds target line. hline for y axis!
g <- g + facet_wrap(~year)
g <- g + theme_bw()
g <- g + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
g <- g + ggtitle("omx30 boxplot per month year by year")
plot(g)

#This plot makes sense. However difficuilt to see the shape of boxex due to same scale over the years.

#Box plot for only one year

#Slice dataset on one year
od16 <- od %>% filter(year == 2016)
od15 <- od %>% filter(year == 2015)


t16 <- mean(od16$close)

g <- ggplot(od16, aes(x = month, y = close))
g <- g + geom_boxplot()
g <- g + geom_jitter(color = "grey")
g <- g + geom_hline(yintercept = t16, linetype = 2, color = 'red', alpha = 0.5) #adds target line. hline for y axis!
g <- g + theme_bw()
g <- g + ggtitle("omx30 boxplot year 2016")
plot(g)

t15 <- mean(od15$close)

#Boxplots using small multiples for several years

od3 <- read_excel("C:Projects/trading/omx30hist.xls", sheet = 1) #Read in historical omx daily values again
od3b <- od3 %>% filter(year < 2017)
od4 <- od3b %>% filter(year > 2012)
od4$month <- factor(od4$month)

t4 <- mean(od4$close)

g <- ggplot(od4, aes(x = month, y = close)) #Needed to remove group = year here
g <- g + geom_boxplot()
#g <- g + geom_jitter(color = "grey")
g <- g + geom_hline(yintercept = t4, linetype = 2, color = 'red', alpha = 0.5) #adds target line. hline for y axis!
g <- g + facet_wrap(~year) #only one year here
g <- g + theme_bw()
g <- g + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
g <- g + ggtitle("omx30 boxplot per year")
plot(g)

#Now we have only four years of data which makes the shapes of the boxes more visible