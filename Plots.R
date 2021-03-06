require(ggplot2)
require(plyr)

# Read in one year of bike count data from Telegraph & 66th in Oakland, 
# 7 Aug 2012 - 7 Aug 2013 
Telegraph66th <- read.csv("~/Desktop/bikecountdata/bikecountdata/oakland/Telegraph66th.csv")

# Rename third column
names(Telegraph66th)[3] <- "count"

# Boxplot of all bike counts by hour
ggplot(Telegraph66th,(aes(x=X,y=count))) + geom_boxplot() + 
  # remove outlier: 181 bikes at 3 am?
  scale_y_continuous(limits = c(0, 85),"Bike Count") + theme_minimal() + 
  scale_x_discrete("Time of Day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Hourly averages by month
hours <- as.character(unique(Telegraph66th$X))
hours[-seq(1,length(hours),4)] <- ""
# extract the month from the date
Telegraph66th$mo <- sapply(as.character(Telegraph66th$Date), function(x) strsplit(x,'/')[[1]][2])
Telegraph66th$mo <- factor(Telegraph66th$mo,levels=unique(Telegraph66th$mo))
# Summarize hourly averages by month, exclude Aug 2013 so that it won't be averaged with Aug 2012
mnthly <- ddply(Telegraph66th[1:8616,],.(X,mo),summarise,mean_count = mean(count,na.rm = T))
ggplot(mnthly,aes(y=mean_count,x=X,color=mo)) + geom_vline(xintercept = 17,color="grey") + 
  geom_point() + 
  facet_wrap(~mo) + theme_minimal() + scale_color_discrete("Month") +
  scale_x_discrete("Time of Day",labels=hours) +
  scale_y_continuous("Bike Count") + ggtitle("Telegraph and 66th, Oakland, CA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot daily counts for the whole period
Telegraph66th$y <- as.numeric(Telegraph66th$X) - 1
dates <- unique(Telegraph66th$Date)
Telegraph66th$Date <- factor(Telegraph66th$Date,levels=as.character(dates))
Telegraph66th$x <- as.numeric(Telegraph66th$Date)
ylabels = unique(Telegraph66th$X)
xlabels = as.character(unique(Telegraph66th$Date))
mondays <- seq(1, length(xlabels), 7) - 1
xlabels[-mondays] <- "" # Only label dates on Mondays
ggplot(subset(Telegraph66th,count < 100),(aes(x=x,y=y))) + 
  geom_vline(xintercept = mondays,color='gray') + 
  geom_tile(alpha=0.6,aes(fill=count)) + 
  scale_fill_gradient(low="transparent", high="darkgreen") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous("Time of Day", labels=ylabels,breaks=0:23) +
  scale_x_discrete("Date",labels=xlabels)

