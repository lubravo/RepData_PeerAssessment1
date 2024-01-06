setwd("/Users/lubravo19/Documents/Labinfo/Cousera\ trainings/Reproducible\ Research/assignment week2")
unzip("repdata_data_activity.zip")
library("dplyr")
library("ggplot2")
install.packages("plotly")
library(plotly)
act_df<-read.csv("activity.csv")
act_df$date<-as.Date(act_df$date)


## Histogram
st_bydy_sum<-aggregate(steps ~ date,data = act_df, FUN = "sum", na.rm = T)
names(st_bydy_sum)[names(st_bydy_sum) == "steps"]<-"Total_steps"

## ggplot(data = act_df,aes(x=date, y=steps)) + geom_col(color = "blue",fill = "blue") + labs(x="Dates", y = "Steps") + theme_bw()
ggplot(data = st_bydy_sum,aes(x=date, y=Total_steps)) + geom_col(color = "blue",fill = "blue") + labs(x="Dates", y = "Steps") + theme_bw()
## Report mean and median

st_bydy_mean<-aggregate(steps ~ date,data = act_df, FUN = "mean", na.rm = T)


names(st_bydy_mean)[names(st_bydy_mean) == "steps"]<-"average_steps"
ggplot(st_bydy_mean, aes(x = date,y=average_steps)) + geom_line() + scale_x_date(date_labels = "%d %b %Y")

st_bydy_median<-aggregate(steps ~ date, data = act_df, FUN = (function(x){ifelse(sum(x==0) > 0 & sum(x != 0) > 0,median(x[x>0]), median(x))}))


names(st_bydy_median)[names(st_bydy_median) == "steps"]<-"median_steps"
ggplot(st_bydy_median, aes(x = date,y=median_steps)) + geom_line() + scale_x_date(date_labels = "%d %b %Y")

nrow(st_bydy_mean)

## 5 minute interval


st_bydy_5min<-aggregate(steps ~ interval,data=act_df, FUN = (function(x){ifelse(sum(x==0) > 0 & sum(x != 0) > 0,mean(x[x>0]), mean(x))})) 

plotl<-ggplot(st_bydy_5min, aes(x = interval,y= steps))+ geom_line(color = "blue")  + labs(y = "Average number of steps", x = "Intervals")
ggplotly(plotl)



## Imputing data

## NA value report

miss_vals<-sum(is.na(act_df$steps))
## adding mean to NA

idx_nas<-is.na(act_df$steps)

## Calculate average steps by intervals

st_bydy_int_avg<-aggregate(steps ~ interval,data=act_df, FUN = (function(x){ifelse(sum(x==0) > 0 & sum(x != 0) > 0,mean(x[x>0]), mean(x))})) 

names(st_bydy_int_avg)[names(st_bydy_int_avg) == "steps"]<-"average_steps"

## Replace NAs with mean values
act_df_imp<- act_df


## Replace NA values with avreage steps by interval
act_df_imp = act_df_imp %>% left_join(st_bydy_int_avg, by="interval") %>% mutate(steps = ifelse(is.na(steps),average_steps,steps), .keep='unused')

## histogram 2

ggplot(data = act_df_imp,aes(x=date, y=steps)) + geom_col(color = "blue",fill = "blue") + labs(x="Dates", y = "Steps") + theme_bw()


## Report mean and median with no NAs

st_bydy_mean2<-aggregate(steps ~ date,data = act_df_imp, FUN = "mean", na.rm = T)


names(st_bydy_mean2)[names(st_bydy_mean2) == "steps"]<-"average_steps"
ggplot(st_bydy_mean2, aes(x = date,y=average_steps)) + geom_line() + scale_x_date(date_labels = "%d %b %Y")

st_bydy_median2<-aggregate(steps ~ date, data = act_df_imp, FUN = (function(x){ifelse(sum(x==0) > 0 & sum(x != 0) > 0,median(x[x>0]), median(x))}))


names(st_bydy_median2)[names(st_bydy_median2) == "steps"]<-"median_steps"
ggplot(st_bydy_median2, aes(x = date,y=median_steps)) + geom_line() + scale_x_date(date_labels = "%d %b %Y")


## Weekends and weekdays analysis

## adding factor based on weekdays and weekends

act_df_imp$weekfact <- factor(ifelse(weekdays(act_df_imp$date, abbr = TRUE) %in% c("Sat","Sun"), "Weekends","Weekdays"))
## Plots
## New average steps 

st_bydy_int_avg2<-aggregate(steps ~ interval + weekfact,data=act_df_imp, FUN = (function(x){ifelse(sum(x==0) > 0 & sum(x != 0) > 0,mean(x[x>0]), mean(x))})) 


weekpl<- ggplot(st_bydy_int_avg2, aes(x = interval,y= steps))+ geom_line(color = "blue")  + labs(y = "Average number of steps", x = "Intervals") + facet_wrap(~weekfact,ncol=1)
ggplotly(weekpl)