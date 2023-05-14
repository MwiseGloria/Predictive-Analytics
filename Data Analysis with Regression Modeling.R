

install.packages(c("car", "forecast"), repos = "http://cran.us.r-project.org")
 
 #missing values 
BB.df <- as.data.frame(read.csv("BaseballData.csv"))
sum(is.na(BB.df))
#no missing value 

# What proportion of players who are free agency eligible are free agents? 
FAEFA=sum((BB.df$free_agency_eligible==1) & (BB.df$free_agent==1))
FAE= sum(BB.df$free_agency_eligible)
round(FAEFA/FAE , 2) 

 
Mike_Name= ifelse(BB.df$player_name =='Mike', 1, 0)
BB.df=as.data.frame(cbind(BB.df, Mike_Name))
sum(BB.df$Mike_Name)

# salary stats of players whose name is not Mike
mean(BB.df[which(BB.df$Mike_Name== 0), 2])
sd(BB.df[which(BB.df$Mike_Name== 0), 2])

# all players salary
mean(BB.df$salary)
sd(BB.df$salary)

# outliers
quantile(BB.df$salary)
IQR=IQR(BB.df$salary)
IQR
#outlier range 
LB=quantile(BB.df$salary)[2] -1.5 *IQR
UB=quantile(BB.df$salary)[4]+1.5 *IQR
outliers= sum(((BB.df$salary>UB) | (BB.df$salary< LB) ))
100*outliers/nrow(BB.df)
# 0.89 % of the data is formed by outliers based on salary. 

#1f. 
#box plot of salary for players named Mike
boxplot(BB.df[which(BB.df$Mike_Name== 1), 2])
#box plot of salary for players not named Mike
boxplot(BB.df[which(BB.df$Mike_Name== 0), 2]) 

## Modeling
set.seed(2)
train.index <- sample(c(1:dim(BB.df)[1]), dim(BB.df)[1]*0.7)  
train.df <- BB.df[train.index, ]
valid.df <- BB.df[-train.index, ]

#M1=lm(BB.df$salary~ BB.df$free_agency_eligible+BB.df$hits+BB.df$strike_outs+BB.df$runs_batted_in+BB.df$Mike_Name, data=train.df)
M1=lm(salary~ free_agency_eligible+hits+strike_outs+runs_batted_in+Mike_Name, data=train.df)
summary(M1)   
#
summary(M1)$coefficients[6,"Pr(>|t|)"]
#  free agency eligible and run batted in are statistically significant at the significance level of 0.001 
#hits and strike_outs and Mike_name are not statistically significant at the level of 0.001. 

# No, name Mike is not statistically significant.  

 
#install.packages("car")
library(car)
vif(M1)>4

M2=step(M1)
summary(M2)
#M2 is salary ~ free_agency_eligible + hits + strike_outs + runs_batted_in
M1S=c(AIC(M1), BIC(M1))
M2S= c(AIC(M2), BIC(M2)) 
M1S
M2S
#M2S both AIC and BIC are lower in M2

# 
#accuracy 
predictM1= predict(M1, data=valid.df)
predictM2= predict(M2, data=valid.df)
#install.packages("forecast", repos = "http://cran.us.r-project.org")
library(forecast)
accuracy(predictM1, valid.df$salary)
accuracy(predictM2, valid.df$salary)
#M2 performs slightly better 

# predictions 
nd=data.frame(free_agency_eligible=c(1,1, 1, 0), hits=c(130,140, 250, 200), strike_outs=c(116,60, 40, 100), runs_batted_in=c(100,50, 100, 100), Mike_Name=c(0,1,0, 1))
predictions1= predict(M1, newdata=nd)
predictions1
nd=cbind(nd, predictions1)
nd
write.csv(nd, "Predictions.csv")
