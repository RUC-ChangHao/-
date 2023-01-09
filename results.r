library(augsynth)
library(reshape2)
library(dplyr)
library(tidyr)

data.raw = read.csv('owid-covid-data.csv')
data.use = data.raw[,c(2,3,4,6)]
data.use <- data.use %>% 
  filter(date > '2022-10-01') %>%
  filter(date < '2022-12-14') %>%
  mutate(trt = case_when(date < '2022-11-20' ~ 0,
                         location != 'Qatar' ~ 0,
                         location == 'Qatar' ~ 1)) %>%
  filter(continent %in% c("Asia","Europe","Africa"))

data.use$date = as.Date(data.use$date,'%Y-%m-%d')
countries.NA = unique(data.use[is.na(data.use$new_cases),]$location)
data.use = data.use %>% 
  filter(!location %in% countries.NA)


temp = data.use[data.use$location=='Qatar',]
######## none
syn.Qatar = augsynth(new_cases ~ trt, 
                     location, date, data.use,
                     progfunc = "None", scm = T, 
                     t_int = as.Date('2022-11-20','%Y-%m-%d'),
                     min_1se = FALSE)
par(mar=c(4,4,2,1))
pred.Qatar = predict(syn.Qatar,att=F)
plot(temp$date,pred.Qatar,type = 'l',col='blue',lwd=2,ylim=c(0,2000),xlab = '',ylab='')
lines(temp$date,data.use[data.use$location=='Qatar',]$new_cases,type = 'l',col='black',lwd=2)
abline(v=as.Date("2022-11-20","%Y-%m-%d"),lty=2,lwd=2,col='black')
mean((pred.Qatar[1:49]-data.use[data.use$location=='Qatar',]$new_cases[1:49])^2)

syn.Qatar.summary = summary(syn.Qatar)
syn.Qatar.fiture=plot(syn.Qatar)
syn.w = round(syn.Qatar$weights,2)
print(syn.w[syn.w!=0,])




######## ridge
syn.Qatar.ridge = augsynth(new_cases ~ trt, 
                     location, date, data.use,
                     progfunc = "Ridge", scm = T, 
                     t_int = as.Date('2022-11-20','%Y-%m-%d'),
                     min_1se = FALSE)
par(mar=c(4,4,2,1))
pred.Qatar.ridge = predict(syn.Qatar.ridge,att=F)
plot(temp$date,pred.Qatar.ridge,type = 'l',col='blue',lwd=2,ylim=c(0,2000),xlab = '',ylab='')
lines(temp$date,data.use[data.use$location=='Qatar',]$new_cases,type = 'l',col='black',lwd=2)
abline(v=as.Date("2022-11-20","%Y-%m-%d"),lty=2,lwd=2,col='black')
mean((pred.Qatar.ridge[1:49]-data.use[data.use$location=='Qatar',]$new_cases[1:49])^2)
mean((pred.Qatar.ridge[50:73]-data.use[data.use$location=='Qatar',]$new_cases[50:73]))


syn.Qatar.summary.ridge = summary(syn.Qatar.ridge)
syn.Qatar.ridge.fiture=plot(syn.Qatar.ridge)
syn.ridge.w = round(syn.Qatar.ridge$weights,2)
print(sum(syn.ridge.w!=0))

par(mar=c(3,3,2,1))
plot(syn.ridge.w,col='red',pch=20,lwd=2,xlab='',ylab='')
lines(syn.w,col='blue',pch=20,type='p',lwd=2)
