
library(augsynth)
library(reshape2)
library(dplyr)
library(tidyr)

data.raw = read.csv('owid-covid-data.csv')
# data.raw = na.omit(data.raw)
# data.raw$date = as.Date(data.raw$date,"%Y/%m/%d")
# data.use = data.raw %>% 
#   filter(date > '2022-10-01') %>%
#   filter(date < '2022-12-14') %>%
#   mutate(trt = case_when(date < '2022-11-20' ~ 0,
#                          location != 'Qatar' ~ 0,
#                          location == 'Qatar' ~ 1))

# data.panel = data.raw[,c(3,4,6)]
# data.panel = dcast(data.panel,location~date)
# data.panel = rbind(data.panel[180,],data.panel)
# data.panel = data.panel[-181,]
# rownames(data.panel) = data.panel$location
# data.panel = data.panel[,-1]
# ## 所有缺失值按0处理，先排除一切技术原因让代码顺利跑起???
# data.panel[is.na(data.panel)] = 0
# data.panel.smoothed = data.raw[,c(3,4,7)]
# data.panel.smoothed = dcast(data.panel.smoothed,location~date)



data.use <- data.raw %>% 
  filter(date > '2022-10-01') %>%
  filter(date < '2022-12-14') %>%
  mutate(trt = case_when(date < '2022-11-20' ~ 0,
                         location != 'Qatar' ~ 0,
                         location == 'Qatar' ~ 1)) %>%
  # filter(location %in% c('Qatar', 
  #                        'United Arab Emirates', 
  #                        'Bahrain',
  #                        'Saudi Arabia'))
  filter(location %in% c('Qatar',
                       'United Arab Emirates',
                       'Bahrain',
                       'Saudi Arabia',
                       'Israel',
                       'South Korea',
                       'Japan',
                       'France',
                       'Germany'))
data.use$date = as.Date(data.use$date,'%Y-%m-%d')
temp = data.use$hospital_beds_per_thousand
data.use$hospital_beds_per_thousand = (data.use$hospital_beds_per_thousand-mean(temp))/sd(temp)

# data.use[data.use$date != '2022-11-19',]$hospital_beds_per_thousand = NA
# data.use[is.na(data.use)] = 0 # 不是这种问题，报错的NA应该是在计算过程中出现的
# syn.Qatar = augsynth(new_cases ~ trt|hospital_beds_per_thousand, location, date, data.use,
#                 progfunc = "Ridge", scm = T, t_int = '2022-11-20')

# data.use[!is.na(data.use$hospital_beds_per_thousand),]$location
syn.Qatar = augsynth(new_cases ~ trt, 
                     location, date, data.use,
                     progfunc = "None", scm = T, 
                     t_int = as.Date('2022-11-20','%Y-%m-%d'),
                     min_1se = FALSE)
summary(syn.Qatar)
plot(syn.Qatar)
# plot(syn.Qatar,cv=T)

syn.Qatar.residual = augsynth(new_cases ~ trt|new_cases+hospital_beds_per_thousand, 
                              location, date, data.use,
                              progfunc = "Ridge", 
                              scm = T, 
                              t_int = as.Date('2022-11-20','%Y-%m-%d'),
                              lambda=syn.Qatar$lambda,
                              residualize=T)
summary(syn.Qatar.residual)
plot(syn.Qatar.residual)
# summary(syn.Qatar, stat_func = function(x) abs(sum(x)))
pred.Qatar = predict(syn.Qatar,att=F)
plot(1:73,pred.Qatar,type = 'l',col='blue',lwd=2,ylim=c(0,2000))
lines(1:73,data.use[data.use$location=='Qatar',]$new_cases,type = 'l',col='black',lwd=2)
abline(v=50,lty=2,lwd=2,col='black')

# plot(syn.Qatar, inf_type = "jackknife+")
