library(Synth)

data.panel = data.use[,c(3,4,6)]
data.panel = dcast(data.panel,location~date)
X1 = t(data.panel[6,51:74])
X0 = t(data.panel[c(1,2,3,4,5,7,8,9),51:74])
Z1 = t(data.panel[6,2:50])
Z0 = t(data.panel[c(1,2,3,4,5,7,8,9),2:50])
old.Qatar=synth(X1=X1,X0=X0,Z1=Z1,Z0=Z0)
pred.Qatar.old = as.vector(t(old.Qatar$solution.w) %*% as.matrix(data.panel[-6,-1]))
plot(1:73,pred.Qatar.old,type = 'l',col='blue',lwd=2,ylim=c(0,2000))
lines(1:73,data.use[data.use$location=='Qatar',]$new_cases,type = 'l',col='black',lwd=2)
abline(v=50,lty=2,lwd=2,col='black')




path.plot(synth.res=old.Qatar,dataprep.res=t(data.panel))
data.sch = data.use[,c(1,3,4,6)]
data.sch$location = as.factor(data.sch$location)
data.sch$location = as.numeric(data.sch$location)
dataprep.sch = 
  dataprep(
    foo=data.sch,
    predictors.op = "mean",
    dependent = "new_cases",
    unit.variable = "location",
    time.variable = "date",
    treatment.identifier = 6,
    controls.identifier = c(1, 2, 3, 4, 5, 7, 8, 9),
    unit.names.variable = "iso_code")



data(synth.data)
dataprep.out<-
  dataprep(
    foo = synth.data,
    predictors = c("X1", "X2", "X3"),
    predictors.op = "mean",
    dependent = "Y",
    unit.variable = "unit.num",
    time.variable = "year",
    special.predictors = list(
      list("Y", 1991, "mean"),
      list("Y", 1985, "mean"),
      list("Y", 1980, "mean")
    ),
    treatment.identifier = 7,
    controls.identifier = c(29, 2, 13, 17, 32, 38),
    time.predictors.prior = c(1984:1989),
    time.optimize.ssr = c(1984:1990),
    unit.names.variable = "name",
    time.plot = 1984:1996
  )
synth.out <- synth(dataprep.out)
round(synth.out$solution.w,2)
synth.out$solution.v 
round(old.Qatar$solution.w,2)
round(syn.Qatar$weights,2)



