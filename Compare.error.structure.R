handl_OneDrive=function(x)paste('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias',x,sep='/')

source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_other/pseudo_r2.R"))
library(bbmle)

#Plot fit diagnostics
fn.plot.diag=function(MODEL,TEXT,SPECIES)
{
  RES=MODEL$residuals   #residuals
  Std.RES=RES/sd(RES)   #standardised residuals (res/SD(res))
  #Std.RES=rstandard(MODEL)
  
  par(mfcol=c(2,2),las=1,mar=c(3,3,2,1),oma=c(2.5,.1,.1,.1),las=1,mgp=c(2,.5,0),cex.axis=.8,cex.lab=1.1)
  qqnorm(RES,main="",ylim=c(-5,5),xlim=c(-5,5),ylab="Residuals",xlab="Quantiles of standard normal distribution")
  qqline(RES, col = 'grey40',lwd=1.5,lty=2)
  
  hist(Std.RES,xlim=c(-5,5),ylab="Frequency",xlab="Stan. residuals",main="",col="grey",breaks=50)
  box()
  
  plot(predict(MODEL),Std.RES,ylim=c(-4,12),ylab="Stan. residuals",xlab="Expected values")
  abline(0,0,lwd=1.5,lty=2,col='grey40')
  
  plot(predict(MODEL),sqrt(abs(Std.RES)),ylim=c(0,2.6),ylab="Square root of stan. residuals",xlab="Expected values")
  
  mtext(paste(TEXT,SPECIES),3,outer=T,lin=-1)
}

#Compare different model deviances
fun.comp.dev=function(MOD1,MOD2)
{
  dLL <- 2 * (logLik(MOD1) - logLik(MOD2))
  dev=pchisq(dLL, df = 1, lower.tail = FALSE)
  return(dev[1])
}

#TABLE FIT
Table.fit=function(MODEL.LIST,Rsquared)
{
  #AIC tables
  TABLA=AICtab(MODEL.LIST,delta=T,weights=T,sort=F)
  LogLike=R2=vector(length=length(MODEL.LIST))
  
  if(Rsquared=="YES")
  {
    for (i in 1:length(LogLike))
    {
      LogLike[i]=logLik(MODEL.LIST[[i]])[1]
      R2[i]=Rsquared.glm(MODEL.LIST[[i]])
    }
    TABLA=data.frame(Model=names(MODEL.LIST),df=TABLA[2],LogLike=LogLike,dAIC=TABLA[1],
                     weight=TABLA[3],R2=R2)
  }

  if(Rsquared=="NO")
  {
    for (i in 1:length(LogLike))
    {
      LogLike[i]=logLik(MODEL.LIST[[i]])[1]
    }
    TABLA=data.frame(Model=names(MODEL.LIST),df=TABLA[2],LogLike=LogLike,dAIC=TABLA[1],
                     weight=TABLA[3])
  }

  return(TABLA)
}

#Residuals vs Fitted values
Res.pred.fun=function(model)
{
  output <- data.frame(resid = resid(model), fitted = fitted(model))
  ggplot(output, aes(fitted, resid)) + geom_jitter(position = position_jitter(width = 0.25),
                                                   alpha = 0.5) 
}
Res.pred.cut.fun=function(model)
{
  output <- within(data.frame(resid = resid(model), fitted = fitted(model)), {
    broken <- cut(fitted, hist(fitted, plot = FALSE)$breaks)
  })
  ggplot(output, aes(broken, resid)) + geom_boxplot() + geom_jitter(alpha = 0.25)
}

fn.compare.errors=function(MODELS.LIST,coef.length)
{
  #Compare coefficients for positive records
  Compare.pos.coefs=sapply(MODELS.LIST, function(x) coef(x)[1:coef.length])
  
  #Compare loglikelihoods
  LogLikes= rbind(logLik = sapply(MODELS.LIST, function(x) round(logLik(x), digits = 0)),
                  Df = sapply(MODELS.LIST, function(x) attr(logLik(x), "df")))
  
  #Compare deviance

  Pairwise=t(combn(names(MODELS.LIST),2))  #grid of models for pairwise comparisons
  Dev.Comp=vector('list',length=nrow(Pairwise))
  names(Dev.Comp)=paste(Pairwise[,1],Pairwise[,2])
  for(p in 1:nrow(Pairwise)) 
    {
    Dev.Comp[[p]]=fun.comp.dev(MODELS.LIST[[match(Pairwise[p,][1],names(MODELS.LIST))]],
                   MODELS.LIST[[match(Pairwise[p,][2],names(MODELS.LIST))]])
    }
  
  #Residuals vs Fitted values                                      
  for (p in 1:length(MODELS.LIST)) Res.pred.fun(MODELS.LIST[[p]])
  for (p in 1:length(MODELS.LIST)) Res.pred.cut.fun(MODELS.LIST[[p]])

  #Diagnostics
  for (p in 1:length(MODELS.LIST))fn.plot.diag(MODELS.LIST[[p]],names(MODELS.LIST)[p],SPECIES.vec[i])


  return(list(Pos.coefs.comp=Compare.pos.coefs,Loglike.comp=LogLikes,Deviance.comp=Dev.Comp))
}

fn.compare.zeroinfl.errors=function(MODELS.LIST,coef.length)
{
  #Compare coefficients for positive records
  Compare.pos.coefs=sapply(MODELS.LIST, function(x) coef(x)[1:coef.length])
  
   
  #Diagnostics
  for (p in 1:length(MODELS.LIST))fn.plot.diag(MODELS.LIST[[p]],names(MODELS.LIST)[p],SPECIES.vec[i])
  
  
  
  #   #Compar 0 predictions
  #   Table.0.preds= round(c("Obs" = sum(All.data.train$Catch.Target < 1),
  #                           #"ML-Pois" = sum(dpois(0, fitted(fm_pois))),
  #                           "Delta-Bin" = sum(predict(GLM.hurdle.Pois, type = "prob")[,1])
  #                           "Pois-Hurdle" = sum(predict(Binomial, type = "prob")[,1]),
  #                           "NB-Hurdle" = sum(predict(GLM.hurdle.neg.bin, type = "prob")[,1]),
  #                           "ZIPois" = sum(predict(Zero.inf.pois, type = "prob")[,1]),
  #                           "ZINB" = sum(predict(Zero.inf.negbin, type = "prob")[,1])))
  
  
  return(list(Pos.coefs.comp=Compare.pos.coefs))
}












                     
# "Perhaps
# the hurdle model is slightly preferable because it has the nicer interpretation: there is one
# process that controls whether a patient sees a physician or not, and a second process that
# determines how many oce visits are made."




