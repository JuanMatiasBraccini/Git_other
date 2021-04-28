if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

if(exists("fn.fig")==F)       source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Population dynamics/fn.fig.R"))
  if(exists("smart.par")==F)  source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Smart_par.R"))
packageLoaded <- function(name) 0 != length(grep(paste("^package:", name, "$", sep=""), search()))
nice.MDS.plot=function(MDS,pt.col,pt.bg,txt.col,txt.cex,PRED,col.pred,pred.cex)
{
  sites <- scores(MDS, display = "sites")
  spps  <- scores(MDS, display = "species")
  xlim <- range(sites[,1], spps[,1])
  ylim <- range(sites[,2], spps[,2])
  plot(MDS, type = "n", xlim = xlim, ylim = ylim,ylab='',xlab='')
  points(sites, bg = pt.bg, col=pt.col,pch = 21, cex = 1.2)
  text(spps, labels = rownames(spps), col =txt.col, cex = txt.cex)
  if(is.numeric(PRED)) PRED=round(PRED,1)
  if(!is.null(PRED)) text(sites, labels = PRED, col =col.pred, cex = pred.cex)
  legend("bottomright",paste("Stress=",round(MDS$stress,2)),bty='n')
}
interpret.multivariate=function(permanova)
{
  PERMANOVA=as.data.frame(permanova$aov.tab)
  Variation.explained=1-PERMANOVA$R2[match("Residuals",rownames(PERMANOVA))]
  return(list(PERMANOVA=PERMANOVA,Variation.explained=Variation.explained))
}
pairwise.adonis <- function(x,factors, sim.function = 'vegdist', sim.method = Best.indx, p.adjust.m ='bonferroni')
{
  co = combn(unique(as.character(factors)),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  
  
  for(elem in 1:ncol(co))
  {
    if(sim.function == 'daisy'){
      library(cluster); x1 = daisy(x[factors %in% c(co[1,elem],co[2,elem]),],metric=sim.method)
    } else{x1 = vegdist(x[factors %in% c(co[1,elem],co[2,elem]),],method=sim.method)}
    
    ad = adonis(x1 ~ factors[factors %in% c(co[1,elem],co[2,elem])] );
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  sig = c(rep('',length(p.adjusted)))
  sig[p.adjusted <= 0.05] <-'.'
  sig[p.adjusted <= 0.01] <-'*'
  sig[p.adjusted <= 0.001] <-'**'
  sig[p.adjusted <= 0.0001] <-'***'
  
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted,sig)
  #print("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  return(pairw.res)
  
}
fn.traditional.multivar=function(d,FORMULA)
{
  if(packageLoaded('vegan')==FALSE)require(vegan)
  if(packageLoaded('parallel')==FALSE)require(parallel)
  if(packageLoaded('MASS')==FALSE)require(MASS)
  d.res.var<<-d[,-which(IDVAR%in%names(d))]
  d.preds=d[,Predictors]
  Cores=detectCores()-1
  
  #1. nMDS ordination to identify groups
  Dis.indx=rankindex(d.preds, d.res.var, c("euc","man","gow","bray","jac","kul"))
  Best.indx<<-names(rev(sort(Dis.indx)))[1]
  MDS <- metaMDS(d.res.var, distance = Best.indx, autotransform = FALSE,trace = FALSE,parallel=Cores)

  #2. Permanova
  #note: adonis studies the differences in the group means
  #     adonis is more robust than anosim
  permanova <- adonis(FORMULA, d.preds,method = Best.indx,parallel=Cores)
  
    #2.1. interpret permanova
  Permanova.table=interpret.multivariate(permanova)
  
    #2.2 Pairwise comparisons
  Fctrs<<-d.preds[,sapply(d.preds,is.factor)]
  if(is.data.frame(Fctrs))permanova.pairwise=vector('list',ncol(Fctrs)) else
    permanova.pairwise=vector('list',1)
  names(permanova.pairwise)=names(Fctrs)
  for(ss in 1:length(permanova.pairwise))
   {
     if(is.data.frame(Fctrs))permanova.pairwise[[ss]] <- pairwise.adonis(x=d.res.var, factors=Fctrs[,ss])else
       permanova.pairwise[[ss]] <- pairwise.adonis(x=d.res.var, factors=Fctrs)
   }
  
  
  #3. Simper analysis to identify species that discriminate among groups
  #note: if there are strong differences, then a few species should discreminate among groups
  Simper=permanova.pairwise
  for(ss in 1:length(Simper))
  {
    if(is.data.frame(Fctrs))Simper[[ss]] <- simper(d.res.var, Fctrs[,ss],parallel=Cores) else
      Simper[[ss]] <- simper(d.res.var,Fctrs,parallel=Cores)
    
  }
    
  #Return stuff
  return(list(d=d,Best.indx=Best.indx,MDS=MDS,permanova=permanova,
              Permanova.table=Permanova.table,permanova.pairwise=permanova.pairwise,Simper=Simper))
  
}
Multivar.fn=function(DATA,ResVar,MultiVar,Predictors,IDVAR,Formula,DataSets)
{
  #create data matrix
  fn.reshp=function(d,Y,TimeVar,IdVAR,DataSets)
  {
    DATA.wide=reshape(d[,match(c(Y,IdVAR,TimeVar),names(d))],v.names=Y,
                      idvar=IdVAR,timevar=TimeVar,direction="wide")
    DATA.wide[is.na(DATA.wide)]=0
    colnames(DATA.wide)=gsub(paste(Y,".",sep=""), "", names(DATA.wide))
    props=DATA.wide
    props[,-which(IdVAR%in%names(props))]=props[,-which(IdVAR%in%names(props))]/rowSums(props[,-which(IdVAR%in%names(props))])
    Lista=vector('list',length(DataSets))
    names(Lista)=DataSets
    Lista$proportion=props
    if(!is.na(match("catch",DataSets)))  Lista$catch=DATA.wide
    if(!is.na(match("cpue",DataSets)))
    {
      DATA.wide.cpue=reshape(d[,match(c("cpue",IdVAR,TimeVar),names(d))],v.names="cpue",
                             idvar=IdVAR,timevar=TimeVar,direction="wide")
      DATA.wide.cpue[is.na(DATA.wide.cpue)]=0
      colnames(DATA.wide.cpue)=gsub(paste("cpue",".",sep=""), "", names(DATA.wide.cpue))
      Lista$cpue=DATA.wide.cpue
    }
    return(Lista)
  }
  Dat=fn.reshp(d=DATA,Y=ResVar,TimeVar=MultiVar,IdVAR=IDVAR,DataSets=DataSets) 
  #check what type of transformation is needed  
  #source:  Clarke & Warwick 2001. Change in marine communities: an approach to 
  #         statistical analysis and interpretation. Chapter 9.
  Trans.matrix=data.frame(Transf=c("none","root2","root4","log"),
                          Min=c(0,0.4,0.65,.875),
                          Max=c(0.4,0.65,0.875,1))  
  
  Store.transf=Dat
  fn.what.trans=function(d)
  {
    Log.Mean=log(apply(d,2,mean,na.rm=T))
    Log.SD=log(apply(d,2,sd,na.rm=T))  
    
    mod=lm(Log.SD~Log.Mean)
    Pred=predict(mod,Log.Mean=seq(min(Log.Mean),max(Log.Mean)),type='response')
    plot(Log.Mean,Log.SD)
    lines(Log.Mean,Pred,col=2)
    SLP=round(coef(mod)[2],3)
    text(mean(Log.Mean),quantile(Log.SD,probs=0.075),paste("slope=",SLP),col=2,cex=1.25)
    Trans=as.character(Trans.matrix$Transf[with(Trans.matrix, Min <= SLP & Max >= SLP)])
    legend("topleft",paste("res var=",names(Store.transf)[s]),bty='n')
    legend("bottomright",paste("Trans=",Trans),bty='n')
    return(Trans)
  }
  smart.par(n.plots=length(Dat),MAR=c(2,2,1,1),OMA=c(1,1.5,.1,.1),MGP=c(2.5,.7,0))
  for(s in 1:length(Dat)) Store.transf[[s]]=fn.what.trans(d=Dat[[s]][,-which(IDVAR%in%names(Dat[[s]]))]) 
  
  #transform data accordingly based on Store.tranfs 
  transf.fn=function(d,transf)
  {
    if(transf=="none")  d[,-which(IDVAR%in%names(d))]=d[,-which(IDVAR%in%names(d))]
    if(transf=="root2") d[,-which(IDVAR%in%names(d))]=d[,-which(IDVAR%in%names(d))]^0.5
    if(transf=="root4") d[,-which(IDVAR%in%names(d))]=d[,-which(IDVAR%in%names(d))]^0.25
    if(transf=="log")   d[,-which(IDVAR%in%names(d))]=log(d[,-which(IDVAR%in%names(d))]+1e-6)
    return(d)
    
  }
  for(s in 1:length(Dat)) Dat[[s]]=transf.fn(d=Dat[[s]],transf=Store.transf[[s]])
  
  #Explore patterns
  explore.fn=function(d)    
  {
    for(p in 1:length(Predictors))
    {
      x=d[,c(Predictors[p],names(d)[-which(IDVAR%in%names(d))])]  
      if(!is.factor(x[,1]))
      {
        if(length(unique(x[,1]))>30)
        {
          x[,1]=cut(x[,1],10) 
        }else x[,1]=as.factor(x[,1])
        
      }
      agg=aggregate(formula(paste(".",names(x)[which(names(x)%in%IDVAR)],sep="~")), x, mean)
      agg.sd=aggregate(formula(paste(".",names(x)[which(names(x)%in%IDVAR)],sep="~")), x, sd)
      smart.par(n.plots=length(2:ncol(agg)),MAR=c(2,2,1,1),OMA=c(1,1.5,.1,.1),MGP=c(2.5,.7,0))
      for(n in 2:ncol(agg))
      {
        xx=1: length(unique(agg[,1]))
        agg.sd[,n][is.na(agg.sd[,n])]=0
        plot(xx,agg[,n],pch=19,ylim=c(0,max(agg[,n]+agg.sd[,n])),ylab="",xlab="",xaxt='n',main=names(agg)[n],cex.axis=.85)
        segments(xx,agg[,n]-agg.sd[,n],xx,agg[,n]+agg.sd[,n])
        
      }
      mtext(names(Dat)[s],2,0.25,outer=T,las=3,cex=1.25)
      mtext(Predictors[p],1,-0.5,outer=T,cex=1.25)
      
    }
  }
  for(s in 1:length(Dat)) explore.fn(d=Dat[[s]])
  
  #apply multivariate analyses   
  STORE=Dat
  for(s in 1:length(STORE)) STORE[[s]]=fn.traditional.multivar(d=Dat[[s]],FORMULA=Formula)
  
  return(STORE)
}
fn.display.multivar=function(d,IDVAR,Predictors,MDS,Permanova.table,permanova.pairwise,Simper,NM,hndl,cexMDS)
{
  d.res.var<<-d[,-which(IDVAR%in%names(d))]
  d.preds=d[,Predictors]
  
  #1. nMDS ordination to identify groups
  fn.fig(paste(hndl,"MDS_stress.plot_",NM,sep=""),2400,2400)  #takes 5 sec per iteration
  par(mfcol=c(1,1))
  stressplot(MDS) #inspect stress with Shepard's plot
  dev.off()
  
    #show species groups
  fn.fig(paste(hndl,"MDS_species.plot_",NM,sep=""),2400,2400)  
  par(mfcol=c(1,1))
  nice.MDS.plot(MDS=MDS,pt.col=1,pt.bg=rgb(.1,.1,1,alpha=.2),txt.col=2,txt.cex=1,PRED=NULL,col.pred=1,pred.cex=.5)
  dev.off()
  
    #show ordination for each predictor
  fn.fig(paste(hndl,"MDS_predictors_",NM,sep=""),1200,2400)
  smart.par(n.plots=length(Predictors),MAR=c(1.75,3.5,1.5,.1),OMA=c(3,1,.1,2),MGP=c(1,.8,0))
  for(p in 1:length(Predictors)) 
  {
    nice.MDS.plot(MDS=MDS,pt.col='transparent',pt.bg="transparent",txt.col="transparent",txt.cex=.9,
                  PRED=d.preds[,match(Predictors[p],names(d.preds))],col.pred=1,pred.cex=cexMDS)
    mtext(Predictors[p],3,cex=1.5)
  }
  dev.off()
  
  
  #2. Permanova
  write.csv(Permanova.table,paste(hndl,"Permanova.table.csv",NM,sep=""))
  
  #2.2 Pairwise comparisons
  #permanova.pairwise
  
  #3. Simper analysis to identify species that discriminate among groups

  #Simper
  
 }
