##################################################################################
###  	SECTION 3: BOOTSTRAP SE of delta indices
##################################################################################

#------------------ Bootstrap parameters --------------------

# Define bootstrap parameters
# Define delta model (1 = Delta-lognormal; 2 - Delta-gamma)
deltaModel <- 1
# Number of bootstrap simulations
nBoot <- 5


#------------------ Bootstrap function --------------------

# FUNCTION:
runDeltaBoot <- function(DataFile,deltaModel,nBoot)
{
  # Record and print start time
  timeStart = Sys.time()
  print(timeStart)
  
  # Defife vector of years
  # Make vector with years for positive (non-zero) dataset
  Years <- DataFile[DataFile[,1]>0,2]
  Years <- as.factor(Years[])
  Years <- levels(Years) # These are all possible years than can show up when bootstraping the delta index
  # Number of years
  nYears <- length(Years)
  
  # Define output dataframe to store bootstrap index results
  BiIndexBoot <-matrix(nrow=nYears, ncol=nBoot)
  PosIndexBoot <- matrix(nrow=nYears, ncol=nBoot)
  DeltaXboot <- matrix(nrow=nYears, ncol=nBoot)
  
  # Loop over number of bootstrap sampling
  for (iBoot in 1:nBoot)
  {	
    # Initialize Bootrows
    BootRows <- rep(NA,0)
    # Resample for each year
    for(iyear in 1:length(Years))
    {
      # Pass year value
      year <- Years[iyear]
      # Identify rows in "DataFile" for current year in the loop
      irows <- 1:nrow(DataFile)
      irows <- irows[DataFile[,2]==year]
      
      # Sample with replacement from "irows"
      BootRowsTemp <- sample(irows,length(irows),replace=T)
      # Bind irows for all years
      BootRows<-c(BootRows,BootRowsTemp)
    }
    
    # Select "BootRows" from "DataFile"
    BootData <- DataFile[BootRows,]
    
    # If delta-lognormal model
    if (deltaModel==1)
      DeltaXout <- runDeltaLog(BootData)
    
    if(deltaModel==2)
      DeltaXout <- runDeltaGam(BootData)
    
    # Populate output tables
    # This loop avoids "NA" problems if a year is lost from delta index after bootstrap resampling, 
    # this can happen for years with low sample sizes if no bsh positive sets are sampled
    for(iyear in 1:length(Years))
    {  			
      year <- Years[iyear]
      # Is the year present in "DeltaXout"?
      testYear <- is.element(year, DeltaXout$year)
      # If T then populate output matrices with corresponding delta index for that year
      if (testYear == T)
      {
        BiIndexBoot[iyear,iBoot] <- DeltaXout[DeltaXout$year==year,2]
        PosIndexBoot[iyear,iBoot] <- DeltaXout[DeltaXout$year==year,3]
        DeltaXboot[iyear,iBoot] <- DeltaXout[DeltaXout$year==year,4]
      }
    }
    print(iBoot) # Print number of bootstrap simulation
  }
  
  # Compute SE of indices
  BiBootSE <- sd(t(BiIndexBoot),na.rm = T)
  cbind(BiBootSE)
  PosBootSE <- sd(t(PosIndexBoot),na.rm = T)
  cbind(PosBootSE)
  DeltaBootSE <- sd(t(DeltaXboot),na.rm = T)
  #cbind(DeltaBootSE)
  
  # Compute 95% CI of indices
  # Binomial
  BiBoot.ci95 <- quantile(t(BiIndexBoot),c(.025,.975), na.rm = T)
  BiBoot.ci95L <- BiBoot.ci95[1]
  BiBoot.ci95U <- BiBoot.ci95[2]
  cbind(BiBoot.ci95L)
  cbind(BiBoot.ci95U)
  # Positve sets
  PosBoot.ci95 <- quantile(t(PosIndexBoot),c(.025,.975), na.rm = T)
  PosBoot.ci95L <- PosBoot.ci95[1]
  PosBoot.ci95U <- PosBoot.ci95[2]
  cbind(PosBoot.ci95L)
  cbind(PosBoot.ci95U)
  # Delta-X
  DeltaBoot.ci95 <- quantile(t(DeltaXboot),c(.025,.975), na.rm = T)
  DeltaBoot.ci95L <- DeltaBoot.ci95[1]
  DeltaBoot.ci95U <- DeltaBoot.ci95[2]
  cbind(PosBoot.ci95L)
  cbind(PosBoot.ci95U)
  
  
  # Record and print end time
  timeEnd = Sys.time()
  print(timeEnd)
  # Compute and print routine time
  print(timeEnd-timeStart)
  
  # Output results
  if (deltaModel==1)
  {
    return(list(biSE=BiBootSE, logSE=PosBootSE, deltaLogSE=DeltaBootSE))
  }
  if (deltaModel==2)
  {
    return(list(biSE=BiBootSE, gamSE=PosBootSE,deltaGamSE=DeltaBootSE))
  }
}
BootSEout <- runDeltaBoot(DataFile,deltaModel,nBoot)
