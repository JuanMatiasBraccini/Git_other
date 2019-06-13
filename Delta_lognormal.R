
# FUNCTION FOR RUNNING DELTA-X STANDARDISATION METHODS (Lo et al. 1992; Vignaux, 1994)

#Notes: . input file: cpue on the first column followed by Year and other explanatory variables
#       . first do model selection outside this code using stepAIC (binomial and lognormal separately)
#				. Delta indices are computed for several error distributions. Bootstraping is used to
# 				compute confidence intervals.
# 			.	Purpose: compute relative abundance indices 

library(MASS)
library(lattice)



Delta_Log=function(DATA,VARIABLES,FORMULA.bin,FORMULA.pos)
{
 
  # 1. Put data in proper shape
  DataFile=DATA[,match(VARIABLES,names(DATA))]
  
      # Create binary dataset (0/1 for CPUE)
  BiData <- DataFile
  BiData[,1] <- as.numeric(DataFile[,1]>0)
  
      # Create positive (non-zero) dataset
  PosData <- DataFile[DataFile[,1]>0,]

  
	# 2. Run binomial GLM
	GLMbi <- glm(FORMULA.bin, data=BiData, family="binomial", maxit=100)
  
  
	    # Compute constant factor for categorical variables 
	 # This quantity is required to extract the year effect (see Maunder and Punt, 2004; Eq. 9)
  # dummy.coef extracts coefficients in terms of the original levels of the coefficients 
  # rather than the coded variables
	biCatTemp <- 0
	if (length(dummy.coef(GLMbi))>2) # At least one explanatory variable besides year (>2, > intercept and year) 
	{
		for (i in 3:length(dummy.coef(GLMbi)))
		{
			# Compute mean coef. for each categorical variable and populate "biCatTemp" vector
			biCatTemp[i-1]<- mean(dummy.coef(GLMbi)[[i]])
		}
		# Sum mean coefs. of categorical variables
		biCatFact <- sum(biCatTemp) # This is the constant factor for exp. vars. to use in Eq. 9 (see Maunder and Punt, 2004)
	}
	if (length(dummy.coef(GLMbi))==2) # No constants are needed to extract the year effect if only the year factor exist
		biCatFact <- 0

	    # Compute the sum of coefficients (intercept + year coeffs + constant factors)
	BiSumCoeff <- dummy.coef(GLMbi)[[1]] + dummy.coef(GLMbi)[[2]] + biCatFact
	    # Extract the year effects by applying the inverse of the logit link (Eq. 9 in Maunder and Punt, 2004) 
	BiIndex <- exp(BiSumCoeff)/(1+exp(BiSumCoeff))

 
	# 3. Run lognormal GLM
	GLMlog <- glm(FORMULA.pos, data=PosData, family=gaussian, maxit=100)
     
	  # Compute bias correction factor for log transformation
    biasCorr <- GLMlog$deviance/GLMlog$df.residual/2     
  	
	  # Compute constant factor for categorical variables 
	 # This quantity is required to extract the year effect (see Maunder and Punt, 2004; Eq. 9) 
    logCatTemp <- 0
    if (length(dummy.coef(GLMlog))>2) # At least one explanatory variable besides year (>2, > intercept and year)
    {
	    for (i in 3:length(dummy.coef(GLMlog))) 
	    {
		    # Compute mean coef. for each categorical variable and populate "logCatTemp" vector
		    logCatTemp[i-1] <- mean(dummy.coef(GLMlog)[[i]])
		}
		# Sum mean coefs. of categorical variables
		logCatFact <- sum(logCatTemp) # This is the constant factor for exp. vars. to use in Eq. 9 (see Maunder and Punt, 2004)
    }
  	if (length(dummy.coef(GLMlog))==2) # No constants are needed to extract the year effect if only the year factor exist
  		logCatFact <- 0
  	
	  # Compute the sum of coefficients (intercept + year coeffs + constant factors + bias factor)
    LogSumCoeff <- dummy.coef(GLMlog)[[1]] + dummy.coef(GLMlog)[[2]] + logCatFact + biasCorr
	  # Extract the year effects by applying the inverse of the logit link (Eq. 9 in Maunder and Punt, 2004) 
  	LogIndex <- exp(LogSumCoeff)

  

	# 4. Compute Delta-lognormal index and store results 

    # Subset "BiIndex" for only the years in "LogIndex"
  	  # (some years are lost in the lognormal GLM due to lack of positive sets)
  	BiIndex <- BiIndex[names(LogIndex)]
  
   	# Compute delta-lognormal index 
    	DeltaLog <- cbind(BiIndex*LogIndex)
  
  	# Define dataframe to store results of delta-lognormal model
  	DeltaLogOut <- matrix(NA, length(BiIndex), 4)
	DeltaLogOut <- as.data.frame(DeltaLogOut)
	names(DeltaLogOut) <- c("year","biIndex","logIndex","deltaLog")
  
	  # Populate dataframe
	DeltaLogOut$year <- row.names(cbind(BiIndex*LogIndex))
	DeltaLogOut$biIndex <- cbind((BiIndex)[])
	DeltaLogOut$logIndex <- cbind((LogIndex)[])
	DeltaLogOut$deltaLog <- DeltaLog[,1]
  
	  # Sort dataframe by ascending year
	DeltaLogOut <- DeltaLogOut[order(DeltaLogOut$year),]
	
  return(list(Bi.part=GLMbi,Lognormal.part=GLMlog,Delta.index=DeltaLogOut))
}