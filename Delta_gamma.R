
# FUNCTION FOR RUNNING DELTA-X STANDARDISATION METHODS (Lo et al. 1992; Vignaux, 1994)

#Notes: . input file: cpue on the first column followed by Year and other explanatory variables
#       . first do model selection outside this code using stepAIC (binomial and lognormal separately)
#				. Delta indices are computed for several error distributions. Bootstraping is used to
# 				compute confidence intervals.
# 			.	Purpose: compute relative abundance indices 

library(MASS)
library(lattice)



Delta_Gam=function(DATA,VARIABLES,FORMULA.bin,FORMULA.pos)
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
  GLMgam<-glm(FORMULA.pos, data=PosData, family=Gamma(link=log), maxit=100)
     
	
	  # Compute constant factor for categorical variables 
	 # This quantity is required to extract the year effect (see Maunder and Punt, 2004; Eq. 9) 
    gamCatTemp <- 0
    if (length(dummy.coef(GLMgam))>2) # At least one explanatory variable besides year (>2, > intercept and year)
    {
	    for (i in 3:length(dummy.coef(GLMgam))) 
	    {
		    # Compute mean coef. for each categorical variable and populate "gamCatTemp" vector
	      gamCatTemp[i-1] <- mean(dummy.coef(GLMgam)[[i]])
		}
		# Sum mean coefs. of categorical variables
	    gamCatFact <- sum(gamCatTemp) # This is the constant factor for exp. vars. to use in Eq. 9 (see Maunder and Punt, 2004)
    }
  	if (length(dummy.coef(GLMgam))==2) # No constants are needed to extract the year effect if only the year factor exist
  	  gamCatFact <- 0
  	
	  # Compute the sum of coefficients (intercept + year coeffs + constant factors)
    GamSumCoeff <- dummy.coef(GLMgam)[[1]] + dummy.coef(GLMgam)[[2]] + gamCatFact 
	  # Extract the year effects by applying the inverse of the log link  
  	GamIndex <- exp(GamSumCoeff)

  

	# 4. Compute Delta-lognormal index and store results 

    # Subset "BiIndex" for only the years in "GamIndex"
  	  # (some years are lost in the gamma GLM due to lack of positive sets)
  	BiIndex <- BiIndex[names(GamIndex)]
  
   	# Compute delta-lognormal index 
    	DeltaGam <- cbind(BiIndex*GamIndex)
  
  	# Define dataframe to store results of delta-lognormal model
    DeltaGamOut <- matrix(NA, length(BiIndex), 4)
    DeltaGamOut <- as.data.frame(DeltaGamOut)
	names(DeltaGamOut) <- c("year","biIndex","GamIndex","DeltaGam")
  
	  # Populate dataframe
  DeltaGamOut$year <- row.names(cbind(BiIndex*GamIndex))
  DeltaGamOut$biIndex <- cbind((BiIndex)[])
  DeltaGamOut$GamIndex <- cbind((GamIndex)[])
  DeltaGamOut$DeltaGam <- DeltaGam[,1]
  
	  # Sort dataframe by ascending year
  DeltaGamOut <- DeltaGamOut[order(DeltaGamOut$year),]
	
  return(list(Bi.part=GLMbi,Gamma.part=GLMgam,Delta.index=DeltaGamOut))
}