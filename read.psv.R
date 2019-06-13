read.psv<-function(fn, nsamples)
{    
	#Use this routine to read the binary output from 
	#the -mcsave option in ADModel builder <program.name.psv>
	#e.g:
	#		mc=read.psv(fn="simple.psv")
	#if the number of samples is greater than 1e5,  you'll 
	#have to increase the second argument to the corresponding 
	#values.  The default size is ok of less than 1e5 samples
	
	#Steve Martell (Adapted from Anders Nielsen)
     filen <- file(fn, "rb")
	 nopar <- readBin(filen, what = integer(), n = 1)
	 mcmc <- readBin(filen, what = numeric(), n = nopar * nsamples)
	 mcmc <- matrix(mcmc, byrow = TRUE, ncol = nopar)
	 close(filen)
	return(mcmc)
}
