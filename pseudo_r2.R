Rsquared.glm <- function(o) {
  n <- length(o$residuals) # number of observations
  R2 <- ( 1 - exp( (o$deviance - o$null.deviance)/n ) ) / ( 1 - exp( -o$null.deviance/n ) )
  names(R2) <- 'pseudo.Rsquared'
  R2
}


Rsquared.Walsh <- function(o) {
  R2 <- 1 - o$deviance/o$null.deviance
  names(R2) <- 'pseudo.Rsquared'
  R2
}

