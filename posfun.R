posfun=function(x,eps,pen)
{
  if (x>=eps) return(x) else
  {
    pen=pen+.01*(x-eps)^2
    return (list(eps/(2-x/eps),pen))
  }
}