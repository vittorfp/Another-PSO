#_FUNÁ√O_ Inicializa a popula√ßao
criaPopulacao <- function(mi,lambda,lower_bounds,upper_bounds){
  dimensoes <- length(lower_bounds)
  LB        <- matrix(rep(lower_bounds, 
                          times = mi + lambda), 
                      ncol  = dimensoes,
                      byrow = TRUE)
  
  UB        <- matrix(rep(upper_bounds, 
                          times = mi + lambda), 
                      ncol  = dimensoes,
                      byrow = TRUE)
  
  populacao  <- LB + (UB - LB) * matrix(runif((mi + lambda) * dimensoes), 
                                        ncol = dimensoes)
  
  populacao  <- cbind(populacao, 0)
  dx         <- cumprod(upper_bounds - lower_bounds)
  populacao  <- cbind(populacao, 
                      (dx[length(dx)] ^ (1/dimensoes)) / sqrt(dimensoes))
  return(populacao)
}