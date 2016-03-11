# #_FUNC√O_ Operador de recombinacao (combina√ßao intermedia)
# recombinacao <- function(populacao,pais,dimensoes,lambda) {
#   
#   
#   filhos <- matrix(nrow = lambda,ncol = dimensoes+2)
#   
#   for (j in 1:lambda){
#     p <- populacao[ pais[ , j] , ]
#     for (i in 1:dimensoes+2) {
#       filhos[j,i] <- mean(p[,i])
#     }
#   }
#   return(filhos)
# }

#_FUN«¬O_ Operador de recombinacao (combinaÁao intermedia)
recombinacao <- function(pais,ro,dimensoes) {
  
  filho <- c( rep(0, dimensoes + 2) )
  for (i in 1:dimensoes+2) {
    filho[i] <- mean(pais[,i])
  }
  return(filho)
  
}