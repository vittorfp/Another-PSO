#_FUNÇÃO Operador de mutaçao
mutacaoDesvio <- function(populacao,delta_desvio,lambda){
  rand <- matrix( exp ( rnorm( lambda ,sd = delta_desvio, mean = 0) ), ncol = 1)  
  populacao[,dimensoes+2] <- populacao[,dimensoes+2] * rand
  return(populacao)
}

