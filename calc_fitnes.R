#_FUN��O_ Calcula o fitnes do vetor de soluções
calculaFitness <- function(populacao,dimensoes){
  
  #Função de rosenbrock
  #populacao[,dimensoes+1] <-  (1 - populacao[,1])^2 + 100 * ( (populacao[,2] - populacao[,1]^2) ^2 )
  
  #paraboloide
  populacao[,dimensoes+1] <-  populacao[,1]^2  + populacao[,2]^2
  
  return(populacao)
}
