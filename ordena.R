#_FUNÃÇÃO_ Ordena de acordo com o fitness
ordenaFitness <- function(populacao,dimensoes){
  ord <- order(populacao[,dimensoes+1])
  populacao <- populacao[ord,]
  return(populacao)
}


