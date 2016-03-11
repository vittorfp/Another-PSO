source("init.R")
source("calc_fitnes.R")
source("mutacao.R")
source("mutacao_desvio.R")
source("ordena.R")
source("recombinacao.R")
source("paragem.R")



ES <- function(mi,lambda,ro,dimensoes,lower_bounds,upper_bounds,virgula = FALSE){
  
  #verifica a validade dos paarmetros
  erro <- FALSE
  if( ( length(lower_bounds) != dimensoes ) || (length(upper_bounds) != dimensoes ) ){
    print("Os vetores de lower/upper bounds tem tamanho diferente do numero de dimensÃµes\n")
    erro <- TRUE
  }
  
  if( (mi < 1) || (lambda < 1) || (ro < 1) || (dimensoes < 1) ){
    print("Os de mi/lambda/ro/dimensoes nao podem ser menores que 1\n")
    erro <- TRUE
  }
  
  
  if(virgula){
    if(lambda < mi){
      print("O valor de Lambda nao pode ser inferior ao de Mi quando a opÃ§Ã£o Virgula Ã© setada em TRUE")
      erro <- TRUE
    }
  }
  
  if(erro) return()
  
  ger <- 0
  delta_desvio <- 1/sqrt(dimensoes)
  
  #Inicializa a populaÃ§ao de mi individuos
  pop <- criaPopulacao(mi = mi,
                       lambda = lambda,
                       lower_bounds,upper_bounds)
  
  #A matriz da populaÃ§ao tem o numero de variaveis de decisao +2 colunas, sendo que
  #os dois ultimos representam respectivamente o fitness e o desvio padrao que sera eventualmente usado na mutacao
  
  #Calcula o fitness
  pop <- calculaFitness(pop,dimensoes)
  pop <- ordenaFitness(pop,dimensoes)
  
  #Itera sobre a populaÃ§ao ate que o criterio de paragem seja satisfeito
  while ( Paragem(pop,mi) )  {
    ger <- ger + 1
    
    filhos <- matrix(ncol = dimensoes+2,
                     nrow = lambda)
    #escolhe ro individuos para gerar a proxima geraÃ§ao
    pais <- matrix( sample( c( 1:mi ), lambda*ro , replace = TRUE),
                    ncol = lambda, 
                    nrow = ro)
    
    for (i in 1:lambda){
      p <- pop[ pais[,i] ,]
      
      #Aplica a recombinaçao
      filhos[i,] <- recombinacao(p,ro,dimensoes)
    }
    
    #Aplica a mutação aos desvios padrao
    filhos <- mutacaoDesvio(filhos,delta_desvio,lambda)
    
    #Aplica a mutação
    filhos <- mutacao(filhos, dimensoes,lambda)
    
    #Recalcula o fitness
    filhos <- calculaFitness(filhos,dimensoes)
    
    if(virgula){
      pop <- filhos
    }else{
      #Junta populacao de de filhos com a de progenitores
      pop[ mi+1:lambda , ] <- filhos
    }
    
    #seleciona
    pop <- ordenaFitness(pop,dimensoes);
  }
  
  print(ger)
  print(pop) 
  return(pop)
}