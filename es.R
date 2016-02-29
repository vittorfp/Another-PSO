
#_FUNÇÂO_ Inicializa a populaçao
criaPopulacao <- function(mi,lambda,dimensoes,lower_bounds,upper_bounds){
  
  populacao <- matrix(ncol = dimensoes + 2, nrow = mi+lambda)
  for(i in 1:dimensoes){
    populacao[,i] <- runif(mi+lambda,lower_bounds[i],upper_bounds[i])
  }
  d_x <- 1
  for (i in 1:dimensoes){
    d_x <- d_x * abs(upper_bounds[i]-lower_bounds[i])
  }
  d_x <- d_x ^ (1 / dimensoes)
  populacao[,dimensoes+2] <- d_x / sqrt(dimensoes)
  populacao
  
}


#_FUNÇÂO_ Calcula o fitnes do vetor de soluções
calculaFitness <- function(populacao,dimensoes){
  
  #Função de rosenbrock
  #populacao[,dimensoes+1] <-  (1 - populacao[,1])^2 + 100 * ( (populacao[,2] - populacao[,1]^2) ^2 )
  
  #Esfera
  f <- matrix(ncol = 1,nrow = )
  for( i in 1:dimensoes){
    f <- f + populacao[,i]^2
  }
  populacao[,dimensoes+1] <-  populacao[,1]^2  + populacao[,2]^2
  
  populacao
}



#_FUNÇÂO_ Operador de mutação
mutacao <- function(populacao,dimensoes,lambda){
  for(i in 1:dimensoes){
    populacao[,i] <- populacao[,i] + rnorm(lambda,mean = 0, sd = populacao[,dimensoes+2])
  }
  populacao
}



#_FUNÇÂO_ Operador de mutação
mutacaoDesvio <- function(populacao,delta_desvio,lambda){
  for(i in 1:lambda){
    populacao[i,dimensoes+2] <- populacao[i,dimensoes+2] * exp(rnorm( 1 ,sd = delta_desvio, mean = 0))
  }
  populacao
}



#_FUNÇÂO_ Ordena de acordo com o fitness
#utiliza insertion sort
ordenaFitness <- function(populacao,mi,lambda,dimensoes){
  
  for(i in 1:mi+lambda) {
    key <- populacao[i,]
    j <- i
    while( j > 1 && populacao[j - 1,dimensoes+1] > key[dimensoes+1]) {
      populacao[j,] <- populacao[j - 1,]
      j <- j - 1
    }
    populacao[j,] <- key
  }
  populacao
}




#_FUNÇÂO_ Operador de recombinacao (combinaçao intermedia)
recombinacao <- function(pais,ro,dimensoes) {
  
  filho <- c( rep(0, dimensoes + 2) )
  for (i in 1:dimensoes+2) {
    filho[i] <- sum(pais[,i]) / ro
  }
  filho
  
}



#_FUNÇÂO_ Verifica o criterio de paragem
Paragem <- function(populacao,mi){
  #med <- sum(populacao[1:mi,dimensoes+1])
  #med <- med / mi
  if( abs( populacao[ mi , dimensoes+1 ] - populacao[ 1 , dimensoes + 1 ] )  > 0.0001 ){
    TRUE
  }else{
    FALSE
  }
}




ES <- function(mi,lambda,ro,dimensoes,lower_bounds,upper_bounds,virgula = FALSE){
  
  #verifica a validade dos paarmetros
  erro <- FALSE
  if( ( length(lower_bounds) != dimensoes ) || (length(upper_bounds) != dimensoes ) ){
    print("Os vetores de lower/upper bounds tem tamanho diferente do numero de dimensões\n")
    erro <- TRUE
  }
  
  if( (mi < 1) || (lambda < 1) || (ro < 1) || (dimensoes < 1) ){
    print("Os de mi/lambda/ro/dimensoes nao podem ser menores que 1\n")
    erro <- TRUE
  }
  
  
  if(virgula){
    if(lambda < mi){
      print("O valor de Lambda nao pode ser inferior ao de Mi quando a opção Virgula é setada em TRUE")
      erro <- TRUE
    }
  }
  
  if(erro) return()
  
  ger <- 0
  delta_desvio <- 1/sqrt(dimensoes)
  
  #Inicializa a populaçao de mi individuos
  pop <- criaPopulacao(mi = mi,lambda = lambda,dimensoes = dimensoes,lower_bounds,upper_bounds)
  
  #A matriz da populaçao tem o numero de variaveis de decisao +2 colunas, sendo que
  #os dois ultimos representam respectivamente o fitness e o desvio padrao que sera eventualmente usado na mutacao
  
  #Calcula o fitness
  pop <- calculaFitness(pop,dimensoes)
  pop <- ordenaFitness(pop,mi,lambda,dimensoes)
  
  #Itera sobre a populaçao ate que o criterio de paragem seja satisfeito
  while ( Paragem(pop,mi) )  {
    ger <- ger + 1
    
    filhos <- matrix(ncol = dimensoes+2, nrow = lambda)
    for (i in 1:lambda){
      #escolhe ro individuos para gerar a proxima geraçao
      pais <- sample( c( 1:mi ), ro)
      pais <- pop[pais,]
      
      #Aplica a recombinaçao
      filhos[i,] <- recombinacao(pais,ro,dimensoes)
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
    pop <- ordenaFitness(pop,mi,lambda,dimensoes);
    
    
  }
  
  #Exibe a solução
  print("Best fit:")
  print(ger)
  print(pop)
}










#Inicializa os parametros do algoritmo
mi <- 10
lambda <- 5
ro <- 5
dimensoes <- 2

#inicializa os "lower and upper bounds" para cada variavel
#define o espaço de busca
lower_bounds <- c(-5,-5)
upper_bounds <- c(5,5)


ES(mi,lambda,ro,dimensoes,lower_bounds,upper_bounds)
