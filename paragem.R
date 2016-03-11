#_FUNÇÃO_ Verifica o criterio de paragem
Paragem <- function(populacao,mi){
  if( abs( populacao[ mi , dimensoes+1 ] - populacao[ 1 , dimensoes + 1 ] )  > 0.0001 ){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


