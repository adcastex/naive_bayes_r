library(parallel)


dis<-function(X,nb_classe=6){
  
  mini=min(X)
  print(mini)
  maxi=max(X)
  print (maxi)
  inter=(maxi-mini)/nb_classe
  points_de_coupure <- seq(from = mini, to = maxi, by = inter)

  disc <- cut(X, breaks = points_de_coupure, labels = FALSE, include.lowest=TRUE)
  return(disc)
}



gen_disc<-function(X,nb_classe=6){
  X <- data.frame(apply(X, MARGIN = 2, FUN = function(i) dis(i,nb_classe)))
return(X)
}


data("iris")
X= iris[, -ncol(iris)]

print(gen_disc(X))

irlande=gen_disc(X)
