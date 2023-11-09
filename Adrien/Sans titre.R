data("iris")

colnames(iris)[colnames(iris) == "Species"] <- "Class"

str(iris)



fit<-function(X,y){
  
 n=ncol(X)
 it=seq(0, it, by = 1)
 
 for (i in it){
   
   if (is.factor(variable_categorique)) {
     print("La variable est catégorielle.")
   } else {
     print("La variable n'est pas catégorielle.")
   }
 }
}