

fit<-function(X,y,transfo=FALSE,nb_classe=6){
 print("Entree dans la fonction ")
 n=ncol(X)
 it=seq(1, n, by = 1)
 print(it)
 taille=length(X)
 for (i in it){
   print("entree dans le for ")
   #print(X[,])
   if (is.factor(X[,i])) {
     #La variable est catégorielle

     
   } else {
     #La variable n'est pas catégorielle
       if(transfo==TRUE){
         #La variable est catégorielle
         # je discretise
         print("dans le if")
         mini=min(X[,i])
         maxi=max(X[,i])
        
         inter=(maxi-mini)/nb_classe
         points_de_coupure <- seq(from = mini, to = maxi, by = inter)
         print(points_de_coupure)
         print("types de x")
         
         print(str(X[,i]))
         disc <- cut(X[,i], breaks = points_de_coupure, labels = FALSE, include.lowest=TRUE)
         print(disc)
         
         
         #Calculs des prba
         liste_classe_x=unique(disc)
         liste_classe_y=unique(y)
         
         
          table_valeurs <- table(disc,y)

         
         table_proba=table_valeurs/taille
         print(table_proba)
         
         liste_t_proba=list()
         liste_t_proba=append(liste_t_proba,table_proba)
       }
   }
 }
 return(liste_t_proba)
}

data("iris")
colnames(iris)[colnames(iris) == "Species"] <- "Class"


X= iris[, -ncol(iris)]
y=iris$Class

print(X)
#fit(X,y,transfo=TRUE)

print(table(y))


