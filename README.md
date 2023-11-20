# naive_bayes_r

This R package was developed by Cyrielle, Victor and Adrien. It can be used to create a Naive Bayes model of the categorical type. This package was developed under R for use on R. It has been developed as an R6 class.

## Library import 




## Library functions

### modNB::naive_bayes_r$new()

To train the model, first call the class constructor using the function : modNB::naive_bayes_r$new()

Explique ce que ça fait


### modNB::modNB::naive_bayes_r$fit(X, y, preproc = TRUE, nb_classe = 6, epsilon = NULL, g_na = TRUE)

This function is used to pre-process the data and train the naive bayes categorical model.
the function parameters are as follows :
- X : The dataframe of variables used for prediction
- y : The variable to be predicted
- preproc : A boolean which, if set to TRUE, launches the Preprocecing function, which discretizes numeric variables
- nb_classe : Number of classes after discretization (default 6)
- epsilon : 
- g_na : If TRUE, launches a preprocecing function which replaces NA with another value

### modNB::modNB::naive_bayes_r$predict(new_data)




### modNB::modNB::naive_bayes_r$predict_proba(new_data)



### modNB::modNB::naive_bayes_r$print()



### modNB::modNB::naive_bayes_r$summary()
