# naive_bayes_r

This R package was developed by Cyrielle, Victor and Adrien. It can be used to create a Naive Bayes model of the categorical type. This package was developed under R for use on R. It has been developed as an R6 class.

## Library import 




## Library functions

### naive_bayes_r$new()

To train the model, first call the class constructor using the function : modNB::naive_bayes_r$new()

Explique ce que ça fait


### naive_bayes_r$fit(X, y, preproc = TRUE, nb_classe = 6, epsilon = NULL, g_na = TRUE)

This function is used to pre-process the data and train the naive bayes categorical model.
the function parameters are as follows :
- X : The dataframe of variables used for prediction
- y : The variable to be predicted
- preproc : A boolean which, if set to TRUE, launches the Preprocecing function, which discretizes numeric variables
- nb_classe : Number of classes after discretization (default 6)
- epsilon : 
- g_na : If TRUE, launches a preprocecing function which replaces NA with another value

### naive_bayes_r$predict(new_data)

This function launches predictions from the model trained in the fit function on the new_data dataframe, which has the same number of variables as X. The function returns a vector containing the predictions made.

### naive_bayes_r$predict_proba(new_data)

This function launches predictions from the model trained in the fit function on the new_data dataframe, which has the same number of variables as X. The function returns a vector of prediction probabilities.

### naive_bayes_r$print()

This function takes no parameters. It displays some minimal information about the model (the number of variables, the number of output classes and the size of the training sample).

### naive_bayes_r$summary()

This function takes no parameters. This function displays model details.
This function will display :
- A summary of variables (number of predictor variables, number of classes to predict)
- A summary of precessing
- A summary of variables (number of observations, min and max of each variable, number of classes of each variable)
- Prior probabilities


### naive_bayes$compute_and_plot_importance()

This function displays a graph showing the importance of variables 