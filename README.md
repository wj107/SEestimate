# STANDARD ERROR ESTIMATE for predictive models

### v0.3 -- 9 April 2018

#### Author: Will Johnson
#### Email: will.johnson@gmail.com

SE.estimate is an R function that takes as arugments a data frame, a response variable, and specifications about what type of predictive model
to build (GLM or NN) and what type of cross-validation to perform (train/test splits, _k_-fold CV).  The function then creates _k_ predictive models,
calculates standard errors based on these models, and then outputs the results as a list containing:

* A vector with _k_ standard errors for each of the predictive models,
* A 5-number summary of the collection of standard errors, and
* If specified, a vector of _k_ build times for the predictive models.

Once the response variable for your data frame is specified, SE.estimate uses all other variables are predictive variables.

If it is specified to build a GLM predictive model, SE.estimate also requires to know the distribution of the response variable -- is it categorical
or continuous? -- so that the correct GLM link function will be chosen.

It has come to my attention that SE.estimate performs many of the same things the caret package for R does.  On my dusty laptop, though, I don't
have enough memory to install caret... so I wrote up this function instead!  It works for [my analysis](https://github.com/wj107/495project) and
has served me well as a good coding exercise, if nothing else.  If anyone else can find some use from it, thumbs up!!

#### v0.2 has:
* input data, name/distrib of response, # of resamples, train/test split, method of fitting (glm,nn, both), progress bar (y/n)
* stop messages for invalid arguments
* default data if no data frame is supplied
* creates regression formula, coerces all data to numeric, normalized data (for nn models)
* resamples data, creates model, tests model, computes&records standard error approximation, for both GLM/NN
* summarizes&outputs standard error data in list
* option to time how long it takes to create a model!!

#### v0.2+ needs:
* standardize variable names
* break apart code into sub-functions?
* require certain packages.
* fix text on progress bars
* visualization options!!

Wouldn't it be cool to re-write this function with a formal R help page?  Legitimize this operation!
