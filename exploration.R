library(tidymodels)
library(tidyverse)
library(highcharter)

# Following this amazing tutorial: https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/


hchart(cor(iris %>% select_if(., is.numeric)))

iris_split <- initial_split(iris, prop = 0.6)

iris_split %>%
  training() %>%
  glimpse()

iris_recipe <- training(iris_split) %>%
  recipe(Species ~.) %>%
  # step_corr(all_predictors())
  step_corr(all_predictors()) %>% 
  # normalises numeric dat ato have a mean of zero
  step_center(all_predictors(), -all_outcomes()) %>%
  # Normalizes numeric data to have a standard deviation of one
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

iris_recipe

# Training data is now prepared. To load the prepared training data into a variable, 
# we use juice(). It will extract the data from the iris_recipe object.

iris_training <- juice(iris_recipe)

iris_training %>% glimpse()

# The testing data can now be transformed using the exact same steps, weights, and 
# categorization used to pre-process the training data. To do this, another function with a 
# cooking term is used: bake(). Notice that the testing() function is used in order 
# to extract the appropriate data set.

iris_testing <- iris_recipe %>%
  bake(testing(iris_split)) 

iris_testing


# there are multiple packages that fit the same type of model. 
# It is common for each package to provide a unique interface. In other words, 
# things such as an argument for the same model attribute is defined differently for each package. 
# For example, the ranger and randomForest packages fit Random Forest models. In the ranger() function, 
# to define the number of trees we use num.trees. In randomForest, that argument is named ntree. 
# It is not easy to switch between packages to run the same model. 
# In the example below, the rand_forest() function is used to initialize a Random Forest model. 
# To define the number of trees, the trees argument is used. To use the ranger version of Random Forest, 
# the set_engine() function is used. Finally, to execute the model, the fit() function is used. 
# The expected arguments are the formula and data. Notice that the model runs on top of the juiced trained data.


iris_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>%
  fit(Species ~ ., data = iris_training)

# to use a model from a different package, one change is needed (engine):

iris_rf <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(Species ~ ., data = iris_training)

# It is also worth mentioning that the model is not defined in a single, 
# large function with a lot of arguments. The model definition is separated into smaller 
# functions such as fit() and set_engine(). This allows for a more flexible - and easier to learn - interface.


# Instead of a vector, the predict() function ran against a parsnip model 
# returns a tibble. By default, the prediction variable is called .pred_class. 
# In the example, notice that the baked testing data is used.

predict(iris_ranger, iris_testing)

# It is very easy to add the predictions to the baked testing data by using dplyr’s bind_cols() function.

iris_ranger %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing) %>%
  glimpse()



# Use the metrics() function to measure the performance of the model. 
# It will automatically choose metrics appropriate for a given type of model. 
# The function expects a tibble that contains the actual results (truth) and what the model predicted (estimate).



iris_ranger %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing) %>%
  metrics(truth = Species, estimate = .pred_class)

# changing just the dataframe results in another model evaluation

iris_rf %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing) %>%
  metrics(truth = Species, estimate = .pred_class)




# It is easy to obtain the probability for each possible predicted value by setting the type argument to prob. 
# That will return a tibble with as many variables as there are possible predicted values. 
# Their name will default to the original value name, prefixed with .pred_.


iris_ranger %>%
  predict(iris_testing, type = "prob") %>%
  glimpse()

## to add to the baked testing data

iris_probs <- iris_ranger %>%
  predict(iris_testing, type = "prob") %>%
  bind_cols(iris_testing)

glimpse(iris_probs)

# The curve methods include an autoplot() function that easily creates a ggplot2 visualization.
iris_probs%>%
  gain_curve(Species, .pred_setosa:.pred_virginica) %>%
  autoplot()


# This is an example of a roc_curve(). 
# Again, because of the consistency of the interface, only the function name needs to be modified; 
# even the argument values remain the same.

iris_probs%>%
  roc_curve(Species, .pred_setosa:.pred_virginica) %>%
  autoplot()


