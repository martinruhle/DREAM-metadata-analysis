---
  title: "Preterm Prediction Model using Tidymodels"
author: "Martin Ruhle"
date: "2024-08-03"
output: html_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# Preterm Prediction Model 
# by tidymodels

# Packages and Data Loading
library(tidymodels)
metadata <- read.csv("C:/Users/User/Documents/DREAM/Git_hub_repository/data/metadata.csv")

# Preprocessing: Use only the necessary columns for the model, filtering out other data. 
# Encode was_preterm and was_early_preterm as factors for classification.
metadata <- metadata %>%
  select(NIH.Racial.Category, was_preterm, was_early_preterm) %>%
  mutate(across(c(was_preterm, was_early_preterm), as.factor))

# Split the Data: Split the data into training and testing sets.
# ´group_initial_split´ could be used considering participants as groups 
set.seed(123)
data_split <- initial_split(metadata, prop = 0.8, strata = was_preterm)
train_data <- training(data_split)
test_data <- testing(data_split)

# Create a Recipe: Set up a recipe for preprocessing, 
# such as converting categorical variables to dummy variables.
preterm_recipe <- recipe(was_preterm ~ NIH.Racial.Category, data = train_data) %>%
  step_dummy(all_nominal_predictors())

early_preterm_recipe <- recipe(was_early_preterm ~ NIH.Racial.Category, data = train_data) %>%
  step_dummy(all_nominal_predictors())

# Specify the Model: Choose a classification model, 
# like logistic regression or decision tree, and specify the model parameters.
model_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# Create a Workflow: Combine the recipe and model specification.
workflow_preterm <- workflow() %>%
  add_recipe(preterm_recipe) %>%
  add_model(model_spec)

workflow_early_preterm <- workflow() %>%
  add_recipe(early_preterm_recipe) %>%
  add_model(model_spec)

# Fit the Model: Fit the model on the training data.
preterm_fit <- fit(workflow_preterm, data = train_data)

early_preterm_fit <- fit(workflow_early_preterm, data = train_data)

# Evaluate the Model: Use the testing set to evaluate model performance.
predictions <- predict(preterm_fit, test_data) %>%
  bind_cols(test_data)
metrics <- metrics(predictions, truth = was_preterm, estimate = .pred_class)

predictions_early <- predict(early_preterm_fit, test_data) %>%
  bind_cols(test_data)
metrics_early <- metrics(predictions_early, truth = was_preterm, estimate = .pred_class)

# Calculate the confusion matrix
conf_matrix <- predictions %>%
  conf_mat(truth = was_preterm, estimate = .pred_class)

# Print the confusion matrix
print(conf_matrix)

# Define the metric set with yardstick functions only
metrics <- metric_set(accuracy, kap, sensitivity, specificity,
                      ppv, npv, detection_prevalence, mcc, bal_accuracy)

# Calculate metrics using the correct functions
model_metrics <- predictions %>%
  metrics(truth = was_preterm, estimate = .pred_class)

# Print the model metrics
print(model_metrics)

# Calculate the No Information Rate (NIR)
nir <- max(prop.table(table(test_data$was_preterm)))

# Calculate accuracy of the model
accuracy_metric <- model_metrics %>%
  filter(.metric == "accuracy") %>%
  pull(.estimate)

# Perform a binomial test to get the P-value for Acc > NIR
p_value_acc_gt_nir <- binom.test(
  x = round(accuracy_metric * nrow(test_data)), # number of correct predictions
  n = nrow(test_data),                          # total number of predictions
  p = nir                                       # probability under the null (NIR)
)$p.value

# Extract values from the confusion matrix
false_neg <- conf_matrix$table[2, 1]  # True class was FALSE, predicted TRUE
false_pos <- conf_matrix$table[1, 2]  # True class was TRUE, predicted FALSE

# Perform McNemar's test
mcnemar_test <- mcnemar.test(matrix(c(conf_matrix$table[1, 1], false_pos, 
                                      false_neg, conf_matrix$table[2, 2]), 
                                    nrow = 2))

# Display results
nir
p_value_acc_gt_nir
mcnemar_test$p.value

# Accuracy: This is the overall proportion of correctly predicted samples (both true positives and true negatives) out of the total number of samples. Here, an accuracy of 0.659 means that about 65.9% of predictions matched the true labels.
# 
# Kappa (kap): Cohen’s Kappa measures the agreement between the predictions and actual values, adjusted for the likelihood of agreement by chance. Values range from -1 to 1, where 1 is perfect agreement, 0 is chance-level, and negative values indicate worse-than-chance predictions. Here, a kappa of 0.116 suggests low agreement between predictions and actual values beyond what would be expected by chance.
# 
# Sensitivity: Also called the true positive rate or recall, sensitivity measures the proportion of actual positive cases that were correctly identified. In this case, 0.926 sensitivity indicates that 92.6% of true preterm cases were correctly identified by the model.
# 
# Specificity: This measures the proportion of actual negative cases (non-preterm) that were correctly identified. A specificity of 0.172 here means that only 17.2% of non-preterm cases were correctly classified, indicating the model is not very effective at predicting the "FALSE" class (non-preterm).
# 
# Positive Predictive Value (PPV): Also known as precision, PPV is the proportion of positive predictions that are actually correct. A PPV of 0.671 means that 67.1% of cases predicted as preterm were indeed preterm.
# 
# Negative Predictive Value (NPV): This is the proportion of negative predictions (non-preterm) that are actually correct. An NPV of 0.561 means that 56.1% of cases predicted as non-preterm were actually non-preterm.
# 
# Detection Prevalence: This metric shows the proportion of samples predicted as positive (preterm) by the model, regardless of whether they are true positives. Here, a detection prevalence of 0.892 indicates that 89.2% of samples were predicted as preterm.
# 
# Matthews Correlation Coefficient (MCC): MCC is a balanced measure that considers true positives, true negatives, false positives, and false negatives, with values ranging from -1 to 1. A value closer to 1 indicates better predictive performance across both classes. The MCC of 0.151 here suggests a weak correlation between predictions and actual outcomes.
# 
# Balanced Accuracy: This is the average of sensitivity and specificity, providing an adjusted measure of accuracy that accounts for imbalanced class distributions. A balanced accuracy of 0.549 indicates that the model performs slightly better than random chance (0.5) across both classes.