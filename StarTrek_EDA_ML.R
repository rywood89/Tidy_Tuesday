# Star Trek data analysis and prep
# Use this analysis for the xaringan slides

# Author: Ryan Woodbury
# Date: August, 17, 2021


## Libraries ----
library(tidyverse)
library(tidytext)
library(tidymodels)
library(ggiraph)
library(textrecipes)



computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')


#' The main question is "what type of commands cause errors?"

## Explore data ----

glimpse(computer) # 2214 rows, 14 columns # Sure, we can try machine learning.
head(computer)

computer %>% 
  count(name) # 137 IDs (name of episode?)

computer %>% 
  count(char) # 76 characters (name of character)

head(computer$line) # line spoken

computer %>% 
  count(name, value_id, sort = T) # name of episode and line number in the episode. # There are 1300 unique rows of name and value_ids. 

computer %>% 
  filter(name == "116" & value_id == "107") %>% 
  view("116.107")
# Yeah, something weird is going on. Why are there so many duplicates? This has to be showing different categories of statements. The line and interactions are coded in multiple ways.
# Perhaps remove type and sub_domain, keep pri_type and domain, then clean.

computer %>% 
  filter(error == "TRUE") %>% 
  count(name, value_id, error) # This is weird. There is a command and a response, which suggests a dialog? Why is this happening?

computer %>% 
  count(direction) # Stage direction

computer %>% 
  count(nv_resp) # non-verbal response

computer %>% 
  count(is_fed) # all records are fed

computer %>% 
  count(error) # ope, there are only 11 times where there is an error.

### Let's check out the errors
computer %>% 
  filter(error == "TRUE") %>% 
  view("errors")
#### Picard has issues with errors!! He is the only one who has caused an error.

## Clean data ----

computer_clean <- computer %>% 
  select(-c(type, sub_domain, direction)) %>% 
  #count(name, value_id, sort = T)
  unique()


## 

glimpse(computer_clean)

# I do not care about name of the episode or the value_id, among other things. I want to focus on direction, domain, nv_resp, pri_type, domain, and of course, error.
# The question is what type of interaction is causing an error. I know it's always Picard, but you cannot generalize that.

computer_model <- computer_clean %>% 
  select(line, domain, nv_resp, pri_type, domain, error) %>% 
  mutate(across(domain:error, ~as.factor(.)))

glimpse(computer_model)


## Modeling (clean) ----

set.seed(345678)

computer_split <- initial_split(computer_model, prop = .75, strata = error)

computer_other <- training(computer_split)
computer_test <- testing(computer_split)

computer_other %>% 
  count(error) %>% 
  mutate(prop = n/sum(n))

computer_other %>% 
  select(-error) %>% 
  transmute(total_missing = rowSums(is.na(.), na.rm = T),
         total_predictors = length(names(.)),
         pct_missing = total_missing / total_predictors) %>% 
  arrange(desc(pct_missing))
## Looks like we will need to impute words?!


computer_test %>% 
  count(error) %>% 
  mutate(prop = n/sum(n))

# SEVERELY IMBALANCED DATA!!

val_set <- validation_split(computer_other,
                            strata = error,
                            prop = .75)

train_4fold <- computer_other %>% 
  vfold_cv(4, strata = error)

lr_mod <-
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

####
# Testing the stopwords function

computer_other %>% 
  tidytext::unnest_tokens(word, line) %>% 
  #anti_join(stop_words) %>% 
  count(word) %>% 
  slice_max(n, n = 30) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip()
  
####


lr_recipie <-
  recipe(error ~ ., data = computer_other) %>% 
  step_impute_mode(all_predictors()) %>%
  step_novel(line, domain, pri_type) %>%
  themis::step_upsample(error, over_ratio = .5) %>% #The imbalance is so bad, I don't know what the right ratio should be. It's literally 1:1000!
  step_dummy(domain, nv_resp, pri_type, domain) %>% 
  step_tokenize(line) %>% 
  #step_stem(line) %>% 
  textrecipes::step_stopwords(line) %>% # I had to install a "stopwords" package.
  step_tokenfilter(line, max_tokens = 10) %>% 
  #step_tfidf(line) %>% 
  step_tf(line)

lr_wf <-
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipie)


lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_result <- lr_wf %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = T),
            metrics = metric_set(roc_auc))




lr_plot <- 
  lr_result %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 

lr_result %>% 
  collect_metrics() %>% 
  arrange(mean)

lr_fit <- lr_wf %>% 
  finalize_workflow(select_best(lr_result)) %>% 
  fit(computer_other)

lr_fit %>% 
  augment(computer_test, type.predict = "prob") %>% 
  roc_auc(error, .pred_FALSE) # The ROC_AUC is OK. ~75%

## Oof, we are maxing out at ~62% on the training set and 25% on the test set. Let's try a different model, for example a tree-based model.
## Also, this is a weird model to use for all categorical variables.

xg_mod <- 
  boost_tree(trees = tune(),
             mtry = tune(),
             tree_depth = tune(),
             learn_rate = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

xg_wf <- workflow() %>% 
  add_recipe(lr_recipie) %>% 
  add_model(xg_mod)

control_xg <- control_grid(save_workflow = T,
                           save_pred = T,
                           extract = extract_model)


xg_tune <- xg_wf %>% 
  tune_grid(train_4fold,
            metrics = metric_set(roc_auc),
            control = control_xg,
            grid = crossing(trees = seq(50, 500, 25),
                            mtry = c(2,3,4),
                            tree_depth = c(5),
                            learn_rate = c(0.018, .02)))


autoplot(xg_tune)


xg_tune %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  select(mtry:learn_rate, mean, n)


xg_fit <- xg_wf %>% 
  finalize_workflow(select_best(xg_tune)) %>% 
  fit(computer_other)

xg_fit %>% 
  augment(computer_test, type.predict = "prob") %>% 
  roc_auc(error, .pred_FALSE) # The ROC_AUC is OK ~70%, but better than the logistic regression.

importances <- xgboost::xgb.importance(model = extract_fit_engine(xg_fit))

importances %>%
  mutate(Feature = fct_reorder(Feature, Gain)) %>%
  slice_head(n = 10) %>% 
  ggplot(aes(Gain, Feature)) +
  geom_col()


#' There are only 6 errors among the data and the data are all categorical.
#' Picard runs into four errors
#' Basically what we know now is that a non-verbal response from the computer is not a guarantee that it will perform the command in question.
#' Further, "Command Communications" seem to be more error-prone than other statement domains or primary types.
#' 
#' 


### Wake word EDA
computer %>% 
  filter(type == "Wake Word" | pri_type == "Wake Word") %>% view("wake_word")


### Error stats
computer_clean %>% 
  count(error)

computer_clean %>% 
  filter(error == "TRUE") %>% 
  view("clean_errors")


computer_clean %>% 
  filter(name == "141") %>% 
  view("name_141")

computer %>% 
  filter(name == "141") %>% 
  view("all_141")

computer %>% 
  filter(name == "193") %>% 
  view("all_193")

computer %>% 
  filter(name == "275") %>% 
  view("all_275")

#' one of the errors is due to damage (141.98)
#' one of the errors is due to a weak signal (141.258)
#' Episode 193 does not give us any reason why the computer hits an error
#' Episode 275 doesn't give any reason why the computer hits an error
#' I might have to watch Star Trek.


## Modeling (ALL) ----

set.seed(345678)

#computer_new <- computer %>% 
 # mutate(nv_resp = as.factor(nv_resp))

computer1 <- computer %>% 
  select(type, pri_type, domain, sub_domain, nv_resp, char_type, error) %>% 
  mutate(across(type:error, ~as.factor(.)))

all_split <- initial_split(computer1, prop = .75, strata = error)

all_other <- training(all_split)
all_test <- testing(all_split)

all_other %>% 
  count(error) %>% 
  mutate(prop = n/sum(n))

all_other %>% 
  select(-error) %>% 
  transmute(total_missing = rowSums(is.na(.), na.rm = T),
            total_predictors = length(names(.)),
            pct_missing = total_missing / total_predictors) %>% 
  arrange(desc(pct_missing))
## Looks like we will need to impute words?!


all_test %>% 
  count(error) %>% 
  mutate(prop = n/sum(n))

# SEVERELY IMBALANCED DATA!!

all_val_set <- validation_split(all_other,
                            strata = error,
                            prop = .75)

all_train_5fold <- all_other %>% 
  vfold_cv(5, strata = error)

all_lr_mod <-
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

####
# Testing the stopwords function

# all_other %>% 
#   tidytext::unnest_tokens(word, line) %>% 
#   anti_join(stop_words) %>% 
#   filter(word != "computer") %>% 
#   count(word) %>% 
#   slice_max(n, n = 30) %>% 
#   ggplot(aes(word, n)) +
#   geom_col() +
#   coord_flip()

####


full_recipie <- 
  recipe(error ~ ., data = all_other) %>%
  step_impute_mode(all_predictors()) %>%
  step_novel(domain, pri_type) %>% 
  themis::step_upsample(error, over_ratio = .75) %>% #The imbalance is so bad, I don't know what the right ratio should be.
  step_dummy(domain, nv_resp, pri_type, sub_domain) #%>% 


  # step_tokenize(line) %>% 
  # #step_stem(line) %>% 
  # step_stopwords(line) %>% # Why is this not working?
  # # I fixed this by installing the stopwords package.
  # step_tokenfilter(line, max_tokens = 15) %>% 
  # #step_tfidf(line) %>% 
  # step_tf(line) %>% 
  # step_rm(line)
  

all_lr_wf <-
  workflow() %>% 
  add_model(all_lr_mod) %>% 
  add_recipe(full_recipie)


#lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

all_lr_result <- all_lr_wf %>% 
  tune_grid(all_val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = T),
            metrics = metric_set(roc_auc))
## Not a helpful error message... "error"
# View(all_lr_result[[4]][[1]])

all_lr_plot <- 
  all_lr_result %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

all_lr_plot 

## Oof, we are maxing out at xx%. Let's try a different model, for example a tree-based model.

all_xg_mod <- 
  boost_tree(trees = tune(),
             mtry = tune(),
             tree_depth = tune(),
             learn_rate = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

all_xg_wf <- workflow() %>% 
  add_recipe(full_recipie) %>% 
  add_model(all_xg_mod)

all_control_xg <- control_grid(save_workflow = T,
                           save_pred = T,
                           extract = extract_model)


all_xg_tune <- all_xg_wf %>% 
  tune_grid(all_train_5fold,
            metrics = metric_set(roc_auc),
            control = all_control_xg,
            grid = crossing(trees = seq(50, 500, 25),
                            mtry = c(2,3,4),
                            tree_depth = c(5),
                            learn_rate = c(0.018, .02)))
# View(all_xg_tune[[4]][[1]])

autoplot(xg_tune)


xg_tune %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  select(mtry:learn_rate, mean, n)


xg_fit <- xg_wf %>% 
  finalize_workflow(select_best(xg_tune)) %>% 
  fit(all_other)

xg_fit %>% 
  augment(all_test, type.predict = "prob") %>% 
  roc_auc(error, .pred_TRUE) # The ROC_AUC sucks.

importances <- xgboost::xgb.importance(model = extract_fit_engine(xg_fit))

importances %>% 
  mutate(Feature = fct_reorder(Feature, Gain)) %>% 
  ggplot(aes(Gain, Feature)) +
  geom_col()