pacman::p_load(tidyverse, skimr, corrplot, rpart, rcompanion, corrr, lubridate,detectseparation, cowplot,
               caTools, ROSE,rpart, Rborist,xgboost, smotefamily,themis, recipes, modeldata,caret, mlbench, FSelector)

data <- read_csv("posts/Me/Fraud Detection/claimdata.csv")

data %>%
  skim()
character_cols <- c("Deductible","DriverRating", "RepNumber")

data[character_cols] <- lapply(data[character_cols], as.character)

data$FraudFound <- as.factor(data$FraudFound)

colnames(data)[16] <- "class"
levels(data$class) <- c("No", "Yes")

data_numeric <- data %>%
  dplyr::select(where(is.numeric))

cluster_vars.cor = cor(data_numeric)

corrplot.mixed(cluster_vars.cor,
               lower = "ellipse", 
               upper = "number",
               tl.pos = "lt",
               diag = "l",
               tl.col = "black",
               tl.cex = 0.5
)

data_categorical <- data %>%
  dplyr::select(where(is.character))

# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}

association <- mixed_assoc (data_categorical) %>%
  filter (assoc > 0.8 & x != y)

association

data <- data %>%
  dplyr::select(-PolicyType, -PolicyNumber)



endo_sep <- glm(class ~ ., data = data,
                family = binomial("logit"),
                method = "detect_separation")

endo_ml <- update(endo_sep, method = "glm.fit")

seperation <- as.data.frame(coef(endo_sep) + coef(summary(endo_ml)))



data$DayOfWeek <- factor(data$DayOfWeek,levels=c("Sunday","Monday","Tuesday","Wednesday",
                                                  "Thursday","Friday","Saturday"),ordered=TRUE)

data$DayOfWeekClaimed <- factor(data$DayOfWeekClaimed,levels=c("Sunday","Monday","Tuesday","Wednesday",
                                                                "Thursday","Friday","Saturday"),ordered=TRUE)

data$Month <- factor(data$Month, levels = c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec"),ordered=TRUE)

data$MonthClaimed <- factor(data$MonthClaimed, levels = c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec"),ordered=TRUE)


# data <- data %>%
#   mutate(InsuredYear = case_when(
#     MonthClaimed < Month ~ Year+1,
#     TRUE ~ Year
#   )) %>%
#   mutate(
#     DayOfWeekNum = as.integer(factor(data$DayOfWeek,levels=c("Sunday","Monday","Tuesday","Wednesday",
#                                                                   "Thursday","Friday","Saturday"),ordered=TRUE)), 
#     date01 = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")
#   ) %>%
#   mutate(weekday01 = as.integer(format(date01, format = "%w")) + 1L) %>%
#   mutate(accidentdate = date01 + (WeekOfMonth - 1L) * 7L + (DayOfWeekNum - weekday01)) %>%
#   dplyr::select(-DayOfWeekNum,-date01,-weekday01) %>%
#   mutate(
#     DayOfWeekNum = as.integer(factor(data$DayOfWeekClaimed,levels=c("Sunday","Monday","Tuesday","Wednesday",
#                                                                   "Thursday","Friday","Saturday"),ordered=TRUE)), 
#     date01 = as.Date(paste(InsuredYear, MonthClaimed, "01", sep = "-"), format = "%Y-%b-%d")
#   ) %>%
#   mutate(weekday01 = as.integer(format(date01, format = "%w")) + 1L) %>%
#   mutate(claimdate = date01 + (WeekOfMonthClaimed - 1L) * 7L + (DayOfWeekNum - weekday01)) %>%
#   dplyr::select(-DayOfWeekNum,-date01,-weekday01) %>%
#   mutate (daysdifference = as.numeric(difftime(claimdate,accidentdate, unit = "days")))
  



my_plots <- lapply(names(data), function(var_x){
  p <- 
    ggplot(data) +
    aes_string(var_x)
  
  if(is.numeric(data[[var_x]])) {
    p <- p + geom_density()
    
  } else {
    p <- p + geom_bar()
  } 
  
})


plot_grid(plotlist = my_plots)


table(data$Make)

conticar <- c("BMW", "Ferrari", "Jaguar", "Lexus", "Mecedes", "Porche", "Saab", "VW")
americacar <- c("Saturn", "Pontiac", "Mercury", "Ford", "Dodge", "Chevrolet")
japcar <- c("Accura", "Honda", "Nisson", "Toyota", "Mazda")

data <- data %>% 
  mutate (Make = case_when(
    Make %in% conticar ~ "Continental Car",
    Make %in% americacar ~ "American Car",
    Make %in% japcar ~ "Japan Car",
    TRUE ~ Make
  ))

table(data$VehiclePrice)

data <- data %>%
  mutate(VehiclePrice = recode(VehiclePrice,
                               "40000 to 59000" = "more than 40000",
                               "60000 to 69000" = "more than 40000",
                               "more than 69000" = "more than 40000"))

table(data$AgeOfPolicyHolder)

data <- data %>%
  mutate(AgeOfPolicyHolder = recode(AgeOfPolicyHolder,
                               "16 to 17" = "16 to 25",
                               "18 to 20" = "16 to 25",
                               "21 to 25" = "16 to 25"))

summary(data$Age)

table(data$AgeOfPolicyHolder)

table(data$AgeOfVehicle)

table(data$AddressChange_Claim)

data <- data %>%
  mutate(AddressChange_Claim = recode(AddressChange_Claim,
                               "1 year" = "change",
                               "2 to 3 years" = "change",
                               "4 to 8 years" = "change",
                               "under 6 months" = "change"))

data <- data %>%
  dplyr::select(-c(1:3), -c(6:8), -11, -30)

data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                       as.factor)


split <- sample.split(data$class, SplitRatio = 0.7)
train <-  subset(data, split == TRUE)
test <- subset(data, split == FALSE)

table(train$class)

## Feature Selection


weights <- chi.squared(class~., train)
print(weights)
subset <- cutoff.k(weights, 13)
f <- as.simple.formula(subset, "class")
print(f)

train <- train %>% 
  dplyr::select(all_of(subset), class)

test <- test %>%
  dplyr::select(all_of(subset), class)

training <- recipe(f, data = train) %>%
  step_impute_knn(all_predictors()) %>%
  step_smotenc(class, over_ratio = 0.8) %>%
  prep() %>%
  bake(new_data = NULL)

table(training$class)

table(training$class,training$BasePolicy)

table(train$class,train$BasePolicy)


ctrl <- trainControl(method = 'cv',
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)



dt_orig <- train(class ~ .,
                 data = training,
                 method = "rpart",
                 trControl = ctrl,
                 metric = "ROC")

#C. Naive Bayes regression: original data

nb_orig <- train(class ~ .,
                 data = training,
                 method = "naive_bayes",
                 trControl = ctrl,
                 metric = "ROC")


#A. Decision Tree Model predictions

dt_orig_pred <- predict(dt_orig,test,type = "prob")
plsClasses <- predict(dt_orig, newdata = test)

confusionMatrix(data = plsClasses, test$class)


#B. Decision Tree - Assign class to probabilities

dt_orig_test <- factor(ifelse(dt_orig_pred$Yes > 0.5,"Yes","No"))

#C. Decision Tree Save Precision/Recall/F

precision_dtOrig <- posPredValue(dt_orig_test,test$class,positive = "Yes")
recall_dtOrig    <- sensitivity(dt_orig_test,test$class,positive = "Yes")
F1_dtOrig         <- (2 * precision_dtOrig * recall_dtOrig) / (recall_dtOrig + precision_dtOrig)

confusionMatrix(dt_orig_test,test$class,positive = "Yes")

#################################################
#Naive Bayes Model - Trained on original dataset#
#################################################
#A. NB Model predictions

nb_orig_pred <- predict(nb_orig,test,type = "prob")

#B. NB - Assign class to probabilities

nb_orig_test <- factor(ifelse(nb_orig_pred$Yes > 0.5,"Yes","No"))

#C. NB Save Precision/Recall/F

precision_nbOrig <- posPredValue(nb_orig_test,test$class,positive = "Yes")
recall_nbOrig    <- sensitivity(nb_orig_test,test$class,positive = "Yes")
F1_nbOrig         <- (2 * precision_nbOrig * recall_nbOrig) / (recall_nbOrig + precision_nbOrig)




glm_fit <- glm(f, data = training, family = 'binomial')
pred_glm <- predict(glm_fit, newdata = test, type = 'response')
roc.curve(test$class, pred_glm, plotit = TRUE)


smote_fit <- rpart(class ~ ., data = training)
pred_smote <- predict(smote_fit, newdata = test)
roc.curve(test$class, pred_smote[,2], plotit = FALSE)

x = training[,-11]
y = training[,11]

rf_fit <- Rborist(x, y, ntree = 1000, minNode = 20, maxLeaf = 13)

rf_pred <- predict(rf_fit, test[,-11], ctgCensus = "prob")
prob <- rf_pred$prob

roc.curve(test$Class, prob[,2], plotit = TRUE)



X_train = xgb.DMatrix(as.matrix(training %>% select(-class)))
y_train = training$class


set.seed(42)

xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,
  search = "grid"
)

gbmGrid <-  expand.grid(max_depth = 10, 
                        nrounds = c(300,500),    # number of trees
                        # default values below
                        eta = 0.1,
                        gamma = 0.1,
                        subsample = 0.5,
                        min_child_weight = 2,
                        colsample_bytree = 0.5)

xgb_model = train(
  class ~ .,
  data = training,  
  trControl = xgb_trcontrol,
  tuneGrid = gbmGrid,
  method = "xgbTree"
)

predict_xgb_model <- predict(xgb_model, newdata = test)

confusionMatrix(data = predict_xgb_model, test$class)

xgb_model$bestTune
