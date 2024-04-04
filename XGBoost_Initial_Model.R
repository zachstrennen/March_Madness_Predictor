library(xgboost)
library(hoopR)
library(tictoc)
library(Matrix)
library(caret)
library(data.table)
library(modelr)

# Get NCAA data
tictoc::tic()
progressr::with_progress({
    mbb_pbp <-  hoopR::load_mbb_pbp()
})
tictoc::toc()

# Filter to current season
mbb_pbp <- filter(mbb_pbp, season == 2024)

# Function to get "home" team data cleaned
filter_home <- function(df,team_name) {
    # Filter to chosen team
    home_df <- filter(df, home_team_name == team_name)
    # Filter to only plays made by chosen team
    home_df <- filter(home_df, team_id == home_team_id)
    # Select wanted variables for model building
    final_home_df <- select(home_df, type_text, period_number, clock_minutes, shooting_play)
    # 1 is likelihood for "home" team
    final_home_df$shooting_play <- ifelse(final_home_df$shooting_play == TRUE, 1, 0)
    # Return home team data
    return(final_home_df)
}

# Function to get "away" team data cleaned
filter_away <- function(df, team_name) {
    # Filter to chosen team
    away_df <- filter(df, away_team_name == team_name)
    # Filter to only plays made by chosen team
    away_df <- filter(away_df, team_id == away_team_id)
    # Select wanted variables for model building
    final_away_df <- select(away_df, type_text, period_number, clock_minutes, shooting_play)
    # 1 is likelihood for "away" team
    final_away_df$shooting_play <- ifelse(final_away_df$shooting_play == TRUE, 0, 1)
    # Return away team data
    return(final_away_df)
}

# Combine the data for a matchup
set_up_matchup<- function(home_df, away_df) {
    
    # combine data for both teams
    matchup <- rbind(home_df, away_df)
    
    # Make player id a factor
    matchup$period_number <- as.factor(matchup$period_number)
    # Return data
    return(matchup)
}

# Function to build xgboost classifier
build_model <- function(df, seed_home, seed_away, max_depth, eta, gamma, subsample, colsample_bytree) {
    # Randomly shuffle the data
    df <- df[sample(1:nrow(df)), ]
    # Omit NAs
    df <- na.omit(df)
    # Convert factor variables into dummy variables
    
    # Duplicate the desired features
    df$period_number_duplicated <- df$period_number
    df$clock_minutes_duplicated <- df$clock_minutes
    
    df_transformed <- df %>%
        mutate_if(is.factor, as.character) %>%
        mutate(across(where(is.character), ~factor(.))) %>%
        model.matrix(~.-1, data = .)
    
    # Separate features and target variable
    target <- df$shooting_play
    data <- subset(df_transformed, select = -shooting_play)
    
    # Splitting the data into training and testing sets
    trainIndex <- createDataPartition(target, p = .6, list = FALSE, times = 1)
    data_train <- data[trainIndex, ]
    target_train <- target[trainIndex]
    data_test <- data[-trainIndex, ]
    target_test <- target[-trainIndex]
    
    # Convert training data to DMatrix object, which is optimized for XGBoost
    dtrain <- xgb.DMatrix(data = as.matrix(data_train), label = target_train)
    # Optionally, convert testing data to DMatrix for evaluation
    dtest <- xgb.DMatrix(data = as.matrix(data_test), label = target_test)
    
    # Parameters for the XGBoost model
    params <- list(
        objective = "binary:logistic",
        eval_metric = "logloss", # can use "auc" for area under curve
        max_depth = max_depth, #3
        eta = eta, # 0.3
        gamma = gamma, # 0
        subsample = subsample, # 0.7
        colsample_bytree = colsample_bytree # 0.5
    )
    
    # Training the model
    bst_model <- xgb.train(
        params = params,
        data = dtrain,
        nrounds = 200, # number of boosting rounds
        nthread = 2, # number of threads
        verbose = 0 # silent mode
    )
    
    # Evaluate the model on the testing set
    pred <- predict(bst_model, dtest)
    
    # Weight the results by seed
    home_likely <- mean(pred)
    home_likely <- home_likely + (17 - seed_home) * 0.002
    home_likely <- home_likely - (17 - seed_away) * 0.002
    
    # Return the likelihood of "home" team winning
    return(home_likely)
}

# Select teams and ensure data is there data
home_df <- filter_home(mbb_pbp, "BYU")
away_df <- filter_away(mbb_pbp, "Duquesne")
home_df
away_df

# Combine team data
matchup <- set_up_matchup(home_df, away_df)
mean_list <- c()
likelihood_list <- c()
# Fold model over and over again
for (x in 1:5) {
    # Build and store model results
    likelihood <- build_model(matchup, 11, 11,  max_depth =3, eta=3, gamma=0, subsample=0.7, colsample_bytree=0.8)
    likelihood_list <- c(likelihood_list, likelihood)
}
# Likelihood home team will win
mean(likelihood_list)
