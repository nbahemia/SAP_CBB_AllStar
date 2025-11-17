# All-Star College Basketball Player Project - PREDICTIVE MODEL
# Years covered: 2009 - 2021 (Training), 2022+ (Prediction)

library(dplyr)
library(readxl)
library(ggplot2)
library(tibble)
library(caret)
library(pROC)
library(glmnet)  # For regularized logistic regression

# ============================================================
# 1. Load datasets
# ============================================================
CBBPlayers <- read_excel("/Users/naitikrambhia/Downloads/CBBPlayers_with_Positions.xlsx")

# ============================================================
# 2. All-Star Lookup Table
# ============================================================
allstars_lookup <- tribble(
  ~player_name,              ~team,              ~start_year, ~end_year,
  "Andrew Wiggins",          "Kansas",            2013,        2014,
  "Anthony Davis",           "Kentucky",          2011,        2012,
  "Anthony Edwards",         "Georgia",           2019,        2020,
  "Edrice Adebayo",          "Kentucky",          2016,        2017,
  "Ben Simmons",             "LSU",               2015,        2016,
  "Bradley Beal",            "Florida",           2011,        2012,
  "Brandon Ingram",          "Duke",              2015,        2016,
  "Cade Cunningham",         "Oklahoma St.",      2020,        2021,
  "D'Angelo Russell",        "Ohio St.",          2014,        2015,
  "Damian Lillard",          "Weber St.",         2008,        2012,
  "Darius Garland",          "Vanderbilt",        2018,        2019,
  "Dejounte Murray",         "Washington",        2015,        2016,
  "DeMarcus Cousins",        "Kentucky",          2009,        2010,
  "Devin Booker",            "Kentucky",          2014,        2015,
  "Domantas Sabonis",        "Gonzaga",           2014,        2016,
  "Donovan Mitchell",        "Louisville",        2015,        2017,
  "Draymond Green",          "Michigan St.",      2008,        2012,
  "Evan Mobley",             "USC",               2020,        2021,
  "Fred VanVleet",           "Wichita St.",       2012,        2016,
  "Gordon Hayward",          "Butler",            2008,        2010,
  "Isaiah Thomas",           "Washington",        2008,        2011,
  "Ja Morant",               "Murray St.",        2017,        2019,
  "Jalen Brunson",           "Villanova",         2015,        2018,
  "Jalen Williams",          "Santa Clara",       2019,        2022,
  "Jaren Jackson Jr.",       "Michigan St.",      2017,        2018,
  "Jarrett Allen",           "Texas",             2016,        2017,
  "Jaylen Brown",            "California",        2015,        2016,
  "Jayson Tatum",            "Duke",              2016,        2017,
  "Jimmy Butler",            "Marquette",         2008,        2011,
  "John Wall",               "Kentucky",          2009,        2010,
  "Julius Randle",           "Kentucky",          2013,        2014,
  "Karl-Anthony Towns",      "Kentucky",          2014,        2015,
  "Kawhi Leonard",           "San Diego St.",     2009,        2011,
  "Kemba Walker",            "Connecticut",       2008,        2011,
  "Khris Middleton",         "Texas A&M",         2009,        2012,
  "Klay Thompson",           "Washington St.",    2008,        2011,
  "Kyrie Irving",            "Duke",              2010,        2011,
  "Lauri Markkanen",         "Arizona",           2016,        2017,
  "Nikola Vucevic",          "USC",               2008,        2011,
  "Pascal Siakam",           "New Mexico St.",    2014,        2016,
  "Paul George",             "Fresno St.",        2008,        2010,
  "Shai Gilgeous-Alexander", "Kentucky",          2017,        2018,
  "Trae Young",              "Oklahoma",          2017,        2018,
  "Tyler Herro",             "Kentucky",          2018,        2019,
  "Tyrese Haliburton",       "Iowa St.",          2018,        2020,
  "Tyrese Maxey",            "Kentucky",          2019,        2020,
  "Victor Oladipo",          "Indiana",           2010,        2013,
  "Zach LaVine",             "UCLA",              2013,        2014,
  "Zion Williamson",         "Duke",              2018,        2019
)

# ============================================================
# 3. Prepare Training Data (2009-2021)
# ============================================================
CBBPlayers_labeled <- CBBPlayers %>%
  left_join(allstars_lookup, by = c("player_name", "team")) %>%
  mutate(
    is_all_star = ifelse(!is.na(start_year) & year >= start_year & year <= end_year, 1, 0)
  ) %>%
  select(-start_year, -end_year)

# Most recent year per player for training
CBBPlayers_Training <- CBBPlayers_labeled %>%
  filter(year <= 2021) %>%
  group_by(player_name) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(simple_pos = factor(simple_pos, levels = c("G", "F", "C")))

# ============================================================
# 4. Define predictors (including height)
# ============================================================

# Check if height_in column exists and add it
if ("height_in" %in% names(CBBPlayers_Training)) {
  raw_stats <- c(
    "height_in", "Ortg", "usg", "eFG", "TS_per", "ORB_per", "DRB_per", "AST_per",
    "TO_per", "blk_per", "stl_per", "porpag", "adjoe",
    "drtg", "adrtg", "dporpag"
  )
  cat("✓ Height variable 'height_in' found and added to model (16 stats total)\n\n")
} else {
  raw_stats <- c(
    "Ortg", "usg", "eFG", "TS_per", "ORB_per", "DRB_per", "AST_per",
    "TO_per", "blk_per", "stl_per", "porpag", "adjoe",
    "drtg", "adrtg", "dporpag"
  )
  cat("⚠ Height variable 'height_in' not found. Using 15 stats without height.\n")
  cat("Available columns:", paste(head(names(CBBPlayers_Training), 20), collapse=", "), "...\n\n")
}

full_formula <- as.formula(
  paste("is_all_star ~", paste(raw_stats, collapse = " + "))
)

# ============================================================
# 5. Train Models with Regularization (FIXED for better predictions)
# ============================================================

train_and_evaluate <- function(data, pos_name) {
  cat("\n=== Training model for", pos_name, "===\n")
  
  data_clean <- data %>%
    select(is_all_star, all_of(raw_stats)) %>%
    na.omit()
  
  # Add small penalty to prevent perfect separation
  # Use ridge regression (L2 penalty) with very small lambda
  X <- as.matrix(data_clean[, raw_stats])
  y <- data_clean$is_all_star
  
  # Scale features for better numerical stability
  X_scaled <- scale(X)
  scaling_params <- list(
    center = attr(X_scaled, "scaled:center"),
    scale = attr(X_scaled, "scaled:scale")
  )
  
  # Use glmnet with very small regularization
  cv_model <- cv.glmnet(X_scaled, y, family = "binomial", alpha = 0, 
                        lambda = 10^seq(-4, -1, length.out = 20))
  
  # Get predictions
  data_clean$pred_prob <- as.vector(predict(cv_model, newx = X_scaled, 
                                            s = "lambda.min", type = "response"))
  
  # Calculate AUC
  roc_obj <- roc(data_clean$is_all_star, data_clean$pred_prob, quiet = TRUE)
  auc_value <- auc(roc_obj)
  
  cat("AUC:", round(auc_value, 3), "\n")
  cat("Sample size:", nrow(data_clean), 
      "| All-Stars:", sum(data_clean$is_all_star), "\n")
  cat("Prediction range:", round(min(data_clean$pred_prob), 4), "to", 
      round(max(data_clean$pred_prob), 4), "\n")
  
  return(list(
    model = cv_model, 
    auc = auc_value, 
    data = data_clean,
    scaling = scaling_params
  ))
}

# Train position-specific models
results_G <- train_and_evaluate(
  CBBPlayers_Training %>% filter(simple_pos == "G"), 
  "Guards"
)

results_F <- train_and_evaluate(
  CBBPlayers_Training %>% filter(simple_pos == "F"), 
  "Forwards"
)

results_C <- train_and_evaluate(
  CBBPlayers_Training %>% filter(simple_pos == "C"), 
  "Centers"
)

# ============================================================
# 6. Plot ROC Curves
# ============================================================

roc_G <- roc(results_G$data$is_all_star, results_G$data$pred_prob, quiet = TRUE)
roc_F <- roc(results_F$data$is_all_star, results_F$data$pred_prob, quiet = TRUE)
roc_C <- roc(results_C$data$is_all_star, results_C$data$pred_prob, quiet = TRUE)

cat("\n=== Model Performance (AUC) ===\n")
cat("Guards:  ", round(auc(roc_G), 3), "\n")
cat("Forwards:", round(auc(roc_F), 3), "\n")
cat("Centers: ", round(auc(roc_C), 3), "\n\n")

roc_plot <- ggroc(list(Guards = roc_G, Forwards = roc_F, Centers = roc_C)) +
  theme_minimal() +
  labs(title = "ROC Curves by Position",
       x = "Specificity",
       y = "Sensitivity") +
  scale_color_manual(values = c("Guards" = "blue", "Forwards" = "green", "Centers" = "red")) +
  annotate("text", x = 0.25, y = 0.2, 
           label = paste0("Guards AUC: ", round(auc(roc_G), 3)), 
           color = "blue", size = 4) +
  annotate("text", x = 0.25, y = 0.1, 
           label = paste0("Forwards AUC: ", round(auc(roc_F), 3)), 
           color = "green", size = 4) +
  annotate("text", x = 0.25, y = 0.0, 
           label = paste0("Centers AUC: ", round(auc(roc_C), 3)), 
           color = "red", size = 4)

print(roc_plot)

# ============================================================
# 7. PREDICTION FUNCTION for Current/Future Players (FIXED)
# ============================================================

predict_allstar_probability <- function(new_data, guard_result, forward_result, center_result) {
  new_data_pred <- new_data %>%
    group_by(player_name) %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(simple_pos = factor(simple_pos, levels = c("G", "F", "C")))
  
  # Initialize prediction column
  new_data_pred$allstar_probability <- NA
  
  # Check for missing required stats
  missing_stats <- setdiff(raw_stats, names(new_data_pred))
  if (length(missing_stats) > 0) {
    cat("WARNING: Missing required stats:", paste(missing_stats, collapse = ", "), "\n")
    cat("Predictions may be inaccurate or fail.\n\n")
  }
  
  # Predict by position with error handling
  guards <- new_data_pred %>% filter(simple_pos == "G")
  if (nrow(guards) > 0) {
    tryCatch({
      X_g <- as.matrix(guards[, raw_stats])
      X_g_scaled <- scale(X_g, center = guard_result$scaling$center, 
                          scale = guard_result$scaling$scale)
      new_data_pred$allstar_probability[new_data_pred$simple_pos == "G"] <- 
        as.vector(predict(guard_result$model, newx = X_g_scaled, 
                          s = "lambda.min", type = "response"))
    }, error = function(e) {
      cat("Error predicting for guards:", e$message, "\n")
    })
  }
  
  forwards <- new_data_pred %>% filter(simple_pos == "F")
  if (nrow(forwards) > 0) {
    tryCatch({
      X_f <- as.matrix(forwards[, raw_stats])
      X_f_scaled <- scale(X_f, center = forward_result$scaling$center, 
                          scale = forward_result$scaling$scale)
      new_data_pred$allstar_probability[new_data_pred$simple_pos == "F"] <- 
        as.vector(predict(forward_result$model, newx = X_f_scaled, 
                          s = "lambda.min", type = "response"))
    }, error = function(e) {
      cat("Error predicting for forwards:", e$message, "\n")
    })
  }
  
  centers <- new_data_pred %>% filter(simple_pos == "C")
  if (nrow(centers) > 0) {
    tryCatch({
      X_c <- as.matrix(centers[, raw_stats])
      X_c_scaled <- scale(X_c, center = center_result$scaling$center, 
                          scale = center_result$scaling$scale)
      new_data_pred$allstar_probability[new_data_pred$simple_pos == "C"] <- 
        as.vector(predict(center_result$model, newx = X_c_scaled, 
                          s = "lambda.min", type = "response"))
    }, error = function(e) {
      cat("Error predicting for centers:", e$message, "\n")
    })
  }
  
  return(new_data_pred)
}

# ============================================================
# 8. Apply Predictions to Current Players (2022+)
# ============================================================

Current_Players <- CBBPlayers %>%
  filter(year > 2021)

if (nrow(Current_Players) > 0) {
  Predictions <- predict_allstar_probability(
    Current_Players,
    results_G,
    results_F,
    results_C
  )
  
  Top_Prospects <- Predictions %>%
    select(player_name, team, year, simple_pos, allstar_probability, 
           conf, Ortg, usg, eFG, AST_per, DRB_per) %>%
    arrange(desc(allstar_probability)) %>%
    head(20)
  
  cat("\n=================================================\n")
  cat("===        TOP 20 ALL-STAR PROSPECTS          ===\n")
  cat("=================================================\n\n")
  print(Top_Prospects, n = 20)
  
  cat("\n\n=== TOP GUARDS ===\n")
  Top_Guards <- Predictions %>%
    filter(simple_pos == "G") %>%
    select(player_name, team, year, allstar_probability, Ortg, AST_per) %>%
    arrange(desc(allstar_probability)) %>%
    head(10)
  print(Top_Guards, n = 10)
  
  cat("\n\n=== TOP FORWARDS ===\n")
  Top_Forwards <- Predictions %>%
    filter(simple_pos == "F") %>%
    select(player_name, team, year, allstar_probability, Ortg, DRB_per) %>%
    arrange(desc(allstar_probability)) %>%
    head(10)
  print(Top_Forwards, n = 10)
  
  cat("\n\n=== TOP CENTERS ===\n")
  Top_Centers <- Predictions %>%
    filter(simple_pos == "C") %>%
    select(player_name, team, year, allstar_probability, Ortg, blk_per) %>%
    arrange(desc(allstar_probability)) %>%
    head(10)
  print(Top_Centers, n = 10)
  
  hist_plot <- ggplot(Predictions, aes(x = allstar_probability)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    facet_wrap(~simple_pos) +
    labs(title = "Predicted All-Star Probability Distribution by Position",
         x = "All-Star Probability",
         y = "Count") +
    theme_minimal()
  print(hist_plot)
  
  write.csv(Predictions %>% 
              select(player_name, team, year, simple_pos, allstar_probability, 
                     conf, Ortg, usg, eFG, AST_per, DRB_per, blk_per) %>%
              arrange(desc(allstar_probability)),
            file = "AllStar_Predictions.csv", 
            row.names = FALSE)
  cat("\n✓ Full predictions saved to: AllStar_Predictions.csv\n")
  
} else {
  cat("\n=================================================\n")
  cat("No players found after 2021 in dataset.\n")
  cat("Testing model on recent players (2019-2021)...\n")
  cat("=================================================\n\n")
  
  # Test on recent players from training set
  Test_Players <- CBBPlayers_labeled %>%
    filter(year >= 2019, year <= 2021) %>%
    group_by(player_name) %>%
    filter(year == max(year)) %>%
    ungroup()
  
  Test_Predictions <- predict_allstar_probability(
    Test_Players,
    results_G,
    results_F,
    results_C
  )
  
  cat("\n=== TOP 20 PREDICTED ALL-STARS (2019-2021) ===\n")
  Top_Test <- Test_Predictions %>%
    select(player_name, team, year, simple_pos, is_all_star, allstar_probability, 
           Ortg, usg, eFG) %>%
    arrange(desc(allstar_probability)) %>%
    head(20)
  print(Top_Test, n = 20)
  
  cat("\n=== ACTUAL ALL-STARS IN TEST SET ===\n")
  Actual_AllStars <- Test_Predictions %>%
    filter(is_all_star == 1) %>%
    select(player_name, team, year, simple_pos, allstar_probability, Ortg) %>%
    arrange(desc(allstar_probability))
  print(Actual_AllStars, n = 20)
}

# ============================================================
# 9. Feature Importance Visualization (FIXED for glmnet)
# ============================================================

plot_feature_importance <- function(result, pos_name) {
  # Extract coefficients from glmnet model
  coef_matrix <- as.matrix(coef(result$model, s = "lambda.min"))
  coef_values <- coef_matrix[-1, 1]  # Remove intercept
  
  coefs <- data.frame(
    stat = raw_stats,
    estimate = coef_values,
    abs_estimate = abs(coef_values)
  )
  
  ggplot(coefs, aes(x = reorder(stat, abs_estimate), y = estimate, 
                    fill = estimate > 0)) +
    geom_col() +
    coord_flip() +
    labs(title = paste("Feature Importance -", pos_name),
         x = "Feature",
         y = "Coefficient (Log-Odds)") +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "salmon"),
                      name = "Effect",
                      labels = c("Negative", "Positive")) +
    theme_minimal()
}

plot_feature_importance(results_G, "Guards")
plot_feature_importance(results_F, "Forwards")
plot_feature_importance(results_C, "Centers")

# ============================================================
# 10. Save Models
# ============================================================

saveRDS(results_G, "allstar_model_guards.rds")
saveRDS(results_F, "allstar_model_forwards.rds")
saveRDS(results_C, "allstar_model_centers.rds")

cat("\n✓ Models saved successfully!\n")
cat("Use readRDS() to load models for future predictions.\n")

# ============================================================
# 11. Prediction Functions with Calibrated Probabilities
# ============================================================

# Helper function to calibrate probabilities to a better scale
calibrate_probability <- function(raw_prob, position) {
  # Get the max probability from training data for this position
  max_prob <- switch(position,
                     "G" = max(results_G$data$pred_prob),
                     "F" = max(results_F$data$pred_prob),
                     "C" = max(results_C$data$pred_prob))
  
  # Rescale so the best player gets ~80-90%
  calibrated <- (raw_prob / max_prob) * 0.85
  calibrated <- pmin(calibrated, 0.99)  # Cap at 99%
  
  return(calibrated)
}

# Function 1: Quick single player prediction (FIXED with calibration)
quick_predict_v2 <- function(player_name, stats_list, position, use_calibrated = TRUE) {
  result <- switch(position,
                   "G" = results_G,
                   "F" = results_F,
                   "C" = results_C)
  
  player_data <- as.data.frame(stats_list)
  
  # Scale the input data using training parameters
  X <- as.matrix(player_data[, raw_stats])
  X_scaled <- scale(X, center = result$scaling$center, scale = result$scaling$scale)
  
  # Predict
  raw_prob <- as.vector(predict(result$model, newx = X_scaled, 
                                s = "lambda.min", type = "response"))
  
  # Calibrate probability
  if (use_calibrated) {
    prob <- calibrate_probability(raw_prob, position)
    prob_label <- "Calibrated All-Star Probability"
  } else {
    prob <- raw_prob
    prob_label <- "Raw All-Star Probability"
  }
  
  cat("\n=================================================\n")
  cat("Player:", player_name, "(", position, ")\n")
  cat(prob_label, ":", round(prob * 100, 1), "%\n")
  if (use_calibrated) {
    cat("(Raw probability:", round(raw_prob * 100, 1), "%)\n")
  }
  
  # Interpretation with calibrated scale
  if (prob >= 0.7) {
    cat("Rating: ⭐⭐⭐⭐⭐ ELITE ALL-STAR PROSPECT\n")
  } else if (prob >= 0.5) {
    cat("Rating: ⭐⭐⭐⭐ STRONG ALL-STAR PROSPECT\n")
  } else if (prob >= 0.3) {
    cat("Rating: ⭐⭐⭐ GOOD ALL-STAR POTENTIAL\n")
  } else if (prob >= 0.15) {
    cat("Rating: ⭐⭐ MODERATE ALL-STAR POTENTIAL\n")
  } else if (prob >= 0.05) {
    cat("Rating: ⭐ DEVELOPING ALL-STAR POTENTIAL\n")
  } else {
    cat("Rating: SOLID PLAYER, LOWER ALL-STAR PROBABILITY\n")
  }
  
  cat("=================================================\n\n")
  
  return(invisible(list(raw = raw_prob, calibrated = prob)))
}

# Function 2: Batch prediction with data frame
predict_batch <- function(players_df, use_calibrated = TRUE) {
  # players_df should have columns: player_name, position, and all 15 stat columns
  
  results_df <- players_df %>%
    mutate(
      raw_probability = NA,
      allstar_probability = NA
    )
  
  for (i in 1:nrow(results_df)) {
    pos <- results_df$position[i]
    result <- switch(pos,
                     "G" = results_G,
                     "F" = results_F,
                     "C" = results_C)
    
    stats <- results_df[i, raw_stats]
    X <- as.matrix(stats)
    X_scaled <- scale(X, center = result$scaling$center, scale = result$scaling$scale)
    
    raw_prob <- as.vector(
      predict(result$model, newx = X_scaled, s = "lambda.min", type = "response")
    )
    
    results_df$raw_probability[i] <- raw_prob
    results_df$allstar_probability[i] <- if(use_calibrated) {
      calibrate_probability(raw_prob, pos)
    } else {
      raw_prob
    }
  }
  
  results_df <- results_df %>%
    arrange(desc(allstar_probability)) %>%
    mutate(
      rank = row_number(),
      probability_pct = round(allstar_probability * 100, 1)
    )
  
  return(results_df)
}

# ============================================================
# USAGE INSTRUCTIONS & EXAMPLES
# ============================================================

cat("\n=================================================\n")
cat("===     PREDICTION TOOLS READY               ===\n")
cat("=================================================\n\n")
cat("Your models have been trained on 2009-2021 data.\n")
cat("You can now predict All-Star probability for ANY player!\n\n")

cat("EXAMPLE 1: Quick prediction with all stats (including height)\n")
cat("-" %>% rep(50) %>% paste(collapse=""), "\n")
cat("quick_predict_v2('Cooper Flagg',\n")
cat("                 list(height_in=80, Ortg=125, usg=28, eFG=62, TS_per=65,\n")
cat("                      ORB_per=10, DRB_per=18, AST_per=15, TO_per=10,\n")
cat("                      blk_per=6, stl_per=2.5, porpag=1.2, adjoe=125,\n")
cat("                      drtg=92, adrtg=88, dporpag=0.9),\n")
cat("                 'F')\n\n")

cat("EXAMPLE 2: Multiple players\n")
cat("-" %>% rep(50) %>% paste(collapse=""), "\n")
cat("quick_predict_v2('Ace Bailey', list(...stats...), 'F')\n")
cat("quick_predict_v2('Dylan Harper', list(...stats...), 'G')\n")
cat("quick_predict_v2('Khaman Maluach', list(...stats...), 'C')\n\n")

cat("REQUIRED STATS (16 total with height):\n")
cat("-" %>% rep(50) %>% paste(collapse=""), "\n")
for(i in 1:length(raw_stats)) {
  cat(sprintf("%2d. %-12s", i, raw_stats[i]))
  if(i %% 3 == 0) cat("\n") else cat("  ")
}
cat("\n\n")

cat("NOTE: height_in should be in inches (e.g., 6'8\" = 80 inches)\n")
cat("TIP: Find other stats on sports-reference.com or KenPom.com\n")
cat("=================================================\n\n")
quick_predict_v2('Deandre Ayton',
                 list(
                   height_in = 83,      # 6'11"
                   
                   # Scoring efficiency + usage
                   Ortg     = 123,      
                   usg      = 25.4,
                   eFG      = 61.7,
                   TS_per   = 65.0,
                   
                   # Rebounding
                   ORB_per  = 9.7,
                   DRB_per  = 25.0,
                   
                   # Playmaking + ball security
                   AST_per  = 12.4,
                   TO_per   = 15.2,
                   
                   # Defense
                   blk_per  = 6.1,
                   stl_per  = 1.0,
                   
                   # Overall impact metrics (converted)
                   porpag   = 6.5,     # elite scorer + rebounder
                   adjoe    = 133,     # top-tier offensive rating
                   drtg     = 95,
                   adrtg    = 92,
                   dporpag  = 3.8      # solid but not elite defensive impact
                 ),
                 'C')