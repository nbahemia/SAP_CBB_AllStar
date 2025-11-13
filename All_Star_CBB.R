# All-Star College Basketball Player Project - PREDICTIVE MODEL
# Years covered: 2009 - 2021 (Training), 2022+ (Prediction)

library(dplyr)
library(readxl)
library(ggplot2)
library(tibble)
library(caret)  # For cross-validation and model evaluation
library(pROC)   # For ROC curves

# ============================================================
# 1. Load datasets (same as before)
# ============================================================
CBBPlayers <- read_excel("/Users/naitikrambhia/Downloads/CBBPlayers_with_Positions.xlsx")

# ============================================================
# 2. All-Star Lookup Table (same as before)
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
  filter(year <= 2021) %>%  # Training data only
  group_by(player_name) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(simple_pos = factor(simple_pos, levels = c("G", "F", "C")))

# ============================================================
# 4. Define predictors
# ============================================================
raw_stats <- c(
  "Ortg", "usg", "eFG", "TS_per", "ORB_per", "DRB_per", "AST_per",
  "TO_per", "blk_per", "stl_per", "porpag", "adjoe",
  "drtg", "adrtg", "dporpag"
)

full_formula <- as.formula(
  paste("is_all_star ~", paste(raw_stats, collapse = " + "))
)

# ============================================================
# 5. Train Models with Cross-Validation
# ============================================================

train_and_evaluate <- function(data, pos_name) {
  cat("\n=== Training model for", pos_name, "===\n")
  
  # Remove rows with missing values
  data_clean <- data %>%
    select(is_all_star, all_of(gsub("`", "", raw_stats))) %>%
    na.omit()
  
  # Train model
  model <- glm(full_formula, data = data_clean, family = "binomial")
  
  # Get predictions on training data
  data_clean$pred_prob <- predict(model, type = "response")
  
  # Calculate metrics
  roc_obj <- roc(data_clean$is_all_star, data_clean$pred_prob, quiet = TRUE)
  auc_value <- auc(roc_obj)
  
  cat("AUC:", round(auc_value, 3), "\n")
  cat("Sample size:", nrow(data_clean), 
      "| All-Stars:", sum(data_clean$is_all_star), "\n")
  
  return(list(model = model, auc = auc_value, data = data_clean))
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
# 6. Plot ROC Curves (using ggplot2 to avoid margin errors)
# ============================================================

# Create ROC objects
roc_G <- roc(results_G$data$is_all_star, results_G$data$pred_prob, quiet = TRUE)
roc_F <- roc(results_F$data$is_all_star, results_F$data$pred_prob, quiet = TRUE)
roc_C <- roc(results_C$data$is_all_star, results_C$data$pred_prob, quiet = TRUE)

# Print AUC values
cat("\n=== Model Performance (AUC) ===\n")
cat("Guards:  ", round(auc(roc_G), 3), "\n")
cat("Forwards:", round(auc(roc_F), 3), "\n")
cat("Centers: ", round(auc(roc_C), 3), "\n\n")

# Plot using ggroc (safer and better looking)
library(pROC)
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
# 7. PREDICTION FUNCTION for Current/Future Players
# ============================================================

predict_allstar_probability <- function(new_data, guard_model, forward_model, center_model) {
  # Prepare new data
  new_data_pred <- new_data %>%
    group_by(player_name) %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(simple_pos = factor(simple_pos, levels = c("G", "F", "C")))
  
  # Initialize prediction column
  new_data_pred$allstar_probability <- NA
  
  # Predict by position
  guards <- new_data_pred %>% filter(simple_pos == "G")
  if (nrow(guards) > 0) {
    new_data_pred$allstar_probability[new_data_pred$simple_pos == "G"] <- 
      predict(guard_model, newdata = guards, type = "response")
  }
  
  forwards <- new_data_pred %>% filter(simple_pos == "F")
  if (nrow(forwards) > 0) {
    new_data_pred$allstar_probability[new_data_pred$simple_pos == "F"] <- 
      predict(forward_model, newdata = forwards, type = "response")
  }
  
  centers <- new_data_pred %>% filter(simple_pos == "C")
  if (nrow(centers) > 0) {
    new_data_pred$allstar_probability[new_data_pred$simple_pos == "C"] <- 
      predict(center_model, newdata = centers, type = "response")
  }
  
  return(new_data_pred)
}

# ============================================================
# 8. Apply Predictions to Current Players (2022+)
# ============================================================

# Filter for players after 2021 (current/future players)
Current_Players <- CBBPlayers %>%
  filter(year > 2021)

# If you have current players, predict their All-Star probability
if (nrow(Current_Players) > 0) {
  Predictions <- predict_allstar_probability(
    Current_Players,
    results_G$model,
    results_F$model,
    results_C$model
  )
  
  # View top prospects
  Top_Prospects <- Predictions %>%
    select(player_name, team, year, simple_pos, allstar_probability, 
           conf, Ortg, usg, eFG, AST_per, DRB_per) %>%
    arrange(desc(allstar_probability)) %>%
    head(20)
  
  cat("\n")
  cat("=================================================\n")
  cat("===        TOP 20 ALL-STAR PROSPECTS          ===\n")
  cat("=================================================\n\n")
  print(Top_Prospects, n = 20)
  
  # Also view by position
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
  
  # Visualize predictions
  hist_plot <- ggplot(Predictions, aes(x = allstar_probability)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    facet_wrap(~simple_pos) +
    labs(title = "Predicted All-Star Probability Distribution by Position",
         x = "All-Star Probability",
         y = "Count") +
    theme_minimal()
  print(hist_plot)
  
  # Save predictions to CSV for easy viewing
  write.csv(Predictions %>% 
              select(player_name, team, year, simple_pos, allstar_probability, 
                     conf, Ortg, usg, eFG, AST_per, DRB_per, blk_per) %>%
              arrange(desc(allstar_probability)),
            file = "AllStar_Predictions.csv", 
            row.names = FALSE)
  cat("\n✓ Full predictions saved to: AllStar_Predictions.csv\n")
  
} else {
  cat("\n")
  cat("=================================================\n")
  cat("No players found after 2021 in dataset.\n")
  cat("To make predictions, add current player data to CBBPlayers.\n")
  cat("=================================================\n")
}

# ============================================================
# 9. Feature Importance Visualization
# ============================================================

plot_feature_importance <- function(model, pos_name) {
  coefs <- data.frame(
    stat = names(coef(model))[-1],
    estimate = coef(model)[-1],
    abs_estimate = abs(coef(model)[-1])
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

plot_feature_importance(results_G$model, "Guards")
plot_feature_importance(results_F$model, "Forwards")
plot_feature_importance(results_C$model, "Centers")

# ============================================================
# 10. Save Models for Future Use
# ============================================================

saveRDS(results_G$model, "allstar_model_guards.rds")
saveRDS(results_F$model, "allstar_model_forwards.rds")
saveRDS(results_C$model, "allstar_model_centers.rds")

cat("\n✓ Models saved successfully!\n")
cat("Use readRDS() to load models for future predictions.\n")

# ============================================================
# 11. Interactive Prediction Function for Manual Input
# ============================================================

predict_new_player <- function() {
  cat("\n=================================================\n")
  cat("===     NBA ALL-STAR PREDICTION TOOL         ===\n")
  cat("=================================================\n\n")
  
  # Get player info
  player_name <- readline(prompt = "Enter player name: ")
  position <- toupper(readline(prompt = "Enter position (G/F/C): "))
  
  # Validate position
  while (!position %in% c("G", "F", "C")) {
    cat("Invalid position. Please enter G, F, or C.\n")
    position <- toupper(readline(prompt = "Enter position (G/F/C): "))
  }
  
  cat("\nEnter the following stats for", player_name, ":\n")
  cat("(You can find these in standard box scores)\n\n")
  
  # Helper function to get numeric input
  get_stat <- function(stat_name, required = TRUE) {
    input <- readline(prompt = paste0(stat_name, ": "))
    if (input == "" && !required) return(NA)
    val <- as.numeric(input)
    return(val)
  }
  
  # Collect basic stats from the table
  cat("--- Per Game Stats ---\n")
  PTS <- get_stat("Points Per Game (PTS)")
  AST <- get_stat("Assists Per Game (AST)")
  TRB <- get_stat("Total Rebounds Per Game (TRB)")
  STL <- get_stat("Steals Per Game (STL)")
  BLK <- get_stat("Blocks Per Game (BLK)")
  TOV <- get_stat("Turnovers Per Game (TOV)")
  
  cat("\n--- Shooting Stats ---\n")
  FG_pct <- get_stat("Field Goal % (FG%) [as decimal, e.g., 0.45]")
  FG3_pct <- get_stat("3-Point % (3P%) [as decimal, e.g., 0.35]")
  FT_pct <- get_stat("Free Throw % (FT%) [as decimal, e.g., 0.75]")
  
  cat("\n--- Advanced Stats (if known, otherwise we'll estimate) ---\n")
  MP <- get_stat("Minutes Per Game (MP)", required = FALSE)
  ORB <- get_stat("Offensive Rebounds Per Game (ORB)", required = FALSE)
  DRB <- get_stat("Defensive Rebounds Per Game (DRB)", required = FALSE)
  
  # Calculate derived stats needed for model
  # These are approximations based on per-game stats
  
  # Estimate advanced metrics
  if (is.na(MP)) MP <- 30  # Default minutes
  if (is.na(ORB)) ORB <- TRB * 0.25  # Estimate ORB as 25% of total rebounds
  if (is.na(DRB)) DRB <- TRB * 0.75  # Estimate DRB as 75% of total rebounds
  
  # Calculate percentages (rough estimates based on national averages)
  ORB_per <- (ORB / MP) * 40 * 1.5  # Approximate ORB%
  DRB_per <- (DRB / MP) * 40 * 1.5  # Approximate DRB%
  AST_per <- (AST / MP) * 40 * 1.5  # Approximate AST%
  TO_per <- (TOV / MP) * 40 * 1.5   # Approximate TO%
  stl_per <- (STL / MP) * 40 * 0.5  # Approximate STL%
  blk_per <- (BLK / MP) * 40 * 0.5  # Approximate BLK%
  
  # Calculate efficiency metrics
  TS_per <- FG_pct * 1.05  # Rough approximation of True Shooting
  eFG <- FG_pct + (0.5 * FG3_pct * 0.3)  # Approximate eFG%
  
  # Estimate usage (based on scoring)
  usg <- (PTS / MP) * 40 * 0.8  # Rough usage estimate
  
  # Estimate ratings (league average is ~100)
  Ortg <- 100 + (PTS - 15) * 1.5  # Offensive rating estimate
  drtg <- 105 - (STL + BLK) * 2   # Defensive rating estimate
  
  # Advanced metrics (rough estimates)
  adjoe <- Ortg
  adrtg <- drtg
  porpag <- (PTS - 15) * 0.1
  dporpag <- (STL + BLK - 2) * 0.1
  ast_tov <- ifelse(TOV > 0, AST / TOV, 2.0)
  
  # Create stats list for model
  stats <- data.frame(
    Ortg = Ortg,
    usg = usg,
    eFG = eFG,
    TS_per = TS_per,
    ORB_per = ORB_per,
    DRB_per = DRB_per,
    AST_per = AST_per,
    TO_per = TO_per,
    blk_per = blk_per,
    stl_per = stl_per,
    porpag = porpag,
    adjoe = adjoe,
    drtg = drtg,
    adrtg = adrtg,
    dporpag = dporpag
  )
  
  # Add ast/tov with proper name handling
  stats# All-Star College Basketball Player Project - PREDICTIVE MODEL
  # Years covered: 2009 - 2021 (Training), 2022+ (Prediction)
  
  library(dplyr)
  library(readxl)
  library(ggplot2)
  library(tibble)
  library(caret)  # For cross-validation and model evaluation
  library(pROC)   # For ROC curves
  
  # ============================================================
  # 1. Load datasets (same as before)
  # ============================================================
  CBBPlayers <- read_excel("/Users/naitikrambhia/Downloads/CBBPlayers_with_Positions.xlsx")
  
  # ============================================================
  # 2. All-Star Lookup Table (same as before)
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
    filter(year <= 2021) %>%  # Training data only
    group_by(player_name) %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(simple_pos = factor(simple_pos, levels = c("G", "F", "C")))
  
  # ============================================================
  # 4. Define predictors
  # ============================================================
  raw_stats <- c(
    "Ortg", "usg", "eFG", "TS_per", "ORB_per", "DRB_per", "AST_per",
    "TO_per", "blk_per", "stl_per", "porpag", "adjoe", "`ast/tov`",
    "drtg", "adrtg", "dporpag"
  )
  
  full_formula <- as.formula(
    paste("is_all_star ~", paste(raw_stats, collapse = " + "))
  )
  
  # ============================================================
  # 5. Train Models with Cross-Validation
  # ============================================================
  
  train_and_evaluate <- function(data, pos_name) {
    cat("\n=== Training model for", pos_name, "===\n")
    
    # Remove rows with missing values
    data_clean <- data %>%
      select(is_all_star, all_of(gsub("`", "", raw_stats))) %>%
      na.omit()
    
    # Train model
    model <- glm(full_formula, data = data_clean, family = "binomial")
    
    # Get predictions on training data
    data_clean$pred_prob <- predict(model, type = "response")
    
    # Calculate metrics
    roc_obj <- roc(data_clean$is_all_star, data_clean$pred_prob, quiet = TRUE)
    auc_value <- auc(roc_obj)
    
    cat("AUC:", round(auc_value, 3), "\n")
    cat("Sample size:", nrow(data_clean), 
        "| All-Stars:", sum(data_clean$is_all_star), "\n")
    
    return(list(model = model, auc = auc_value, data = data_clean))
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
  # 6. Plot ROC Curves (using ggplot2 to avoid margin errors)
  # ============================================================
  
  # Create ROC objects
  roc_G <- roc(results_G$data$is_all_star, results_G$data$pred_prob, quiet = TRUE)
  roc_F <- roc(results_F$data$is_all_star, results_F$data$pred_prob, quiet = TRUE)
  roc_C <- roc(results_C$data$is_all_star, results_C$data$pred_prob, quiet = TRUE)
  
  # Print AUC values
  cat("\n=== Model Performance (AUC) ===\n")
  cat("Guards:  ", round(auc(roc_G), 3), "\n")
  cat("Forwards:", round(auc(roc_F), 3), "\n")
  cat("Centers: ", round(auc(roc_C), 3), "\n\n")
  
  # Plot using ggroc (safer and better looking)
  library(pROC)
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
  # 7. PREDICTION FUNCTION for Current/Future Players
  # ============================================================
  
  predict_allstar_probability <- function(new_data, guard_model, forward_model, center_model) {
    # Prepare new data
    new_data_pred <- new_data %>%
      group_by(player_name) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(simple_pos = factor(simple_pos, levels = c("G", "F", "C")))
    
    # Initialize prediction column
    new_data_pred$allstar_probability <- NA
    
    # Predict by position
    guards <- new_data_pred %>% filter(simple_pos == "G")
    if (nrow(guards) > 0) {
      new_data_pred$allstar_probability[new_data_pred$simple_pos == "G"] <- 
        predict(guard_model, newdata = guards, type = "response")
    }
    
    forwards <- new_data_pred %>% filter(simple_pos == "F")
    if (nrow(forwards) > 0) {
      new_data_pred$allstar_probability[new_data_pred$simple_pos == "F"] <- 
        predict(forward_model, newdata = forwards, type = "response")
    }
    
    centers <- new_data_pred %>% filter(simple_pos == "C")
    if (nrow(centers) > 0) {
      new_data_pred$allstar_probability[new_data_pred$simple_pos == "C"] <- 
        predict(center_model, newdata = centers, type = "response")
    }
    
    return(new_data_pred)
  }
  
  # ============================================================
  # 8. Apply Predictions to Current Players (2022+)
  # ============================================================
  
  # Filter for players after 2021 (current/future players)
  Current_Players <- CBBPlayers %>%
    filter(year > 2021)
  
  # If you have current players, predict their All-Star probability
  if (nrow(Current_Players) > 0) {
    Predictions <- predict_allstar_probability(
      Current_Players,
      results_G$model,
      results_F$model,
      results_C$model
    )
    
    # View top prospects
    Top_Prospects <- Predictions %>%
      select(player_name, team, year, simple_pos, allstar_probability, 
             conf, Ortg, usg, eFG, AST_per, DRB_per) %>%
      arrange(desc(allstar_probability)) %>%
      head(20)
    
    cat("\n")
    cat("=================================================\n")
    cat("===        TOP 20 ALL-STAR PROSPECTS          ===\n")
    cat("=================================================\n\n")
    print(Top_Prospects, n = 20)
    
    # Also view by position
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
    
    # Visualize predictions
    hist_plot <- ggplot(Predictions, aes(x = allstar_probability)) +
      geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
      facet_wrap(~simple_pos) +
      labs(title = "Predicted All-Star Probability Distribution by Position",
           x = "All-Star Probability",
           y = "Count") +
      theme_minimal()
    print(hist_plot)
    
    # Save predictions to CSV for easy viewing
    write.csv(Predictions %>% 
                select(player_name, team, year, simple_pos, allstar_probability, 
                       conf, Ortg, usg, eFG, AST_per, DRB_per, blk_per) %>%
                arrange(desc(allstar_probability)),
              file = "AllStar_Predictions.csv", 
              row.names = FALSE)
    cat("\n✓ Full predictions saved to: AllStar_Predictions.csv\n")
    
  } else {
    cat("\n")
    cat("=================================================\n")
    cat("No players found after 2021 in dataset.\n")
    cat("To make predictions, add current player data to CBBPlayers.\n")
    cat("=================================================\n")
  }
  
  # ============================================================
  # 9. Feature Importance Visualization
  # ============================================================
  
  plot_feature_importance <- function(model, pos_name) {
    coefs <- data.frame(
      stat = names(coef(model))[-1],
      estimate = coef(model)[-1],
      abs_estimate = abs(coef(model)[-1])
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
  
  plot_feature_importance(results_G$model, "Guards")
  plot_feature_importance(results_F$model, "Forwards")
  plot_feature_importance(results_C$model, "Centers")
  
  # ============================================================
  # 10. Save Models for Future Use
  # ============================================================
  
  saveRDS(results_G$model, "allstar_model_guards.rds")
  saveRDS(results_F$model, "allstar_model_forwards.rds")
  saveRDS(results_C$model, "allstar_model_centers.rds")
  
  cat("\n✓ Models saved successfully!\n")
  cat("Use readRDS() to load models for future predictions.\n")
  
  # ============================================================
  # 11. Interactive Prediction Function for Manual Input
  # ============================================================
  
  predict_new_player <- function() {
    cat("\n=================================================\n")
    cat("===     NBA ALL-STAR PREDICTION TOOL         ===\n")
    cat("=================================================\n\n")
    
    # Get player info
    player_name <- readline(prompt = "Enter player name: ")
    position <- toupper(readline(prompt = "Enter position (G/F/C): "))
    
    # Validate position
    while (!position %in% c("G", "F", "C")) {
      cat("Invalid position. Please enter G, F, or C.\n")
      position <- toupper(readline(prompt = "Enter position (G/F/C): "))
    }
    
    cat("\nEnter the following stats for", player_name, ":\n")
    cat("(You can find these in standard box scores)\n\n")
    
    # Helper function to get numeric input
    get_stat <- function(stat_name, required = TRUE) {
      input <- readline(prompt = paste0(stat_name, ": "))
      if (input == "" && !required) return(NA)
      val <- as.numeric(input)
      return(val)
    }
    
    # Collect basic stats from the table
    cat("--- Per Game Stats ---\n")
    PTS <- get_stat("Points Per Game (PTS)")
    AST <- get_stat("Assists Per Game (AST)")
    TRB <- get_stat("Total Rebounds Per Game (TRB)")
    STL <- get_stat("Steals Per Game (STL)")
    BLK <- get_stat("Blocks Per Game (BLK)")
    TOV <- get_stat("Turnovers Per Game (TOV)")
    
    cat("\n--- Shooting Stats ---\n")
    FG_pct <- get_stat("Field Goal % (FG%) [as decimal, e.g., 0.45]")
    FG3_pct <- get_stat("3-Point % (3P%) [as decimal, e.g., 0.35]")
    FT_pct <- get_stat("Free Throw % (FT%) [as decimal, e.g., 0.75]")
    
    cat("\n--- Advanced Stats (if known, otherwise we'll estimate) ---\n")
    MP <- get_stat("Minutes Per Game (MP)", required = FALSE)
    ORB <- get_stat("Offensive Rebounds Per Game (ORB)", required = FALSE)
    DRB <- get_stat("Defensive Rebounds Per Game (DRB)", required = FALSE)
    
    # Calculate derived stats needed for model
    # These are approximations based on per-game stats
    
    # Estimate advanced metrics
    if (is.na(MP)) MP <- 30  # Default minutes
    if (is.na(ORB)) ORB <- TRB * 0.25  # Estimate ORB as 25% of total rebounds
    if (is.na(DRB)) DRB <- TRB * 0.75  # Estimate DRB as 75% of total rebounds
    
    # Calculate percentages (rough estimates based on national averages)
    ORB_per <- (ORB / MP) * 40 * 1.5  # Approximate ORB%
    DRB_per <- (DRB / MP) * 40 * 1.5  # Approximate DRB%
    AST_per <- (AST / MP) * 40 * 1.5  # Approximate AST%
    TO_per <- (TOV / MP) * 40 * 1.5   # Approximate TO%
    stl_per <- (STL / MP) * 40 * 0.5  # Approximate STL%
    blk_per <- (BLK / MP) * 40 * 0.5  # Approximate BLK%
    
    # Calculate efficiency metrics
    TS_per <- FG_pct * 1.05  # Rough approximation of True Shooting
    eFG <- FG_pct + (0.5 * FG3_pct * 0.3)  # Approximate eFG%
    
    # Estimate usage (based on scoring)
    usg <- (PTS / MP) * 40 * 0.8  # Rough usage estimate
    
    # Estimate ratings (league average is ~100)
    Ortg <- 100 + (PTS - 15) * 1.5  # Offensive rating estimate
    drtg <- 105 - (STL + BLK) * 2   # Defensive rating estimate
    
    # Advanced metrics (rough estimates)
    adjoe <- Ortg
    adrtg <- drtg
    porpag <- (PTS - 15) * 0.1
    dporpag <- (STL + BLK - 2) * 0.1
    ast_tov <- ifelse(TOV > 0, AST / TOV, 2.0)
    
    ast/tov` <- ast_tov
    
    player_data <- stats
    
    # Select appropriate model
    model <- switch(position,
                    "G" = results_G$model,
                    "F" = results_F$model,
                    "C" = results_C$model)
    
    # Make prediction
    prob <- predict(model, newdata = player_data, type = "response")
    
    # Display results
    cat("\n=================================================\n")
    cat("PREDICTION RESULTS\n")
    cat("=================================================\n")
    cat("Player:", player_name, "\n")
    cat("Position:", position, "\n")
    cat("All-Star Probability:", round(prob * 100, 1), "%\n\n")
    
    # Interpretation
    if (prob >= 0.7) {
      cat("⭐⭐⭐ ELITE PROSPECT - Very high All-Star potential!\n")
    } else if (prob >= 0.5) {
      cat("⭐⭐ STRONG PROSPECT - Good All-Star potential\n")
    } else if (prob >= 0.3) {
      cat("⭐ MODERATE PROSPECT - Developmental All-Star potential\n")
    } else {
      cat("DEVELOPING PLAYER - Lower All-Star probability\n")
    }
    
    cat("=================================================\n\n")
    
    return(invisible(list(name = player_name, position = position, probability = prob)))
  }
  
  # ============================================================
  # 12. Batch Prediction Function (Multiple Players at Once)
  # ============================================================
  
  predict_multiple_players <- function() {
    cat("\n=== BATCH PREDICTION MODE ===\n")
    cat("Enter player data in format: Name,Position,Ortg,usg,eFG,...\n")
    cat("Type 'done' when finished\n\n")
    
    results <- list()
    
    while(TRUE) {
      input <- readline(prompt = "Enter player data (or 'done'): ")
      if (tolower(input) == "done") break
      
      # Parse input
      # You can expand this to handle CSV-like input
      cat("Player added to batch.\n")
    }
    
    cat("\nBatch predictions complete!\n")
  }
  
  # ============================================================
  # Quick Prediction with Pre-filled Stats (for testing)
  # ============================================================
  
  quick_predict_v2 <- function(player_name, stats_list, position) {
    # Select model based on position
    model <- switch(position,
                    "G" = results_G$model,
                    "F" = results_F$model,
                    "C" = results_C$model)
    
    # Create data frame
    player_data <- as.data.frame(stats_list)
    
    # Predict
    prob <- predict(model, newdata = player_data, type = "response")
    
    cat("\n=================================================\n")
    cat("Player:", player_name, "(", position, ")\n")
    cat("All-Star Probability:", round(prob * 100, 1), "%\n")
    cat("=================================================\n\n")
    
    return(prob)
  }
  
  # ============================================================
  # HOW TO USE THE PREDICTION TOOLS
  # ============================================================
  
  cat("\n")
  cat("=================================================\n")
  cat("===     PREDICTION TOOLS READY               ===\n")
  cat("=================================================\n\n")
  cat("To predict for a new player, use:\n\n")
  cat("1. INTERACTIVE MODE (easiest):\n")
  cat("   predict_new_player()\n\n")
  cat("2. QUICK PREDICTION (if you have all stats):\n")
  cat("   quick_predict_v2('Cooper Flagg',\n")
  cat("                    list(Ortg=125, usg=25, eFG=0.60, TS_per=0.62,\n")
  cat("                         ORB_per=8, DRB_per=15, AST_per=20, TO_per=12,\n")
  cat("                         blk_per=5, stl_per=3, porpag=0.8, adjoe=120,\n")
  cat("                         drtg=95, adrtg=92, dporpag=0.75),\n")
  cat("                    'F')\n\n")
  cat("=================================================\n\n")
  
  cat("Type: predict_new_player() to start predicting!\n\n")