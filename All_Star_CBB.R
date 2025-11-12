# All-Star College Basketball Player Project
# Years covered, 2009 - 2021.

library(readxl)
library(dplyr)
library(writexl)
library(knitr)

# --- 0. Load Data ---
CBBPlayers <- read_excel("/Users/varunrao/Downloads/CBBPlayers_with_Positions (1).xlsx")
DraftedPlayers <- read_excel("/Users/varunrao/Downloads/DraftedPlayers2009-2021.xlsx")

# --- 1. Height and Position Cleaning ---
CBBPlayers <- CBBPlayers %>%
  mutate(
    # Handle missing or malformed heights safely
    ht_clean = ifelse(grepl("^[0-9]+'[0-9]+", ht), ht, NA_character_),
    
    # Extract feet and inches from the "6'8"" style format
    feet = as.numeric(sub("'[0-9]+.*", "", ht_clean)),
    inches = as.numeric(sub(".*'(\\d+).*", "\\1", ht_clean)),
    
    # Create a nicely formatted height string
    ht = paste0(feet, "'", inches, "\""),
    
    # Calculate total height in inches
    height_in = feet * 12 + inches
  ) %>%
  select(-ht_clean)

CBBPlayers <- CBBPlayers %>%
  mutate(
    simple_pos = case_when(
      grepl("pg|guard|combo g|wing g", pos, ignore.case = TRUE) ~ "G",
      grepl("stretch 4|wing f", pos, ignore.case = TRUE) ~ "F",
      grepl("pf/c", pos, ignore.case = TRUE) & height_in >= 82 ~ "C",
      grepl("pf/c", pos, ignore.case = TRUE) & height_in < 82 ~ "F",
      grepl("c|center", pos, ignore.case = TRUE) ~ "C",
      TRUE ~ "F"
    )
  )

# --- 2. Calculate Position-Specific Percentiles ---
CBBPlayers <- CBBPlayers %>%
  group_by(simple_pos) %>%
  mutate(
    Ortg_percentile    = percent_rank(Ortg) * 100,
    usg_percentile     = percent_rank(usg) * 100,
    eFG_percentile     = percent_rank(eFG) * 100,
    TS_per_percentile  = percent_rank(TS_per) * 100,
    ORB_per_percentile = percent_rank(ORB_per) * 100,
    DRB_per_percentile = percent_rank(DRB_per) * 100,
    AST_per_percentile = percent_rank(AST_per) * 100,
    TO_per_percentile  = percent_rank(TO_per) * 100,
    blk_per_percentile = percent_rank(blk_per) * 100,
    stl_per_percentile = percent_rank(stl_per) * 100,
    porpag_percentile  = percent_rank(porpag) * 100,
    adjoe_percentile   = percent_rank(adjoe) * 100,
    ast_tov_percentile = percent_rank(`ast/tov`) * 100,
    drtg_percentile    = percent_rank(drtg) * 100,
    adrtg_percentile   = percent_rank(adrtg) * 100,
    dporpag_percentile = percent_rank(dporpag) * 100
  ) %>%
  ungroup()


# --- 3. Filter to Final Season, Add All-Star Grouping, and Filter Minutes ---

players_of_interest <- c(
  "Anthony Davis", "Edrice Adebayo", "Bradley Beal", "Damian Lillard", 
  "DeMarcus Cousins", "Devin Booker", "Donovan Mitchell", "Draymond Green", 
  "Gordon Hayward", "Isaiah Thomas", "Jayson Tatum", "Jaylen Brown", 
  "Jimmy Butler", "John Wall", "Julius Randle", "Karl-Anthony Towns", 
  "Kawhi Leonard", "Kemba Walker", "Khris Middleton", "Klay Thompson", 
  "Kyrie Irving", "Nikola Vucevic", "Paul George", "Trae Young", 
  "Victor Oladipo", "Zach LaVine", "Zion Williamson", "Ben Simmons", 
  "Brandon Ingram", "Pascal Siakam", "D'Angelo Russell", "Domantas Sabonis", 
  "Ja Morant", "Dejounte Murray", "Jarrett Allen", "Andrew Wiggins", 
  "Shai Gilgeous-Alexander", "Jalen Williams", "Evan Mobley", 
  "Jaren Jackson Jr.", "Jalen Brunson", "Cade Cunningham", "Tyler Herro", 
  "Darius Garland", "Anthony Edwards", "Tyrese Maxey", "Tyrese Haliburton", 
  "Fred VanVleet", "Lauri Markkanen"
)

CBBPlayers_filtered <- CBBPlayers %>%
  group_by(simple_pos) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(is_interest = ifelse(player_name %in% players_of_interest, "Interest", "Other")) %>%
  filter(simple_pos %in% c("G", "F", "C"))



# Keep your original stats_to_test
stats_to_test <- c(
  "Ortg_percentile", "usg_percentile", "eFG_percentile", "TS_per_percentile",
  "ORB_per_percentile", "DRB_per_percentile", "AST_per_percentile",
  "TO_per_percentile", "blk_per_percentile", "stl_per_percentile",
  "porpag_percentile", "adjoe_percentile", "ast_tov_percentile",
  "drtg_percentile", "adrtg_percentile", "dporpag_percentile"
)

t_test_by_pos <- data.frame(
  position = character(),
  stat = character(),
  interest_mean = numeric(),
  other_mean = numeric(),
  t_statistic = numeric(),
  p_value = numeric()
)

# --- 4. T-Test Loop by Position ---
for (pos in c("G", "F", "C")) {
  data_pos <- CBBPlayers_filtered %>%
    filter(simple_pos == pos)
  
  interest_count <- sum(data_pos$is_interest == "Interest", na.rm = TRUE)
  other_count <- sum(data_pos$is_interest == "Other", na.rm = TRUE)
  
  if (interest_count < 2 || other_count < 2) {
    cat(paste("\nSkipping Position:", pos, "- Not enough players in both groups (Interest:", interest_count, ", Other:", other_count, ").\n"))
    next
  }
  
  cat("\n--- Position:", pos, "---\n")
  cat("Interest players:", interest_count, "| Other players:", other_count, "\n")
  
  for (stat in stats_to_test) {
    data_sub <- data_pos %>%
      filter(!is.na(.data[[stat]]))
    
    # Extract numeric vectors for the two groups
    interest_vals <- data_sub[[stat]][data_sub$is_interest == "Interest"]
    other_vals <- data_sub[[stat]][data_sub$is_interest == "Other"]
    
    # Skip if one group is empty
    if (length(interest_vals) < 2 || length(other_vals) < 2) next
    
    # Run two-sample t-test properly
    t_res <- t.test(x = interest_vals, y = other_vals, var.equal = FALSE)
    
    # Append results
    t_test_by_pos <- rbind(
      t_test_by_pos,
      data.frame(
        position = pos,
        stat = stat,
        interest_mean = mean(interest_vals, na.rm = TRUE),
        other_mean = mean(other_vals, na.rm = TRUE),
        t_statistic = t_res$statistic,
        p_value = t_res$p.value
      )
    )
  }
}

# --- 5. Format and Display Results ---
t_test_table <- t_test_by_pos %>%
  arrange(position, p_value) %>%
  mutate(
    across(where(is.numeric), round, 2),
    significant = ifelse(p_value < 0.05, "✅ Yes", "❌ No")
  )

# THIS IS THE FINAL OUTPUT COMMAND
kable(t_test_table, caption = "Two-sample t-test: All-Stars vs. Others, by Position (Min_per > 200)")