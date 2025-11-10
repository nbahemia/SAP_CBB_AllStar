# All-Star College Basketball Player Project
# Years covered, 2009 - 2021.

library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)

# Load data
CBBPlayers <- read_excel("C:\\Users\\naeem\\OneDrive\\Documents\\GitHub\\SAP_CBB_AllStar\\CBBPlayers_with_Positions.xlsx")

# Filter for most recent year per player
CBBPlayers_Recent <- CBBPlayers %>%
  group_by(player_name) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    simple_pos = factor(simple_pos, levels = c("G", "F", "C"))
  )

# Players of interest
players_of_interest <- tibble::tibble(
  player_name = c(
    "Andrew Wiggins", "Anthony Davis", "Anthony Edwards", "Edrice Adebayo",
    "Ben Simmons", "Bradley Beal", "Brandon Ingram", "Cade Cunningham",
    "D'Angelo Russell", "Damian Lillard", "Darius Garland", "Dejounte Murray",
    "DeMarcus Cousins", "Devin Booker", "Domantas Sabonis", "Donovan Mitchell",
    "Draymond Green", "Evan Mobley", "Fred VanVleet", "Gordon Hayward",
    "Isaiah Thomas", "Ja Morant", "Jalen Brunson", "Jalen Williams",
    "Jaren Jackson Jr.", "Jarrett Allen", "Jaylen Brown", "Jayson Tatum",
    "Jimmy Butler", "John Wall", "Julius Randle", "Karl-Anthony Towns",
    "Kawhi Leonard", "Kemba Walker", "Khris Middleton", "Klay Thompson",
    "Kyrie Irving", "Lauri Markkanen", "Nikola Vucević",
    "Pascal Siakam", "Paul George", "Shai Gilgeous-Alexander", "Trae Young",
    "Tyler Herro", "Tyrese Haliburton", "Tyrese Maxey", "Victor Oladipo",
    "Zach LaVine", "Zion Williamson"
  )
)

# Compute percentiles for ALL players
CBBPlayers_percentiles <- CBBPlayers_Recent %>%
  mutate(
    Ortg_pct    = percent_rank(Ortg) * 100,
    usg_pct     = percent_rank(usg) * 100,
    eFG_pct     = percent_rank(eFG) * 100,
    TS_pct      = percent_rank(TS_per) * 100,
    ORB_pct     = percent_rank(ORB_per) * 100,
    DRB_pct     = percent_rank(DRB_per) * 100,
    AST_pct     = percent_rank(AST_per) * 100,
    TO_pct      = percent_rank(TO_per) * 100,
    blk_pct     = percent_rank(blk_per) * 100,
    stl_pct     = percent_rank(stl_per) * 100,
    porpag_pct  = percent_rank(porpag) * 100,
    adjoe_pct   = percent_rank(adjoe) * 100,
    ast_tov_pct = percent_rank(`ast/tov`) * 100,
    drtg_pct    = percent_rank(drtg) * 100,
    adrtg_pct   = percent_rank(adrtg) * 100,
    dporpag_pct = percent_rank(dporpag) * 100
  )

# Label dataset with all-star info AND keep positions
CBBPlayers_labeled <- CBBPlayers_percentiles %>%
  mutate(
    simple_pos = factor(simple_pos, levels = c("G", "F", "C")),
    is_all_star = ifelse(player_name %in% players_of_interest$player_name, 1, 0)
  )

# Verify simple_pos is valid
print(table(CBBPlayers_labeled$simple_pos))

# Percentile columns
percentile_cols <- grep("_pct$", names(CBBPlayers_labeled), value = TRUE)

# Correlation function
get_position_cor <- function(pos) {
  CBBPlayers_labeled %>%
    filter(simple_pos == pos) %>%
    select(is_all_star, all_of(percentile_cols)) %>%
    cor(use = "pairwise.complete.obs") %>%
    .["is_all_star", ]
}

# Correlations per position
cor_G <- get_position_cor("G")
cor_F <- get_position_cor("F")
cor_C <- get_position_cor("C")

# Combined table
cor_by_pos <- data.frame(
  stat = names(cor_G),
  Guards = as.numeric(cor_G),
  Forwards = as.numeric(cor_F),
  Centers = as.numeric(cor_C)
)

cor_by_pos

mean_by_group <- CBBPlayers_labeled %>%
  group_by(simple_pos, is_all_star) %>%
  summarize(across(all_of(percentile_cols), mean, na.rm = TRUE))

mean_by_group

full_formula <- as.formula(
  paste("is_all_star ~", paste(percentile_cols, collapse = " + "))
)

glm_G_full <- glm(
  full_formula,
  data = CBBPlayers_labeled %>% filter(simple_pos == "G"),
  family = "binomial"
)


glm_F_full <- glm(
  full_formula,
  data = CBBPlayers_labeled %>% filter(simple_pos == "F"),
  family = "binomial"
)

glm_C_full <- glm(
  full_formula,
  data = CBBPlayers_labeled %>% filter(simple_pos == "C"),
  family = "binomial"
)

summary(glm_G_full)

summary(glm_F_full)

summary(glm_C_full)


