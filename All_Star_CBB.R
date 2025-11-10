# All-Star College Basketball Player Project
# Years covered: 2009 - 2021

library(dplyr)
library(readxl)
library(ggplot2)

# -------------------------------
# Load data
# -------------------------------
CBBPlayers <- read_excel("C:\\Users\\naeem\\OneDrive\\Documents\\GitHub\\SAP_CBB_AllStar\\CBBPlayers_with_Positions.xlsx")

# -------------------------------
# Filter for most recent year per player
# -------------------------------
CBBPlayers_Recent <- CBBPlayers %>%
  group_by(player_name, team) %>%  # use team to distinguish duplicates
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(simple_pos = factor(simple_pos, levels = c("G", "F", "C")))

# -------------------------------
# All-Star Players
# -------------------------------
players_of_interest <- c(
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

# -------------------------------
# Add All-Star label
# -------------------------------
CBBPlayers_labeled <- CBBPlayers_Recent %>%
  mutate(is_all_star = ifelse(player_name %in% players_of_interest, 1, 0))

# -------------------------------
# Raw stats to use in GLM
# -------------------------------
raw_stats <- c(
  "Ortg", "usg", "eFG", "TS_per", "ORB_per", "DRB_per", "AST_per",
  "TO_per", "blk_per", "stl_per", "porpag", "adjoe", "`ast/tov`",
  "drtg", "adrtg", "dporpag"
)

# -------------------------------
# Full GLM formula
# -------------------------------
full_formula <- as.formula(paste("is_all_star ~", paste(raw_stats, collapse = " + ")))

# -------------------------------
# Fit GLM by position
# -------------------------------
glm_G <- glm(full_formula, data = CBBPlayers_labeled %>% filter(simple_pos == "G"), family = "binomial")
glm_F <- glm(full_formula, data = CBBPlayers_labeled %>% filter(simple_pos == "F"), family = "binomial")
glm_C <- glm(full_formula, data = CBBPlayers_labeled %>% filter(simple_pos == "C"), family = "binomial")

# -------------------------------
# Summaries
# -------------------------------
summary(glm_G)
summary(glm_F)
summary(glm_C)

# -------------------------------
# Bar plot of coefficients
# -------------------------------
plot_glm_coefs <- function(glm_model, pos_name) {
  coefs <- data.frame(
    stat = names(coef(glm_model))[-1],
    estimate = coef(glm_model)[-1]
  )
  
  ggplot(coefs, aes(x = reorder(stat, estimate), y = estimate, fill = estimate > 0)) +
    geom_col() +
    coord_flip() +
    labs(title = paste("GLM Coefficients for", pos_name), x = "Stat", y = "Coefficient") +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "salmon")) +
    theme_minimal()
}

plot_glm_coefs(glm_G, "Guards")
plot_glm_coefs(glm_F, "Forwards")
plot_glm_coefs(glm_C, "Centers")
