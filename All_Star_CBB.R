# All-Star College Basketball Player Project
# Years covered: 2009 - 2021

library(dplyr)
library(readxl)
library(ggplot2)
library(tibble)

# ============================================================
# 1. Load dataset
# ============================================================
CBBPlayers <- read_excel(
  "C:/Users/naeem/OneDrive/Documents/GitHub/SAP_CBB_AllStar/CBBPlayers_with_Positions.xlsx"
)

DraftedPlayers <- read_excel(
  "C:/Users/naeem/OneDrive/Documents/GitHub/SAP_CBB_AllStar/DraftedPlayers2009-2021.xlsx"
)

view(CBBPlayers)
view(DraftedPlayers)

# ============================================================
# 2. All-Star Lookup Table (Name, School, College Years)
# ============================================================
allstars_lookup_pid <- tribble(
  ~player_name,               ~pid,
  "Khris Middleton",         12631,
  "Draymond Green",           8827,
  "Jalen Brunson",           40885,
  "Dejounte Murray",         40260,
  "Pascal Siakam",           37123,
  "Jarrett Allen",           47193,
  "Tyrese Maxey",            71900,
  "Nikola Vucevic",           5408,
  "Kawhi Leonard",           13395,
  "Edrice Adebayo",          47569,
  "Donovan Mitchell",        41754,
  "Zach LaVine",             31114,
  "Tyler Herro",             66474,
  "Devin Booker",            52495,
  "Tyrese Haliburton",       65663,
  "Klay Thompson",            8716,
  "Shai Gilgeous-Alexander", 51731,
  "Domantas Sabonis",        33822,
  "Paul George",              5903,
  "Kemba Walker",             9075,
  "Gordon Hayward",           6696,
  "Lauri Markkanen",         44973,
  "Julius Randle",           29051,
  "Damian Lillard",           4509,
  "Trae Young",              51469,
  "Darius Garland",          65919,
  "DeMarcus Cousins",        13656,
  "Jaren Jackson Jr.",       51788,
  "Evan Mobley",             72870,
  "Bradley Beal",            22042,
  "Jaylen Brown",            38792,
  "Jayson Tatum",            45563,
  "Victor Oladipo",          18513,
  "D'Angelo Russell",        38468,
  "Brandon Ingram",          40152,
  "Kyrie Irving",            18579,
  "John Wall",               12637,
  "Anthony Edwards",         71877,
  "Zion Williamson",         65847,
  "Andrew Wiggins",          31626,
  "Cade Cunningham",         72372,
  "Anthony Davis",           22364,
  "Ben Simmons",             41356,
  "Karl-Anthony Towns",      35836,
  "Fred VanVleet",           26143,
  "Jalen Williams",          70269,
  "Isaiah Thomas",            8712,
  "Jimmy Butler",             9284,
  "Ja Morant",               50678
)

# ============================================================
# 3. Join lookup BEFORE selecting most recent season
# ============================================================
allstars_lookup_pid <- allstars_lookup_pid %>%
  rename(pid_lookup = pid, player_name_lookup = player_name)

CBBPlayers_labeled <- CBBPlayers %>%
  left_join(allstars_lookup_pid, by = c("pid" = "pid_lookup")) %>%
  mutate(is_all_star = ifelse(!is.na(player_name_lookup), 1, 0))


# ============================================================
# 4. Select most recent year per player
# ============================================================
CBBPlayers_Recent <- CBBPlayers_labeled %>%
  group_by(player_name) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(simple_pos = factor(simple_pos, levels = c("G", "F", "C")))

# ============================================================
# 5. Raw stats to use in GLM
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
# 6. Fit GLMs by position
# ============================================================
glm_G <- glm(full_formula, data = CBBPlayers_Recent %>% filter(simple_pos == "G"), family = "binomial")
glm_F <- glm(full_formula, data = CBBPlayers_Recent %>% filter(simple_pos == "F"), family = "binomial")
glm_C <- glm(full_formula, data = CBBPlayers_Recent %>% filter(simple_pos == "C"), family = "binomial")

summary(glm_G)
summary(glm_F)
summary(glm_C)

# ============================================================
# 7. Coefficient barplot function
# ============================================================
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


#allstars_found_with_pos <- CBBPlayers_labeled %>%
 # filter(is_all_star == 1) %>%
 # distinct(player_name, pid, simple_pos)  # include positions

# print(allstars_found_with_pos, n = Inf)
