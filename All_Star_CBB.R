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
# 3. Join lookup BEFORE selecting most recent season
# ============================================================
CBBPlayers_labeled <- CBBPlayers %>%
  left_join(allstars_lookup, by = c("player_name", "team")) %>%
  mutate(
    is_all_star = ifelse(!is.na(start_year) & year >= start_year & year <= end_year, 1, 0)
  ) %>%
  select(-start_year, -end_year)

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


