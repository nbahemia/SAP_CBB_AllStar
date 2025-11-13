# ============================================================
# Libraries
# ============================================================
library(dplyr)
library(readxl)
library(ggplot2)
library(knitr)

# ============================================================
# 1. Load datasets
# ============================================================
CBBPlayers <- read_excel("/Users/varunrao/Downloads/CBBPlayers_with_Positions (1).xlsx")
DraftedPlayers <- read_excel("/Users/varunrao/Downloads/DraftedPlayers2009-2021.xlsx")

# ============================================================
# 2. All-Star lookup table (accurate name + school matching)
# ============================================================
allstars_lookup <- tribble(
  ~player_name,              ~team,              ~start_year, ~end_year,
  "Andrew Wiggins",          "Kansas",            2013, 2014,
  "Anthony Davis",           "Kentucky",          2011, 2012,
  "Anthony Edwards",         "Georgia",           2019, 2020,
  "Edrice Adebayo",          "Kentucky",          2016, 2017,
  "Ben Simmons",             "LSU",               2015, 2016,
  "Bradley Beal",            "Florida",           2011, 2012,
  "Brandon Ingram",          "Duke",              2015, 2016,
  "Cade Cunningham",         "Oklahoma St.",      2020, 2021,
  "D'Angelo Russell",        "Ohio St.",          2014, 2015,
  "Damian Lillard",          "Weber St.",         2008, 2012,
  "Darius Garland",          "Vanderbilt",        2018, 2019,
  "Dejounte Murray",         "Washington",        2015, 2016,
  "DeMarcus Cousins",        "Kentucky",          2009, 2010,
  "Devin Booker",            "Kentucky",          2014, 2015,
  "Domantas Sabonis",        "Gonzaga",           2014, 2016,
  "Donovan Mitchell",        "Louisville",        2015, 2017,
  "Draymond Green",          "Michigan St.",      2008, 2012,
  "Evan Mobley",             "USC",               2020, 2021,
  "Fred VanVleet",           "Wichita St.",       2012, 2016,
  "Gordon Hayward",          "Butler",            2008, 2010,
  "Isaiah Thomas",           "Washington",        2008, 2011,
  "Ja Morant",               "Murray St.",        2017, 2019,
  "Jalen Brunson",           "Villanova",         2015, 2018,
  "Jalen Williams",          "Santa Clara",       2019, 2022,
  "Jaren Jackson Jr.",       "Michigan St.",      2017, 2018,
  "Jarrett Allen",           "Texas",             2016, 2017,
  "Jaylen Brown",            "California",        2015, 2016,
  "Jayson Tatum",            "Duke",              2016, 2017,
  "Jimmy Butler",            "Marquette",         2008, 2011,
  "John Wall",               "Kentucky",          2009, 2010,
  "Julius Randle",           "Kentucky",          2013, 2014,
  "Karl-Anthony Towns",      "Kentucky",          2014, 2015,
  "Kawhi Leonard",           "San Diego St.",     2009, 2011,
  "Kemba Walker",            "Connecticut",       2008, 2011,
  "Khris Middleton",         "Texas A&M",         2009, 2012,
  "Klay Thompson",           "Washington St.",    2008, 2011,
  "Kyrie Irving",            "Duke",              2010, 2011,
  "Lauri Markkanen",         "Arizona",           2016, 2017,
  "Nikola Vucevic",          "USC",               2008, 2011,
  "Pascal Siakam",           "New Mexico St.",    2014, 2016,
  "Paul George",             "Fresno St.",        2008, 2010,
  "Shai Gilgeous-Alexander", "Kentucky",          2017, 2018,
  "Trae Young",              "Oklahoma",          2017, 2018,
  "Tyler Herro",             "Kentucky",          2018, 2019,
  "Tyrese Haliburton",       "Iowa St.",          2018, 2020,
  "Tyrese Maxey",            "Kentucky",          2019, 2020,
  "Victor Oladipo",          "Indiana",           2010, 2013,
  "Zach LaVine",             "UCLA",              2013, 2014,
  "Zion Williamson",         "Duke",              2018, 2019
)

# ============================================================
# 3. Merge and label All-Stars correctly
# ============================================================
CBBPlayers_labeled <- CBBPlayers %>%
  left_join(allstars_lookup, by = c("player_name", "team")) %>%
  mutate(is_all_star = ifelse(!is.na(start_year) & year >= start_year & year <= end_year, 1, 0)) %>%
  select(-start_year, -end_year)

# ============================================================
# 4. Select most recent college season per player
# ============================================================
CBBPlayers_Recent <- CBBPlayers_labeled %>%
  group_by(player_name) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(simple_pos = toupper(trimws(simple_pos))) %>%
  filter(simple_pos %in% c("G", "F", "C"))

# ============================================================
# 5. Split into position datasets
# ============================================================
guards <- CBBPlayers_Recent %>% filter(simple_pos == "G")
forwards <- CBBPlayers_Recent %>% filter(simple_pos == "F")
centers <- CBBPlayers_Recent %>% filter(simple_pos == "C")

# ============================================================
# 6. T-tests: All-Stars vs. non–All-Stars, filtering by minutes
# ============================================================
min_minutes_threshold <- 20   # threshold for % of team minutes played

stats_to_test <- c(
  "Ortg", "usg", "eFG", "TS_per", "ORB_per", "DRB_per",
  "AST_per", "TO_per", "blk_per", "stl_per", "porpag",
  "adjoe", "ast/tov", "drtg", "adrtg", "dporpag"
)

run_ttests <- function(df, pos_name, min_minutes) {
  results <- data.frame()
  
  for (stat in stats_to_test) {
    if (stat %in% names(df)) {
      sub <- df %>%
        filter(!is.na(.data[[stat]])) %>%
        # Only include All-Stars OR players who played >= 20% of team minutes
        filter(is_all_star == 1 | Min_per >= min_minutes)
      
      # Skip if there aren’t both groups
      if (length(unique(sub$is_all_star)) < 2) next
      
      t_res <- t.test(sub[[stat]] ~ sub$is_all_star, var.equal = FALSE)
      
      results <- rbind(
        results,
        data.frame(
          position = pos_name,
          stat = stat,
          all_star_mean = mean(sub[[stat]][sub$is_all_star == 1], na.rm = TRUE),
          others_mean = mean(sub[[stat]][sub$is_all_star == 0], na.rm = TRUE),
          t_value = round(t_res$statistic, 2),
          p_value = round(t_res$p.value, 4),
          significant = ifelse(t_res$p.value < 0.05, "✅ Yes", "❌ No")
        )
      )
    }
  }
  return(results)
}

# ============================================================
# 7. Run t-tests by position
# ============================================================
t_G <- run_ttests(guards, "G", min_minutes_threshold)
t_F <- run_ttests(forwards, "F", min_minutes_threshold)
t_C <- run_ttests(centers, "C", min_minutes_threshold)

t_test_table <- bind_rows(t_G, t_F, t_C) %>%
  arrange(position, p_value)

# ============================================================
# 8. Display results
# ============================================================
kable(
  t_test_table,
  caption = paste("Two-sample t-tests (Min_per ≥", min_minutes_threshold, "%): All-Stars vs. Others per position")
)
t_test_heatmap <- t_test_table %>%
  mutate(
    pct_diff = ((all_star_mean - others_mean) / others_mean) * 100,
    sig_marker = ifelse(p_value < 0.05, "*", "")
  )

ggplot(t_test_heatmap, 
       aes(x = position, y = reorder(stat, pct_diff), fill = pct_diff)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sig_marker), size = 8, vjust = 0.7, color = "black") +
  scale_fill_gradientn(
    colors = c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", 
               "#FDDBC7", "#F4A582", "#D6604D", "#B2182B"),
    values = scales::rescale(c(min(t_test_heatmap$pct_diff, na.rm = TRUE), 
                               -50, -10, 0, 10, 50, 100, 
                               max(t_test_heatmap$pct_diff, na.rm = TRUE))),
    name = "% Difference\n(All-Stars\nvs Others)"
  ) +
  labs(
    title = "All-Star Statistical Profile by Position",
    subtitle = paste("Percent difference from high-minutes players (Min% ≥", 
                     min_minutes_threshold, "%). * = p < 0.05"),
    x = "Position",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 11),
    panel.grid = element_blank()
  )