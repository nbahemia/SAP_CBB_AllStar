# All-Star College Basketball Player Project
# Years covered, 2009 - 2021.

library(readxl)
library(dplyr)
library(stringr)

CBBPlayers <- read_excel("C:\\Users\\naeem\\OneDrive\\Documents\\GitHub\\SAP_CBB_AllStar\\CBBPlayers_with_Positions.xlsx")
DraftedPlayers <- read_excel("C:\\Users\\naeem\\OneDrive\\Documents\\GitHub\\SAP_CBB_AllStar\\DraftedPlayers2009-2021.xlsx")

head(CBBPlayers)
head(DraftedPlayers)

# Filtering for the most recent year of the CBB players career(the year they got drafted)

CBBPlayers_Recent <- CBBPlayers %>%
  group_by(player_name) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup()

# Mutating the dataset

CBBPlayers_Recent <- CBBPlayers_Recent %>%
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
  )

full_dataset_player_percentiles <- CBBPlayers_Recent %>%
  select(player_name, ends_with("_percentile"))

# Making a list of players

players_of_interest <- c(
  "Anthony Davis",
  "Bam Adebayo",
  "Bradley Beal",
  "Damian Lillard",
  "DeMarcus Cousins",
  "Devin Booker",
  "Donovan Mitchell",
  "Draymond Green",
  "Gordon Hayward",
  "Isaiah Thomas",
  "Jayson Tatum",
  "Jaylen Brown",
  "Jimmy Butler",
  "John Wall",
  "Julius Randle",
  "Karl-Anthony Towns",
  "Kawhi Leonard",
  "Kemba Walker",
  "Khris Middleton",
  "Klay Thompson",
  "Kyrie Irving",
  "Nikola Vučević",
  "Paul George",
  "Trae Young",
  "Victor Oladipo",
  "Zach LaVine",
  "Zion Williamson",
  "Ben Simmons",
  "Brandon Ingram",
  "Pascal Siakam",
  "D’Angelo Russell",
  "Domantas Sabonis",
  "Ja Morant",
  "Dejounte Murray",
  "Jarrett Allen",
  "Andrew Wiggins",
  "Shai Gilgeous-Alexander",
  "Jalen Williams",
  "Evan Mobley",
  "Jaren Jackson Jr.",
  "Jalen Brunson",
  "Cade Cunningham",
  "Tyler Herro",
  "Darius Garland",
  "Anthony Edwards",
  "Tyrese Maxey",
  "Tyrese Haliburton",
  "Paolo Banchero",
  "Fred VanVleet",
  "Lauri Markkanen"
)


# Testing view

interest_percentiles <- interest_percentiles %>%
  mutate(simple_pos = factor(simple_pos, levels = c("G", "F", "C")))

interest_percentiles %>%
  arrange(simple_pos, player_name) %>%
  group_by(simple_pos) %>%
  do(print(., n = Inf))


interest_percentiles %>%
  arrange(simple_pos, player_name) %>%
  group_by(simple_pos) %>%
  group_walk(~ {
    pos <- as.character(.y$simple_pos)
    
    cat("\n\n========================\n")
    cat("Position:", pos, "\n")
    cat("========================\n")
    
    print(.x, n = Inf, width = Inf)
  })
