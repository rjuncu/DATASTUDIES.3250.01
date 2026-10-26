# ============================================================
# Non-Hodgkin Lymphoma: which states have the highest rates,
# compared across race/ethnicity groups (2018-2022, age-adjusted)
#
# Strategy: instead of chaining left_joins on identically-named
# columns (which creates confusing .x / .y suffixes), read each
# table, label it with its race group, standardize column names,
# then stack them into one "tidy" long table with bind_rows().
# A wide version is also produced for state-by-state comparisons.
# ============================================================

library(dplyr)
library(tidyr)

# When run from the command line (Rscript), make relative paths work.
# In RStudio this block is skipped (set Project > Restore Working Dir to Project Dir).
if (!interactive()) {
  this_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(this_file) > 0) setwd(dirname(normalizePath(sub("^--file=", "", this_file))))
}

data_dir  <- "./US_maps_data"
out_dir   <- "./analysis_output"
dir.create(out_dir, showWarnings = FALSE)

# ---- 1. Read every table, tagging it with its race group -------------------
files <- list(
  "All"         = "Incidence_US_by_State_All_Races_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv",
  "NH Black"    = "Incidence_US_by_State_Non-Hispanic_Black_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv",
  "Hispanic"    = "Incidence_US_by_State_Hispanic_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv",
  "NH AI/AN"    = "Incidence_US_by_State_Non-Hispanic_American_Indian_Alaskan_Native_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv",
  "NH White"    = "Incidence_US_by_State_Non-Hispanic_White_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv",
  "NH API"      = "Incidence_US_by_State_Non-Hispanic_Asian_Pacific_Islander_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv"
)

read_race <- function(path, race) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  # columns arrive as: Area, State FIPS, Age-Adjusted Rate..., Lower CI, Upper CI, Count
  names(df) <- c("state", "fips", "rate", "lower_ci", "upper_ci", "count")
  # Some cells read "No Data/Suppressed" when case counts are too small to
  # publish; coerce numeric columns explicitly so those become NA, and drop
  # the trailing blank line each CSV contains.
  df %>%
    mutate(across(c(rate, lower_ci, upper_ci, count),
                  ~ suppressWarnings(as.numeric(.x))),
           race = race, .before = 1) %>%
    filter(!is.na(state), nchar(state) > 0)
}

master_long <- bind_rows(lapply(names(files), function(race) {
  read_race(file.path(data_dir, files[[race]]), race)
}))

cat("Combined table dims:", dim(master_long), "\n")   # 53 regions x 6 groups = 318 rows
print(head(master_long, 6))

# Sanity check: same region list in every file?
cat("\nRegions per group:\n")
print(table(master_long$race, master_long$state)[, 1])   # should all be 1

cat("\n=== Suppressed (unpublishable) region-cells per group ===\n")
print(master_long %>%
  group_by(race) %>%
  summarise(observed = sum(!is.na(rate)), suppressed = sum(is.na(rate)), .groups = "drop"))

# ---- 2. Wide version: one row per state, rate columns per race -------------
master_wide <- master_long %>%
  pivot_wider(id_cols = c(state, fips),
              names_from = race,
              values_from = c(rate, count),
              names_sep = "_") %>%
  arrange(desc(rate_All))

write.csv(master_long, file.path(out_dir, "nhl_long.csv"),   row.names = FALSE)
write.csv(master_wide, file.path(out_dir, "nhl_wide.csv"),   row.names = FALSE)

# ---- 3. Highest-rate states within each race group -------------------------
race_only <- master_long %>% filter(race != "All")

top_by_race <- race_only %>%
  filter(!is.na(rate)) %>%
  group_by(race) %>%
  slice_max(order_by = rate, n = 10) %>%
  arrange(race, desc(rate)) %>%
  select(race, state, rate, lower_ci, upper_ci, count)

cat("\n=== Top 10 states by NHL rate, per race group ===\n")
print(as.data.frame(top_by_race))
write.csv(top_by_race, file.path(out_dir, "top10_by_race.csv"), row.names = FALSE)

# Mean state-level rate per race (quick national comparison)
cat("\n=== Average state rate by race ===\n")
print(race_only %>% group_by(race) %>%
        summarise(mean_state_rate = round(mean(rate, na.rm = TRUE), 1),
                  sd_state_rate   = round(sd(rate, na.rm = TRUE), 1), .groups = "drop") %>%
        arrange(desc(mean_state_rate)))

# ---- 4. Where do the races differ MOST within a state? ---------------------
# Drop tiny cell counts (rate is statistically unstable; see CI width).
MIN_COUNT <- 20
disparity <- race_only %>%
  filter(!is.na(count), count >= MIN_COUNT) %>%
  group_by(state) %>%
  summarise(groups_compared = n(),
            highest_race  = race[which.max(rate)], highest_rate = max(rate),
            lowest_race   = race[which.min(rate)], lowest_rate  = min(rate),
            rate_diff     = round(max(rate) - min(rate), 1),
            rate_ratio    = round(max(rate) / min(rate), 2)) %>%
  filter(groups_compared >= 4) %>%
  arrange(desc(rate_ratio))

cat("\n=== States with the largest gaps between race groups (top 15) ===\n")
print(head(as.data.frame(disparity), 15))
write.csv(disparity, file.path(out_dir, "race_disparity_by_state.csv"), row.names = FALSE)

# Small-cell caveat: which race groups have counts below the threshold, and where
cat("\n=== Small-count rows excluded from disparity analysis (unstable rates) ===\n")
print(race_only %>%
  filter(is.na(count) | count < MIN_COUNT) %>%
  group_by(race) %>%
  summarise(n_suppressed = sum(is.na(count)),
            n_low_count  = sum(!is.na(count) & count < MIN_COUNT),
            min_observed = if (all(is.na(count))) NA_real_ else min(count, na.rm = TRUE),
            widest_ci_observed = if (all(is.na(upper_ci))) NA_real_ else suppressWarnings(max(upper_ci - lower_ci, na.rm = TRUE)),
            .groups = "drop"))

# ---- 5. Plots ---------------------------------------------------------------
library(ggplot2)

# Plot A: top 10 states within each race group (faceted, sorted per facet).
# Note: a shared factor ordering across facets leaves blank rows; instead we
# rank states 1-10 within each race and print state names at the bar ends.
top_by_race <- top_by_race %>%
  group_by(race) %>%
  mutate(rank = row_number()) %>%   # rows already sorted desc(rate) per race
  ungroup()

p1 <- ggplot(top_by_race, aes(x = rank, y = rate, fill = race)) +
  geom_col() +
  geom_text(aes(label = state), hjust = 0, size = 3.3,
            position = position_nudge(y = 0.35)) +
  facet_wrap(~ race, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reverse(breaks = 1:10, labels = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(title = "Top 10 states by age-adjusted NHL incidence, 2018-2022",
       subtitle = "Rate per 100,000, ranked separately within each race/ethnicity group",
       x = NULL, y = "Age-adjusted rate per 100,000",
       caption = "Source: USA Facts / CDC state cancer profiles") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave(file.path(out_dir, "top10_by_race.png"), p1, width = 10, height = 9, dpi = 150)

# Plot B: race gaps within the most-disparate states (with CIs)
worst_states <- head(disparity$state, 12)
p2 <- race_only %>%
  filter(state %in% worst_states, count >= MIN_COUNT) %>%
  mutate(state = factor(state, levels = rev(worst_states))) %>%
  ggplot(aes(x = state, y = rate, color = race, group = race)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.8), width = 0.4) +
  coord_flip() +
  labs(title = "Largest race/ethnicity gaps in NHL rates within a state",
       subtitle = "Top 12 states by rate ratio, with 95% confidence intervals",
       y = "Age-adjusted rate per 100,000", x = NULL,
       caption = "Source: USA Facts / CDC state cancer profiles") +
  theme_minimal()
ggsave(file.path(out_dir, "race_gaps_worst_states.png"), p2, width = 9, height = 7, dpi = 150)

cat("\nSaved:", list.files(out_dir), sep = "\n")
