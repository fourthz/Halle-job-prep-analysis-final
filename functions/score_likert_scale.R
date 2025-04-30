# -------------------------------------
# score_likert_scale()
# Purpose: scale/score likert scales
# Author: Halle E. Prine
# Date: April 29th 2025
# -------------------------------------

score_likert_scale <- function(df, scale_max, reverse_items) {
  # Ensure all item columns are numeric
  df <- df %>%
    mutate(across(where(is.numeric), as.numeric))
  
  # Reverse-score specified items by name
  df <- df %>%
    mutate(across(
      .cols = all_of(reverse_items),
      .fns = ~ (scale_max + 1) - .,
      .names = "rev_{.col}"
    ))
  
  # Replace original items with reversed
  for (col in reverse_items) {
    df[[col]] <- df[[paste0("rev_", col)]]
    df[[paste0("rev_", col)]] <- NULL
  }
  
  # Identify item columns again (exclude ID if present)
  item_cols <- df %>% select(starts_with("item_")) %>% names()
  
  # Add total score
  df$total_score <- rowSums(df[item_cols], na.rm = TRUE)
  
  return(df)
}


# survey <- read_csv(here("Data", "Job Prep Survey Results.csv"),
#                   skip = 4,
#                   col_types = "cnnnnnnnnnnnnnn",
#                   col_names = c("ID", paste0("item_", 1:14)))

# Assume survey_items is just the 14 items (not ID)
# survey_scored <- score_likert_scale(survey, scale_max = 6, reverse_items = c("item_4", "item_9", "item_12", "item_13"))

