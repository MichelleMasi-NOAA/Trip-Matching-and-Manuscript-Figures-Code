# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script: SEFHIER Trip Matching , Threshold Optimization, and Outputs for MS
# Repository: SEFHIERtripMatching
# Authors: Michelle Masi, Kyle Dettloff
# Dependencies: tidyverse, stringdist
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(stringdist)
library(readxl) #to read and save csv files


# 1. Data Acquisition ----------------------------------------------------------
Michelles_path <- "C:/Users/michelle.masi/Documents/SEFHIER/R code/Validation Survey data and analyses/Matching Logbooks to Validation Survey Intercepts/include all permit types/"

# !! Change to your path !!
Path <- Michelles_path

# create these folders in your directory first
Inputs <- "Inputs"
Outputs <- "Outputs"

# Read in data directly from GitHub
logbooks_list <- readRDS(paste0(Path, Outputs, "/Real_logbookdata.rds"))
surveys_list  <- readRDS(paste0(Path, Outputs, "/Real_surveydata.rds"))

# combine lists into dataframes and assign unique row numbers
log_df  <- bind_rows(logbooks_list) %>% 
  rename_with(~paste0("Log_", .x)) %>% 
  mutate(Log_Logbook_RowID = row_number())

surv_df <- bind_rows(surveys_list)  %>% 
  rename_with(~paste0("Surv_", .x)) %>% 
  mutate(Surv_Survey_RowID = row_number())

# 2. Date-Matched Candidate Pool & Similarity Metric Calculation ----------------
message("Joining datasets on date and computing candidate pool similarity scores creates the 'haystack'...")

## 2.1 Join on Date and compute similarity metrics across ALL date-matched pairs ----
matched_pool <- inner_join(
  log_df, surv_df, 
  by = c("Log_Full_Date" = "Surv_Full_Date"),
  relationship = "many-to-many"
) %>%
  mutate(
    # Exponential Similarity
    Anglers_Sim = exp(log(0.8) * abs(as.numeric(Log_Num_Anglers) - as.numeric(Surv_Num_Anglers))),
    Hours_Sim =   exp(log(0.8) * abs(as.numeric(Log_Hours_Fished) - as.numeric(Surv_Hours_Fished))),
    
    # Continuous Similarity: 1 / (1 + abs(diff))
    Time_Sim    = 1 / (1 + abs(Log_TIME - Surv_TIME) / 60),
    
    # Binary Exact-Match Features (1 = Match, 0 = Disagreement)
    Caught_Sim  = as.numeric(as.character(Log_Anything_Caught_Flag) == as.character(Surv_Anything_Caught_Flag)),
    Site_Sim    = as.numeric(as.character(Log_State) == as.character(Surv_State) & as.character(Log_County) == as.character(Surv_County))
  ) %>%
  
  # assign 0 similarity if data is missing (NA)
  mutate(across(ends_with("_Sim"), ~replace_na(.x, 0)))


#### Save Date Matched DF, as rds bc so many cols----
saveRDS(matched_pool, file = paste0(Path, Outputs, "/Datematched_pool.rds"))

#check how many survey rows were match to a logbook in the final DF, based on date alone matching between logbook and survey record (since the for-loop only inserts them into final DF IF the dates match)
NumberUniqueSurveys_MatchedAll <- length(unique(matched_pool$Surv_Survey_RowID))  #1528 matched surveys using all matches in final_df, but matched to multiple logbooks when there are no filters

#check how many survey rows were match to a logbook in the final DF, based on date alone matching between logbook and survey record (since the for-loop only inserts them into final DF IF the dates match)
nrow(matched_pool)  #350,660 matched surveys using all matches in final_df, but matched to multiple logbooks when there are no filters

#3. Truth Match DF (Deterministic, Ground-Truth Definition): ----
message("True Matches are resolved using simple, deterministic grouping on date + vessel identifier, and resolve ties using raw trip time. No algorithm or threshold is used here....")

## 3.1: Extract empirical true matches deterministically by Vessel Official Number and date----
# Ties (multiple trips for same vessel on same date) are resolved by selecting the closest trip time
true_matches <- matched_pool %>%
  filter(Log_Vessel_Official_Num == Surv_Vessel_Official_Num) %>%
  mutate(Time_Diff = abs(Log_TIME - Surv_TIME)) %>%
  group_by(Surv_Survey_RowID) %>%
  slice_min(Time_Diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(is_match = 1) %>%
  select(-Time_Diff)

message("Total true matches identified: ", nrow(true_matches)) #941

#### Save True Match DF ----
write.csv(true_matches, paste0(Path, Outputs, "/TrueMatch_DF.csv"))

## 3.2 Table 2: Reliability Matrix of Non-Unique Variables ----
# PURPOSE: This diagnostic step quantifies data entry and reporting error 
# rates among verified ground-truth pairs. 
# It calculates exact-match frequencies of sim scores in the true match DF to empirically justify classifying 
# variables into "High Reliability" vs. "Lower Reliability" for the grid search.

table2_data <- true_matches %>%
  
  # Step 1: Count exact, perfect matches (similarity score == 1.0) for each variable.
  # Using sum(Condition == 1) treats TRUE as 1 and FALSE as 0, counting how many 
  # ground-truth pairs exhibited zero human reporting error for that specific variable.
  summarise(
    `Site (County + State)` = sum(Site_Sim == 1, na.rm = TRUE),
    `Trip Start/End Time`   = sum(Time_Sim == 1, na.rm = TRUE),
    `Hours Fished`          = sum(Hours_Sim == 1, na.rm = TRUE),
    `Number of Anglers`     = sum(Anglers_Sim == 1, na.rm = TRUE),
    `Anything Caught`       = sum(Caught_Sim == 1, na.rm = TRUE)
  ) %>%
  
  # Step 2: Reshape the single row of variable counts into a long 2-column format.
  # This converts column names into row labels under "Non-Unique Linking Variable" 
  # to match the layout required for publication in Table 2.
  pivot_longer(
    cols = everything(),
    names_to = "Non-Unique Linking Variable",
    values_to = "Count with Perfect Similarity Score (=1)"
  ) %>%
  
  # Step 3: Calculate derived metrics for reporting error and accuracy proportions.
  mutate(
    # Dynamically capture total ground-truth baseline size 
    `Total Rows` = nrow(true_matches),
    
    # Calculate count of imperfect matches (< 1.0), representing human reporting error
    `Count with Imperfect Similarity Score (<1)` = `Total Rows` - `Count with Perfect Similarity Score (=1)`,
    
    # Compute the proportion of perfect matches (rounded to 2 decimal places)
    `Proportion Perfect Match` = round(`Count with Perfect Similarity Score (=1)` / `Total Rows`, 2)
  ) %>%
  
  # Step 4: Sort rows from highest to lowest proportion of perfect matches.
  arrange(desc(`Proportion Perfect Match`)) %>%
  
  # Step 5: Reorder and select final columns matching manuscript Table 2 format.
  # Drops the temporary 'Total Rows' column.
  select(
    `Non-Unique Linking Variable`,
    `Count with Perfect Similarity Score (=1)`,
    `Count with Imperfect Similarity Score (<1)`,
    `Proportion Perfect Match`
  )

# Display populated Table 2 in R console
print(table2_data)

# # A tibble: 5 × 4
# `Non-Unique Linking Variable` Count with Perfect Simila…¹ Count with Imperfect…² Proportion Perfect M…³
# <chr>                                               <int>                  <int>                  <dbl>
# 1 Site (County + State)                                 937                      4                   1   
# 2 Trip Start/End Time                                    24                    917                   0.03
# 3 Hours Fished                                          501                    440                   0.53
# 4 Number of Anglers                                     792                    149                   0.84
# 5 Anything Caught                                       913                     28                   0.97

### Save Table 2 data to CSV for manuscript preparation ----
write.csv(table2_data, paste0(Path, Outputs, "/table2_data.csv"))  


# 4. Evaluation DF: join ground-truth match indicator (is_match = 1) back onto full candidate pool----
eval_df <- matched_pool %>%
  # require site to match for optimization of other thresholds
  filter(Site_Sim == 1) %>% # optimization runs much faster if filtered first- result is same
  left_join(
    select(true_matches, Log_Logbook_RowID, Surv_Survey_RowID, is_match),
    by = c("Log_Logbook_RowID", "Surv_Survey_RowID")) %>%
  mutate(is_match = replace_na(is_match, 0))


# 5. DIAGNOSTIC FIGURES ----
## 5.1 FIGURE 1 - CANDIDATE POOL SIZES ("THE HAYSTACK") ----

### 5.1.1 Summarize candidate logbook pool sizes per survey after blocking on high-reliability variables ----
#for each survey record (needle), quantify # of potential matches (haystack) = show trying to find needle (true match) in a haystack 
#filter by threshold = 1 on reliable linking variables
needle_haystack_summary <- eval_df %>%  #use eval_df bc it has is_match variable added
  # #now use Anything Caught threshold to weed out some unlikely matches (where a threshold of 1 means they identical, both 1 or both 0, among the survey and logbook DFs)
  # filter(Caught_Sim == AnythingCaughtThreshold) %>%
  group_by(Surv_Survey_RowID) %>%
  summarize(Candidate_Count = n(), # Count of all logbook rows associated with this survey date
            # Check if the 'is_match' flag (True Match) is present in this group
            Is_True_Match_Present = if_else(any(is_match == 1), "True Match Found", "No Match in Pool")
  )

### 5.1.2 Plot histogram illustrating spatiotemporal overlap and true match presence/absence ----
p_haystack <- ggplot(needle_haystack_summary, aes(x = Candidate_Count, fill = Is_True_Match_Present)) +
  # Histogram with clean borders for high-DPI output
  geom_histogram(binwidth = 5, color = "white", linewidth = 0.3) +  
  
  # Color palette & legend labels
  scale_fill_manual(
    values = c("True Match Found" = "black", "No Match in Pool" = "gray75"),
    name = NULL 
  ) +
  labs(
    x = "Candidate Pool Size (Logbooks per Survey)",
    y = "Number of Surveys"
  ) +
  
  # Publication Theme Adjustments
  theme_bw(base_size = 14) + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 12)),
    axis.text = element_text(color = "black", size = 14),
    legend.position = "inside",
    legend.position.inside = c(0.70, 0.80),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    legend.key.size = unit(1.2, "lines"),
    legend.text = element_text(size = 14),
    legend.title = element_text(face = "bold", size = 14),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )


# Print Plot
print(p_haystack)

### 5.1.3 Save Fig 1 as high-resolution image for journal submission ----
ggsave(
  filename = file.path(Path, Outputs, "Fig 1 - needle_haystack_Figure.tiff"),
  plot = p_haystack,
  width = 10, 
  height = 8, 
  dpi = 600, 
  compression = "lzw"
)

## 5.2. FIGURE 2 - SIGNAL-TO-NOISE OVERLAP DIAGNOSTIC PLOT ----
# Compare normalized proportional similarity distributions (y = Proportion) 
# side-by-side (position = "dodge") between verified True Matches ("Signal") and 
# candidate pool Potential Mismatches ("Noise") across lower-reliability variables.
# "Signal" = True Matches: Represents the true physical trip pairings.
# "Noise" = Potential Mismatches: Represents the background candidate pool of plausible coincidental matches in data-matched DF

#Reshape the data to a "long" format for faceting
plot_data <- eval_df %>%  #use eval_df bc it has is_match variable added
  # #now use Anything Caught threshold to weed out some unlikely matches (where a threshold of 1 means they identical, both 1 or both 0, among the survey and logbook DFs)
  # filter(Caught_Sim == AnythingCaughtThreshold) %>%
  # Select the similarity scores and the match flag
  select(is_match, Anglers = Anglers_Sim, Hours = Hours_Sim, `Trip Time` = Time_Sim) %>%
  # Pivot the three variables into one column for faceting
  pivot_longer(
    cols = c(Anglers, Hours, `Trip Time`),
    names_to = "Variable",
    values_to = "SimilarityScore") %>%  #pivoting 3 variables, so the math looks like this: 30,195 (original rows) × 3 (variables) = 90,585 (new rows)
  # Rename the logical/factor for better legend labels
  mutate(
    Variable = case_when(
      Variable == "Anglers" ~ "Number of Anglers",
      Variable == "Hours"   ~ "Hours Fished",
      .default = Variable
    ),
    #Pre-calculate exact proportions within each (Variable, Category) group
    Variable = factor(Variable, levels = c("Number of Anglers", "Hours Fished", "Trip Time")),
    Category = if_else(is_match == 1, "True Match", "Potential Mismatch")
  )


### 5.2.1: Plot using geom_col (position = "dodge") with square-root scaling ----
fig2_plot <- ggplot(plot_data, aes(x = SimilarityScore, fill = Category)) +
  # Use an inline group sum so each Category within each Panel sums to 1.0 (100%)
  geom_histogram(
    aes(y = after_stat(count / ave(count, PANEL, group, FUN = sum))),
    position = position_dodge(width = 0.05),
    binwidth = 0.05,
    boundary = 0,
    closed = "left",
    alpha = 1.0,
    color = "white",
    linewidth = 0.3) +  
  
  #use facet wrap to wrap variable plots on same figure
  facet_wrap(~Variable, scales = "fixed", ncol = 1, axes = "all_x") +
  
  # Clean 0 to 1 scale
  scale_y_continuous(
    limits = c(0, 1.0),
    breaks = seq(0, 1, by = 0.20),
    expand = expansion(mult = c(0, 0.02))) +
  
  #create colors and labels for plot
  scale_fill_manual(values = c("True Match" = "black", "Potential Mismatch" = "gray65")) +
  labs(x = "Similarity Score", y = "Proportion", fill = "") +
  
  # Publication Theme
  theme_classic(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 14, face = "plain"),
    axis.text.y = element_text(color = "black", size = 14),
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 12)),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(face = "bold", size = 14),
    legend.key.size = unit(1.2, "lines")
  )


# Print Plot
print(fig2_plot)

### 5.2.2 Save Fig 2 as high resolution image for publication----
# save with landscape aspect ratio to minimize vertical whitespace
ggsave(
  filename = paste0(Path, Outputs, "/Fig 2- Signal-to-Noise.tiff"), 
  plot = fig2_plot,
  width = 7, 
  height = 10, 
  dpi = 600, 
  compression = "lzw")

## 5.3 Grid Search Optimization --------------------------------------------------
### 5.3.1. Define the thresholds to test ---- 
surv_ids   <- eval_df$Surv_Survey_RowID
a_sim      <- eval_df$Anglers_Sim
t_sim      <- eval_df$Time_Sim
h_sim      <- eval_df$Hours_Sim
is_m       <- eval_df$is_match 
tm         <- nrow(true_matches)
#caught_sim <- eval_df$Caught_Sim  (this is removed bc it adds bias, and F1 is higher without it)

# --- Function to apply thresholds and calculate performance metric (e.g., F1 Score) ---
# ang, tim, hrs represent dynamic thresholds being tested for anglers, time, and hours fished
calc_f1 <- function(ang, tim, hrs) {
  
  # 1. Filter candidate pool by thresholds
  idx <- which(a_sim >= ang & t_sim >= tim & h_sim >= hrs)
  
  # 2. Handle edge cases to prevent division-by-zero errors. 
  # If there are no true positives, precision and recall drop to 0, making F1 0.
  if (length(idx) == 0) {
    return(c(f1_score = 0, n_matches = 0))
  }
  
  # 3. Subset simulation vectors to retain only candidates meeting all criteria
  surv_sub <- surv_ids[idx]
  t_sub    <- t_sim[idx]
  a_sub    <- a_sim[idx]
  h_sub    <- h_sim[idx]
  m_sub    <- is_m[idx]
  
  # 4. De-duplicate candidates so each survivor is counted only once,
  # prioritizing highest time, angle, and hours thresholds
  ord      <- order(surv_sub, -t_sub, -a_sub, -h_sub, na.last = TRUE)
  surv_ord <- surv_sub[ord]
  keep     <- !duplicated(surv_ord)
  m_final  <- m_sub[ord][keep]
  
  # 5. Track total count of unique matches for plotting
  total_matches <- length(m_final)
  
  # 6. Calculate confusion matrix components (True Positives, False Positives, False Negatives)
  # Note: 'tm' represents total true ground-truth targets
  tp <- sum(m_final == 1)
  fp <- sum(m_final == 0)
  fn <- tm - tp
  
  # 7. Check if any true positives exist to safely handle cases with zero valid matches
  if (tp == 0) {
    return(c(f1_score = 0, n_matches = total_matches))
  }
  
  # 8. Compute evaluation metrics (Precision, Recall, and harmonic mean F1 Score)
  precision <- tp / (tp + fp)
  recall    <- tp / (tp + fn)
  f1        <- 2 * (precision * recall) / (precision + recall)
  
  # 9. Return final metric array containing F1 score and match count
  return(c(f1_score = f1, n_matches = total_matches))
}

# create parameter grid to optimize over
threshold_grid <- expand.grid(
  t_anglers = seq(0, 1, by = 0.2),
  t_time    = seq(0, 1, by = 0.01),
  t_hours   = seq(0, 1, by = 0.01)
)

# calculate f1 scores
message("Optimizing thresholds...")

# Evaluate F1 scores across a grid of threshold combinations
results <- threshold_grid %>%
  bind_cols(
    # Iterate row-by-row through the combinations of three threshold variables:
    # ..1 = t_anglers, ..2 = t_time, ..3 = t_hours
    # Execute calc_f1() for each set of inputs and combine the returned data frames by row
    pmap_dfr(list(threshold_grid$t_anglers, threshold_grid$t_time, threshold_grid$t_hours),
             ~calc_f1(..1, ..2, ..3))
  )

# Identify and extract the single best parameter configuration
opt <- results %>% 
  # Rank results by performance (F1 score first) and use parameters as tie-breakers
  arrange(desc(f1_score), desc(t_anglers), desc(t_hours), desc(t_time)) %>% 
  # Select the top row representing the optimal configuration
  slice(1)

print("Optimal Threshold Combination:")

### 5.3.2 Optimal threshold results for MS text ----
print(opt)

# t_anglers t_time    t_hours  f1_score   n_matches
# 1         1   0.45    0.26   0.5550617      1084


## 5.4 FIGURE 3 - Plot optimal threshold combination ----------------------------------------------------
## use ggplot to create heat-map to visualize threshold combinations to show optimal combination

# Create faceted heat map data
fig3_data <- results %>% 
  filter(round(t_anglers / 0.2, 5) %% 1 == 0) %>%
  rename("Angler Threshold" = t_anglers)

#create plot with plot data
fig3_plot <- ggplot(fig3_data, aes(x = t_time, y = t_hours, fill = f1_score)) +
  geom_tile() +
  
  # Colorblind-friendly perceptually uniform color scale
  scale_fill_gradient2(
    low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c",
    # center contrast around 40th %ile score
    midpoint = quantile(plot_data$f1_score, probs = 0.4, na.rm = TRUE),
    name = "F1 Score"
  ) +
  
  # Arrange panels into 2 columns so tiles are wide and scannable
  facet_wrap(~`Angler Threshold`, labeller = label_both, ncol = 1) +
  
  # Highlight the globally optimal combination with black asterisk
  geom_point(data = opt %>% rename("Angler Threshold" = t_anglers),
             aes(x = t_time, y = t_hours), 
             color = "black", shape = 8, size = 3.5, stroke = 1.5) +
  
  # Scientific styling
  theme_bw(base_size = 13) + 
  theme(
    panel.grid       = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text       = element_text(face = "bold"),
    legend.position  = "top",
    legend.key.width = unit(2, "cm")
  ) +
  labs(
    x = "Time Similarity Threshold",
    y = "Hours Fished Similarity Threshold",
    caption = paste0("Global Optimal F1: ", round(opt$f1_score, 3))
  )

# Display plot in console (Warning will now be gone!)
print(fig3_plot)

### 5.4.1 Save Fig 3 as high-resolution TIFF for publication ----
ggsave(
  paste0(Path, Outputs, "/Fig 3 - Optimized Thresholds.tiff"), 
  plot = fig3_plot,
  width = 10, 
  height = 9, 
  dpi = 600, 
  compression = "lzw"
)

## 5.5 Find "core" F1 score cutoff within optimal angler threshold ----
# Determine an optimal F1 score threshold using a maximum distance (elbow/knee) method
optimal_f1_cutoff <- results %>%
  # Filter the grid search results to keep only rows matching the optimal 't_anglers' value found previously
  filter(t_anglers == opt$t_anglers) %>%
  # Sort the subset in ascending order by F1 score
  arrange(f1_score) %>%
  # Calculate normalized metrics to find the point of maximum deviation from a linear baseline
  mutate(
    # Assign an ascending rank index (1, 2, ..., N) based on the sorted F1 scores
    rank_f1 = row_number(),
    # Calculate a composite score: [Min-Max normalized F1 score] minus [Normalized rank position (0 to 1)].
    # This measures how far each point sits above the diagonal baseline connecting the min and max scores.
    score   = (f1_score - min(f1_score)) / (max(f1_score) - min(f1_score)) - 
      (rank_f1 - 1) / (n() - 1)
  ) %>%
  # Select the single row that maximizes this deviation score (breaks ties automatically if any exist)
  slice_max(score, n = 1, with_ties = FALSE) %>%
  # Extract the resulting F1 score value as a vector/scalar variable
  pull(f1_score)

## 5.6 Filter top F1 results restricted to optimal angler threshold ----
top_f1_data <- fig3_data %>%
  # Keep only data for the optimal angler threshold where F1 score meets or exceeds the calculated cutoff
  filter(`Angler Threshold` == opt$t_anglers, f1_score >= optimal_f1_cutoff) %>%
  # Calculate match ratio relative to total actual matches ('tm') and compute percentage bias
  mutate(match_ratio = n_matches / tm,
         percent_bias = (1 / match_ratio - 1) * 100)

## 5.7 FIGURE 4 - Plot the number of matches relative to actual within core F1 range ----
Fig4 <- ggplot() +
  # Base layer: draw all tiles in light grey for the cut-out combinations
  geom_tile(
    data = fig3_data %>% filter(`Angler Threshold` == opt$t_anglers),
    aes(x = t_time, y = t_hours),
    fill = "grey90"
  ) +
  # Top layer: draw tiles for the retained top F1 combinations colored by match ratio
  geom_tile(
    data = top_f1_data,
    aes(x = t_time, y = t_hours, fill = match_ratio)
  ) +
  # Diverging color scale centered at 1 (equal ratio of estimated to true matches)
  scale_fill_gradient2(
    low = "#d7191c",
    mid = "white",
    high = "#2c7bb6",
    midpoint = 1,
    name = "True Match Ratio"
  ) +
  # Facet panel by the Angler Threshold setting
  facet_wrap(~`Angler Threshold`, labeller = label_both, ncol = 1) +
  # Highlight the globally optimal parameter point with a star symbol (shape 8)
  geom_point(
    data = opt %>% rename("Angler Threshold" = t_anglers),
    aes(x = t_time, y = t_hours), 
    color = "black", shape = 8, size = 3.5, stroke = 1.5
  ) +
  # Apply clean black-and-white theme styling
  theme_bw(base_size = 14) + 
  theme(
    panel.grid       = element_blank(),       # Remove background grid lines
    strip.background = element_rect(fill = "white"), # Set facet title background to white
    strip.text       = element_text(face = "bold"),  # Bold facet header text
    legend.position  = "top",                 # Move color legend to top
    legend.key.width = unit(2, "cm")          # Widen color legend bar
  ) +
  # Define axis labels and dynamically formatted caption summary
  labs(
    x = "Time Similarity Threshold",
    y = "Hours Fished Similarity Threshold",
    caption = paste0("Matches at Optimal F1 Threshold: ", opt$n_matches, " (Ratio: ", round(opt$n_matches / tm, 2), ")")
  )

# Print Plot
print(Fig4)

### 5.7.1 Save Fig 4 as high-resolution TIFF for publication ----
#Export image with landscape aspect ratio to minimize vertical whitespace
ggsave(
  filename = paste0(Path, Outputs, "/Fig 4- Threshold Match Sensitivity.tiff"), 
  plot = Fig4,
  width = 11, 
  height = 6.5, 
  dpi = 600, 
  compression = "lzw")

## 5.8 FIGURE 5 - Distribution of bias introduced ----
Fig5 <- ggplot(top_f1_data, aes(x = percent_bias)) +
  # Plot relative frequency histogram of percent bias for top performing thresholds
  geom_histogram(
    aes(
      y = after_stat(count / sum(count)), # Convert raw counts to proportions
      fill = after_stat(1 / (x / 100 + 1)) # Re-evaluate match ratio from bias to sync fill colors with plot p4
    ), 
    binwidth = 5, 
    boundary = 0,
    color = "gray30", 
    alpha = 0.9
  ) +
  # Secondary reference lines at +/- 5% bias tolerance thresholds
  geom_vline(xintercept = c(-5, 5), linetype = "dashed", color = "gray40", linewidth = 0.5) +
  # Primary reference line at 0% bias (unbiased target)
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  # Apply identical color scale matching p4's aesthetic gradient
  scale_fill_gradient2(
    low = "#d7191c",
    mid = "white",
    high = "#2c7bb6",
    midpoint = 1,
    name = "True Match Ratio"
  ) +
  # Format x-axis with +/- signs and percentage symbols
  scale_x_continuous(
    n.breaks = 10,
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")
  ) +
  # Format y-axis values as percentages
  scale_y_continuous(
    n.breaks = 8,
    labels = scales::percent_format(accuracy = 1)
  ) +
  # Apply clean black-and-white theme styling
  theme_bw(base_size = 14) +
  theme(
    panel.grid      = element_blank(),       # Remove background grid lines
    plot.title      = element_text(face = "bold"),  # Bold title text formatting
    legend.position = "top",                 # Place legend at the top
    legend.key.width = unit(2, "cm")          # Widen color legend bar
  ) +
  # Define axis titles and dynamic caption reporting summary metrics
  labs(
    x = "Bias in Estimates of Total",
    y = "Proportion of Threshold Combinations",
    caption = paste0(
      "Combinations at F1 >= ", round(optimal_f1_cutoff, 3), "\n",
      round(mean(abs(top_f1_data$percent_bias) <= 5, na.rm = TRUE) * 100, 1),
      "% of combinations within ±5% bias"
    )
  )

# Print Plot
print(Fig5)

### 5.7.1 Save Fig 5 as high-resolution TIFF for publication ----
#Export image with landscape aspect ratio to minimize vertical whitespace
ggsave(
  filename = paste0(Path, Outputs, "/Fig 5- Estimation Bias in Near-Optimal F1 Zone.tiff"), 
  plot = Fig5,
  width = 11, 
  height = 6.5, 
  dpi = 600, 
  compression = "lzw")


#6. Linkage Eval DF, using the DF optimal_combination for the non-reliable linking variable thresholds ----

## 6.1 Assign optimal thresholds derived from Section 4 grid search ----
#Threshold values of 1 used for reliable linking variables (site and anything caught)
Site_threshold          <- 1
#AnythingCaughtThreshold <- 1
Timethreshold          <- opt$t_time
HoursFishedThreshold   <- opt$t_hours
NumAnglersThreshold    <- opt$t_anglers

## 6.2 Apply threshold filtering approach to simulate record linkage without unique IDs ----
LinkageEval_DF <- eval_df %>% 
  #filter out only those with the same Site (County and State)
  filter(Site_Sim == Site_threshold) %>%
  # #now use Anything Caught threshold to weed out some unlikely matches (where a threshold of 1 means they identical, both 1 or both 0, among the survey and logbook DFs)
  # filter(Caught_Sim == AnythingCaughtThreshold) %>%
  #now filter by TimeScore
  filter(Time_Sim >= Timethreshold) %>%
  #now filter by hours fished threshold
  filter(Hours_Sim >= HoursFishedThreshold) %>%
  #now only keep rows where anglers are equal (per accsp method)
  filter(Anglers_Sim >= NumAnglersThreshold) %>%
  #RESOLVE DUPLICATES: Since 1 survey should only have 1 logbook
  #This occurs bc 1 survey is matched to 2 logbooks from the same vessel on the same day
  # apply same tiebreaking logic as used in the optimization step
  arrange(Surv_Survey_RowID, desc(Time_Sim), desc(Anglers_Sim), desc(Hours_Sim)) %>%
  slice_head(n = 1, by = Surv_Survey_RowID)
  

#check how many matches in the final DF
nrow(LinkageEval_DF) #1084

#########Save final_Logbook_RowsToKeep_MatchedCounty DF
write.csv(LinkageEval_DF, paste0(Path, Outputs, "/LinkageEval_DF.csv"))

## 6.3 Quantify empirical false-positive match rate and false-negative match rate for manuscript ----

#quantify
TotalMatches_true_baseline  <- nrow(true_matches)
TotalMatches_LinkageEval    <- nrow(LinkageEval_DF)
Number_truepositives        <- sum(LinkageEval_DF$is_match == 1)
Number_falsepositives       <- sum(LinkageEval_DF$is_match == 0)
FalsePositive_rate          <- (Number_falsepositives / nrow(LinkageEval_DF))*100 # False Match Rate (1 - Precision)
true_eliminated             <- TotalMatches_true_baseline - Number_truepositives
FalseNegative_rate          <- ((TotalMatches_true_baseline - Number_truepositives) / TotalMatches_true_baseline)*100 # False Non-Match Rate (1 - Recall)
 
cat("Total Baseline True Matches:", TotalMatches_true_baseline, "\n") #941 
cat("Total Matches in Linkage Eval DF:", TotalMatches_LinkageEval, "\n") #1084 
cat("True Matches Retained:", Number_truepositives, "\n")        #562
cat("False Positives in Linkage Eval DF:", Number_falsepositives, "\n")  #522
cat("False Positive Rate (of Linkage Eval DF):", sprintf("%.2f%%", FalsePositive_rate), "\n\n") #48.15%
cat("True Matches Eliminated (False Negatives):", true_eliminated, "\n")  #379
cat("Eliminated True Match Rate (False Negative Rate):", sprintf("%.2f%%", FalseNegative_rate), "\n\n") #40.28%


