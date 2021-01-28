# Data loading, merging and cleaning
# author: "Santiago Muñoz Moldes"
# date: "1/27/2021"

library(tidyverse)
library("tidylog", warn.conflicts = FALSE)

# Background

# 11 individuals completed 180 trials each of a neurofeedback task (three sessions of 60 trials). For each of the 180 total trials, the participant performed self-regulation (which we measured internally as a value from 1 to 12) to a certain target level (6 or 9 on the 12-point scale), they then predicted the self-regulation they had achieved (*performance prediction*, also a value from 1 to 12), and reported the confidence in that prediction (50% to 100%, in steps of 5%), before seeing their actual achieved self-regulation value. 

# The dataset can be built directly from the individual data files.
P2_to_P8_raw <- read_csv("https://www.dropbox.com/s/gbbyzu3qq8w4pen/master.csv?dl=1")
P2_to_P8 <- P2_to_P8_raw %>% select(-"rown")  # remove rownames column
P09 <- read_csv("https://www.dropbox.com/s/2ozqk5ni4h75g98/P09_allfmriruns.csv?dl=1") 
P10 <- read_csv("https://www.dropbox.com/s/yr96jstyqao30qj/P10_allfmriruns.csv?dl=1") 
P11 <- read_csv("https://www.dropbox.com/s/g50u0xe5efxecwg/P11_allfmriruns.csv?dl=1") 
# Merge together
raw <- bind_rows(P2_to_P8, P09, P10, P11)
#readr::write_csv(raw, 'raw.csv')

# Or it can also be loaded directly from an already merged file ("master.csv").
raw <- readr::read_csv("https://raw.githubusercontent.com/Santihago/neurofeedback/main/data/raw.csv")

# Data pre-processing

included <- c(2,4,5,6,7,8,9,11)

df <- raw %>% 
    dplyr::transmute(
        id = subject_id,
        session = Session,
        run = Run,
        trial_in_run = TrialNr+1,
        ctrl = Control,
        cond = Target,
        fb = FB,
        pred = FinalRating,
        conf = FinalConf
    ) %>%
    filter(id %in% included)


# Visualise the structure of the clean dataset using `plotrix` package. Note that 18 `control` trials have been removed per participant.
# 
# + **id** : Participant identifier number (162 trials per participant)
# + **session** : 3 sessions per participant (54 trials per session)
# + **run** : 6 runs per session (9 trials per run) (except P9)
# 
# library(plotrix)
# sizetree(df[,c(1:3)])

# Add continuous trial number for grouped days (not all participants completed all runs)
# 
# + **trial** (int) : A continous trial number for the 180 trials of each participant 
# + **target** (num) : The target level as numerical value (6 or 9), as used in the experiment
# + **fb_2_target** (num) : Neurofeedback centered around the target level
# + **pred_clean** (int) : predictions after removing control trials (where rating was forced)
# + **pred_2_target** (num) : predictions centered around the target level
# + **pred_2_fb** (num) : predictions centered around the neurofeedback value

df <- df %>% 
    group_by(id) %>% 
    mutate(trial = row_number())

df <- df %>%
    mutate(target = case_when(cond == 1 ~ 6,
                              cond == 2 ~ 9)) %>%
    mutate(fb_2_target = fb - target) %>%
    mutate(pred_clean = case_when(ctrl == 1 ~ NA_real_, 
                                  TRUE ~ pred)) %>%  # Replace control trials by NA (it evaluates in order)
    mutate(pred_2_target = pred_clean - target) %>%
    mutate(pred_2_fb = pred_clean - fb)

#Calculating the prior 
# **fb_prior** (num) : Variable representing the average of previous neurofeedback, calculated for each participant separately. The calculation can be changed so the prior reflects a running average, or the last 5 values, etc.

# Spread the neurofeedback into two different columns, depending on the target level, to calculate the prior for them separately
p <- tidyr::spread(df, "target", fb_2_target)

winl = 5  # window length
for (index in 1:nrow(p)) { 
    trial_n <- df$trial[index] # Set this trial number
    level_n <- df$cond[index] # Set this level
    
    if (trial_n==1){
        start <- index # Lock 1st trial for this participant
        df$prior[index] <- 0  # Set nothing for first 5 trials
    }  
    else if (trial_n>1 & trial_n<=winl){
        df$prior[index] <- 0  # Set nothing for first 5 trials
    }
    else if (trial_n>winl){
        # 1. Calculate mean of all trials for this participant
        #df_ratings$fb_prior[index] <- mean(df_ratings$FB2TARGET[start:(index-1)]) 
        # 2. Calculate mean of last X trials
        # df_ratings$fb_prior[index] <- mean(df_ratings$FB2TARGET[(index-winl):(index-1)])  
        # 3. Calculate mean of last X trials for each level separately
        if (level_n == 1){
            nr <- p$'6'[(start):(index-1)]
            nr <- nr[!is.na(nr)]
            prev_vals <- tail(nr, winl)  # Last X values, or less if not available
        } 
        else if (level_n == 2){
            nr <- p$'9'[(start):(index-1)]
            nr <- nr[!is.na(nr)]
            prev_vals <- tail(nr, winl) # Last X values, or less if not available
        }
        df$prior[index] <- mean(prev_vals, na.rm = TRUE)
    }
} 

# Delete unnecessary variables
rm(p, index, level_n, nr, prev_vals, start, trial_n, winl)

## More new variables for predictions
df <- df %>%
    mutate(round_prior = round(prior)) %>%
    mutate(pred_2_prior_abs = abs(pred_2_target - round_prior)) %>%
    mutate(pred_2_fb_abs = abs(pred_2_fb))

# For analysing them together
df_extended <- df %>%
    gather(key = "reference", value="pred_error", c("pred_2_prior_abs", "pred_2_fb_abs"))

# Export csv
readr::write_csv(df, 'df.csv')
readr::write_csv(df_extended, 'df_extended.csv')