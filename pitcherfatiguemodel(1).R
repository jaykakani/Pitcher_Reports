rm(list = ls())

library(tidyverse)  
library(ggridges)
library(gridExtra)
library(knitr)
library(rmarkdown)
library(tinytex)
library(DT)
library(magrittr)
library(kableExtra)


import_data <- function() {
    file_path <- file.choose()
    d <- read.csv(file_path)
    return(d)
}

d1 <- import_data()

choose_pitcher <- function(d) {
  chosen_pitcher <- readline(prompt = "Enter pitcher name : ")
  d <- d %>% 
    filter(Pitcher == chosen_pitcher)
  return(d)
}
velo_progression <- function(d) {
  d <- d %>%
    mutate(PitchType = case_when(
      TaggedPitchType == "Fastball" ~ "FF",
      TaggedPitchType == "Slider" ~ "SL",
      TaggedPitchType == "Splitter" ~ "SP",
      TaggedPitchType == "ChangeUp" ~ "CH",
      TaggedPitchType == "Curveball" ~ "CB",
      TaggedPitchType == "Cutter" ~ "CU",
      TaggedPitchType == "Sinker" ~ "SI",
      TaggedPitchType == "Sweeper" ~ "SW",
      TRUE ~ "Other"
    )) %>%
    filter(PitchType != "Other") %>%
    arrange(Date, Inning, PitchNo) %>%
    mutate(
      PitchCount = row_number(),
      RelSpeed = round(RelSpeed, 1)
    )
  
  # make the plot
  ggplot(d, aes(x = PitchCount, y = RelSpeed, color = PitchType)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Pitch Velocity Progression by Pitch Types for", d$Pitcher[1]),
      x = "Pitch Count",
      y = "Velocity (MPH)",
      color = "PitchType"
    ) +
    theme_minimal() +
    coord_cartesian(
      xlim = c(1, max(d$PitchCount, na.rm = TRUE)), #set x bounds to go from the first pitch on
      ylim = c(min(d$RelSpeed, na.rm = TRUE) - 1,
               max(d$RelSpeed, na.rm = TRUE) + 1) # set y bounds to be highest and lowest velocities
    ) +
    scale_y_continuous(breaks = scales::breaks_width(1))
} 

fastball_vertical_break_progression <- function(d) {
  library(dplyr)
  library(ggplot2)
  
  # Define fastball types of interest
  fastball_types <- c("Fastball", "FourSeamFastball")
  
  # Filter for those fastball types
  fastballs <- d %>%
    filter(TaggedPitchType %in% fastball_types)
  
  # If no fastballs found, return empty plot
  if (nrow(fastballs) == 0) {
    message("No fastball pitches found for this pitcher.")
    return(ggplot() +
             labs(title = "No fastball data available") +
             theme_minimal())
  }
  
  # Get the most common fastball type safely
  most_common_fastball <- fastballs %>%
    filter(!is.na(TaggedPitchType)) %>%
    count(TaggedPitchType, sort = TRUE) %>%
    slice(1) %>%
    pull(TaggedPitchType)
  
  if (length(most_common_fastball) == 0) {
    message("Most common fastball could not be determined (likely all NA).")
    return(ggplot() +
             labs(title = "No valid fastball type found") +
             theme_minimal())
  }
  
  # Filter for only that pitch type
  d <- fastballs %>%
    filter(TaggedPitchType == most_common_fastball) %>%
    arrange(Date, Inning, PitchNo) %>%
    mutate(
      PitchCount = row_number(),
      InducedVertBreak = round(InducedVertBreak, 1)
    )
  
  # Make plot
  ggplot(d, aes(x = PitchCount, y = InducedVertBreak, color = TaggedPitchType)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Induced Vertical Break Progression on Fastballs for", d$Pitcher[1]),
      x = "Pitch Count",
      y = "Induced Vertical Break (inches)",
      color = "TaggedPitchType"
    ) +
    coord_cartesian(
      xlim = c(1, max(d$PitchCount, na.rm = TRUE)),
      ylim = c(min(d$InducedVertBreak, na.rm = TRUE) - 1,
               max(d$InducedVertBreak, na.rm = TRUE) + 1)
    ) +
    scale_y_continuous(breaks = scales::breaks_width(1)) +
    theme_minimal()
}


horizontal_break_progression <- function(d) {
  horz_break_pitches <- c("Slider", "ChangeUp", "Sweeper", "Splitter", "Sinker", "TwoSeamFastball") #define which pitches horizontal break are most important for
  d <- d%>% 
    mutate(HorzBreak = abs(HorzBreak)) %>%  #make all horizontal break positive
    filter(TaggedPitchType %in% horz_break_pitches) %>% #filter for only that type of pitch
    arrange(Date, Inning, PitchNo) %>%  # Make sure pitches are in correct chronological order
    mutate(
      PitchCount = row_number(),
      HorzBreak = round(HorzBreak, 1)
      )
    
  #make plot
  ggplot(d, aes(x = PitchCount, y = HorzBreak, color = TaggedPitchType)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs (
      title = paste("Horizontal Break Progression by Pitch Type for", d$Pitcher[1]),
      x = "Pitch Count",
      y = "Horizontal Break (inches)",
      color = "TaggedPitchType"
    ) +
    theme_minimal() +
    coord_cartesian(
      xlim = c(1, max(d$PitchCount, na.rm = TRUE)),
      ylim = c(min(d$HorzBreak, na.rm = TRUE) - 1,
               max(d$HorzBreak, na.rm = TRUE) + 1)
      ) +
    scale_y_continuous(breaks = scales::breaks_width(1))
}

release_height_progression <- function(d) {
  top_pitches <- d%>%
    count(TaggedPitchType, sort = TRUE) %>% 
    slice_head(n = 3)%>%
    pull(TaggedPitchType) 
  
d <- d%>%
  filter(TaggedPitchType %in% top_pitches) %>% # filtering to only have a pitcher's 3 most used pitches
  arrange(Date, Inning, PitchNo) %>% 
  mutate(
    PitchCount = row_number(),
    RelHeight = round(RelHeight, 1)
  )
  
  #make plot
  ggplot(d, aes(x = PitchCount, y = RelHeight, color = TaggedPitchType)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Release Point Progression by Pitch Type for", d$Pitcher[1]),
      x = "Pitch Count",
      y = "Release Height (Inches)",
      color = "TaggedPitchType"
    ) +
    theme_minimal() +
    coord_cartesian(
      xlim = c(1, max(d$PitchCount, na.rm = TRUE)),
      ylim = c(min(d$RelHeight, na.rm = TRUE) - 1,
               max(d$RelHeight, na.rm = TRUE) + 1)
    ) +
    scale_y_continuous(breaks = scales::breaks_width(0.1))
}

location_patterns <- function(d) {
  d <- d%>%
    filter(TaggedPitchType !="") %>%
    arrange(Date, Inning, PitchNo) %>% 
    mutate(
      PitchCount = row_number(),
      PlateLocSide = round(PlateLocSide, 2),
      PlateLocHeight = round(PlateLocHeight, 2),
      middle_middle_distance = round(sqrt((PlateLocHeight - 2.55)^2 + (PlateLocSide - 0)^2), 2) #calculate each pitch's distance from "middle middle'
    )
  
  #make plot
  ggplot(d, aes(x = PitchCount, y = middle_middle_distance, color = TaggedPitchType)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Missed Location Patterns by Pitch Type for", d$Pitcher[1]),
      x = "Pitch Count",
      y = "Distance from Middle Middle (Inches)", 
      color = "PitchType"
    ) +
    theme_minimal() +
    coord_cartesian(
      xlim = c(1, max(d$PitchCount, na.rm = TRUE)),
      ylim = c(min(d$middle_middle_distance, na.rm = TRUE),
              max(d$middle_middle_distance, na.rm = TRUE) + 1)
      ) +
    scale_y_continuous(breaks = scales::breaks_width(0.2)) 
}

fastball_velo <- function(d, return_summary = FALSE) {
  library(dplyr)
  library(knitr)
  library(kableExtra)
  #define different types of fastballs
  fastball_types <- c("Fastball", "FourSeamFastball", "Sinker", "TwoSeamFastball")
 #choose the most thrown type of fastball 
  most_common_fastball <- d %>%
    filter(TaggedPitchType %in% fastball_types) %>%
    count(TaggedPitchType, sort = TRUE) %>%
    slice(1) %>%
    pull(TaggedPitchType)
  
  d <- d %>%
    arrange(Date, Inning, PitchNo) %>%
    mutate(PitchCount = row_number())
  
  max_pitch <- max(d$PitchCount)
  #define pitch ranges as intervals of 10 following the first 20 pitches
  get_pitch_range <- function(n, max_pitch) {
    if (n <= 20) {
      return("1-20")
    } else {
      breaks <- seq(21, max_pitch + 10, by = 10)
      idx <- findInterval(n, breaks)
      start <- breaks[idx]
      end <- min(start + 9, max_pitch)
      return(paste0(start, "-", end))
    }
  }
  
  d <- d %>%
    mutate(PitchRange = sapply(PitchCount, get_pitch_range, max_pitch = max_pitch)) %>%
    filter(TaggedPitchType == most_common_fastball)
  
  # Summarise mean and counts by pitch range
  summary_table <- d %>%
    group_by(PitchRange) %>%
    summarise(
      mean_Rel_Speed = round(mean(RelSpeed, na.rm = TRUE), 1),
      sd_Rel_Speed = if ("1-20" %in% PitchRange) round(sd(RelSpeed[PitchRange == "1-20"], na.rm = TRUE), 1) else NA_real_,
      n_fastballs = n(),
      .groups = "drop"
    )
  
  # Sort by numeric start of pitch range
  summary_table <- summary_table %>%
    mutate(
      range_start = as.numeric(sub("-.*", "", PitchRange))
    ) %>%
    arrange(range_start) %>%
    select(-range_start)
  
  # Calculate SD only for the baseline "1-20" group separately
  baseline_sd <- d %>%
    filter(PitchRange == "1-20") %>%
    summarise(sd_Rel_Speed = round(sd(RelSpeed, na.rm = TRUE), 1)) %>%
    pull(sd_Rel_Speed)
  
  # Extract baseline mean for fatigue score calc
  baseline_mean <- summary_table %>%
    filter(PitchRange == "1-20") %>%
    pull(mean_Rel_Speed)
  
  # Calculate fatigue score
  summary_table <- summary_table %>%
    mutate(
      fatigue_score = if_else(
        PitchRange == "1-20" | is.na(baseline_sd) | baseline_sd == 0,
        NA_real_,
        (mean_Rel_Speed - baseline_mean) / baseline_sd
      )
    ) %>%
    rename(
      average_velocity = mean_Rel_Speed,
      velocity_deviation = sd_Rel_Speed,
      fastballs_thrown = n_fastballs,
    )
  
 return(summary_table)

}

vertical_break <- function(d, return_summary = FALSE) {
  fastball_types <- c("Fastball", "FourSeamFastball")
  
  # Get most common fastball type safely
  most_common_fastball <- d %>%
    filter(TaggedPitchType %in% fastball_types, !is.na(TaggedPitchType)) %>%
    count(TaggedPitchType, sort = TRUE) %>%
    slice(1) %>%
    pull(TaggedPitchType)
  
  # Return single-row NA summary if no fastballs found
  if (length(most_common_fastball) == 0 || is.na(most_common_fastball)) {
    message("No valid fastball type found. Returning NA row.")
    return(tibble(
      PitchRange = "1-20",
      average_vert_break = NA_real_,
      vert_break_deviation = NA_real_,
      fastballs_thrown = NA_integer_,
      fatigue_score = NA_real_
    ))
  }
  
  # Proceed as normal if fastballs exist
  d <- d %>%
    arrange(Date, Inning, PitchNo) %>%
    mutate(PitchCount = row_number())
  
  max_pitch <- max(d$PitchCount)
  
  get_pitch_range <- function(n, max_pitch) {
    if (n <= 20) {
      return("1-20")
    } else {
      breaks <- seq(21, max_pitch + 10, by = 10)
      idx <- findInterval(n, breaks)
      start <- breaks[idx]
      end <- min(start + 9, max_pitch)
      return(paste0(start, "-", end))
    }
  }
  
  d <- d %>%
    mutate(PitchRange = sapply(PitchCount, get_pitch_range, max_pitch = max_pitch)) %>%
    filter(TaggedPitchType == most_common_fastball)
  
  summary_table <- d %>%
    group_by(PitchRange) %>%
    summarise(
      mean_Induced_Vert_Break = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      sd_InducedVertBreak = if ("1-20" %in% PitchRange) round(sd(InducedVertBreak[PitchRange == "1-20"], na.rm = TRUE), 1) else NA_real_,
      n_fastballs = n(),
      .groups = "drop"
    ) %>%
    mutate(
      range_start = as.numeric(sub("-.*", "", PitchRange))
    ) %>%
    arrange(range_start) %>%
    select(-range_start)
  
  baseline <- summary_table %>% filter(PitchRange == "1-20")
  baseline_mean <- baseline$mean_Induced_Vert_Break
  baseline_sd <- baseline$sd_InducedVertBreak
  
  summary_table <- summary_table %>%
    mutate(
      fatigue_score = if_else(
        PitchRange == "1-20" | is.na(baseline_sd) | baseline_sd == 0,
        NA_real_,
        (mean_Induced_Vert_Break - baseline_mean) / baseline_sd
      )
    ) %>%
    rename(
      average_vert_break = mean_Induced_Vert_Break,
      vert_break_deviation = sd_InducedVertBreak,
      fastballs_thrown = n_fastballs
    )
  
  return(summary_table)
}

horizontal_break <- function(d, return_summary = FALSE) {
  #define relevant pitches for horizontal break
  relevant_pitches <- c("Slider", "ChangeUp", "Sweeper", "TwoSeamFastball", "Sinker", "Splitter")
  
  # Get most common relevant pitch, then filter to it
  top_pitch <- d %>%
    filter(TaggedPitchType %in% relevant_pitches) %>%
    count(TaggedPitchType, sort = TRUE) %>%
    slice(1) %>%
    pull(TaggedPitchType)
  
  d <- d %>%
    arrange(Date, Inning, PitchNo) %>%
    mutate(
      HorzBreak = abs(HorzBreak),
      PitchCount = row_number()
    )
  
  max_pitch <- max(d$PitchCount)
  
  get_pitch_range <- function(n, max_pitch) {
    if (n <= 20) {
      return("1-20")
    } else {
      breaks <- seq(21, max_pitch + 10, by = 10)
      idx <- findInterval(n, breaks)
      start <- breaks[idx]
      end <- min(start + 9, max_pitch)
      return(paste0(start, "-", end))
    }
  }
  
  d <- d %>%
    mutate(PitchRange = sapply(PitchCount, get_pitch_range, max_pitch = max_pitch)) %>%
    filter(TaggedPitchType == top_pitch)
  
  summary_table <- d%>%
    group_by(PitchRange) %>%
    summarise(
      mean_Horz_Break = round(mean(HorzBreak, na.rm = TRUE), 1),
      sd_Horz_Break = if ("1-20" %in% PitchRange) round(sd(HorzBreak[PitchRange == "1-20"], na.rm = TRUE), 1) else NA_real_,
      n_pitches = n(),
      .groups = "drop"
    )
  
  # Sort by numeric start of pitch range
  summary_table <- summary_table %>%
    mutate(
      range_start = as.numeric(sub("-.*", "", PitchRange))
    ) %>%
    arrange(range_start) %>%
    select(-range_start)
  
  baseline <- summary_table %>% filter(PitchRange == "1-20")
  
  if (nrow(baseline) == 0 || is.na(baseline$sd_Horz_Break) || baseline$sd_Horz_Break == 0) {
    summary_table <- summary_table %>%
      mutate(fatigue_score = NA_real_)
  } else {
    baseline_mean <- baseline$mean_Horz_Break
    baseline_sd <- baseline$sd_Horz_Break
    
    summary_table <- summary_table %>%
      mutate(
        fatigue_score = if_else(
          PitchRange == "1-20",
          NA_real_,
          (mean_Horz_Break - baseline_mean) / baseline_sd
        )
      )
  }
  
  summary_table <- summary_table %>%  
    rename(
      average_horz_break = mean_Horz_Break,
      horz_break_deviation = sd_Horz_Break,
      pitches_thrown = n_pitches,
      )
  
  return(summary_table)
  
}

release_height <- function(d, return_summary = FALSE) {
  
  d <- d %>%
    arrange(Date, Inning, PitchNo) %>%
    mutate(PitchCount = row_number())
  
  max_pitch <- max(d$PitchCount)
  
  get_pitch_range <- function(n, max_pitch) {
    if (n <= 20) {
      return("1-20")
    } else {
      breaks <- seq(21, max_pitch + 10, by = 10)
      idx <- findInterval(n, breaks)
      start <- breaks[idx]
      end <- min(start + 9, max_pitch)
      return(paste0(start, "-", end))
    }
  }
  
  d <- d %>%
    mutate(PitchRange = sapply(PitchCount, get_pitch_range, max_pitch = max_pitch))
  
  summary_table <- d %>%
    group_by(PitchRange) %>%
    summarise(
      mean_release_height = round(mean(RelHeight, na.rm = TRUE), 2),
      sd_release_height = if ("1-20" %in% PitchRange) round(sd(RelHeight[PitchRange == "1-20"], na.rm = TRUE), 1) else NA_real_,
      n_pitches = n(),
      .groups = "drop"
    ) 
  
  # Sort by numeric start of pitch range
  summary_table <- summary_table %>%
    mutate(
      range_start = as.numeric(sub("-.*", "", PitchRange))
    ) %>%
    arrange(range_start) %>%
    select(-range_start)
  #defines baseline as only first 20 pitches  
  baseline <- summary_table %>% filter(PitchRange == "1-20")
  baseline_mean <- baseline$mean_release_height
  baseline_sd <- baseline$sd_release_height
    
  summary_table <- summary_table%>% 
   mutate(
      fatigue_score = if_else(
        PitchRange == "1-20" | is.na(baseline_sd) | baseline_sd == 0,
        NA_real_,
        (mean_release_height - baseline_mean) / baseline_sd
      )
    ) %>%
    rename(
      average_rel_height = mean_release_height,
      rel_height_deviation = sd_release_height,
      pitches_thrown = n_pitches,
    )
  
  return(summary_table)
  
}

pitch_locations <- function(d, return_summary = FALSE) {
  
  d <- d %>%
    filter(TaggedPitchType != "") %>%
    arrange(Date, Inning, PitchNo) %>%
    mutate(
      PitchCount = row_number(),
      PlateLocSide = round(PlateLocSide, 2),
      PlateLocHeight = round(PlateLocHeight, 2),
      middle_middle_distance = round(sqrt((PlateLocHeight - 2.55)^2 + (PlateLocSide - 0)^2), 2) #calculates distance from trackman defined "middle-middle" of zone
    )
  
  max_pitch <- max(d$PitchCount)
  
  get_pitch_range <- function(n, max_pitch) {
    if (n <= 20) {
      return("1-20")
    } else {
      breaks <- seq(21, max_pitch + 10, by = 10)
      idx <- findInterval(n, breaks)
      start <- breaks[idx]
      end <- min(start + 9, max_pitch)
      return(paste0(start, "-", end))
    }
  }
  
  d <- d %>%
    mutate(PitchRange = sapply(PitchCount, get_pitch_range, max_pitch = max_pitch))
  
  summary_table <- d %>%
    group_by(PitchRange) %>%
    summarise(
      mean_middle_middle_distance = round(mean(middle_middle_distance, na.rm = TRUE), 2),
      sd_middle_middle_distance = if ("1-20" %in% PitchRange) round(sd(middle_middle_distance[PitchRange == "1-20"], na.rm = TRUE), 1) else NA_real_,
      n_pitches = n(),
      .groups = "drop"
    ) 
  
  # Sort by numeric start of pitch range
  summary_table <- summary_table %>%
    mutate(
      range_start = as.numeric(sub("-.*", "", PitchRange))
    ) %>%
    arrange(range_start) %>%
    select(-range_start)
  
  baseline <- summary_table %>% filter(PitchRange == "1-20")
  baseline_mean <- baseline$mean_middle_middle_distance
  baseline_sd <- baseline$sd_middle_middle_distance
    
  summary_table <- summary_table%>% 
    mutate(
      fatigue_score = if_else(
        PitchRange == "1-20" | is.na(baseline_sd) | baseline_sd == 0,
        NA_real_,
        (mean_middle_middle_distance - baseline_mean) / baseline_sd
      )
    ) %>%
    rename(
      avg_distance = mean_middle_middle_distance,
      distance_deviation = sd_middle_middle_distance,
      pitches_thrown = n_pitches,
    )
  
 return(summary_table)
  
}
#give velo metric a color based on fatigue scores
color_from_score_velo <- function(score) {
  ifelse(is.na(score), NA_character_,
         ifelse(score < -1.5, "red",
                ifelse(score < -1, "yellow", "green")))
}
#same calculation as velo scores
color_from_score_vert_break <- color_from_score_velo
color_from_score_horz_break <- color_from_score_velo
#give release height metric a color based on fatigue scores
color_from_score_release_height <- function(score) {
  ifelse(is.na(score), NA_character_,
         ifelse(score < -1.5, "red",
                ifelse(score < -1, "yellow",
                       ifelse(score <= 1, "green",
                              ifelse(score <= 1.5, "yellow", "red")))))
}
#give location metric a color based on fatigue scores
color_from_score_location <- function(score) {
  ifelse(is.na(score), NA_character_,
         ifelse(score < 0.5, "green",
                ifelse(score < 1, "yellow", "red")))
}
#use the colors of the final two pitch ranges to return one point value
points_from_two_colors <- function(color1, color2) {
  colors <- sort(c(color1, color2))
  
  if (all(colors == c("green", "green"))) {
    return(3)
  } else if (all(colors == c("yellow", "yellow"))) {
    return(2)
  } else if (all(colors == c("green", "yellow"))) {
    return(2)
  } else if ("red" %in% colors) {
    return(1)
  } else {
    return(NA)  # for any other unexpected combinations
  }
}

# Get fatigue score for final two pitch ranges from a summary table
get_score <- function(table, range_label) {
  table %>%
    filter(PitchRange %in% range_label) %>%
    pull(fatigue_score)
}

# Determine final two pitch range labels based on pitch count
last_two_ranges <- function(n) {
  if (n <= 20) {
    return("1-20")
  }
  
  # Calculate full range buckets starting from 21
  breaks <- seq(21, n, by = 10)
  ranges <- paste0(breaks, "-", pmin(breaks + 9, n))
  
  # Always include "1-20"
  all_ranges <- c("1-20", ranges)
  
  # Return the last two ranges
  tail(all_ranges, 2)
}

last_two_ranges_scores <- function(d) {
  d <- d %>%
    mutate(PitchCount = row_number())
  final_two_ranges <- last_two_ranges(max(d$PitchCount))
  
  # Extract scores for final two pitch ranges
  scores <- list(
    velo = get_score(fastball_velo(d, return_summary = TRUE), final_two_ranges),
    vert_break = get_score(vertical_break(d, return_summary = TRUE), final_two_ranges),
    horz_break = get_score(horizontal_break(d, return_summary = TRUE), final_two_ranges),
    rel_height = get_score(release_height(d, return_summary = TRUE), final_two_ranges),
    location = get_score(pitch_locations(d, return_summary = TRUE), final_two_ranges)
  )
  
  points <- c(
    points_from_two_colors(
      color_from_score_velo(scores$velo[1]),
      color_from_score_velo(scores$velo[2])
    ),
    points_from_two_colors(
      color_from_score_vert_break(scores$vert_break[1]),
      color_from_score_vert_break(scores$vert_break[2])
    ),
    points_from_two_colors(
      color_from_score_horz_break(scores$horz_break[1]),
      color_from_score_horz_break(scores$horz_break[2])
    ),
    points_from_two_colors(
      color_from_score_release_height(scores$rel_height[1]),
      color_from_score_release_height(scores$rel_height[2])
    ),
    points_from_two_colors(
      color_from_score_location(scores$location[1]),
      color_from_score_location(scores$location[2])
    )
  )
#attribute weighted percentages of total score to each metric depending on importance  
  weights <- c(0.3, 0.15, 0.05, 0.2, 0.3)
  names(weights) <- c("velo", "vert_break", "horz_break", "rel_height", "location")
  
  # Step 1: Build score list for checking recent values
  score_list <- list(
    velo = scores$velo,
    vert_break = scores$vert_break,
    horz_break = scores$horz_break,
    rel_height = scores$rel_height,
    location = scores$location
  )
  
  # Step 2: Identify metrics to exclude (if both of last 2 scores are not numeric)
  remove_metrics <- names(score_list)[sapply(score_list, function(s) {
    # Ensure s is numeric (coerce if needed)
    s_numeric <- suppressWarnings(as.numeric(s))
    
    # Pad with NA if fewer than 2 elements
    if (length(s_numeric) < 2) {
      s_numeric <- c(rep(NA, 2 - length(s_numeric)), s_numeric)
    }
    
    # Remove if both last two are NA
    all(is.na(tail(s_numeric, 2)))
  })]
  
  # Step 3: Adjust weights
  adjusted_weights <- weights
  adjusted_weights[remove_metrics] <- 0
  
  # Avoid divide-by-zero if all are removed
  if (sum(adjusted_weights) > 0) {
    adjusted_weights <- adjusted_weights / sum(adjusted_weights)
  }
  
  # Step 4: Ensure points are numeric too
  points <- suppressWarnings(as.numeric(points))
  
  # Step 5: Final score
  weighted_points <- points * adjusted_weights
  final_score <- sum(weighted_points, na.rm = TRUE) * 5
  
  # Step 6: Assign color based on final score
  final_score_color <- if (is.na(final_score)) {
    NA_character_
  } else if (final_score >= 12) {
    "green"
  } else if (final_score >= 8) {
    "yellow"
  } else {
    "red"
  }
  
  # Step 7: Build display table
  score_df <- data.frame(
    Metric = c("Velocity", "Vertical Break", "Horizontal Break", "Release Height", "Location"),
    Fatigue_Score = c(
      paste(round(scores$velo, 2), collapse = ", "),
      paste(round(scores$vert_break, 2), collapse = ", "),
      paste(round(scores$horz_break, 2), collapse = ", "),
      paste(round(scores$rel_height, 2), collapse = ", "),
      paste(round(scores$location, 2), collapse = ", ")
    ),
    Color = c(
      paste(color_from_score_velo(scores$velo), collapse = ", "),
      paste(color_from_score_vert_break(scores$vert_break), collapse = ", "),
      paste(color_from_score_horz_break(scores$horz_break), collapse = ", "),
      paste(color_from_score_release_height(scores$rel_height), collapse = ", "),
      paste(color_from_score_location(scores$location), collapse = ", ")
    ),
    Points = round(points, 2),
    Weight = adjusted_weights,
    Weighted_Points = round(weighted_points, 2),
    stringsAsFactors = FALSE
  )
  
  # Step 8: Add final score row
  final_row <- data.frame(
    Metric = "Final Score",
    Fatigue_Score = round(final_score, 2),
    Color = final_score_color,
    Points = NA_real_,
    Weight = NA_real_,
    Weighted_Points = NA_real_,
    stringsAsFactors = FALSE
  )
  
  score_df <- rbind(score_df, final_row)
  
  return(score_df)
}

#Step 1 - run lines 1-788. This will download the necessary libraries and functions.

#Step 2 - run the choose_pitcher() function below. This will prompt you to type the name of a pitcher in the console. Click enter after you have done so.
d <- choose_pitcher(d1)

fatigue_summary_table <- last_two_ranges_scores(d)

#You can run the following 4 lines to generate and view the tables and graphs that will go into the report.
#graph1 <- velo_progression(d)
#graph2 <- fastball_vertical_break_progression(d)
#graph3 <- horizontal_break_progression(d)
#graph4 <- release_height_progression(d)
#graph5 <- location_patterns(d)
#table1 <- fastball_velo(d)
#table2 <- vertical_break(d)
#table3 <- horizontal_break(d)
#table4 <- release_height(d)
#table5 <- pitch_locations(d)
#table6 <- fatigue_summary_table 

generate_fatigue_report <- function(d) {
  # Get pitcher name
  pitcher_name <- unique(d$Pitcher)
  
  #Get data source
  data_source <- readline(prompt = "Specify the date/opponent range used in this report: ")
  
  # Generate tables and graphs with suppressed warnings and messages
  velocity <- suppressMessages(suppressWarnings(velo_progression(d)))
  fastball_vertical_break <- suppressMessages(suppressWarnings(fastball_vertical_break_progression(d)))
  horizontal_break <- suppressMessages(suppressWarnings(horizontal_break_progression(d)))
  release_height <- suppressMessages(suppressWarnings(release_height_progression(d)))
  location_from_middle <- suppressMessages(suppressWarnings(location_patterns(d)))
  
  fastball_velocity_table <- suppressMessages(suppressWarnings(fastball_velo(d)))
  vertical_break_table <- suppressMessages(suppressWarnings(vertical_break(d)))
  horizontal_break_table <- suppressMessages(suppressWarnings(horizontal_break(d)))
  release_height_table <- suppressMessages(suppressWarnings(release_height(d)))
  pitch_location_table <- suppressMessages(suppressWarnings(pitch_locations(d)))
  fatigue_model_summary <- suppressMessages(suppressWarnings(last_two_ranges_scores(d)))
  
  # Compose the Rmd content
  rmd_content <- c(
    "---",
    paste("title: Fatigue Analysis Report -", pitcher_name),
    paste("subtitle: 'Date/Opponent:", data_source, "'"),
    "output:",
    "  pdf_document:",
    "    latex_engine: xelatex",
    "    extra_dependencies: [booktabs]",
    "header-includes:",
    "  - \\usepackage[table]{xcolor}",
    "---",
    "",
    "## Fatigue Progression Metrics",
    "",
    "```{r velocity-plot, echo=FALSE, warning=FALSE, message=FALSE, fig.height=4, fig.width=7}",
    "print(velocity)",
    "```",
    "",
    "```{r vertical-break-plot, echo=FALSE, warning=FALSE, message=FALSE, fig.height=4, fig.width=7}",
    "print(fastball_vertical_break)",
    "```",
    "",
    "```{r horizontal-break-plot, echo=FALSE, warning=FALSE, message=FALSE, fig.height=4, fig.width=7}",
    "print(horizontal_break)",
    "```",
    "",
    "```{r release-height-plot, echo=FALSE, warning=FALSE, message=FALSE, fig.height=4, fig.width=7}",
    "print(release_height)",
    "```",
    "",
    "```{r location-plot, echo=FALSE, warning=FALSE, message=FALSE, fig.height=4, fig.width=7}",
    "print(location_from_middle)",
    "```",
    "",
    "\\newpage",
    "",
    "## Fatigue Metric Tables",
    "",
    "```{r velo-table, echo=FALSE, warning=FALSE, message=FALSE, results='asis'}",
    "library(dplyr)",
    "library(kableExtra)",
    "",
    "fastball_velocity_table <- fastball_velocity_table %>%",
    "  mutate(RowColor = case_when(",
    "    fatigue_score < -2 ~ '#FFCCCC',          # red-ish",
    "    fatigue_score >= -2 & fatigue_score < -1 ~ '#FFFFCC',  # yellow-ish",
    "    fatigue_score >= -1 ~ '#00FF00',         # bright green-ish",
    "    TRUE ~ NA_character_",
    "  ))",
    "",
    "# Escape underscores in column names for LaTeX output",
    "colnames(fastball_velocity_table) <- gsub(\"_\", \"\\\\\\\\_\", colnames(fastball_velocity_table))",
    "",
    "kbl <- kable(",
    "  fastball_velocity_table %>% select(-RowColor),",
    "  format = 'latex',",
    "  caption = 'Fastball Velocity by Pitch Range',",
    "  booktabs = TRUE",
    ") %>%",
    "  kable_styling(latex_options = c('striped', 'hold_position')) %>%",
    "  row_spec(0, bold = TRUE)",
    "",
    "for (i in seq_len(nrow(fastball_velocity_table))) {",
    "  if (!is.na(fastball_velocity_table$RowColor[i])) {",
    "    kbl <- kbl %>% row_spec(i, background = fastball_velocity_table$RowColor[i])",
    "  }",
    "}",
    "",
    "kbl",
    "```",
    "",
    "```{r vertical-break-table, echo=FALSE, warning=FALSE, message=FALSE, results='asis'}",
    "library(dplyr)",
    "library(kableExtra)",
    "",
    "vertical_break_table <- vertical_break_table %>%",
    "  mutate(RowColor = case_when(",
    "    fatigue_score < -2 ~ '#FFCCCC',",
    "    fatigue_score >= -2 & fatigue_score < -1 ~ '#FFFFCC',",
    "    fatigue_score >= -1 ~ '#00FF00',",
    "    TRUE ~ NA_character_",
    "  ))",
    "",
    "# Escape underscores in colnames for LaTeX",
    "colnames(vertical_break_table) <- gsub(\"_\", \"\\\\\\\\_\", colnames(vertical_break_table))",
    "",
    "table_out <- kable(",
    "  vertical_break_table %>% select(-RowColor),",
    "  format = 'latex',",
    "  booktabs = TRUE,",
    "  escape = TRUE,",
    "  caption = 'Vertical Break on Fastballs by Pitch Range'",
    ") %>%",
    "  kable_styling(latex_options = c('striped', 'hold_position')) %>%",
    "  row_spec(0, bold = TRUE)",
    "",
    "for (i in seq_len(nrow(vertical_break_table))) {",
    "  if (!is.na(vertical_break_table$RowColor[i])) {",
    "    table_out <- table_out %>% row_spec(i, background = vertical_break_table$RowColor[i])",
    "  }",
    "}",
    "",
    "table_out",
    "```",
    "",
    "```{r horizontal-break-table, echo=FALSE, warning=FALSE, message=FALSE, results='asis'}",
    "library(dplyr)",
    "library(kableExtra)",
    "",
    "horizontal_break_table <- horizontal_break_table %>%",
    "  mutate(RowColor = case_when(",
    "    fatigue_score < -2 ~ '#FFCCCC',",
    "    fatigue_score >= -2 & fatigue_score < -1 ~ '#FFFFCC',",
    "    fatigue_score >= -1 ~ '#00FF00',",
    "    TRUE ~ NA_character_",
    "  ))",
    "",
    "# Escape underscores in colnames for LaTeX",
    "colnames(horizontal_break_table) <- gsub(\"_\", \"\\\\\\\\_\", colnames(horizontal_break_table))",
    "",
    "table_out <- kable(",
    "  horizontal_break_table %>% select(-RowColor),",
    "  format = 'latex',",
    "  booktabs = TRUE,",
    "  escape = TRUE,",
    "  caption = 'Horizontal Break by Pitch Range'",
    ") %>%",
    "  kable_styling(latex_options = c('striped', 'hold_position')) %>%",
    "  row_spec(0, bold = TRUE)",
    "",
    "for (i in seq_len(nrow(horizontal_break_table))) {",
    "  if (!is.na(horizontal_break_table$RowColor[i])) {",
    "    table_out <- table_out %>% row_spec(i, background = horizontal_break_table$RowColor[i])",
    "  }",
    "}",
    "",
    "table_out",
    "```",
    "",
    "```{r release-height-table, echo=FALSE, warning=FALSE, message=FALSE, results='asis'}",
    "library(dplyr)",
    "library(kableExtra)",
    "",
    "release_height_table <- release_height_table %>%",
    "  mutate(RowColor = case_when(",
    "    fatigue_score < -2 ~ '#FFCCCC',",
    "    fatigue_score >= -2 & fatigue_score < -1 ~ '#FFFFCC',",
    "    fatigue_score >= -1 & fatigue_score <= 1 ~ '#00FF00',",
    "    fatigue_score > 1 & fatigue_score <= 2 ~ '#FFFFCC',",
    "    fatigue_score > 2 ~ '#FFCCCC',",
    "    TRUE ~ NA_character_",
    "  ))",
    "",
    "# Escape underscores in colnames for LaTeX",
    "colnames(release_height_table) <- gsub(\"_\", \"\\\\\\\\_\", colnames(release_height_table))",
    "",
    "table_out <- kable(",
    "  release_height_table %>% select(-RowColor),",
    "  format = 'latex',",
    "  booktabs = TRUE,",
    "  escape = TRUE,",
    "  caption = 'Release Height by Pitch Range'",
    ") %>%",
    "  kable_styling(latex_options = c('striped', 'hold_position')) %>%",
    "  row_spec(0, bold = TRUE)",
    "",
    "for (i in seq_len(nrow(release_height_table))) {",
    "  if (!is.na(release_height_table$RowColor[i])) {",
    "    table_out <- table_out %>% row_spec(i, background = release_height_table$RowColor[i])",
    "  }",
    "}",
    "",
    "table_out",
    "```",
    "",
    "```{r location-table, echo=FALSE, warning=FALSE, message=FALSE, results='asis'}",
    "library(dplyr)",
    "library(kableExtra)",
    "",
    "# Add row colors based on fatigue_score",
    "pitch_location_table <- pitch_location_table %>%",
    "  mutate(RowColor = case_when(",
    "    fatigue_score >= 1 ~ '#FFCCCC',",
    "    fatigue_score >= 0.5 & fatigue_score < 1 ~ '#FFFFCC',",
    "    fatigue_score < 0.5 ~ '#00FF00',",
    "    TRUE ~ NA_character_",
    "  ))",
    "",
    "# Escape underscores in colnames for LaTeX",
    "colnames(pitch_location_table) <- gsub(\"_\", \"\\\\\\\\_\", colnames(pitch_location_table))",
    "",
    "# Create the kable LaTeX table without the RowColor column",
    "table_out <- kable(",
    "  pitch_location_table %>% select(-RowColor),",
    "  format = 'latex',",
    "  booktabs = TRUE,",
    "  escape = TRUE,",
    "  caption = 'Location Distance from Middle-Middle by Pitch Range'",
    ") %>%",
    "  kable_styling(latex_options = c('striped', 'hold_position')) %>%",
    "  row_spec(0, bold = TRUE)",
    "",
    "# Apply row background colors",
    "for (i in seq_len(nrow(pitch_location_table))) {",
    "  if (!is.na(pitch_location_table$RowColor[i])) {",
    "    table_out <- table_out %>% row_spec(i, background = pitch_location_table$RowColor[i])",
    "  }",
    "}",
    "",
    "table_out",
    "```",
    "",
    "```{r fatigue-summary-table, echo=FALSE, warning=FALSE, message=FALSE, results='asis'}",
    "# Escape underscores in colnames for LaTeX",
    "colnames(fatigue_model_summary) <- gsub(\"_\", \"\\\\\\\\_\", colnames(fatigue_model_summary))",
    "",
    "# Add the RowColor column based on Color values",
    "fatigue_model_summary <- fatigue_model_summary %>%",
    "  mutate(RowColor = case_when(",
    "    Color == \"green\"  ~ \"#00FF00\",",
    "    Color == \"yellow\" ~ \"#FFFFCC\",",
    "    Color == \"red\"    ~ \"#FFCCCC\",",
    "    TRUE               ~ NA_character_",
    "  ))",
    "",
    "# Create the kable table without the RowColor column",
    "table_out <- kable(",
    "  fatigue_model_summary %>% select(-RowColor),",
    "  format = \"latex\",",
    "  booktabs = TRUE,",
    "  escape = TRUE,",
    "  caption = \"Fatigue Model Summary\"",
    ") %>%",
    "  kable_styling(latex_options = c(\"striped\", \"hold_position\")) %>%",
    "  row_spec(0, bold = TRUE) %>%",
    "  row_spec(which(fatigue_model_summary$Metric == \"Final Score\"), bold = TRUE)",
    "",
    "# Apply row background colors according to RowColor",
    "for (i in seq_len(nrow(fatigue_model_summary))) {",
    "  if (!is.na(fatigue_model_summary$RowColor[i])) {",
    "    table_out <- table_out %>%",
    "      row_spec(i, background = fatigue_model_summary$RowColor[i])",
    "  }",
    "}",
    "",
    "print(table_out)",
    "```"
  )
  
  # Write Rmd content to a temporary file
  rmd_file <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, rmd_file)
  
  # PDF output file path
  pdf_output <- file.path(getwd(), paste0(pitcher_name, " Fatigue Report ", data_source, ".pdf"))

  # Render the Rmd with the environment
  rmarkdown::render(
    input = rmd_file,
    output_file = pdf_output,
    output_format = "pdf_document",
  )
  
  # Clean up the temporary Rmd file
  unlink(rmd_file)
  
  return(pdf_output)
}

# Generate the report and save it to the working directory
report_path <- generate_fatigue_report(d)