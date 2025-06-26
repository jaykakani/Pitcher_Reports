rm(list = ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(knitr)
library(rmarkdown)
library(tinytex)

#tinytex::install_tinytex()
#tinytex::tlmgr_install(c('bookmark', 'geometry', 'hyperref', 'unicode-math'))

import_data <- function() {
  repeat{
    file_path <- file.choose()
    d <- read.csv(file_path)
    return(d)
    break
  }
}  
choose_pitcher <- function(d) {
  chosen_pitcher <- readline(prompt = "Enter pitcher name : ")
  d <- d %>% 
    filter(Pitcher == chosen_pitcher)
  return(d)
}
summary_data <- function(d) {
  innings_pitched <- d %>%
    group_by(Date) %>%
    summarize(
      outs_on_play = sum(OutsOnPlay, na.rm = TRUE),
      strikeouts = sum(KorBB == "Strikeout", na.rm = TRUE),
      total_outs = outs_on_play + strikeouts,
      innings = floor(total_outs / 3) + (total_outs %% 3) * 0.1
    )
  
  # Calculate WHIP
  whip <- d %>%
    mutate(
      # Identify baserunners: hits, walks, and hit by pitch
      is_baserunner = ifelse(
        PlayResult %in% c("Single", "Double", "Triple", "HomeRun") |
          KorBB == "Walk" | PitchCall == "HitByPitch",
        1, 0
      )
    ) %>%
    summarise(
      total_baserunners = sum(is_baserunner, na.rm = TRUE),
      outs_on_play = sum(OutsOnPlay, na.rm = TRUE),
      strikeouts = sum(KorBB == "Strikeout", na.rm = TRUE),
      total_outs = outs_on_play + strikeouts,
      innings_pitched = total_outs / 3,
      WHIP = total_baserunners / innings_pitched
    )
  
  
  # Calculate total plate appearances faced
  plate_appearances <- d %>%
    filter(
      (!is.na(PlayResult) & PlayResult != "Undefined") | 
        (!is.na(KorBB) & KorBB %in% c("Walk", "Strikeout")) |
        PitchCall == "HitByPitch"
    ) %>%
    group_by(Inning) %>%
    summarise(pa_in_inning = n()) %>%
    summarise(total_pa = sum(pa_in_inning))
  
  fastball_types <- c("FF", "FT", "SI", "FC", "FA")
  fastball_zone_pct <- d %>%
    filter(TaggedPitchType %in% fastball_types) %>%
    mutate(
      in_zone = ifelse(
        PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
          PlateLocHeight >= 1.17 & PlateLocHeight <= 3.92,
        1, 0
      )
    ) %>%
    summarise(
      total_fastballs = n(),
      fastballs_in_zone = sum(in_zone),
      zone_pct = fastballs_in_zone / total_fastballs * 100
    )
  
  print(fastball_zone_pct)
  
  # Calculate total plate appearances and strikeouts
  k_percentage <- d %>%
    filter(
      (!is.na(PlayResult) & PlayResult != "Undefined") | 
        (!is.na(KorBB) & KorBB %in% c("Walk", "Strikeout")) |
        PitchCall == "HitByPitch"
    ) %>%
    summarise(
      total_pa = n(),
      total_strikeouts = sum(KorBB == "Strikeout", na.rm = TRUE),
      k_percentage = ifelse(total_pa == 0, NA, (total_strikeouts / total_pa) * 100)
    )
  
  # Calculate total plate appearances and walks
  bb_percentage <- d %>%
    filter(
      (!is.na(PlayResult) & PlayResult != "Undefined") | 
        (!is.na(KorBB) & KorBB %in% c("Walk", "Strikeout")) |
        PitchCall == "HitByPitch"
    ) %>%
    summarise(
      total_pa = n(),
      total_walks = sum(KorBB == "Walk", na.rm = TRUE),
      bb_percentage = ifelse(total_pa == 0, NA, (total_walks / total_pa) * 100)
    )
  
  # Calculate total plate appearances, strikeouts, and walks
  k_bb_difference <- d %>%
    filter(
      (!is.na(PlayResult) & PlayResult != "Undefined") | 
        (!is.na(KorBB) & KorBB %in% c("Walk", "Strikeout")) |
        PitchCall == "HitByPitch"
    ) %>%
    summarise(
      total_pa = n(),  # Total valid plate appearances
      total_strikeouts = sum(KorBB == "Strikeout", na.rm = TRUE),
      total_walks = sum(KorBB == "Walk", na.rm = TRUE),
      k_percentage = ifelse(total_pa == 0, NA, (total_strikeouts / total_pa) * 100),
      bb_percentage = ifelse(total_pa == 0, NA, (total_walks / total_pa) * 100),
      k_bb_difference = ifelse(total_pa == 0, NA, k_percentage - bb_percentage)
    )
  
  
  
  # Define metric titles and values
  titles <- c("IP", "WHIP", "PA", "K %", "BB %", "K - BB%")
  values <- c(sum(innings_pitched$innings), whip$WHIP, plate_appearances$total_pa, 
              k_percentage$k_percentage, bb_percentage$bb_percentage, k_bb_difference$k_bb_difference)
  
  
  table <- data.frame(t(values)) %>%   mutate(across(where(is.numeric), ~ round(., 1)))
  colnames(table) <- titles
  
  return(table)  
  
  
}
pitch_data <- function(d) {
  known_order <- c("Fastball", "Cutter", "Curveball", "Changeup", "Slider", "Splitter")
  total_pitches = nrow(d)
  d <- d %>% 
    filter(TaggedPitchType != "") %>%
    mutate(
      in_zone = ifelse(
        PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &      # Check x bounds
          PlateLocHeight >= 1.17 & PlateLocHeight <= 3.92,  # Check z bounds
        1, 0                                                     # Assign 1 if in zone, 0 otherwise
      ),
      outside_zone = ifelse(
        PlateLocSide < -0.83 | PlateLocSide > 0.83 |         # Check x bounds
          PlateLocHeight < 1.17 | PlateLocHeight > 3.92,       # Check z bounds
        1, 0                                                      # Mark as 1 if outside zone
      ),
      swing = ifelse(
        PitchCall %in% c("Foul", "StrikeSwinging", "InPlay"),      # Check if it's a swing
        1, 0
      ),
      TaggedPitchType = factor(TaggedPitchType, levels = c(known_order, sort(setdiff(unique(TaggedPitchType), known_order)))),
      # Tilt conversions - all inside this same mutate
      Tilt_Hour = as.numeric(sub(":.*", "", Tilt)),
      Tilt_Minute = as.numeric(sub(".*:", "", Tilt)),
      TiltDegrees = (ifelse(Tilt_Hour == 12, 0, Tilt_Hour) * 30) + (Tilt_Minute * 0.5)
    ) %>%
    group_by(TaggedPitchType) %>%
    summarise(
      Count = n(),                                    # Count number of pitches for each type
      `%` = (Count / total_pitches) * 100,  # Calculate the pitch percentage
      Velo = mean(RelSpeed, na.rm = TRUE),
      maxVelo = max(RelSpeed, na.rm = TRUE),
      IVB = mean(InducedVertBreak, na.rm = TRUE),
      HB = mean(HorzBreak, na.rm = TRUE),
      Spin = mean(SpinRate, na.rm = TRUE),
      VAA = mean(VertApprAngle, na.rm = TRUE),
      VRel = mean(VertRelAngle, na.rm = TRUE),
      HRel = mean(HorzRelAngle, na.rm = TRUE),
      Ext = mean(Extension, na.rm = TRUE),
      Axis = mean(SpinAxis, na.rm = TRUE),
      Tilt = mean(TiltDegrees, na.rm = TRUE)
    ) %>% 
    mutate(across(where(is.numeric), ~ round(., 1))) %>% 
    rename(Pitch = TaggedPitchType)
  return(d)
  
}
pitch_data2 <- function(d) {
  known_order <- c("Fastball", "Cutter", "Curveball", "Changeup", "Slider", "Splitter")
  total_pitches = nrow(d)
  d <- d %>% 
    filter(TaggedPitchType != "") %>%
    mutate(
      in_zone = ifelse(
        PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &      # Check x bounds
          PlateLocHeight >= 1.17 & PlateLocHeight <= 3.92,  # Check z bounds
        1, 0                                                     # Assign 1 if in zone, 0 otherwise
      ),
      outside_zone = ifelse(
        PlateLocSide < -0.83 | PlateLocSide > 0.83 |         # Check x bounds
          PlateLocHeight < 1.17 | PlateLocHeight > 3.92,       # Check z bounds
        1, 0                                                      # Mark as 1 if outside zone
      ),
      swing = ifelse(
        PitchCall %in% c("Foul", "StrikeSwinging", "InPlay"),      # Check if it's a swing
        1, 0
      ),
      TaggedPitchType = factor(TaggedPitchType, levels = c(known_order, sort(setdiff(unique(TaggedPitchType), known_order))))
    ) %>%
    group_by(TaggedPitchType) %>%
    summarise(
      `Zone%` = (sum(in_zone, na.rm = TRUE))/n() * 100,
      `Chase%` = (sum(outside_zone * swing, na.rm = TRUE))/(sum(outside_zone, na.rm = TRUE)) * 100,
      `Whiff%` = (sum(PitchCall == "StrikeSwinging", na.rm = TRUE))/(sum(PitchCall %in% c("Foul", "StrikeSwinging", "InPlay"), na.rm = TRUE)) * 100
    ) %>% 
    mutate(across(where(is.numeric), ~ round(., 1))) %>% 
    rename(Pitch = TaggedPitchType)
  #overall_metrics <- d %>%
   # mutate(
     # in_zone = ifelse(
       # PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
       #   PlateLocHeight >= 1.17 & PlateLocHeight <= 3.92,
      #  1, 0
     # ),
    #  outside_zone = ifelse(
    #    PlateLocSide < -0.83 | PlateLocSide > 0.83 |
     #     PlateLocHeight < 1.17 | PlateLocHeight > 3.92,
     #   1, 0
    #  ),
    #  swing = ifelse(
    #    PitchCall %in% c("Foul", "StrikeSwinging", "InPlay"),
    #    1, 0
    #  )
   # ) %>%
   # summarise(
   #   `Zone%` = sum(in_zone, na.rm = TRUE) / n() * 100,
   #   `Chase%` = sum(outside_zone * swing, na.rm = TRUE) / sum(outside_zone, na.rm = TRUE) * 100,
    #  `Whiff%` = sum(PitchCall == "StrikeSwinging", na.rm = TRUE) / 
    #    sum(PitchCall %in% c("Foul", "StrikeSwinging", "InPlay"), na.rm = TRUE) * 100
   # ) %>%
   # mutate(across(everything(), ~ round(., 1)))
    
  return(d)
  
}

velo_distribution <- function(d) {
  d <- d %>%
    mutate(PitchType = case_when(                             # Rename pitch types
      TaggedPitchType == "Fastball" ~ "FF",
      TaggedPitchType == "Slider" ~ "SL",
      TaggedPitchType == "Splitter" ~ "SP",
      TaggedPitchType == "Changeup" ~ "CH",
      TaggedPitchType == "Curveball" ~ "CB",
      TaggedPitchType == "Cutter" ~ "CU",
      TRUE ~ "Other"                                          # Catch any unexpected pitch types
    )) %>%
    filter(TaggedPitchType != "Other")                              # Exclude "Other" pitch types
  
  # Create the ridgeline plot
  ggplot(d, aes(x = RelSpeed, y = TaggedPitchType, fill = TaggedPitchType)) +
    geom_density_ridges(alpha = 0.7, scale = 1) +             # Ridgeline density plots
    labs(
      title = paste("Velocity Distribution by Pitch Type for", d$Pitcher[1]),
      x = "Velocity (mph)",
      y = "Pitch Type"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("FF" = "blue", "SL" = "red", "SP" = "green", "CH" = "purple", "CB" = "orange", "CU" = "yellow")) + # Custom pitch type colors
    theme(
      legend.position = "none"                               # Remove legend (optional)
    )
}
pitch_break <- function(d) {
  ggplot(data = d) +
    geom_point(mapping = aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType), size = 3, alpha = 0.7) +
    labs(
      title = paste("Pitch Break for Each Pitch Thrown by", d$Pitcher[1]),
      x = "Horizontal Break (inches)",
      y = "Vertical Break (inches)",
      color = "Pitch Type"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set2") + # Optional: Use a color palette for better distinction
    coord_cartesian(xlim = c(-30, 30), ylim = c(-30, 30))  # Properly added to the ggplot object
}

gtown_stats <- function(d) {
  one_one_win_percentage <- d %>%
    filter(Balls == 1, Strikes == 1) %>%  # Filter for 1-1 counts
    summarise(
      total_1_1 = n(),                                              # Total number of 1-1 counts
      wins_1_1 = sum(                                               # Count pitches with winning outcomes
        PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay"), 
        na.rm = TRUE
      ),
      win_percentage = (wins_1_1 / total_1_1) * 100                 # Calculate the win percentage
    )
  
  zero_zero_win_percentage <- d %>%
    filter(Balls == 0, Strikes == 0) %>% # Filter for 0-0 count
    summarise(
      total_0_0 = n(),                                              # Total number of 0-0 pitches
      wins_0_0 = sum(                                               # Count pitches with winning outcomes
        PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay", "Error"), 
        na.rm = TRUE
      ),
      win_percentage = (wins_0_0 / total_0_0) * 100                 # Calculate the win percentage
    )
  
  two_two_win_percentage <- d %>%
    filter(Balls == 2, Strikes == 2) %>%  # Filter for 2-2 counts
    summarise(
      total_2_2 = n(),                                               # Total number of 2-2 counts
      wins_2_2 = sum(                                                # Count pitches with winning outcomes
        PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay"), 
        na.rm = TRUE
      ),
      win_percentage = (wins_2_2 / total_2_2) * 100                  # Calculate the win percentage
    )
  
  leadoff_stats <- d %>%
    group_by(Inning) %>% 
    filter(PAofInning == 1) %>%
    summarise(
      leadoff_outs = sum(
        if_else(
          OutsOnPlay > 0 | (OutsOnPlay == 0 & KorBB == "Strikeout"),
          1, 0
        ),
        na.rm = TRUE
      )
    )
  
  leadoff_out_percentage = (sum(leadoff_stats$leadoff_outs) / nrow(leadoff_stats)) * 100
  
  
  # Filter for quick outs and calculate quick out percentage
  quick_outs_data <- d %>%
    filter(PitchofPA <= 3) %>%  # Only quick PAs
    filter(OutsOnPlay %in% c(1, 2) | KorBB == "Strikeout") %>%  # Outs or strikeouts
    mutate(
      out_count = case_when(
        OutsOnPlay == 1 ~ 1,
        OutsOnPlay == 2 ~ 2,
        KorBB == "Strikeout" ~ 1,
        TRUE ~ 0
      )
    )
  
  # Step 2: Sum total quick outs (counting 2 outs correctly)
  quick_outs <- sum(quick_outs_data$out_count)
  
  # Step 3: Total outs overall (OutsOnPlay == 1 or 2 OR strikeout)
  total_outs <- d %>%
    filter(OutsOnPlay %in% c(1, 2) | KorBB == "Strikeout") %>%
    mutate(
      out_count = case_when(
        OutsOnPlay == 1 ~ 1,
        OutsOnPlay == 2 ~ 2,
        KorBB == "Strikeout" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    summarise(total_outs = sum(out_count)) %>%
    pull(total_outs)
  
  # Step 4: Calculate quick out percentage (avoid divide-by-zero)
  quick_out_percentage <- ifelse(total_outs > 0, (quick_outs / total_outs) * 100, 0)
  
  
  titles <- c("1-1 win %", "0-0 win %", "2-2 win %", "Leadoff Out %", "Quick Out %")
  values <- c(one_one_win_percentage$win_percentage, zero_zero_win_percentage$win_percentage, two_two_win_percentage$win_percentage, leadoff_out_percentage, quick_out_percentage)
  
  table <- data.frame(t(values)) %>%   mutate(across(where(is.numeric), ~ round(., 1)))
  colnames(table) <- titles
  
  return(table)  
  
}

#Step 1 - run lines 1-282. This will download the necessary libraries and functions.

#Step 2 - run the import_data() function below. This will prompt you to choose the correct data file from your hard drive.
d1 <- import_data()


#Step 3 - run the choose_pitcher() function below. This will prompt you to type the name of a pitcher in the console. Click enter after you have done so.
d <- choose_pitcher(d1)

#You can run the following 4 lines to generate and view the tables and graphs that will go into the report.
#table1 <- summary_data(d)
#table2 <- gtown_stats(d)
#table3 <- pitch_data(d)
#graph1 <- velo_distribution(d)
#graph2 <- pitch_break(d)
#view(table2)
#view(table3)
#graph1
#graph2

generate_pitcher_report <- function(d) {
  # Get pitcher name
  pitcher_name <- unique(d$Pitcher)
  
  #Get data source
  data_source <- readline(prompt = "Specify the date/opponent range used in this report: ")
  
  # Generate tables and graphs with suppressed warnings and messages
  summary_stats <- suppressMessages(suppressWarnings(summary_data(d)))
  performance_stats <- suppressMessages(suppressWarnings(gtown_stats(d)))
  pitch_summary <- suppressMessages(suppressWarnings(pitch_data(d)))
  pitch_summary2 <- suppressMessages(suppressWarnings(pitch_data2(d)))
  velo_plot <- suppressMessages(suppressWarnings(velo_distribution(d)))
  break_plot <- suppressMessages(suppressWarnings(pitch_break(d)))
  
  # Create Rmd file content
  rmd_content <- c(
    "---",
    paste("title: Pitcher Performance Report -", pitcher_name),
    paste("subtitle: 'Date/Opponent:", data_source, "'"),
    "output: pdf_document",
    "---",
    "",
    "## Summary Stats",
    "",
    "```{r echo=FALSE, warning=FALSE, message=FALSE}",
    "kable(summary_stats, caption = 'Summary Stats')",
    "```",
    "",
    "## Pitch Type Summary",
    "",
    "```{r echo=FALSE, warning=FALSE, message=FALSE}",
    "kable(pitch_summary, caption = 'Pitch Type Breakdown')",
    "kable(pitch_summary2, caption = 'Pitch Type Breakdown')",
    "```",
    "",
    "## Performance Metrics",
    "",
    "```{r echo=FALSE, warning=FALSE, message=FALSE}",
    "kable(performance_stats, caption = 'Performance Indicators')",
    "```",
    "",
    "\\newpage",
    "",
    "## Velocity Distribution",
    "",
    "```{r velo-plot, echo=FALSE, warning=FALSE, message=FALSE, fig.height=4, fig.width=7}",
    "print(velo_plot)",
    "```",
    "",
    "## Pitch Breaks",
    "",
    "```{r break-plot, echo=FALSE, warning=FALSE, message=FALSE, fig.height=4, fig.width=7}",
    "print(break_plot)",
    "```"
  )
  
  rmd_file <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, rmd_file)
  
  # Output PDF filename in working directory
  pdf_output <- file.path(getwd(), paste0(pitcher_name, " Performance Report ", data_source, ".pdf"))
  
  # Render PDF
  render(rmd_file, output_file = pdf_output, output_format = "pdf_document")
  
  # Clean up temporary file
  unlink(rmd_file)
  
  return(pdf_output)
}

# Generate the report and save it to the working directory
report_path <- generate_pitcher_report(d)