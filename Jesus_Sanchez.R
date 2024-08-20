library(dplyr)
library(tidyverse)
library(ggplot2)
library(mlbplotR)
library(gt)
library(gtExtras)
library(baseballr)

###Dataframe is called PBPFull###
###Acquiring MLBID's from Jesus Sanhez and Brandon Lowe###
headshots <- load_headshots()

PBP <- PBPFull %>%
  filter(batter %in% c("660821","664040"))

PBP <- PBP %>%
  left_join(headshots %>%
              select(savant_id, player_name), 
            by = c("batter" = "savant_id")) %>%
  mutate(player_name.y = "Hitter")

SwingsDecisions <- PBP %>%
  mutate(Pitch_Group = case_when(
    pitch_name %in% c("4-Seam Fastball", "Sinker") ~ "Fastball",
    pitch_name %in% c("Changeup", "Split-Finger") ~ "Off-Speed",
    pitch_name %in% c("Curveball", "Slider", "Sweeper", "Slurve", "Cutter", "Knuckle Curve") ~ "Breaking Ball",
    TRUE ~ "Other"  
  )) %>%
  mutate(Pitch_Count = case_when(
    strikes %in% c("0", "1") ~ "Less Than Two Strikes",
    strikes == "2" ~ "Two Strikes"
  )) %>%
  mutate(In_Zone = case_when(
    plate_x >= -0.83 & plate_x <= 0.83 & plate_z >= sz_bot & plate_z <= sz_top ~ "In-Zone",
    TRUE ~ "Out of Zone"
  )) %>%
  filter(Pitch_Group != "Other") %>%
  group_by(player_name, Pitch_Group) %>%
  summarise(
    overall_swing_length = mean(swing_length, na.rm = TRUE),  
    less_than_two_strikes_swing_length = mean(swing_length[Pitch_Count == "Less Than Two Strikes"], na.rm = TRUE),   
    Pitches = n(),
    swings = sum(description %in% c("swinging_strike", "foul", "ball_in_play")),  
    whiffs = sum(description == "swinging_strike"),  
    whiff_percentage = (whiffs / swings) * 100,
    in_zone_swings = sum(description %in% c("swinging_strike", "foul", "ball_in_play") & In_Zone == "In-Zone"),
    in_zone_whiffs = sum(description == "swinging_strike" & In_Zone == "In-Zone"),
    in_zone_whiff_percentage = (in_zone_whiffs / in_zone_swings) * 100,  
    less_than_two_strikes_swings = sum(description %in% c("swinging_strike", "foul", "ball_in_play") & Pitch_Count == "Less Than Two Strikes"),
    less_than_two_strikes_whiffs = sum(description == "swinging_strike" & Pitch_Count == "Less Than Two Strikes"),  
    less_than_two_strikes_whiff_percentage = (less_than_two_strikes_whiffs / less_than_two_strikes_swings) * 100  
  ) %>%
  filter(swings > 0) %>%
  select(player_name, Pitch_Group, overall_swing_length, less_than_two_strikes_swing_length, whiff_percentage, less_than_two_strikes_whiff_percentage, in_zone_whiff_percentage, Pitches)

###Rounding###
SwingsDecisions$overall_swing_length <- round(SwingsDecisions$overall_swing_length, 1)
SwingsDecisions$less_than_two_strikes_swing_length <- round(SwingsDecisions$less_than_two_strikes_swing_length, 1)
SwingsDecisions$whiff_percentage <- round(SwingsDecisions$whiff_percentage, 1)
SwingsDecisions$less_than_two_strikes_whiff_percentage <- round(SwingsDecisions$less_than_two_strikes_whiff_percentage , 1)
SwingsDecisions$in_zone_whiff_percentage <- round(SwingsDecisions$in_zone_whiff_percentage , 1)

Swings_Decision_Table <- SwingsDecisions |> 
  gt() |>
  tab_style(
    style = cell_text(weight = "bold", color = "red"),
    locations = cells_row_groups(groups = c("Brandon Lowe", "Jesús Sánchez"))
  ) %>%
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Table: Ethan Mann @ethanmann02| Data: Baseball Savant from pybaseball through R") |> 
  cols_label(
    player_name = "Hitter",
    Pitch_Group = "Pitch",
    overall_swing_length = "Swing Length",
    less_than_two_strikes_swing_length = "<2 Strikes Swing Length",
    whiff_percentage = "Whiff%", 
    less_than_two_strikes_whiff_percentage = "<2 Strikes Whiff%",
    in_zone_whiff_percentage = "In-Zone Whiff%") |>
  opt_row_striping() |> 
  tab_header(title = "Jesus Sanchez Swing Length and Plate Discipline comparisons") %>%
  gt_theme_538()

print(Swings_Decision_Table)

###BattebBall###
BattedBall <- PBP %>%
  mutate(
    Pitch_Group = case_when(
      pitch_name %in% c("4-Seam Fastball", "Sinker") ~ "Fastball",
      pitch_name %in% c("Changeup", "Split-Finger") ~ "Off-Speed",
      pitch_name %in% c("Curveball", "Slider", "Sweeper", "Slurve", "Cutter", "Knuckle Curve") ~ "Breaking Ball",
      TRUE ~ "Other"
    ),
    Pitch_Count = case_when(
      strikes %in% c("0", "1") ~ "Less Than Two Strikes",
      strikes == "2" ~ "Two Strikes"
    ),
    In_Zone = case_when(
      plate_x >= -0.83 & plate_x <= 0.83 & plate_z >= sz_bot & plate_z <= sz_top ~ "In-Zone",
      TRUE ~ "Out of Zone"
    ),
    PFB = case_when(
      player_name == "Jesús Sánchez" & Pitch_Group == "Fastball" ~ 8,
      player_name == "Jesús Sánchez" & Pitch_Group == "Off-Speed" ~ 44,
      player_name == "Jesús Sánchez" & Pitch_Group == "Breaking Ball" ~ 33,
      player_name == "Brandon Lowe" & Pitch_Group == "Fastball" ~ 21,
      player_name == "Brandon Lowe" & Pitch_Group == "Off-Speed" ~ 47,
      player_name == "Brandon Lowe" & Pitch_Group == "Breaking Ball" ~ 43,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(Pitch_Group != "Other") %>%
  group_by(player_name, Pitch_Group) %>%
  summarise(
    overall_bat_speed = mean(bat_speed, na.rm = TRUE),
    less_than_two_strikes_bat_speed = mean(bat_speed[Pitch_Count == "Less Than Two Strikes"], na.rm = TRUE),
    Pitches = n(),
    batted_balls = sum(description == "hit_into_play"),
    ground_balls = sum(bb_type == "ground_ball", na.rm = TRUE),
    gb_percentage = (ground_balls / batted_balls) * 100,
    hard_hits = sum(launch_speed >= 95 & description == "hit_into_play", na.rm = TRUE),
    hard_hit_rate = (hard_hits / batted_balls) * 100,
    PFB = max(PFB, na.rm = TRUE)  # This will retain the assigned PFB value after grouping
  ) %>%
  filter(batted_balls > 0) %>%
  select(player_name, Pitch_Group, overall_bat_speed, less_than_two_strikes_bat_speed, gb_percentage, hard_hit_rate, PFB, Pitches)


BattedBall$overall_bat_speed <- round(BattedBall$overall_bat_speed, 1)
BattedBall$less_than_two_strikes_bat_speed <- round(BattedBall$less_than_two_strikes_bat_speed, 1)
BattedBall$gb_percentage <- round(BattedBall$gb_percentage, 1)
BattedBall$hard_hit_rate <- round(BattedBall$hard_hit_rate, 1)

Batted_Ball_Table <- BattedBall |> 
  gt() |>
  tab_style(
    style = cell_text(weight = "bold", color = "red"),
    locations = cells_row_groups(groups = c("Brandon Lowe", "Jesús Sánchez"))
  ) %>%
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Table: Ethan Mann @ethanmann02| Data: Baseball Savant from pybaseball through R") |> 
  cols_label(
    player_name = "Hitter",
    Pitch_Group = "Pitch",
    overall_bat_speed = "Bat Speed",
    less_than_two_strikes_bat_speed = "2 Strikes Bat Speed",
    gb_percentage = "GB%", 
    hard_hit_rate = "Hard Hit%",
    PFB = "Pulled FB%") |>
  opt_row_striping() |> 
  tab_header(title = "Jesus Sanchez Bat Speed and Quality of Contact Comparison") %>%
  gt_theme_538()

print(Batted_Ball_Table)

###FInding Chase%###

FG <- try(fg_batter_leaders(startseason = 2024, endseason = 2024, qual = 50)) 

ggplot(data = FG, mapping = aes(x = `O-Swing_pct`, y = xwOBA)) +
  geom_point(aes(color = ifelse(PlayerName == "Jesús Sánchez", "Sánchez", "Others")), show.legend = FALSE) + 
  geom_smooth(method = "lm", se = FALSE) + 
  scale_color_manual(values = c("Sánchez" = "red", "Others" = "black")) + 
  labs(x = "Chase%",
       title = "Jesús Sánchez Chase% vs xwOBA w/other MLBers",
       subtitle = "50 Plate Appearances Min. | Sánchez in Red",
       caption = "Data from Fangraphs | Visual by Ethan Mann @ethanmann02") +
  theme_minimal()

correlation <- cor(FG$`O-Swing_pct`, FG$xwOBA, use = "complete.obs")
print(correlation)
