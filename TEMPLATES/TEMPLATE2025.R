library(pdftools)
library(tidyverse)
library(writexl)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

# ------------------------------------------------------------------
# 2025 Season Automated play-by-play and pitches
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# EDIT - Upload Files
# ------------------------------------------------------------------
# box score
pdf_text <- pdf_text("/Users/teahthies/Desktop/LBSU_2025/Box_Score/uh1.pdf") #edit
#lineup
lineup <- read_excel("/Users/teahthies/Desktop/LBSU_2025/lineups/uh1line.xlsx", sheet = 1)
# event code
event <- read_excel("/Users/teahthies/Desktop/BoxPDF2023/DONE/event.xlsx", sheet = 1)
#synergy pitches
pitches <- read_excel("/Users/teahthies/Desktop/LBSU_2025/synergy_charts/UH1syn.xlsx", sheet = 1)

# ------------------------------------------------------------------
# RUN - play-by-play pdf format
# ------------------------------------------------------------------
# page to and on is play by play data 
pdf_text_from_page_2 <- pdf_text[2:length(pdf_text)] 
# get each line a row if its own
pdf_lines <- unlist(strsplit(pdf_text_from_page_2, "\n"))
# make tibble
clean_data <- pdf_lines %>%
  as_tibble() 
# get name of player
clean_data <- clean_data %>%
  filter(grepl("^[A-Za-z]", value))

# ------------------------------------------------------------------
# EDIT - if a row in pdf goes to 2 lines, change it to 1
# ------------------------------------------------------------------
# Define the indices where merging should occur
merge_indices <- c(14,15,60)  # Indices where the next row should be merged
#.                -0, -1, -2, ....
# Loop through indices and merge rows
for (idx in merge_indices) {
  next_idx <- idx + 1
  # Check if the next row exists
  if (next_idx %in% rownames(clean_data)) {
    clean_data$value[rownames(clean_data) == idx] <- paste(clean_data$value[rownames(clean_data) == idx],
                                                           clean_data$value[rownames(clean_data) == next_idx])
    # Remove the next row after merging
    clean_data <- clean_data %>% filter(rownames(.) != next_idx)
  }
}

#------------------------------------------------------------------
# EDIT - if we pitch or bat in top of the inning
#------------------------------------------------------------------
table <- clean_data %>%  
  mutate(
    Inning = case_when(
      grepl("Top", value) ~ (str_extract(value, "\\d+")),
      grepl("Bottom", value) ~ (str_extract(value, "\\d+")),
      TRUE ~ NA_character_
    ), # Bat or Pitch
    BatPitch = case_when(
      grepl("Top", value) ~ "Bat",         #edit - LBSU is Pitching in the Top
      grepl("Bottom", value) ~ "Pitch",    #edit - LBSU is Batting in the Bottom
      TRUE ~ NA_character_
    )
  ) %>%
  fill(Inning, BatPitch, .direction = "down")

#------------------------------------------------------------------
# EDIT - normally not needed unless play by play gives initial
#------------------------------------------------------------------
table <- table %>%
  mutate(Player = case_when(
    str_detect(BatPitch, "Pitch") ~ word(value, 1),  # If BatPitch contains "Bat", take the first word
    str_detect(BatPitch, "Bat") ~ word(value, 1) # If BatPitch contains "Pitch", take the second word
  ))

#------------------------------------------------------------------
# EDIT - opposing team name
#------------------------------------------------------------------
table <- table %>%
  filter(!(Player %in% c("Hawaii", "Long")))  # edit - opponent team name
## this removes columns that subs are made in the game
table <- table[-grep("for", table$value, ignore.case = TRUE), ]
## remove commas and periods and semicolons in Player column
table$Player <- gsub("[^A-Za-z]", "", table$Player)

# ------------------------------------------------------------------
# RUN - combine lineup
# ------------------------------------------------------------------
table <- table %>%
  left_join(lineup %>% select(player, line_up_pos), 
            by = c("Player" = "player"))
table <- table %>%
  left_join(lineup %>% select(player, position), 
            by = c("Player" = "player"))
table <- table %>%
  left_join(lineup %>% select(player, Bat_hand), 
            by = c("Player" = "player"))

#------------------------------------------------------------------
# STOP AND CHECK - check if each player name transfered from lineup 
#------------------------------------------------------------------
table <- table %>% 
  filter(!is.na(Bat_hand))

# ------------------------------------------------------------------
# RUN - combine Event Table
# ------------------------------------------------------------------
table <- table %>%
  rowwise() %>%
  mutate(
    code = {
      # Check if the 'value' contains the 'event' from events_table
      matching_event <- event %>%
        filter(str_detect(value, event)) %>%
        pull(code) # Get the code for the matching event
      
      # If a match is found, use the code; otherwise, NA
      if (length(matching_event) > 0) matching_event[1] else NA
    }
  ) %>%
  ungroup()

#------------------------------------------------------------------
# STOP AND CHECK - check for no NA CODES unless it is a sub
#------------------------------------------------------------------
# remove substituitions
table <- table %>% filter(!is.na(code))
table <- table %>%
  left_join(event %>%
              select(code, event_outs, bat_event_fl, bat_dest, swing), 
            by = "code") 
# remove duplicate rows 
table <- table %>%
  distinct(value, .keep_all = TRUE)

# ------------------------------------------------------------------
# RUN - score columns
# ------------------------------------------------------------------
table <- table %>%
  mutate(
    score_increment = str_count(value, "\\bhomered\\b") + str_count(value, "\\bscored\\b") + str_count(value, "\\bstole home\\b")
  )

#------------------------------------------------------------------
# STOP AND CHECK - check if final score is correct, then continue 
#------------------------------------------------------------------
# Manually adjust a specific row where a missing run needs to be added
#table$score_increment[18] <- table$score_increment[18] + 1
table <- table %>%
  mutate(
    LBSU_score = cumsum(ifelse(BatPitch == "Bat", score_increment, 0)),
    OPP_score = cumsum(ifelse(BatPitch == "Pitch", score_increment, 0))
  ) %>%
  select(-score_increment) # Remove the temporary column

## ------------------------------------------------------
## EDIT - Outs Tracker: in line 176 make sure bat/pitch 
##                                 is in correct order
## ------------------------------------------------------
current_outs <- 0
for(i in 1:nrow(table)) {
  # For the first batter of each half-inning, start with 0 outs
  if(i == 1 || (table$Inning[i] != table$Inning[i - 1])) {
    current_outs <- 0
  }
  # Assign the number of outs before the at-bat
  table$num_outs[i] <- current_outs  
  # Update outs based on 'event_outs' column
  current_outs <- current_outs + table$event_outs[i]
  # Reset the counter at the end of each half-inning (when 'BatPitch' changes from "Pitch" to "Bat")
  if(i < nrow(table) && table$BatPitch[i] == "Bat" && table$BatPitch[i + 1] == "Pitch") {
    current_outs <- 0
  }
}

# table$num_outs[30] <- 0
# table$num_outs[31] <- 1
# table$num_outs[32] <- 2
# table$num_outs[33] <- 2

## ------------------------------------------------------
## RUN - Tracking Base Runners
## ------------------------------------------------------
# table <- table[-49, ]
# Define keywords for tracking base runners
## add: drop 3rd, 
on_base_keywords <- c("walked", "singled", "doubled", "tripled", 
                      "reached on an error", "hit by pitch", 
                      "reached on a fielder's choice", "placed on second")  
## add: caught stealing, pick-off, 
off_base_keywords <- c("reached on a fielder's choice", "caught stealing", 
                       "scored", "picked off", "out at second",
                       "out at third", "out at home", "stole home")
# Create the 'num_baserunners' column and initialize to zero
table$num_baserunners <- 0
# Create a cumulative counter
current_baserunners <- 0
# run loop
for(i in 1:nrow(table)) {
  # For the first batter of each half inning, start with 0 runners
  if(i == 1 || (table$Inning[i] != table$Inning[i - 1])) {
    current_baserunners <- 0
  }
  # Assign the cumulative count to the current batter's 'num_baserunners'
  table$num_baserunners[i] <- current_baserunners
  # Now, update num_baserunners based on if the batter gets on or off base
  if(any(sapply(on_base_keywords, function(x) grepl(x, table$value[i])))) {
    current_baserunners <- current_baserunners + 1
  }
  if(any(sapply(off_base_keywords, function(x) grepl(x, table$value[i])))) {
    current_baserunners <- current_baserunners - 1
  }
  # Reset the counter at the end of each half inning (when 'BatPitch' is "Pitch")
  if(i < nrow(table) && table$BatPitch[i] == "Bat" && table$BatPitch[i + 1] == "Pitch") {
    current_baserunners <- 0
  }
}

## ------------------------------------------------------
## STOP AND CHECK - make sure runners !>4
## ------------------------------------------------------
## manually input for wrong data
# table$num_baserunners[48] <- 3
# table$num_baserunners[49] <- 2
# table$num_baserunners[50] <- 2
# table$num_baserunners[51] <- 2
# table$num_baserunners[52] <- 2

##---------------------------------------------------
## EDIT - add general game info columns
##---------------------------------------------------
table$game_location <- "away"            # home, away, neutral
table$game_type <- "conference"      # conference, non-conference
table$opponent <- "UH"  
table$home_umpire <- "Eric Jones"
table$time_of_day <- 1805               # military time
table$weather <- 70                      # degree F
table$outcome <- "Loss"                  # Win, Loss
table$year <- 2025                     # year
table$time_minutes <- 120               # game length in minutes 
table$attendence <- 375
table$home_vis <- "V"                    # V, H
table$uniform <- "black"          # white_pins, black, black_Y_LB, greypins, LBC
# blackpins, white

# ------------------------------------------------------------------
# RUN - speed
# ------------------------------------------------------------------
table$pitch_speed <- ""


# ------------------------------------------------------------------
# EDIT - add Pitchers and Catchers
# ------------------------------------------------------------------


# catchers
table <- table %>%
  mutate(Catcher = NA) %>% 
  # Assign the pitcher values to specific rows
  { .[1, "Catcher"] <- ""; .[6, "Catcher"] <- "Durazo"; . } %>%
  # Group by BatPitch and fill Pitcher down within each group
  group_by(BatPitch) %>%
  fill(Catcher, .direction = "down") %>%
  ungroup()  # Ungroup after filling






# ------------------------------------------------------------------
# RUN - synergy
# ------------------------------------------------------------------
#synergy pitches
pitches <- read_excel("/Users/teahthies/Desktop/LBSU_2025/synergy_charts/UH1syn.xlsx", sheet = 1)

# get only defense 
table <- table %>%
  filter(BatPitch == "Pitch")
# add plate appearacne to table
table <- table %>%
  mutate(Plate_Appearance = cumsum(Player != lag(Player, default = first(Player))) + 1) %>%
  ungroup()


# fill down and with 0's
pitches <- pitches %>%
  fill(Pitcher, Player, Pitch, .direction = "down") %>%  # Fill down values
  mutate(across(c(Ball, Strike), ~replace_na(., 0)))
# number pitches in game 
pitches <- pitches %>%
  mutate(pitch_count = row_number())

##### GET HITTER AND PLAYER TO MATCH
table <- table %>%
  mutate(Player = tolower(Player))  # Convert Player to lowercase
pitches <- pitches %>%
  mutate(Player = tolower(Player))

# add plate appearance column
pitches <- pitches %>%
  mutate(Plate_Appearance = cumsum(Player != lag(Player, default = first(Player)))+1)
table <- table %>%
  filter(bat_event_fl == TRUE)

# combine last pitches with table_pitch play by play
last_pitches <- pitches %>%
  group_by(Plate_Appearance, Player) %>%  # Group by batter and plate appearance
  slice_tail(n = 1)  # Select only the last pitch for each batter



# Merge with the play-by-play table_pitch
combine <- left_join(pitches, table, by = "Plate_Appearance")

# remove player
combine <- combine %>% select(-Player.x)

# change name to hitter to make consistent
combine <- combine %>%
  rename(Hitter = Player.y)

#### fix columns 
cols_to_keep <- c("code", "event_outs", "bat_event_fl", "bat_dest", "swing", "LBSU_score", "OPP_score")

# Process the dataset
combine <- combine %>%
  group_by(Hitter, Plate_Appearance) %>%  # Group by Hitter and Plate_Appearance
  mutate(across(all_of(cols_to_keep), ~ ifelse(row_number() == n(), ., NA))) %>%  # Keep only in last row
  ungroup()

#score
combine$LBSU_score[1] <- 0
combine$OPP_score[1] <- 0
combine <- combine %>%
  fill(LBSU_score, OPP_score, .direction = "down")


# event_outs, bat_dest
combine <- combine %>%
  group_by(Hitter, Plate_Appearance) %>%  # Group by Hitter and Plate_Appearance
  mutate(event_outs = ifelse(row_number() == n(), event_outs, 0)) %>%
  mutate(bat_dest = ifelse(row_number() == n(), bat_dest, 0)) %>%   # Keep only in last row, else 0
  mutate(bat_event_fl = ifelse(row_number() == n(), bat_event_fl, FALSE)) %>%
  mutate(swing = ifelse(row_number() == n(), swing, FALSE)) %>%
  ungroup()

# swing
combine <- combine %>%
  mutate(swing = ifelse(Swing %in% c("1", "1f", "1c") & swing == FALSE, TRUE, swing))

# add foul and chase
combine <- combine %>%
  mutate(Pitch_Result = case_when(
    Swing == "1" & Strike == 1 ~ "miss",
    Swing == "1c" & Strike == 1 ~ "chase",
    Swing == "1f" & Strike %in% c(0, 1) ~ "foul",
    Swing == "1" & Strike == 0 ~ "BIP",
    is.na(Swing) & Strike == 1 ~ "StrikeTaken",
    is.na(Swing) & Strike == 0 ~ "ball",
    TRUE ~ NA_character_  # Assign NA if none of the conditions are met
  ))

## remove value column
combine <- combine %>% select(-Swing)

# make count column
# balls and strikes
combine <- combine %>%
  group_by(Hitter, Plate_Appearance) %>%
  mutate(
    strike_count = cumsum(lag(Strike, default = 0)),  # Cumulative sum of strikes
    ball_count = cumsum(lag(Ball, default = 0))       # Cumulative sum of balls
  )


## remove extra columns
colnames(combine)




## play by play to excel
write_xlsx(combine, "/Users/teahthies/Desktop/LBSU_2025/game_charts/UH1.xlsx")
