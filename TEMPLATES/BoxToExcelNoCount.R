# ------------------------------------------------------------------
# This is the script that will take the play by play pdf and turn them
# into an organized excel file.
# The only thing this pdf is missing that we will need to get from synergy 
# is count, pitch type, and location.
# ------------------------------------------------------------------

library(pdftools)
library(tidyverse)
library(writexl)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

# ------------------------------------------------------------------
# Upload PDF
# ------------------------------------------------------------------
# import pdf
pdf_text <- pdf_text("/Users/teahthies/Desktop/BoxPDF2023/PDF/USD02.pdf") #edit
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
# Make rows with desired features that are already in the tibble at 
# this point
# This is the info that they play by play has typed out already 
# in the column named "value"
# ------------------------------------------------------------------


##### ------------------------------------------------------------------
# STOP AND GET ALL ROWS THAT GO INTO SECOND LINE AND COMBINE THEM 
#### ------------------------------------------------------------------
# Define the indices where merging should occur
merge_indices <- c(91,92,93)  # Indices where the next row should be merged
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
      grepl("Top", value) ~ "Pitch",         #edit - LBSU is Pitching in the Top
      grepl("Bottom", value) ~ "Bat",    #edit - LBSU is Batting in the Bottom
      TRUE ~ NA_character_
    )
  ) %>%
  fill(Inning, BatPitch, .direction = "down")

#------------------------------------------------------------------
# EDIT - normally not needed unless play by play gives initial
#------------------------------------------------------------------
table <- table %>%
  mutate(Hitter = case_when(
    str_detect(BatPitch, "Bat") ~ word(value, 1),  # If BatPitch contains "Bat", take the first word
    str_detect(BatPitch, "Pitch") ~ word(value, 1) # If BatPitch contains "Pitch", take the second word
  ))

#------------------------------------------------------------------
# EDIT - opposing team name
#------------------------------------------------------------------
table <- table %>%
  filter(!(Hitter %in% c("Purdue", "Long")))  # edit - opponent team name
## this removes columns that subs are made in the game
table <- table[-grep("for", table$value, ignore.case = TRUE), ]
## remove commas and periods and semicolons in Player column
table$Hitter <- gsub("[^A-Za-z]", "", table$Hitter)


# ------------------------------------------------------------------
# balls, strikes, pitch type, pitch location, speed
# ------------------------------------------------------------------
table$ball_count <- ""
table$strike_count <- ""
table$Pitch <- ""
table$Location <- ""
table$pitch_speed <- ""
table$Catcher


# ------------------------------------------------------------------
# EDIT - get lineup
# ------------------------------------------------------------------
lineup <- read_excel("/Users/teahthies/Desktop/BoxPDF2023/lineup2023/purd.xlsx", sheet = 1)

# ------------------------------------------------------------------
# RUN - combine lineup
# ------------------------------------------------------------------
# table$Hitter[19] <- "Bailey"
# table$Hitter[71] <- "Bailey"
# table$Hitter[58] <- "Bailey"
# table$Hitter[43] <- "Scarmardo"
# table$Hitter[61] <- "Scarmardo"
table <- table %>%
  left_join(lineup %>% select(player, line_up_pos), 
            by = c("Hitter" = "player"))
table <- table %>%
  left_join(lineup %>% select(player, position), 
            by = c("Hitter" = "player"))
table <- table %>%
  left_join(lineup %>% select(player, Bat_hand), 
            by = c("Hitter" = "player"))
# table$Hitter[58] <- "Bailey"
# table$line_up_pos[58] <- 8
# table$position[58] <- 9
# table$Bat_hand[58] <- "L"

#------------------------------------------------------------------
# STOP AND CHECK - check if each player name transfered from lineup 
#------------------------------------------------------------------
table <- table %>% 
  filter(!is.na(Bat_hand))


# ------------------------------------------------------------------
# RUN - combine Event Table
# ------------------------------------------------------------------
event <- read_excel("/Users/teahthies/Desktop/BoxPDF2023/event.xlsx", sheet = 1)

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
### remove substituitions
table <- table %>% filter(!is.na(code))
table <- table %>%
  left_join(event %>%
              select(code, event_outs, bat_event_fl, bat_dest, swing), 
            by = "code") 
#remove duplicate rows 
table <- table %>%
  distinct(value, .keep_all = TRUE)



# ------------------------------------------------------------------
# RUN - score columns
# ------------------------------------------------------------------
table <- table %>%
  mutate(
    score_increment = str_count(value, "\\bhomered\\b") + str_count(value, "\\bscored\\b")
  )

########### STOPPPPP - if you need to manually adjust rows, do it here
# Manually adjust a specific row where a missing run needs to be added
# Example: Let's say we know row 5 (Inning 3) should have had an extra run.
#table$score_increment[18] <- table$score_increment[18] + 1
#table$score_increment[50] <- table$score_increment[50] + 1
#table$score_increment[51] <- table$score_increment[51] + 1
#table$score_increment[47] <- table$score_increment[47] + 1

table <- table %>%
  mutate(
    LBSU_score = cumsum(ifelse(BatPitch == "Bat", score_increment, 0)),
    OPP_score = cumsum(ifelse(BatPitch == "Pitch", score_increment, 0))
  ) %>%
  select(-score_increment) # Remove the temporary column



#------------------------------------------------------------------
# STOP AND CHECK - check if final score is correct, then continue 
#------------------------------------------------------------------
# Manually adjust a specific row where a missing run needs to be added
#table$score_increment[18] <- table$score_increment[18] + 1

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
  
  ###### NEED TO BE IN ORDER OF HWEN WE ARE PITCHING AND BATTING
  # Reset the counter at the end of each half-inning (when 'BatPitch' changes from "Pitch" to "Bat")
  if(i < nrow(table) && table$BatPitch[i] == "Pitch" && table$BatPitch[i + 1] == "Bat") {
    current_outs <- 0
  }
}

# if needed to be adjusted
# table$num_outs[62] <- 2
# table$num_outs[60] <- 1
# table$num_outs[61] <- 1
#table$num_outs[29] <- 2

## ------------------------------------------------------
## RUN - Tracking Base Runners
## ------------------------------------------------------








## ---------------------------
## Tracking Base Runners
## ---------------------------
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
  if(i < nrow(table) && table$BatPitch[i] == "Pitch" && table$BatPitch[i + 1] == "Bat") {
    current_baserunners <- 0
  }
}

## manually inout for missing data
table$num_baserunners[24] <- 1
table$num_baserunners[25] <- 0













# ------------------------------------------------------------------
# EDIT - add Pitchers and Catchers
# ------------------------------------------------------------------
#pitchers
table <- table %>%
  mutate(Pitcher = NA) %>% 
  # Assign the pitcher values to specific rows
  { .[1, "Pitcher"] <- "Fernandez"; .[6, "Pitcher"] <- "R"; .[52, "Pitcher"] <- "Quinlan"; .[66, "Pitcher"] <- "Frutoz"; . } %>%
  # Group by BatPitch and fill Pitcher down within each group
  group_by(BatPitch) %>%
  fill(Pitcher, .direction = "down") %>%
  ungroup()  # Ungroup after filling

# catchers
table <- table %>%
  mutate(Catcher = NA) %>% 
  # Assign the pitcher values to specific rows
  { .[1, "Catcher"] <- "Thies"; .[6, "Catcher"] <- ""; .[45, "Catcher"] <- "Magadan"; . } %>%
  # Group by BatPitch and fill Pitcher down within each group
  group_by(BatPitch) %>%
  fill(Catcher, .direction = "down") %>%
  ungroup()  # Ungroup after filling













##---------------------------------------------------
## add general game info columns
##---------------------------------------------------
table$game_location <- "home"            # home, away, neutral
table$game_type <- "non-conference"      # conference, non-conference
table$opponent <- "Purdue"  
table$home_umpire <- "Bill Plante"
table$time_of_day <- 900               # military time
table$weather <- 58                      # degree F
table$outcome <- "Win"                  # Win, Loss
table$year <- "2023"                     # year
table$time_minutes <- 140               # game length in minutes 
table$attendence <- 363
table$home_vis <- "H"                    # V, H
table$uniform <- "white_pins"          # white_pins, black, black_Y_LB, greypins, LBC
# blackpins, white


# add plate appearacne
table <- table %>%
  mutate(Plate_Appearance = cumsum(Hitter != lag(Hitter, default = first(Hitter))) + 1) %>%
  ungroup()

# add pitch result
table$Pitch_Result <- ifelse(table$bat_event_fl == TRUE, "BIP", NA)

table <- table %>%
  mutate(
    Strike = ifelse(code %in% c(4, 5, 11), 1, NA),
    Ball = ifelse(code %in% c(7, 12, 13, 18, 19), 1, NA)
  )









## play by play to excel
write_xlsx(table, "/Users/teahthies/Desktop/BoxPDF2023/excel/USD02.xlsx")

