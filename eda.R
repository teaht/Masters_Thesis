#---------------------------------------------
# packages
#---------------------------------------------
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(ggplot2)
library(caret)
library(dplyr)


#---------------------------------------------
# load play-by-play (pbp) dataset
#---------------------------------------------
pbp <- read_excel("/Users/teahthies/Desktop/Modeling/cleanpitch.xlsx", sheet=1)

# split to just training data for EDA, we will not look at testing data 
n <- nrow(pbp)

# index cutoff for 80%
cutoff <- floor(0.80 * n)

# split: first 80% = train, last 20% = test
pbp <- pbp[1:cutoff, ]

pbp <- subset(pbp, bat_event_fl == TRUE)
# sanity check
nrow(pbp)   # 3675


#---------------------------------------------
# cols rows type
#---------------------------------------------
dim(pbp)

#---------------------------------------------
# missing data
#---------------------------------------------
# empty strings to NA
pbp[pbp == ""] <- NA

# how much is missing in each column
colSums(is.na(pbp))
threshold <- 0.3
missing_percent <- colSums(is.na(pbp)) / nrow(pbp)
names(missing_percent[missing_percent > threshold])
                    # hit_type was only recorded for some games (keep and subset)
                    # balls and strikes have lots of missing
                    # still need to figure out what to do with value column
                    # catchers were not recorded for opposing teams
                          # (this will need to be subsetted)
# look put for player, Strike, Ball, Catcher, hit_type



#---------------------------------------------
# variable type
#---------------------------------------------
# Convert to numeric
pbp$Inning <- as.numeric(pbp$Inning)
# Convert to factors (categorical)
pbp$Pitch <- as.factor(pbp$Pitch)
pbp$Bat_hand <- as.factor(pbp$Bat_hand)
pbp$code <- as.factor(pbp$code)
pbp$Pitch_Result <- as.factor(pbp$Pitch_Result)
pbp$home_vis <- as.factor(pbp$home_vis)
pbp$hit_type <- as.factor(pbp$hit_type)
pbp$Strike <- as.factor(pbp$Strike)
pbp$Ball <- as.factor(pbp$Ball)
pbp$pitch_count <- as.factor(pbp$pitch_count)
# Convert to logical (binary)
pbp$swing <- as.logical(pbp$swing)
pbp$outcome <- as.factor(pbp$outcome)

pbp <- pbp %>%
  mutate(
    Pitch = if_else(Pitch == "B", "C", Pitch),
    Pitch = if_else(Pitch == "4", "F", Pitch),
    Pitch = if_else(Pitch == "I", NA, Pitch),
    Location = if_else(Location == "1f", "1", Location),
    num_outs = if_else(num_outs == 4, 2, num_outs),
    num_outs = if_else(num_outs == 3, 2, num_outs),
    num_baserunners = if_else(num_baserunners == -1, 0, num_baserunners),
    num_baserunners = if_else(num_baserunners == 5, 3, num_baserunners),
    num_baserunners = if_else(num_baserunners == 4, 3, num_baserunners),
    Pitcher = if_else(Pitcher == "G.", "Gonzales", Pitcher),
    Pitcher = if_else(Pitcher == "Frutos", "Frutoz", Pitcher),
    Pitcher = if_else(Pitcher == "Kostrencich", NA, Pitcher),
    Pitcher = if_else(Pitcher == "Unknown", NA, Pitcher),
    Strike = if_else(Strike == "2", "1", Strike)
  )

pbp$Location <- as.factor(pbp$Location)
pbp$Strike <- as.logical(pbp$Strike)
pbp$Ball <- as.logical(pbp$Ball)
pbp$pitch_count <- as.numeric(pbp$pitch_count)

# str(pbp)
# sapply(pbp, class)

#---------------------------------------------
# correlation
#---------------------------------------------
numeric_data <- pbp[sapply(pbp, is.numeric)]
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

threshold <- 0.5  # you can adjust this
high_cor <- which(abs(cor_matrix) > threshold & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor_pairs <- unique(t(apply(high_cor, 1, sort)))
for (i in 1:nrow(high_cor_pairs)) {
  row <- high_cor_pairs[i, 1]
  col <- high_cor_pairs[i, 2]
  cat(rownames(cor_matrix)[row], " & ", colnames(cor_matrix)[col], 
      ": ", cor_matrix[row, col], "\n")
}

# Inning  &  Plate_Appearance :  0.8093022 
# OPP_score  &  Plate_Appearance :  0.5261127 
# ball_count  &  pitch_count :  0.977381 
# strike_count  &  pitch_count :  0.6014809 

# take out pitch count and Plate Appearance 
pbp <- pbp %>% select(-c(Hitter, Plate_Appearance, pitch_count, Ball, Strike))
colnames(pbp)


#---------------------------------------------
# Prepare Data - Split RHH & LHH
#---------------------------------------------
char_cols <- names(pbp)[sapply(pbp, is.character)]
setwd("/Users/teahthies/Desktop/EDA/eda_paper_FIGS")
r <- subset(pbp, Bat_hand == "R")
l <- subset(pbp, Bat_hand == "L")
nrow(r)
nrow(l)



#---------------------------------------------
# Univariate
#---------------------------------------------
### PITCH TYPE COUNT
# RHH
# can find these imgs in eda_pitch_FIGS folder in EDA
for (col in char_cols) {
  # Open a new PNG device to save the plot
  png(paste0(col, "_RIGHT.png"))

  # Create the bar plot
  barplot(table(r[[col]]),
          main = paste("RHH - Bar Plot of", col),
          xlab = col,
          col = "lightblue",
          las = 2,
          cex.names = 0.8)

  # Close the PNG device to save the file
  dev.off()
}

# LHH
# can find these imgs in eda_pitch_FIGS folder in EDA
for (col in char_cols) {
  # Open a new PNG device to save the plot
  png(paste0(col, "_LEFT.png"))
  
  # Create the bar plot
  barplot(table(l[[col]]),
          main = paste("LHH - Bar Plot of", col),
          xlab = col,
          col = "lightblue",
          las = 2,
          cex.names = 0.8)
  
  # Close the PNG device to save the file
  dev.off()
}
#---------------------------------------------

### HEAT MAP LOCATION
# RHH
r$Location <- as.numeric(as.character(r$Location))
r2 <- r %>%
  mutate(
    row = case_when(
      Location %in% c(1, 4, 7) ~ 1,  # Top row
      Location %in% c(2, 5, 8) ~ 2,  # Middle row
      Location %in% c(3, 6, 9) ~ 3   # Bottom row
    ),
    col = case_when(
      Location %in% c(1, 2, 3) ~ 1,  # Left column
      Location %in% c(4, 5, 6) ~ 2,  # Middle column
      Location %in% c(7, 8, 9) ~ 3   # Right column
    )
  )


# Count the number of pitches in each (row, col) grid location
location_counts <- r2 %>%
  count(row, col)
# Plot the heatmap
ggplot(location_counts, aes(x = col, y = row)) +
  geom_tile(aes(fill = n), color = "white") +
  scale_fill_gradient(low = "white", high = "black") +  # Adjust the color scale as needed
  theme_minimal() +
  scale_x_continuous(breaks = 1:3, labels = c("Left", "Center", "Right")) +
  scale_y_continuous(breaks = 1:3, labels = c("Bottom", "Middle", "Top")) +
  labs(title = "RHH - Pitch Location Heatmap (Pitcher's View)", x = "Column", y = "Row") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

# LHH
l$Location <- as.numeric(as.character(l$Location))
l2 <- l %>%
  mutate(
    row = case_when(
      Location %in% c(1, 4, 7) ~ 1,  # Top row
      Location %in% c(2, 5, 8) ~ 2,  # Middle row
      Location %in% c(3, 6, 9) ~ 3   # Bottom row
    ),
    col = case_when(
      Location %in% c(1, 2, 3) ~ 1,  # Left column
      Location %in% c(4, 5, 6) ~ 2,  # Middle column
      Location %in% c(7, 8, 9) ~ 3   # Right column
    )
  )


# Count the number of pitches in each (row, col) grid location
location_counts <- l2 %>%
  count(row, col)
# Plot the heatmap
ggplot(location_counts, aes(x = col, y = row)) +
  geom_tile(aes(fill = n), color = "white") +
  scale_fill_gradient(low = "white", high = "black") +  # Adjust the color scale as needed
  theme_minimal() +
  scale_x_continuous(breaks = 1:3, labels = c("Left", "Center", "Right")) +
  scale_y_continuous(breaks = 1:3, labels = c("Bottom", "Middle", "Top")) +
  labs(title = "LHH - Pitch Location Heatmap (Pitcher's View)", x = "Column", y = "Row") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))




#---------------------------------------------
# Multivariate
#---------------------------------------------
# pitch count vs outcomes
# RHH
r2 <- r %>%
  mutate(
    count = paste0(ball_count, "-", strike_count),
    is_hit = code %in% c(20, 21, 22, 23),  
    is_SO = code %in% c(4),
    is_HR = code %in% c(23),
    is_BB = code %in% c(18),
    is_on_base = bat_dest > 0                          
  ) %>%
  # Remove NA counts or incomplete data
  filter(!is.na(ball_count), !is.na(strike_count)) %>%
  group_by(count) %>%
  summarize(
    batting_avg = round(mean(is_hit, na.rm = TRUE), 3),
    SO_rate = round(mean(is_SO, na.rm = TRUE), 3),
    HR_rate = round(mean(is_HR, na.rm = TRUE), 3),
    BB_rate = round(mean(is_BB, na.rm = TRUE), 3),
    on_base_rate = round(mean(is_on_base, na.rm = TRUE), 3),
    attempts = n()
  ) %>%
  arrange(count)
View(r2)

# LHH
l2 <- l %>%
  mutate(
    count = paste0(ball_count, "-", strike_count),
    is_hit = code %in% c(20, 21, 22, 23),  
    is_SO = code %in% c(4),
    is_HR = code %in% c(23),
    is_BB = code %in% c(18),
    is_on_base = bat_dest > 0                          
  ) %>%
  # Remove NA counts or incomplete data
  filter(!is.na(ball_count), !is.na(strike_count)) %>%
  group_by(count) %>%
  summarize(
    batting_avg = round(mean(is_hit, na.rm = TRUE), 3),
    SO_rate = round(mean(is_SO, na.rm = TRUE), 3),
    HR_rate = round(mean(is_HR, na.rm = TRUE), 3),
    BB_rate = round(mean(is_BB, na.rm = TRUE), 3),
    on_base_rate = round(mean(is_on_base, na.rm = TRUE), 3),
    attempts = n()
  ) %>%
  arrange(count)
View(l2)



### location vs outcomes
# RHH
r2_location <- r %>%
  mutate(
    is_hit = code %in% c(20, 21, 22, 23),  
    is_SO = code %in% c(4),
    is_HR = code %in% c(23),
    is_BB = code %in% c(18),
    is_on_base = bat_dest > 0                          
  ) %>%
  filter(!is.na(Location)) %>%
  group_by(Location) %>%
  summarize(
    batting_avg = round(mean(is_hit, na.rm = TRUE), 3),
    SO_rate = round(mean(is_SO, na.rm = TRUE), 3),
    HR_rate = round(mean(is_HR, na.rm = TRUE), 3),
    BB_rate = round(mean(is_BB, na.rm = TRUE), 3),
    on_base_rate = round(mean(is_on_base, na.rm = TRUE), 3),
    attempts = n()
  ) %>%
  arrange(Location)
View(r2_location)

# LHH
l2_location <- l %>%
  mutate(
    is_hit = code %in% c(20, 21, 22, 23),  
    is_SO = code %in% c(4),
    is_HR = code %in% c(23),
    is_BB = code %in% c(18),
    is_on_base = bat_dest > 0                          
  ) %>%
  filter(!is.na(Location)) %>%
  group_by(Location) %>%
  summarize(
    batting_avg = round(mean(is_hit, na.rm = TRUE), 3),
    SO_rate = round(mean(is_SO, na.rm = TRUE), 3),
    HR_rate = round(mean(is_HR, na.rm = TRUE), 3),
    BB_rate = round(mean(is_BB, na.rm = TRUE), 3),
    on_base_rate = round(mean(is_on_base, na.rm = TRUE), 3),
    attempts = n()
  ) %>%
  arrange(Location)
View(l2_location)


### pitch vs outcomes
# RHH
r2_pitch<- r %>%
  mutate(
    is_hit = code %in% c(20, 21, 22, 23),  
    is_SO = code %in% c(4),
    is_HR = code %in% c(23),
    is_BB = code %in% c(18),
    is_on_base = bat_dest > 0                          
  ) %>%
  filter(!is.na(Pitch)) %>%
  group_by(Pitch) %>%
  summarize(
    batting_avg = round(mean(is_hit, na.rm = TRUE), 3),
    SO_rate = round(mean(is_SO, na.rm = TRUE), 3),
    HR_rate = round(mean(is_HR, na.rm = TRUE), 3),
    BB_rate = round(mean(is_BB, na.rm = TRUE), 3),
    on_base_rate = round(mean(is_on_base, na.rm = TRUE), 3),
    attempts = n()
  ) %>%
  arrange(Pitch)
View(r2_pitch)
# LHH
l2_pitch <- l %>%
  mutate(
    is_hit = code %in% c(20, 21, 22, 23),  
    is_SO = code %in% c(4),
    is_HR = code %in% c(23),
    is_BB = code %in% c(18),
    is_on_base = bat_dest > 0                          
  ) %>%
  filter(!is.na(Pitch)) %>%
  group_by(Pitch) %>%
  summarize(
    batting_avg = round(mean(is_hit, na.rm = TRUE), 3),
    SO_rate = round(mean(is_SO, na.rm = TRUE), 3),
    HR_rate = round(mean(is_HR, na.rm = TRUE), 3),
    BB_rate = round(mean(is_BB, na.rm = TRUE), 3),
    on_base_rate = round(mean(is_on_base, na.rm = TRUE), 3),
    attempts = n()
  ) %>%
  arrange(Pitch)
View(l2_pitch)



#---------------------------------------------
# correlation
#---------------------------------------------
pbp <- subset(pbp, select = -pitch_count)
numeric_data <- pbp[sapply(pbp, is.numeric)]
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
cor_matrix

threshold <- 0.5  # you can adjust this
high_cor <- which(abs(cor_matrix) > threshold & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor_pairs <- unique(t(apply(high_cor, 1, sort)))
for (i in 1:nrow(high_cor_pairs)) {
  row <- high_cor_pairs[i, 1]
  col <- high_cor_pairs[i, 2]
  cat(rownames(cor_matrix)[row], " & ", colnames(cor_matrix)[col], 
      ": ", cor_matrix[row, col], "\n")
}

# heatmap
library(corrplot)

corrplot(cor_matrix,
         method = "color",      # colored tiles
         type   = "upper",      # upper triangle only
         order  = "hclust",     # cluster similar vars
         addCoef.col = NA,      # set to "black" to print r values on tiles
         tl.cex = 0.8,          # axis label size
         col = colorRampPalette(c("blue", "white", "red"))(200))








