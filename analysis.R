data <- read.csv("NTAD_National_Bridge_Inventory.csv")
cols <- data.frame(colnames(data))

library(dplyr)
library(tidyverse)

new_dat <- data %>%
  filter(!is.na(STATE_CODE_001) & as.numeric(STATE_CODE_001) %in% c(19, 31))


# Define bounding box
lat_min <- 41.0
lat_max <- 41.5
lon_min <- 96.2
lon_max <- 95.8

new_dat$LAT_016 <- as.numeric(new_dat$LAT_016) / 1e6
new_dat$LONG_017 <- as.numeric(new_dat$LONG_017) / 1e6

# Filter the dataset
omaha_bridges <- new_dat %>%
  filter(LAT_016 >= lat_min & LAT_016 <= lat_max &
           LONG_017 >= lon_max & LONG_017 <= lon_min)



fil_dat <- omaha_bridges %>%
  select(c(1,2,5,15,21,25, 28:29,31:32, 68:77,88,103,107,112,116, 122:125))
col <- data.frame(colnames(fil_dat)) 

fil_dat <- fil_dat %>%
  mutate(across(everything(), ~ ifelse(. == "N", NA, .)))

filtered <- fil_dat %>%
  # Group by OBJECTID first
  group_by(OBJECTID) %>%
  summarise(
    avg_rate = mean(
      c(
        as.numeric(DECK_COND_058), 
        as.numeric(SUPERSTRUCTURE_COND_059), 
        as.numeric(SUBSTRUCTURE_COND_060),
        as.numeric(CULVERT_COND_062),
        as.numeric(CHANNEL_COND_061),
        as.numeric(STRUCTURAL_EVAL_067)
      ), 
      na.rm = TRUE
    ),
    
    avg_inv_op = mean(
      c(as.numeric(OPERATING_RATING_064), as.numeric(INVENTORY_RATING_066)),
      na.rm = TRUE
    ),
    avg_traffic = ADT_029,
    percent_truck = PERCENT_ADT_TRUCK_109
    
    )
    groups = "drop"  # Drops the grouping after summarizing
  )

print(filtered)


high_risk_bridges <- filtered %>%
  filter(
    avg_rate < 6 &
    avg_inv_op < 50
  )


collapse <- filtered%>%
  filter(
    avg_rate <5 &
      avg_inv_op < 45 &
      avg_traffic > 100
    
  )

collapse_now <- filtered %>%
  filter(
    avg_rate < 4.5 &
      avg_inv_op < 30 &
      avg_traffic > 150 &
      percent_truck > 10
  )

Do_not_drive <- filtered %>%
  filter(
    avg_rate <4 &
      avg_inv_op < 25
  )

Casualties <- filtered %>%
  filter(
    avg_rate < 6 &
    avg_inv_op < 50 &
    avg_traffic > 1000 &
      percent_truck > 10
  )

percents <- data.frame(
  "high risk bridges" = nrow(high_risk_bridges)/nrow(fil_dat),
  "Could collapse" = nrow(collapse)/nrow(fil_dat),
  "collapsing soon" = nrow(collapse_now)/nrow(fil_dat)
)

row.names(percents)[row.names(percents) == "1"] <- "Proportion of Bridges at Risk of Collapse"

table <- t(percents)



