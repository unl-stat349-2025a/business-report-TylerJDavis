data <- read.csv("NTAD_National_Bridge_Inventory.csv")
cols <- data.frame(colnames(data))

library(dplyr)
library(tidyverse)

new_dat <- data %>%
  filter(!is.na(STATE_CODE_001) & as.numeric(STATE_CODE_001) %in% c(19, 31))

fil_dat <- new_dat %>%
  select(c())
  


