

library(tidyverse)
library(dplyr)
library(psych)
library(knitr)

omaha_bridges <- read.csv("omaha_bridges.csv")
library(leaflet)
leaflet(omaha_bridges) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~ LONG_017,
    lat = ~ LAT_016,
    radius = 5,
    color = "red",
    popup = ~paste("Bridge ID:", FACILITY_CARRIED_007)
  )

fil_dat <- omaha_bridges %>%
  select(c(1,2,5,14,21,22,25, 28:29,31:32, 68:77,88,103,107,112,116, 122:125))
library(magrittr)
desc_dat <- data.frame(variable = names(fil_dat),
                       classes = sapply(fil_dat, typeof),
                       first_values = sapply(fil_dat, function(x) paste0(head(x),  collapse = ", ")),
                       row.names = NULL)
kable(desc_dat)

library(ggplot2)

ggplot(fil_dat,aes(x = BRIDGE_CONDITION)) + geom_bar() + labs(title ="Bridge Condition") + xlab("Bridge Rating")

fil_dat <- fil_dat %>%
  mutate(across(everything(), ~ ifelse(. == "N", NA, .)))

bridge_cond <- fil_dat %>%
  filter(
    BRIDGE_CONDITION == "P"
  ) %>%
  select(c(1,4,5,6,8,9:21,29,27,23))


leaflet(bridge_cond) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~ LONG_017,
    lat = ~ LAT_016,
    radius = 5,
    color = "red",
    popup = ~paste("Bridge ID:", FACILITY_CARRIED_007)
  )

high_traffic <- bridge_cond %>%
  filter (
    ADT_029 > 5000 & TRAFFIC_LANES_ON_028A > 2
  )
leaflet(high_traffic) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~ LONG_017,
    lat = ~ LAT_016,
    radius = 5,
    color = "red",
    popup = ~paste("Bridge ID:", FACILITY_CARRIED_007)
  )

library(psych)
desc_brid_cond <- describe(bridge_cond1)
desc_h_t <- describe(high_traffic)

kable(desc_brid_cond[, c("mean", "sd", "n","median","se")], caption = "Bridges with Poor Rating Descriptive Statistics")
kable(desc_h_t[, c("mean", "sd", "n","median","se")], caption = "High Traffic Bridges with a Rating of Poor")

percents <- data.frame(
  "bridges at risk" = nrow(bridge_cond1)/nrow(fil_dat),
  "High traffic bridges at risk" = nrow(high_traffic)/nrow(fil_dat))
kable(percents, col.names = c("Bridges at Risk", "High Traffic Bridges at Risk"))

Omaha <- fil_dat %>%
  mutate(collapse_risk = ifelse(BRIDGE_CONDITION == "P" & STRUCTURAL_EVAL_067 <= 4, 1, 0)) %>%
  select(c(1,8,12,18,26,31))

library(VIM)
one <- kNN(Omaha, k = 5)


library(caret)
train_indices <- createDataPartition(one$OBJECTID, 
                                     p = 0.8, 
                                     list = FALSE)

train_data <- one[train_indices, ]

# Testing data
test_data <- one[-train_indices, ]


# Logistic regression model
model1 <- glm(collapse_risk ~as.numeric(DECK_COND_058) + OPERATING_RATING_064 + as.numeric(YEAR_BUILT_027), 
              family = binomial(link = "logit"), data = train_data)

library(gt)
library(broom)

tidy(model1) %>%
  gt() %>%
  tab_header(
    title = "Logistic Regression Summary"
  )

predicted_probabilities <- predict(model1, newdata = test_data, type = "response")

predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
kable(table(predicted_classes),col.names = c("Value","Frequency"))

actual_classes <- test_data$collapse_risk
# Compare predicted vs actual values
comparison <- data.frame(Actual = actual_classes, Predicted = predicted_classes)

accuracy <- mean(predicted_classes == actual_classes)
kable(accuracy,col.names = c("Accuracy"))
