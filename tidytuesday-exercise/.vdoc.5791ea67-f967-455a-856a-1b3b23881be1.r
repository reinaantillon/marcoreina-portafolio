#
#
#Loading the TidyTuesday library and data

library(tidytuesdayR)
library(dplyr)
library(tidymodels)
library(tidyverse)
library(here)
library(maps)
library(mapdata)

tuesdata <- tidytuesdayR::tt_load('2026-04-14')

#beaufort_scale <- tuesdata$beaufort_scale #Not necessary for my analysis
birds <- tuesdata$birds
#sea_states <- tuesdata$sea_states #Not necessary for my analysis
ships <- tuesdata$ships
#
#
#
#
#
# I took a look at the datasets in Positron and the Data Dictionary
# I am interested in understanding seasonal changes in birds sightings


# I decided to eliminate some of the columns that were mostly empty in both datasets (>50% empty)

birds$age = NULL
birds$wan_plumage_phase = NULL
birds$plumage_phase = NULL
birds$sex = NULL
birds$n_feeding = NULL
birds$n_sitting_on_water = NULL
birds$n_sitting_on_ice = NULL
birds$n_flying_past = NULL
birds$n_accompanying = NULL
birds$n_flying_past = NULL
birds$n_following_ship = NULL
birds$moulting = NULL

ships$direction = NULL
ships$wind_direction = NULL
ships$air_temperature = NULL
ships$pressure = NULL
ships$sea_surface_temperature = NULL
ships$depth = NULL
ships$time = NULL

# I also decided to remove "not interesting (for me)" columns

birds$bird_observation_id = NULL
birds$feeding = NULL
birds$sitting_on_water = NULL
birds$sitting_on_ice = NULL
birds$sitting_on_ship = NULL
birds$flying_past = NULL
birds$accompanying = NULL
birds$following_ship = NULL
birds$naturally_feeding = NULL
birds$in_hand = NULL

ships$speed = NULL
ships$direction = NULL
ships$cloud_cover = NULL
ships$precipitation = NULL
ships$wind_speed_class = NULL
ships$sea_state_class = NULL
ships$observer = NULL
ships$census_method = NULL
ships$hemisphere = NULL
ships$activity = NULL

#
#
#
#
#
merged_df <- left_join(birds, ships, by = "record_id")

saveRDS(merged_df, here("tidytuesday-exercise/tidytuesday_df.rds"))
#
#
#
#
merged_df <- merged_df %>%
    mutate(
        aggr_spc = 1 + str_count(species_scientific_name, "/"),
        genus = word(species_scientific_name, 1),
        genus_code = substr(species_abbreviation, 1, 3),
        year  = year(date),
        month = month(date), 
        lat_bin = floor(latitude / 10) * 10, #creating larger bins for latitude and longitude
        lon_bin = floor(longitude / 10) * 10,
        count = as.numeric(count)
    ) %>%
        drop_na()

summary_genus <- merged_df %>%
    group_by(genus) %>% 
    summarise(
        count = n(), 
        .groups = "drop"
    ) %>%
    arrange(desc(count))

summary_genus %>%
    ggplot(aes(x = genus, y = count)) + geom_col() + coord_flip()
#
#
#
#
#
merged_df %>%
    filter(genus_code == "DIO") %>%
    ggplot(aes(x = year, y = count, fill = as.factor(aggr_spc))) + 
    geom_col()
#
#
#
#
#
#
#
#
#
#
#
# Get world map data
world_map <- map_data("world")

#All sightings of DIO
merged_df %>%
  filter(genus_code == "DIO") %>%
  group_by(lon_bin, lat_bin) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group), 
               fill = "gray90", color = "gray50", size = 0.1) +
  geom_raster(aes(x = lon_bin, y = lat_bin, fill = count)) +
  scale_fill_viridis_c(option = "plasma", direction = -1, alpha = 0.5) +
  coord_quickmap(xlim = c(50, 180), ylim = c(-75, -25)) +
  theme_bw()

#
#
#
#
#Sightings before 1983
merged_df %>%
  filter(genus_code == "DIO", year <= 1983) %>%
  group_by(lon_bin, lat_bin) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group), 
               fill = "gray90", color = "gray50", size = 0.1) +
  geom_raster(aes(x = lon_bin, y = lat_bin, fill = count)) +
  scale_fill_viridis_c(option = "plasma", direction = -1, alpha = 0.5) +
  coord_quickmap(xlim = c(50, 180), ylim = c(-75, -25)) + 
  theme_bw()

#Sightings after 1983
merged_df %>%
  filter(genus_code == "DIO", year > 1983) %>%
  group_by(lon_bin, lat_bin) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group), 
               fill = "gray90", color = "gray50", size = 0.1) +
  geom_raster(aes(x = lon_bin, y = lat_bin, fill = count)) +
  scale_fill_viridis_c(option = "plasma", direction = -1, alpha = 0.5) +
  coord_quickmap(xlim = c(50, 180), ylim = c(-75, -25)) + 
  theme_bw()

#
#
#
#
#
#
#Sightings before 1983 per season
merged_df %>%
  filter(genus_code == "DIO", year <= 1983) %>%
  group_by(lon_bin, lat_bin, season) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group), 
               fill = "gray90", color = "gray50", size = 0.1) +
  geom_raster(aes(x = lon_bin, y = lat_bin, fill = count)) +
  scale_fill_viridis_c(option = "plasma", direction = -1, alpha = 0.5) +
  coord_quickmap(xlim = c(50, 180), ylim = c(-75, -25)) + 
  theme_bw() +
  facet_wrap(~season)

#
#
#
#Sightings after 1983 per season
merged_df %>%
  filter(genus_code == "DIO", year > 1983) %>%
  group_by(lon_bin, lat_bin, season) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group), 
               fill = "gray90", color = "gray50", size = 0.1) +
  geom_raster(aes(x = lon_bin, y = lat_bin, fill = count)) +
  scale_fill_viridis_c(option = "plasma", direction = -1, alpha = 0.5) +
  coord_quickmap(xlim = c(50, 180), ylim = c(-75, -25)) +
  theme_bw() +
  facet_wrap(~season)
#
#
#
#
#
#Let's create the dataset of interest
trends_df <- merged_df %>%
  filter(genus_code == "DIO") %>%
  group_by(lon_bin, lat_bin, season, year, month) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    wave = if_else(year >1983, 2, 1),
    geo = paste(lon_bin, lat_bin, sep = ", ")
  )

freq_df <- trends_df %>%
  group_by(geo) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(count))

freq_df
#
#
#
#
#
#
#
#
#
#
#
#Let's calculate distance

#Setting the main coordinate bins
lat_i <- -40
lon_i <- 150

# As the Earth is a globe, we need a special formula to calculate distance from A to B
library(geosphere)

DIO_df <- merged_df %>%
  filter(genus_code == "DIO") 

DIO_df$species_scientific_name = NULL
DIO_df$species_common_name = NULL
DIO_df$species_abbreviation = NULL
DIO_df$genus_code = NULL
DIO_df$genus = NULL

DIO_df <- DIO_df %>%
  mutate(
    spreadkm = 1 + distHaversine(
      cbind(lon_i, lat_i), 
      cbind(longitude, latitude)
    )/1000,
    spread_rounded = round(spreadkm/10, digits = 0) * 10,
    wave = if_else(year >1983, 2, 1)
  ) 
#
#
#
#
#
#Historical spread
DIO_df %>%
  ggplot(aes(x = spreadkm)) + 
  geom_histogram() +
  labs(title = "HISTORICAL")

#Histograms to understand spread across both waves
DIO_df %>%
  ggplot(aes(x = spreadkm)) + 
  geom_histogram() +
  facet_wrap(~ wave) +
  labs(title = "HISTORICAL PER WAVE")

#Seasons in wave 1
DIO_df %>%
  filter(wave == 1) %>%
  ggplot(aes(x = spreadkm)) + 
  geom_histogram() +
  facet_wrap(~ season) +
  labs(title = "WAVE 1 per season")

#Seasons in wave 2
DIO_df %>%
  filter(wave == 2) %>%
  ggplot(aes(x = spreadkm)) + 
  geom_histogram() +
  facet_wrap(~ season) +
  labs(title = "WAVE 2 per season")
#
#
#
#
#
#
DIO_df <- DIO_df %>%
  mutate(
    spread_log2 = log2(spreadkm)
  ) %>%
    filter(
      !spread_log2 == 0
    )

#Let's repeat the histograms
#Historical spread
DIO_df %>%
  ggplot(aes(x = spread_log2)) + 
  geom_histogram() +
  labs(title = "HISTORICAL")

#Histograms to understand spread across both waves
DIO_df %>%
  ggplot(aes(x = spread_log2)) + 
  geom_histogram() +
  facet_wrap(~ wave) +
  labs(title = "HISTORICAL PER WAVE")

#Seasons in wave 1
DIO_df %>%
  filter(wave == 1) %>%
  ggplot(aes(x = spread_log2)) + 
  geom_histogram() +
  facet_wrap(~ season) +
  labs(title = "WAVE 1 per season")

#Seasons in wave 2
DIO_df %>%
  filter(wave == 2) %>%
  ggplot(aes(x = spread_log2)) + 
  geom_histogram() +
  facet_wrap(~ season) +
  labs(title = "WAVE 2 per season")

#
#
#
#
#
#
#
#
#
#Making number variables into factors
DIO_df %>%
  mutate(
    year = as.factor(year),
    month = as.factor(month)
  )

#Creating a null linear model
lm_null <- linear_reg() %>%
  set_engine("lm") %>% 
  fit(spread_log2 ~ 1, data = DIO_df)

#Creating models with known variables
lm_fit1 <- linear_reg() %>%
  set_engine("lm") %>% 
  fit(spread_log2 ~ wave + season + year + month + count + latitude + longitude, data = DIO_df)

lm_fit2 <- linear_reg() %>%
  set_engine("lm") %>% 
  fit(spread_log2 ~ wave + season + year + month, data = DIO_df)

#
#
#
#
#Predidiction and differences for each fit
DIO_df <- DIO_df %>%
  mutate(
    pred0 = predict(lm_null, new_data = DIO_df, type = "numeric")$.pred,
    pred1 = predict(lm_fit1, new_data = DIO_df, type = "numeric")$.pred,
    pred2 = predict(lm_fit2, new_data = DIO_df, type = "numeric")$.pred
  ) %>%
    mutate(
    res0 = pred0 - spread_log2,
    res1 = pred1 - spread_log2,
    res2 = pred2 - spread_log2
  )
#
#
#
#
ggplot(DIO_df, aes(x = spread_log2, y = pred0)) +
    geom_point() +
    theme_bw()

ggplot(DIO_df, aes(x = spread_log2, y = pred1)) +
    geom_point() +
    theme_bw()

ggplot(DIO_df, aes(x = spread_log2, y = pred2)) +
    geom_point() +
    theme_bw()

#
#
#
#
ggplot(DIO_df, aes(x = spread_log2, y = res0)) +
    geom_point() +
    theme_bw()

ggplot(DIO_df, aes(x = spread_log2, y = res1)) +
    geom_point() +
    theme_bw()

ggplot(DIO_df, aes(x = spread_log2, y = res2)) +
    geom_point() +
    theme_bw()
#
#
#
#
#
#
#Set the seed
rngseed <- 1234
set.seed(rngseed)

#Lasso fit
lasso_fit <- linear_reg(penalty = 0.1) %>%
  set_engine("glmnet") %>% 
  fit(spread_log2 ~ wave + season + year + month + count + latitude + longitude, data = DIO_df)

# Model summary
lasso_fit

# Creating prediction and residuals
DIO_df <- DIO_df %>%
  mutate(
    lasso_pred = predict(lasso_fit, new_data = DIO_df)$.pred,
    lasso_res  = lasso_pred - spread_log2
  )

# New Pred vs Actual Plot
ggplot(DIO_df, aes(x = spread_log2, y = lasso_pred)) + 
  geom_point() + 
  labs(title = "LASSO predictions") +
  theme_bw()

ggplot(DIO_df, aes(x = spread_log2, y = lasso_res)) + 
  geom_point() + 
  labs(title = "LASSO residuals") +
  theme_bw()

#
#
#
#
#
#
#Set the seed
set.seed(rngseed)

# Define the model spec
rf_spec <- rand_forest(mode = "regression") %>%
  set_engine("ranger", seed = rngseed) 

# Fit the model
rf_fit <- rf_spec %>%
  fit(spread_log2 ~ wave + season + year + month + count + latitude + longitude, data = DIO_df)

# Model summary
rf_fit

# Creating prediction and residuals
DIO_df <- DIO_df %>%
  mutate(
    rf_pred = predict(rf_fit, new_data = DIO_df)$.pred,
    rf_res  = rf_pred - spread_log2
  )

# New Pred vs Actual Plot
ggplot(DIO_df, aes(x = spread_log2, y = rf_pred)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Random Forest predictions") +
  theme_bw()

ggplot(DIO_df, aes(x = spread_log2, y = rf_res)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Random Forest residuals") +
  theme_bw()

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
