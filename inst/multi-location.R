## Multi-location analysis


library(tidyverse)
library(predictability)
theme_set(theme_bw())

ili_dat <- cdcfluview::ilinet(region = "state", years = 2010:2019)

ili_dat_filtered <- ili_dat |>
  select(region, year, week, unweighted_ili, week_start) |>
  group_by(region) |>
  arrange(region, week_start) |>
  mutate(
    season = ifelse(
      week < 40,
      paste0(year - 1, "/", year),
      paste0(year, "/", year + 1)
    ),
    unweighted_ili_lag1 = lag(unweighted_ili),
    smooth_unweighted_ili = zoo::rollmean(
      unweighted_ili,
      k = 4,
      fill = NA,
      align = "right"
    ),
    smooth_unweighted_ili_lag1 = lag(smooth_unweighted_ili, 1)
  )

num_cores <- (detectCores() - 1)/2 # Use all but one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

## list of regions, removing Virgin Islands, Northern Mariana Islands, PR and DC
regions <- unique(ili_dat_filtered$region) |>
  setdiff(c("Virgin Islands",
            "Commonwealth of the Northern Mariana Islands",
            "Puerto Rico",
            "District of Columbia"))
region_preds <- foreach(i = 1:length(regions), .combine = c) %dopar%
  {
    region_data <- ili_dat_filtered |> dplyr::filter(region == regions[i])
    tmp <- predictability::run_analogue_simulation(
      data = region_data,
      outcome_col = "unweighted_ili",
      h_vals = 1:6,
      start_idx = 105,
      k_vals_dist = 6,
      k_val_seas = 30
    )
    ## return a list, concatenating the region name onto each list
    list(c(tmp, region = regions[[i]]))
  }

## extract dataset with the hindcast and dist-based rsq values for each region

region_df <- purrr::map(
  region_preds,
  function(x) {
    x$rsq_data |> mutate(region = x$region)
  }
) |>
  list_rbind()

## summarize by state
region_df |>
  group_by(region) |>
  summarize(n=n()) |>
  print(n=Inf)

state_dat <- read_csv("https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/refs/heads/main/auxiliary-data/locations.csv")

## TODO: Deal with NYC, which is currently labeled as NA

## plot skills by region for a fixed horizon
region_df |>
  filter(h == 1) |>
  left_join(state_dat, by = c("region" = "location_name")) |>
  mutate(skill = rsq_dist/rsq_hindcast,
         abbreviation = reorder(abbreviation, population)) |>
  ggplot(aes(x = abbreviation)) +
  geom_point(aes(y=skill), alpha = .5) +
  ## plot mean as geom_point
  stat_summary(aes(y=skill), fun = mean, geom = "point", color = "red", shape = "x", size = 3)

## plot rsq_hindcasts by region
region_df |>
  group_by(region, pred_date_season) |>
  ## n unique rsq_hindcast values
  summarize(n = n(), nunique = length(unique(rsq_hindcast)))

region_df |>
  filter(h == 1) |>
  left_join(state_dat, by = c("region" = "location_name")) |>
  mutate(abbreviation = reorder(abbreviation, population)) |>
  ggplot(aes(x = abbreviation)) +
  geom_point(aes(y=rsq_hindcast), alpha = .5) +
  ## plot mean as geom_point
  stat_summary(aes(y=rsq_hindcast), fun = mean, geom = "point", color = "red", shape = "x", size = 3)

## TODO: explore locations with abnormally low hindcast rsq: MD and IA in particular
md_data <- ili_dat_filtered |>
  dplyr::filter(region == "Maryland")
tf <- genlasso::trendfilter(y = md_data$unweighted_ili, ord = 2)
cv_tf <- genlasso::cv.trendfilter(tf, k = 10)
par(mfrow=c(2,1))
plot(tf, lambda=cv_tf$lambda.1se, main="One standard error rule")
plot(tf, lambda=cv_tf$lambda.min, main="minimum MSE")


## TODO: explore location with h=1 skill < 0 (NE and MT in particular)

## TODO: analyze another disease
