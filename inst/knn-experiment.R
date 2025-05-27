library(cdcfluview)
library(ggplot2)

ma_dat <- cdcfluview::ilinet(region = "state", years = 2010:2019) |>
  filter(region == "Massachusetts") |>
  mutate(season = ifelse(week < 40,
                         paste0(year - 1, "/", year),
                         paste0(year, "/", year+1))
         )

ggplot(ma_dat) +
  geom_path(aes(x = week_start, y = unweighted_ili))


## loop through each week in the dataset, starting with the second season and return the kNN predictions

## end result is a dataframe with columns for
##  method: seasonal, distance and uniform
##  k: values from 1 to 10 by 1, then 15 to 100 by 5
##  h: 1 to 5

start_idx <- 53

knn_data <- data.frame(
  week_start = ma_dat$week_start[start_idx:nrow(ma_dat)],
  season = ma_dat$season[start_idx:nrow(ma_dat)],
  unweighted_ili = ma_dat$unweighted_ili[start_idx:nrow(ma_dat)],
  h = NA,
  k = NA,
  seasonal_pred = NA,
  distance_pred = NA,
  uniform_pred = NA
)

for(t in start_idx:(nrow(ma_dat)-1)) {
  h = 1
  k = 20
  knn_data[t-52, "seasonal_pred"] <- return_knn_preds(y=ma_dat$unweighted_ili[1:(t-1)], h=h, k=k, method = "seasonal", rho=pi/52, eta=1/10)$pred
  knn_data[t-52, "distance_pred"] <- return_knn_preds(y=ma_dat$unweighted_ili[1:(t-1)], h=h, k=k, method = "distance", p=4)$pred
  knn_data[t-52, "uniform_pred"] <- return_knn_preds(y=ma_dat$unweighted_ili[1:(t-1)], h=h, k=k, method = "uniform")$pred
}

knn_data$seasonal_sq_error <- (knn_data$seasonal_pred - knn_data$unweighted_ili)^2
knn_data$distance_sq_error <- (knn_data$distance_pred - knn_data$unweighted_ili)^2
knn_data$uniform_sq_error <- (knn_data$uniform_pred - knn_data$unweighted_ili)^2

knn_data_summary <- knn_data |>
  group_by(season) |>
  summarise(
    seasonal_mse = mean(seasonal_sq_error, na.rm = TRUE),
    distance_mse = mean(distance_sq_error, na.rm = TRUE),
    uniform_mse = mean(uniform_sq_error, na.rm = TRUE)
  ) |>
  mutate(
    scaled_seasonal_mse = seasonal_mse/uniform_mse,
    scaled_distance_mse = distance_mse/uniform_mse,
    scaled_uniform_mse = uniform_mse/uniform_mse)

ggplot(knn_data_summary) +
  geom_point(aes(x= season, y = scaled_seasonal_mse), color = "blue") +
  geom_point(aes(x= season, y = scaled_distance_mse), color = "red") +
  geom_point(aes(x= season, y = scaled_uniform_mse), color = "green")


ggplot(knn_data) +
  geom_point(aes(x= week_start, y = seasonal_sq_error), color = "blue") +
  geom_point(aes(x= week_start, y = distance_sq_error), color = "red") +
  scale_y_log10()


Tind = nrow(ma_dat)

y_T <- ma_dat$unweighted_ili[Tind]
one_seasonal_pred <- return_knn_preds(y=ma_dat$unweighted_ili[-Tind], h=1, k=50, method = "seasonal", rho=pi/52, eta=1/10)

one_distance_pred <- return_knn_preds(y=ma_dat$unweighted_ili[-Tind], h=1, k=50, method = "distance", p=4)

## check
y_T
one_seasonal_pred$pred
one_distance_pred$pred

