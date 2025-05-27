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
maxh <- 5

knn_data <- expand.grid(
  data_idx = start_idx:(nrow(ma_dat)-maxh),
  h = 1:5,
  k = c(1:10, seq(15, 40, by = 5)),
  method = c("seasonal", "distance", "uniform"),
  stringsAsFactors = FALSE
) |>
  mutate(
    pred = NA
  )

# knn_data <- data.frame(
#   week_start = ma_dat$week_start[start_idx:nrow(ma_dat)],
#   season = ma_dat$season[start_idx:nrow(ma_dat)],
#   unweighted_ili = ma_dat$unweighted_ili[start_idx:nrow(ma_dat)],
#   h = NA,
#   k = NA,
#   seasonal_pred = NA,
#   distance_pred = NA,
#   uniform_pred = NA
# )

for(i in 1:nrow(knn_data)) {
  if(i%%1000==0)
    print(paste0("Processing row ", i, " of ", nrow(knn_data), "::", Sys.time()))
  idx <- knn_data$data_idx[i]
  knn_data[i, "pred"] <- return_knn_preds(y=ma_dat$unweighted_ili[1:(idx-1)],
                                          h=knn_data$h[i],
                                          k=knn_data$k[i],
                                          method = knn_data$method[i],
                                          p=4,
                                          rho=pi/52,
                                          eta=1/10)$pred
}

knn_data$unweighted_ili <- ma_dat$unweighted_ili[knn_data$data_idx]
knn_data$sq_error <- (knn_data$pred - knn_data$unweighted_ili)^2
knn_data$season <- ma_dat$season[knn_data$data_idx]

knn_data_summary <- knn_data |>
  group_by(season, method, k) |>
  summarise(mse = mean(sq_error, na.rm = TRUE)) |>
  group_by(season, k) |>
  ## scale mse by mse value
  mutate(
    scaled_mse = mse/ mse[method == "seasonal"]
  )

## plotting scaled seasonal summaries faceted by k
ggplot(knn_data_summary) +
  geom_point(aes(x= season, y = scaled_mse, color = method)) +
  scale_y_log10() +
  facet_wrap(~k)

## plotting scaled seasonal summaries by k, faceted by method
ggplot(knn_data_summary, aes(x= k, y = scaled_mse, color = season)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  scale_y_log10() +
  scale_color_brewer(type = "seq") +
  facet_wrap(~method)

## plotting UNscaled seasonal summaries by k, faceted by method
ggplot(knn_data_summary, aes(x= k, y = mse, color = season)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  scale_y_log10() +
  scale_color_brewer(type = "seq") +
  facet_wrap(~method)


ggplot(knn_data_summary) +
  geom_point(aes(x=season, y = mse, color = k)) +
  scale_y_log10() +
  scale_color_gradient() +
  facet_wrap(~method)

