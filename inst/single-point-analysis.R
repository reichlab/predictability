## examine the analogues of one point

## illustrate how with p=2 you can see the distance in the attractor
## it's like having a ball centered at the observation

## look at it with and without smoothing

## to compute:
##  - mean/variance of distances: how "good" are the analogues
##  - mean/variance of the analogue future values: what is the uncertainty

source("R/analogue-utils.R")

ma_dat <- cdcfluview::ilinet(region = "state", years = 2010:2019) |>
  filter(region == "Massachusetts") |>
  mutate(season = ifelse(week < 40,
                         paste0(year - 1, "/", year),
                         paste0(year, "/", year+1)),
         unweighted_ili_lag1  = lag(unweighted_ili),
         smooth_unweighted_ili = zoo::rollmean(unweighted_ili, k = 4, fill = NA, align = "right"),
         smooth_unweighted_ili_lag1 = lag(smooth_unweighted_ili, 1)
  )


idx <- 478 ## row 478 is week 48, 20219
this_h <- 4

ma_dat[idx, ]

## get the analogues
analogue_data <- return_analogue_preds(
  y = ma_dat$smooth_unweighted_ili[1:idx],
  h = this_h,
  k = 10,
  method = "distance",
  p = 2
)



ggplot(ma_dat) +
  geom_path(aes(x = week_start, y = smooth_unweighted_ili)) +
  ## plot the index point
  geom_point(data = ma_dat[idx, ],
             aes(x = week_start, y = smooth_unweighted_ili),
             color = "blue", size = 3, shape = 4) +
  ## plot the analogues
  geom_point(data = ma_dat[analogue_data[["nn_indices"]]$topk_indices, ],
             aes(x = week_start, y = smooth_unweighted_ili),
             color = "red", size = 3, shape = 3) +
  ## plot the analogues' futures
  geom_point(data = ma_dat[analogue_data[["nn_indices"]]$topk_indices+this_h, ],
             aes(x = week_start, y = smooth_unweighted_ili),
             color = "red", shape = 20) +
  ## plot the prediction
  geom_point(x = ma_dat$week_start[idx + this_h],
             y = analogue_data$pred,
             color = "green", size = 3, shape = 8)


## same as above but visualizing on the t, t-1 attractor
ggplot(ma_dat) +
  geom_path(aes(y = smooth_unweighted_ili,
                x = smooth_unweighted_ili_lag1),
            alpha = .5) +
  geom_point(aes(y = smooth_unweighted_ili,
                x = smooth_unweighted_ili_lag1),
            alpha = .2) +
  ## plot the index point
  geom_point(data = ma_dat[idx, ],
             aes(y = smooth_unweighted_ili,
                 x = smooth_unweighted_ili_lag1),
             color = "blue", size = 3, shape = 4) +
  ## plot the analogues
  geom_point(data = ma_dat[analogue_data[["analogue_indices"]]$topk_indices, ],
             aes(y = smooth_unweighted_ili,
                 x = smooth_unweighted_ili_lag1),
             color = "red", size = 3, shape = 3) +
  ## plot the analogues' futures
  geom_point(data = ma_dat[analogue_data[["analogue_indices"]]$topk_indices+this_h, ],
             aes(y = smooth_unweighted_ili,
                 x = smooth_unweighted_ili_lag1),
             color = "red", shape = 20) +
  ## plot the observation
  annotate("point",
           y = ma_dat$smooth_unweighted_ili[idx + this_h],
           x = ma_dat$smooth_unweighted_ili_lag1[idx + this_h],
           color = "blue", size = 3, shape = 20) +

  ## plot the prediction
  ## this doesn't make sense because the x isn't right.
  # annotate("point",
  #          y = analogue_data$pred,
  #          x = ma_dat$smooth_unweighted_ili_lag1[idx + this_h],
  #          color = "green", size = 3, shape = 8) +
  scale_x_log10() +
  scale_y_log10()


plotly::ggplotly()


