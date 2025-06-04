## examine the analogues of one point

source("R/knn-utils.R")

ma_dat <- cdcfluview::ilinet(region = "state", years = 2010:2019) |>
  filter(region == "Massachusetts") |>
  mutate(season = ifelse(week < 40,
                         paste0(year - 1, "/", year),
                         paste0(year, "/", year+1))
  )


idx <- 478 ## week 48, 20219

ma_dat[idx, ]

## get the analogues
analogue_data <- return_knn_preds(
  y = ma_dat$unweighted_ili[1:(idx - 1)],
  h = 4,
  k = 10,
  method = "distance",
  p = 1
)

ggplot(ma_dat) +
  geom_path(aes(x = week_start, y = unweighted_ili)) +
  ## plot the index point
  geom_point(data = ma_dat[idx, ],
             aes(x = week_start, y = unweighted_ili),
             color = "blue", size = 3) +
  geom_point(data = ma_dat[analogue_data[["nn_indices"]]$topk_indices, ],
             aes(x = week_start, y = unweighted_ili),
             color = "red", size = 3)
