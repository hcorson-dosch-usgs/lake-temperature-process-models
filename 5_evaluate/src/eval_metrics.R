library(tidyverse)
library(lubridate)

munge_pred_obs <- function(pred_obs) {
  pred_obs %>%
    mutate(pred_diff = pred - obs,
           doy = yday(time),
           season = case_when(
             doy >= 60 & doy < 173 ~ 'spring',
             doy >= 173 & doy < 243 ~ 'summer',
             doy >= 243 & doy < 335 ~ 'fall',
             TRUE ~ 'winter'
           ),
           season = factor(season, levels=c('winter','spring','summer','fall')))
}

##### GCM evaluation metrics #####
gcm_pred_obs <- readr::read_csv('5_evaluate/out/Tallgrass/GCM_full_20220901/GCMs_matched_to_observations.csv', col_types = cols())

gcm_pred_obs_munged <- munge_pred_obs(gcm_pred_obs)
 
get_gcm_metrics <- function(pred_obs) {
  pred_obs %>%
    summarize(bias = mean(pred_diff, na.rm=TRUE),
              n_obs = n(),
              n_lakes = length(unique(site_id)))
}
  
evaluation_metrics_GCM_overall <- gcm_pred_obs_munged %>%
  group_by(driver) %>%
  get_gcm_metrics()

evaluation_metrics_GCM_seasonal <- gcm_pred_obs_munged %>%
  group_by(driver, season) %>%
  get_gcm_metrics()

evaluation_metrics_GCM <- bind_rows(
  mutate(evaluation_metrics_GCM_overall, season = 'overall', .after = 'driver'),
  evaluation_metrics_GCM_seasonal
)

readr::write_csv(evaluation_metrics_GCM, '5_evaluate/out/Tallgrass/evaluation_metrics_GCM.csv')

##### NLDAS evaluation metrics #####

nldas_pred_obs <- readr::read_csv('5_evaluate/out/Tallgrass/NLDAS_220826_run/NLDAS_matched_to_observations.csv', col_types = cols())

nldas_pred_obs_munged <- munge_pred_obs(nldas_pred_obs)

get_nldas_metrics <- function(pred_obs) {
  pred_obs %>%
    summarize(RMSE = sqrt(mean((pred_diff)^2, na.rm=TRUE)),
              bias = mean(pred_diff, na.rm=TRUE),
              n_obs = n(),
              n_lakes = length(unique(site_id)))
}

evaluation_metrics_nldas_overall <- nldas_pred_obs_munged %>%
  group_by(driver) %>%
  get_nldas_metrics()

evaluation_metrics_nldas_seasonal <- nldas_pred_obs_munged %>%
  group_by(driver, season) %>%
  get_nldas_metrics()

evaluation_metrics_nldas <- bind_rows(
  mutate(evaluation_metrics_nldas_overall, season = 'overall', .after = 'driver'),
  evaluation_metrics_nldas_seasonal
)

readr::write_csv(evaluation_metrics_nldas, '5_evaluate/out/Tallgrass/evaluation_metrics_NLDAS.csv')
