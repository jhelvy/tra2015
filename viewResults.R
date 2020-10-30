library('logitr')

models <- readRDS(file.path('results', 'models.Rds'))

# View summaries of MNL models
summary(models$mnl_pref_china)
summary(models$mnl_wtp_china)
summary(models$mnl_pref_us)
summary(models$mnl_wtp_us)

# View summaries of MXL models
summary(models$mxl_wtp_china)
summary(models$mxl_wtp_us)
