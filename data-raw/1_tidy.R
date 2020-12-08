library(tidyverse)
devtools::load_all()

# dados
# https://www.dropbox.com/sh/rihxoyhf92cqy8d/AAClgQVnPjrvnZ45fmBTxG3Xa?dl=0

# source("data-raw/1_tidy_candidatas.R")
# source("data-raw/1_tidy_instagram.R")
# source("data-raw/1_tidy_twitter.R")
# source("data-raw/1_tidy_youtube.R")

# da_tidy <- bind_rows(da_ig_tidy, da_tw_tidy, da_yt_tidy)


# Xingamentos -------------------------------------------------------------

# aa <- dplyr::bind_rows(
#   readr::read_rds("~/Desktop/data-raw/da_tw_tidy.rds"),
#   readr::read_rds("~/Desktop/data-raw/da_yt_tidy.rds")
# )
#
# aa <- monitora::da_tidy %>%
#   dplyr::filter(!rede_social %in% c("twitter", "youtube")) %>%
#   dplyr::bind_rows(aa)
#
# da_tidy <- aa
#
# usethis::use_data(da_tidy, overwrite = TRUE, compress = "xz")
#
#
# da_xingamento <- da_tidy %>%
#   dplyr::filter(xingamento == "Sim") %>%
#   add_lista_xingamentos()
#
# usethis::use_data(da_xingamento, overwrite = TRUE)
#
#
# da_xingamento <- readr::read_rds("~/Desktop/data-raw/da_xingamento.rds")
# usethis::use_data(da_xingamento, overwrite = T)
# devtools::document()
