# library(tidyverse)
# devtools::load_all()



# Vídeos ------------------------------------------------------------------

yt <- get_youtube("~/Desktop/data-raw/turno_1/Youtube/semana8_15nov2020/")
dplyr::glimpse(yt)

yt_tidy <- tidy_youtube(yt)
dplyr::glimpse(yt_tidy$yt)
dplyr::glimpse(yt_tidy$yt_recebidos)



# Padroniza da_tidy -------------------------------------------------------

yt_tidy_yt <- yt_tidy$yt %>%
  dplyr::transmute(
    status_id = video_id,
    author = autor,
    text = paste0("[", titulo, "] ", descricao),
    tipo_publicacao = type,
    data = lubridate::ymd(data),
    likes = total_likes,
    rts = NA_integer_,
    link = link_video,
    handle = autor,
    path, original = TRUE,
    id, candidata, partido, uf, cidade, cargo_pretendido,
    genero, raca, cis_trans_lgbtqa, nicho_educacao_religiao_feminista_etc,
    negras_indigenas, lgbt, brancas_cis_hetero,
    rede_social = "youtube",
    turno
  )

yt_tidy_yt_comentarios <- yt_tidy$yt_recebidos %>%
  dplyr::transmute(
    status_id = video_id,
    author,
    text = content,
    tipo_publicacao = type,
    data,
    likes = total_likes,
    rts = NA_integer_,
    link = link_comentarios,
    handle = cand,
    path = path.x, original = TRUE,
    id, candidata, partido, uf, cidade, cargo_pretendido,
    genero, raca, cis_trans_lgbtqa, nicho_educacao_religiao_feminista_etc,
    negras_indigenas, lgbt, brancas_cis_hetero,
    rede_social = "youtube",
    turno
  )
dplyr::glimpse(yt_tidy_yt)
yt_tidy_yt_comentarios <- yt_tidy_yt_comentarios %>%
  dplyr::mutate(
    data = lubridate::ymd(data),
    likes = as.integer(likes)
  ) %>%
  dplyr::filter(!is.na(data))

da_yt_tidy <- dplyr::bind_rows(yt_tidy_yt, yt_tidy_yt_comentarios) %>%
  dplyr::distinct()

dplyr::glimpse(da_yt_tidy)
da_text_novo <- da_tidy %>%
  dplyr::bind_rows(da_yt_tidy) %>%
  dplyr::mutate(id_texto = ifelse(is.na(id_texto), dplyr::row_number(), id_texto)) %>%
  dplyr::filter(!is.na(text)) %>%
  dplyr::select(id_texto, text)
readr::read_rds("data-raw/da_texto.rds") %>%
  dplyr::bind_rows(da_text_novo) %>%
  readr::write_rds("data-raw/da_texto.rds")

da_yt_tidy <- da_tidy %>%
  dplyr::bind_rows(da_yt_tidy) %>%
  dplyr::mutate(id_texto = ifelse(is.na(id_texto), dplyr::row_number(), id_texto)) %>%
  dplyr::filter(!is.na(text)) %>%
  add_xingamentos()

da_tidy <- da_yt_tidy %>%
  dplyr::select(-text) %>%
  dplyr::bind_rows(da_tidy, .)

usethis::use_data(da_tidy, overwrite = T)

da_yt_xingamentos <- da_yt_tidy %>%
  dplyr::filter(xingamento == "Sim") %>%
  add_lista_xingamentos()

da_xingamento <- da_xingamento %>%
  dplyr::bind_rows(da_yt_xingamentos %>% dplyr::select(-id_texto))
usethis::use_data(da_xingamento, overwrite = T)

# da_yt_tidy <- dplyr::bind_rows(da_yt_tidy1, da_yt_tidy)
# da_yt_tidy <- da_yt_tidy %>% add_xingamentos()
# readr::write_rds(da_yt_tidy, "data-raw/da_yt_tidy.rds")
#
# da_tidy <- da_tidy %>% dplyr::filter(rede_social != "youtube") %>%
#   dplyr::bind_rows(da_yt_tidy)
#
# usethis::use_data(da_tidy, overwrite = TRUE)
