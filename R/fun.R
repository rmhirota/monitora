
#' Data extração
#'
#' Extrai data a partir de path do arquivo
#'
#' @param csv path do arquivo de Twitter
#' @param rede c("tweets", "instagram")
#'
#' @return date
#'
data_extracao <- function(csv, rede = "tweets") {
  re <- paste0("/", rede, "_(.*?)/@")
  csv %>%
    stringr::str_extract(re) %>%
    dirname() %>% basename() %>%
    stringr::str_extract_all("\\d{2}[:alpha:]{3}") %>%
    unlist() %>%
    paste0("2020") %>%
    lubridate::dmy()
}

#' Handle
#'
#' Extrai nome a partir de path do arquivo
#'
#' @param csv path do arquivo de Twitter
#'
#' @return string
#'
handle <- function(csv) {
  csv %>%
    dirname() %>%
    basename() %>%
    stringr::str_remove("@")
}

#' Tipo tweet
#'
#' Extrai tipo de arquivo de tweet
#'
#' @param csv path do arquivo de Twitter
#'
#' @return string
#'
tipo_tweet <- function(csv) {
  arq <- csv %>% basename()
  if (stringr::str_detect(arq, "(?<=_)tweets_.*(?=\\.csv)")) {
    tipo <- arq %>% stringr::str_extract("(?<=_)tweets_.*(?=\\.csv)")
  } else if (stringr::str_detect(arq, "(?<=_)re(plies|tweets)_.*(?=\\.csv)")) {
    tipo <- arq %>% stringr::str_extract("(?<=_)re(plies|tweets)_.*(?=\\.csv)")
  } else {
    tipo <- arq %>% stringr::str_extract("(?<=_)re(plies|tweets)(?=\\.csv)")
  }

}

#' Get tweets
#'
#' Tidy arquivo de tweets
#'
#' @param csv arquivo com dados de Twitter
#'
#' @return tibble
#'
get_tweets_ <- function(csv) {
  data.table::fread(
    csv, colClasses = rep("character", 7),
    fill = TRUE
  ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      path = csv,
      tipo_tweet = tipo_tweet(csv),
      handle = handle(csv),
      data_extracao = data_extracao(csv)
    )
}

#' Get tweets - diretório
#'
#' Tidy diretório de csvs de tweets
#'
#' @param arq diretório com arquivos relacionados a Twitter
#'
#' @return tibble
#'
get_tweets <- function(arq) {
  csvs <- fs::dir_ls(arq, glob = "*.csv", recurse = TRUE)
  base <- csvs %>%
    purrr::map_df(get_tweets_)
  if ("retweet" %in% names(base)) {
    base <- base %>%
      dplyr::transmute(
        id, date, author, text, link, path, tipo_tweet, handle, data_extracao,
        favorite_count = dplyr::coalesce(likes, favorite_count),
        retweet_count = dplyr::coalesce(retweets, retweet_count, retweet)
      )
  } else {
    base <- base %>%
      dplyr::transmute(
        id, date, author, text, link, path, tipo_tweet, handle, data_extracao,
        favorite_count = dplyr::coalesce(likes, favorite_count),
        retweet_count = dplyr::coalesce(retweets, retweet_count)
      )
  }
}

#' Get instagram
#'
#' Tidy dados de instagram
#'
#' @param csv arquivo com dados de Instagram
#'
#' @return tibble
#'
get_instagram_ <- function(csv) {
  data.table::fread(csv, colClasses = rep("character", 6), fill = TRUE) %>%
    dplyr::as_tibble() %>%
    setNames(c("user", "content", "type", "data", "likes", "link")) %>%
    dplyr::mutate(
      handle = handle(csv),
      path = csv
    )
}

#' Get instagram - diretório
#'
#' Tidy dados de instagram
#'
#' @param arq diretório com arquivos relacionados a Instagram
#'
#' @return tibble
#'
get_instagram <- function(arq) {
  csvs <- fs::dir_ls(arq, glob = "*.csv", recurse = TRUE)
  base <- csvs %>%
    purrr::map_df(get_instagram_)
}

#' Get instagram
#'
#' Tidy dados de instagram
#'
#' @param csv arquivo com dados de Instagram separados por ;
#'
#' @return tibble
#'
get_instagram2_ <- function(csv) {
  readr::read_csv2(csv) %>%
    setNames(c("user", "content", "type")) %>%
    dplyr::mutate(
      handle = handle(csv),
      data = data_extracao(csv, "instagram"),
      path = csv
    )
}


#' Get youtube - diretório
#'
#' Tidy dados de youtube
#'
#' @param arq diretório com arquivos ods de YouTube
#'
#' @return tibble
#'
get_youtube <- function(arq, type = "post") {
  ods <- fs::dir_ls(arq, glob = "*.ods", recurse = TRUE)
  base <- ods %>%
    purrr::map_df(get_youtube_)
  base
}

#' Get youtube
#'
#' Tidy dados de youtube
#'
#' @param arq arquivo ods com dados de YouTube
#'
#' @return tibble
#'
get_youtube_ <- function(ods) {
  video <- readODS::read_ods(ods, col_types = NA) %>%
    setNames(c("video_id", "data", "hora", "autor", "titulo", "descricao",
               "total_visualizacoes", "total_likes", "total_dislikes",
               "total_comentarios", "link_video", "link_comentarios")) %>%
    dplyr::mutate(type = "post", path = ods)
  video
}

#' Get youtube comments
#'
#' Tidy dados de youtube
#'
#' @param yt_path caminho para arquivo ods com dados de YouTube
#' @param id id do vídeo
#'
#' @return tibble
#'
get_yt_comments <- function(yt_path, id) {
  cat(paste0(yt_path, "\n", id, "\n\n"))
  readODS::read_ods(yt_path, sheet = id) %>%
    setNames(c("author", "content", "data", "hour", "total_likes", "total_dislikes")) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(path = yt_path,
                  link_comentarios = id)
}


#' Get youtube channel name
#'
#' Tidy dados de youtube
#'
#' @param link id do canal de YouTube
#'
#' @return nome do canal
#'
get_yt_channel <- function(link) {
  r <- httr::GET(link)
  r %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//link[@itemprop='name']") %>%
    xml2::xml_attr("content")
}

