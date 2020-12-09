# Tidy

utils::data("cand", package = "monitora", envir = environment())

tidy_twitter <- function(tw) {
  cat(" ---- NAs ----\n\n")

  tw %>% dplyr::filter(is.na(text)) %>% nrow() %>%
    paste0("text:\t", ., " observações NA\n") %>% cat()
  tw %>% dplyr::filter(is.na(link)) %>% nrow() %>%
    paste0("link:\t", ., " observações NA\n") %>% cat()
  tw %>% dplyr::filter(is.na(date)) %>% nrow() %>%
    paste0("date:\t", ., " observações NA\n") %>% cat()
  tw %>% dplyr::filter(is.na(tipo_tweet)) %>% nrow() %>%
    paste0("tipo_tweet:\t", ., " observações NA\n") %>% cat()

  cat("\n\n ---- data ----\n\n")
  tw_tidy <- tw %>%
    dplyr::filter(date != "date") %>%
    dplyr::mutate(data = lubridate::as_datetime(date)) %>%
    dplyr::select(-date)

  cat("\n\n ---- padronização author, handle, counts ----\n\n")
  tw_tidy <- tw_tidy %>%
    dplyr::mutate(
      author = stringr::str_to_lower(author),
      handle = stringr::str_to_lower(handle),
      # likes e RTs numéricos
      favorite_count = as.integer(favorite_count),
      retweet_count = as.integer(retweet_count)
    ) %>%
    rename(status_id = id)

  cat("\n\n ---- arrumações manuais  ----\n\n")
  tw_tidy <- tw_tidy %>%
    dplyr::mutate(handle = dplyr::case_when(
      handle == "dasilvabenedita1" ~ "dasilvabenedita",
      handle == "dhoratorio" ~ "mandahoratorio",
      handle == "tatimaciel" ~ "tatimaciel_novo",
      handle == "deputadaleninha" ~ "leninhamoc13",
      handle == "marcela_prest" ~ "marcelaprest50",
      handle == "eleniramtm" ~ "elenira13013",
      handle == "ivan_duarte" ~ "ivan_duarte_pt",
      TRUE ~ handle
    ))

  cat("\n\n ---- marca RTs  ----\n\n")
  tw_tidy <- tw_tidy %>%
    dplyr::mutate(tweet_original = dplyr::case_when(
      stringr::str_detect(text, regex("^RT ")) ~ FALSE,
      !stringr::str_detect(text, regex("^RT ")) ~ TRUE
    ))

  cat("\n\n ---- junta com candidatas  ----\n\n")
  tw_cand <- cand %>%
    dplyr::filter(!is.na(twitter)) %>%
    dplyr::mutate(twitter = stringr::str_to_lower(twitter)) %>%
    dplyr::select(id, twitter)

  tw_tidy <- tw_tidy %>%
    left_join(tw_cand, c("handle" = "twitter")) %>%
    left_join(cand, "id") %>%
    dplyr::select(-c(instagram, youtube))

  tw_tidy
}


tidy_instagram <- function(ig) {
  cat(" ---- NAs ----\n\n")
  ig %>% dplyr::filter(is.na(user)) %>% nrow() %>%
    paste0("user:\t", ., " observações NA\n") %>% cat()
  ig %>% dplyr::filter(is.na(content)) %>% nrow() %>%
    paste0("content:\t", ., " observações NA\n") %>% cat()
  ig %>% dplyr::filter(is.na(data)) %>% nrow() %>%
    paste0("data:\t", ., " observações NA\n") %>% cat()
  ig %>% dplyr::filter(is.na(likes)) %>% nrow() %>%
    paste0("likes:\t", ., " observações NA\n") %>% cat()
  ig %>% dplyr::filter(is.na(link)) %>% nrow() %>%
    paste0("link:\t", ., " observações NA\n") %>% cat()

  ig %>% dplyr::filter(user == "") %>% nrow() %>%
    paste0("user:\t", ., " observações vazias\n") %>% cat()
  ig %>% dplyr::filter(is.na(content)) %>% nrow() %>%
    paste0("content:\t", ., " observações vazias\n") %>% cat()
  ig %>% dplyr::filter(data == "") %>% nrow() %>%
    paste0("data:\t", ., " observações vazias\n") %>% cat()
  ig %>% dplyr::filter(likes == "") %>% nrow() %>%
    paste0("likes:\t", ., " observações vazias\n") %>% cat()
  ig %>% dplyr::filter(link == "") %>% nrow() %>%
    paste0("link:\t", ., " observações vazias\n") %>% cat()

  cat(" ---- data ----\n\n")
  ig_tidy <- ig %>% dplyr::mutate(data = dplyr::na_if(data, "")) %>%
    tidyr::fill(data, .direction = "up") %>%
    dplyr::mutate(data = lubridate::ymd(data))

  cat(" ---- padronização likes, handle, user ----\n\n")
  ig_tidy <- ig_tidy %>%
    dplyr::mutate(
      likes = str_extract(likes, "[0-9]*") %>% as.integer(),
      handle = str_to_lower(handle),
      user = str_to_lower(user),
    )

  cat(" ---- arrumações manuais ----\n\n")
  ig_tidy <- ig_tidy %>%
    dplyr::mutate(
      handle = dplyr::case_when(
        handle == "profa_alessandradepaula" ~ "profa_alessandradepaulaoficial",
        handle == "liasmbarroso" ~ "liasmbarroso_77",
        handle == "mariadocarmostm" ~ "mariadcarmolara",
        handle == "norma_venancio" ~ "norma_venancio_",
        TRUE ~ handle
      ),
      user = dplyr::case_when(
        user == "profa_alessandradepaula" ~ "profa_alessandradepaulaoficial",
        user == "liasmbarroso" ~ "liasmbarroso_77",
        user == "mariadocarmostm" ~ "mariadcarmolara",
        user == "norma_venancio" ~ "norma_venancio_",
        TRUE ~ user
      )
    )

  cat("\n\n ---- junta com candidatas  ----\n\n")
  ig_cand <- cand %>%
    dplyr::filter(!is.na(instagram)) %>%
    dplyr::transmute(id, instagram = stringr::str_remove_all(instagram, "/") %>% tolower())

  ig_tidy <- ig_tidy %>%
    dplyr::mutate(handle = tolower(handle)) %>%
    dplyr::left_join(ig_cand, c("handle" = "instagram")) %>%
    dplyr::left_join(cand, "id") %>%
    dplyr::select(-youtube, -twitter) %>%
    tidyr::fill(link) %>%
    ig_tidy

}

tidy_youtube <- function(yt) {
  cat(" ---- arquivo repetido ----\n\n")
  yt_tidy <- yt %>%
    dplyr::filter(!stringr::str_detect(path, "copy"))

  cat(" ---- números ----\n\n")
  # Números
  yt_tidy <- yt_tidy %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("total"), as.integer),
      dplyr::across(dplyr::starts_with("total"), ~tidyr::replace_na(., 0))
    )

  cat(" ---- autor ----\n\n")
  # Autor
  yt_tidy <- yt_tidy %>%
    dplyr::mutate(autor = stringr::str_remove_all(autor, stringr::regex('["\\\\]')))

  sem_comentarios <- yt_tidy %>%
    dplyr::count(link_comentarios) %>%
    dplyr::filter(n > 1) %>%
    with(link_comentarios)

  yt_comentarios <- yt_tidy %>%
    dplyr::filter(!link_comentarios %in% sem_comentarios) %>%
    dplyr::mutate(link_comentarios = stringr::str_remove_all(link_comentarios, "#")) %>%
    dplyr::transmute(path, video_id, link_comentarios, cand = autor) %>%
    dplyr::mutate(link_comentarios = dplyr::case_when(
      link_comentarios == "" ~ video_id,
      TRUE ~ link_comentarios
    )) %>%
    dplyr::filter(!stringr::str_detect(video_id, "3gApeJdQx20"))

  yt_com <- purrr::map2_dfr(
    yt_comentarios$path,
    yt_comentarios$link_comentarios,
    get_yt_comments
  ) %>%
    dplyr::mutate(type = "comment")

  cat(" ---- vazios ----\n\n")
  yt_com <- yt_com %>% dplyr::filter(!is.na(content))
  yt_com <- yt_com %>% dplyr::left_join(yt_comentarios, "link_comentarios")

  cat("\n\n ---- junta com candidatas  ----\n\n")
  yt_cand <- cand %>%
    dplyr::filter(!is.na(youtube)) %>%
    dplyr::transmute(id, candidata, youtube)

  canal <- yt_cand$youtube %>%
    purrr::map(get_yt_channel) %>%
    unlist()

  yt_cand <- yt_cand %>% dplyr::bind_cols(channel_name = canal)
  yt_tidy <- yt_tidy %>%
    dplyr::left_join(yt_cand, by = c("autor" = "channel_name")) %>%
    dplyr::left_join(cand) %>%
    dplyr::select(-c(twitter, instagram, youtube))

  yt_com <- yt_com %>%
    dplyr::left_join(yt_cand, by = c("cand" = "channel_name")) %>%
    dplyr::left_join(cand) %>%
    dplyr::select(-c(twitter, instagram, youtube))

  list(yt = yt_tidy, yt_recebidos = yt_com)

}
