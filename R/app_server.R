#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  app_data <- shiny::reactive({

    # variaveis_cat <- function() {
    #   list(
    #     "Cargo" = "cargo_pretendido",
    #     "Partido" = "partido",
    #     "UF" = "uf",
    #     "Municipio" = "municipio",
    #     "Gênero" = "genero",
    #     "Conteúdo original (não RT)" = "tweet_original",
    #     "Xingamento" = "xingamento"
    #   )
    # }

    filter_in <- function(.data, col, input) {
      if (length(input[[col]]) > 0) {
        return(dplyr::filter(.data, .data[[col]] %in% input[[col]]))
      } else {
        return(.data)
      }
    }

    filter_data <- function(.data, input) {
      .data %>% dplyr::filter(
        dplyr::between(
          lubridate::as_date(data),
          lubridate::as_date(input$data[1]),
          lubridate::as_date(input$data[2])))
    }

    conteudo_filtrado <- purrr::reduce(
      variaveis_cat(), filter_in, input, .init = monitora::da_tidy
    ) %>% filter_data(input)
    xingamento_filtrado <- purrr::reduce(
      variaveis_cat(), filter_in, input, .init = monitora::da_xingamento
    ) %>% filter_data(input)

    list(
      conteudo = conteudo_filtrado,
      xingamento = xingamento_filtrado
    )

  })

  mod_twitter_server("twitter_ui_1", app_data)
  mod_instagram_server("instagram_ui_1", app_data)
  mod_youtube_server("youtube_ui_1", app_data)
  mod_xingamento_server("xingamento_ui_1", app_data)


}
