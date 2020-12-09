##' psj UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_instagram_ui <- function(id){
  ns <- NS(id)
  tagList(

    # infobox ----
    shiny::fluidRow(
      col_3(bs4Dash::bs4ValueBoxOutput(ns("total_publicados"), width = 12)),
      col_3(bs4Dash::bs4ValueBoxOutput(ns("total_terceiros"), width = 12)),
      col_3(bs4Dash::bs4ValueBoxOutput(ns("tx_xingamento"), width = 12)),
      col_3(bs4Dash::bs4ValueBoxOutput(ns("tx_terceiro_publicado"), width = 12))
    ),

    # primeira linha ----
    shiny::fluidRow(

      bs4Dash::tabBox(
        width = 6,
        collapsible = FALSE,
        closable = FALSE,

        id = ns("p_tempo"),

        title = "Por dia",

        bs4Dash::tabPanel(
          tabName = "Conteúdo publicado",
          highcharter::highchartOutput(ns("p_proprios")),
          downloadButton(ns("download_p_proprios"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "Comentários",
          highcharter::highchartOutput(ns("p_likes")),
          downloadButton(ns("download_p_likes"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "Xingamentos",
          highcharter::highchartOutput(ns("p_xingamentos")),
          downloadButton(ns("download_p_xingamentos"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "Taxa de xingamentos",
          highcharter::highchartOutput(ns("p_tx_xingamentos")),
          downloadButton(ns("download_p_tx_xingamentos"), "Download dados")
        )
      ),


      bs4Dash::tabBox(
        width = 6,
        collapsible = FALSE,
        closable = FALSE,

        id = ns("ranking"),

        title = "Rankings",

        bs4Dash::tabPanel(
          tabName = "Conteúdo próprio",
          highcharter::highchartOutput(ns("r_proprios")),
          downloadButton(ns("download_r_proprios"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "Comentários com e sem ofensas",
          highcharter::highchartOutput(ns("r_comentarios")),
          downloadButton(ns("download_r_comentarios"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "Mais curtidos",
          highcharter::highchartOutput(ns("r_curtidos")),
          downloadButton(ns("download_r_curtidos"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "Taxa de xingamentos",
          highcharter::highchartOutput(ns("r_tx_xingamentos")),
          downloadButton(ns("download_r_tx_xingamentos"), "Download dados")
        )
      )
    ),

    # segunda linha ----
    shiny::fluidRow(


      bs4Dash::box(
        width = 12,
        collapsible = FALSE,
        closable = FALSE,
        id = ns("v_box"),
        title = "Mais curtidos",

        reactable::reactableOutput(ns("v_posts")),
        downloadButton(ns("download_v_posts"), "Download dados")
      )
    )
  )
}

#' psj Server Functions
#'
#' @noRd
mod_instagram_server <- function(id, app_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # reactives ----

    # bases
    da_proprios <- shiny::reactive({
      app_data()$conteudo %>%
        dplyr::filter(
          rede_social == "instagram",
          author == handle
        )
    })

    da_terceiros <- shiny::reactive({
      app_data()$conteudo %>%
        dplyr::filter(
          rede_social == "instagram",
          author != handle
        )
    })

    validar_proprios <- shiny::reactive({
      shiny::validate(
        shiny::need(
          nrow(da_proprios()) > 0,
          "N\\u00e3o foi poss\\u00edvel gerar a visualiza\\u00e7\\u00e3o com os par\\u00e2metros selecionados."
        )
      )
    })
    validar_terceiros <- shiny::reactive({
      shiny::validate(
        shiny::need(
          nrow(da_terceiros()) > 0,
          "N\\u00e3o foi poss\\u00edvel gerar a visualiza\\u00e7\\u00e3o com os par\\u00e2metros selecionados."
        )
      )
    })

    validar_infobox_proprios <- shiny::reactive({
      shiny::need(nrow(da_proprios()) > 0, "<vazio>")
    })

    validar_infobox_terceiros <- shiny::reactive({
      shiny::need(nrow(da_terceiros()) > 0, "<vazio>")
    })

    # infobox ----

    output$total_publicados <- bs4Dash::renderbs4ValueBox({

      val <- nrow(da_proprios())
      val <- scales::number(val, big.mark = ".", decimal.mark = ",")

      bs4Dash::valueBox(
        subtitle = "Posts publicados",
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        icon = "instagram",
        status = "primary"

      )
    })

    output$total_terceiros <- bs4Dash::renderbs4ValueBox({

      val <- nrow(da_terceiros())
      val <- scales::number(val, big.mark = ".", decimal.mark = ",")

      bs4Dash::valueBox(
        subtitle = "Comentários",
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        icon = "comment-dots",
        status = "primary"
      )
    })

    output$tx_xingamento <- bs4Dash::renderbs4ValueBox({

      val <- validar_infobox_terceiros()

      if (is.null(val)) {
        val1 <- da_terceiros() %>% dplyr::filter(xingamento == "Sim") %>% nrow()
        val2 <- da_terceiros() %>% dplyr::filter(xingamento == "Não") %>% nrow()
        val <- val1/val2
        val <- scales::percent(val, .1)
      }

      bs4Dash::valueBox(
        subtitle = "Taxa de comentários ofensivos",
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        icon = "exclamation-circle",
        status = "primary"
      )
    })

    output$tx_terceiro_publicado <- bs4Dash::renderbs4ValueBox({
      val1 <- validar_infobox_proprios()
      val2 <- validar_infobox_terceiros()

      if (is.null(val1) & is.null(val2)) {

        val <- nrow(da_terceiros())/nrow(da_proprios())
        val <- scales::number(val, big.mark = ".", decimal.mark = ",")
      }

      bs4Dash::valueBox(
        subtitle = "Comentários por post",
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        icon = "comments",
        status = "primary"
      )
    })


    # box tweets tempo ----
    output$p_proprios <- highcharter::renderHighchart({
      validar_proprios()
      contagem <- da_proprios() %>%
        dplyr::mutate(dia = lubridate::date(data)) %>%
        dplyr::count(dia, tipo_publicacao)

      # if (!input$desfecho_nivel) {
      #   contagem <- da() %>%
      #     dplyr::arrange(dplyr::desc(desfecho)) %>%
      #     dplyr::distinct(id_processo, .keep_all = TRUE) %>%
      #     dplyr::count(desfecho, sort = TRUE)
      # } else {
      #   contagem <- da() %>%
      #     dplyr::count(desfecho, sort = TRUE)
      # }

      contagem %>%
        linha()
    })
    output$p_likes <- highcharter::renderHighchart({
      validar_terceiros()
      likes <- da_proprios() %>%
        dplyr::mutate(dia = lubridate::date(data)) %>%
        dplyr::group_by(dia) %>%
        dplyr::summarise(likes = sum(likes, na.rm = TRUE)) %>%
        dplyr::transmute(dia, cat = "likes", n = likes)
      comentarios <- da_terceiros() %>%
        dplyr::mutate(dia = lubridate::date(data)) %>%
        dplyr::count(dia) %>%
        dplyr::transmute(dia, cat = "comentarios", n)
      contagem <- dplyr::bind_rows(likes, comentarios)

      contagem %>%
        linha()
    })
    output$p_xingamentos <- highcharter::renderHighchart({
      validar_terceiros()
      contagem <- da_terceiros() %>%
        dplyr::mutate(dia = lubridate::date(data)) %>%
        dplyr::count(dia, xingamento)

      contagem %>%
        linha()
    })
    output$p_tx_xingamentos <- highcharter::renderHighchart({
      validar_terceiros()
      contagem <- da_terceiros() %>%
        dplyr::mutate(dia = lubridate::date(data)) %>%
        dplyr::count(dia, genero, xingamento) %>%
        tidyr::pivot_wider(names_from = xingamento, values_from = n) %>%
        dplyr::mutate(Sim = tidyr::replace_na(Sim, 0)) %>%
        dplyr::mutate(prop = `Sim`/`Não`) %>%
        dplyr::select(dia, genero, prop)

      contagem %>%
        linha()
    })


    # box rankings ----
    output$r_proprios <- highcharter::renderHighchart({
      validar_proprios()

      contagem <- da_proprios() %>%
        dplyr::count(candidata, partido, uf, cidade, cargo_pretendido, sort = TRUE) %>%
        head(10)
      contagem %>%
        barras()
    })
    output$r_comentarios <- highcharter::renderHighchart({
      validar_terceiros()

      contagem <- da_terceiros() %>%
        dplyr::count(candidata, partido, uf, cidade, cargo_pretendido, xingamento) %>%
        dplyr::arrange(desc(n)) %>%
        head(20)
      contagem %>%
        barras(categ = "xingamento")
    })
    output$r_curtidos <- highcharter::renderHighchart({
      validar_terceiros()

      contagem <- da_terceiros() %>%
        dplyr::group_by(candidata, partido, uf, cidade, cargo_pretendido) %>%
        dplyr::summarise(likes = sum(likes), .groups = "drop") %>%
        dplyr::arrange(desc(likes)) %>%
        dplyr::rename(n = likes) %>%
        head(20)
      contagem %>%
        barras()
    })
    output$r_tx_xingamentos <- highcharter::renderHighchart({
      validar_terceiros()

      contagem <- da_terceiros() %>%
        dplyr::count(candidata, partido, uf, cidade, cargo_pretendido, xingamento) %>%
        tidyr::pivot_wider(names_from = xingamento, values_from = n) %>%
        dplyr::mutate(prop = `Sim`/`Não`) %>%
        dplyr::arrange(desc(prop)) %>%
        head(10)

      contagem %>%
        dplyr::mutate(lab = scales::percent(prop)) %>%
        highcharter::hchart(
          "bar",
          highcharter::hcaes(candidata, prop)
        ) %>%
        highcharter::hc_colors("#76d7c4") %>%
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = paste0(
            "<b>Quantidade</b>: {point.Sim}<br>",
            "<b>Propor\u00e7\u00e3o de xingamentos</b>: {point.lab}<br>",
            "<b>Cargo</b>: {point.cargo_pretendido}<br>",
            "<b>Partido</b>: {point.partido}<br>",
            "<b>Município</b>: {point.cidade}<br>",
            "<b>UF</b>: {point.uf}"
          )
        )
    })


    # box visualizar ----
    output$v_posts <- reactable::renderReactable({
      validar_terceiros()

      da_terceiros() %>%
        dplyr::arrange(desc(likes)) %>%
        head(50) %>%
        dplyr::transmute(
          candidata = paste0(candidata, " (", partido, ") | ",
                             cargo_pretendido, " | ", cidade, " - ", uf),
          author,
          # url = paste0("https://twitter.com/", author, "/status/", status_id),
          status_id,
          likes,
          data,
          id_texto
        ) %>%
        dplyr::left_join(
          readr::read_rds("data-raw/da_texto.rds"), "id_texto"
        ) %>%
        dplyr::select(-id_texto) %>%
        reactable::reactable(
          columns = list(
            candidata = reactable::colDef("Candidata", minWidth = 150),
            author = reactable::colDef("Perfil", minWidth = 100),
            status_id = reactable::colDef(
              "link",
              cell = function(value, index) {
                # Render as a link
                link <- .[index, "status_id"]
                htmltools::tags$a(href = link, target = "_blank", "link")
              },
              minWidth = 50
            ),
            text = reactable::colDef("Texto", minWidth = 200),
            likes = reactable::colDef("Likes", minWidth = 50),
            data = reactable::colDef("Data", format = reactable::colFormat(datetime = TRUE, locales = "pt-BR"))
          )
        )
    })

    # Botões download ----

    output$download_p_proprios <- downloadHandler(
      filename = "p_proprios.csv",
      content = function(file) {
        contagem <- da_proprios() %>%
          dplyr::mutate(dia = lubridate::date(data)) %>%
          dplyr::count(dia, tipo_publicacao)
        readr::write_csv(contagem, file)
      }
    )
    output$download_p_likes <- downloadHandler(
      filename = "p_likes.csv",
      content = function(file) {
        likes <- da_proprios() %>%
          dplyr::mutate(dia = lubridate::date(data)) %>%
          dplyr::group_by(dia) %>%
          dplyr::summarise(likes = sum(likes, na.rm = TRUE)) %>%
          dplyr::transmute(dia, cat = "likes", n = likes)
        comentarios <- da_terceiros() %>%
          dplyr::mutate(dia = lubridate::date(data)) %>%
          dplyr::count(dia) %>%
          dplyr::transmute(dia, cat = "comentarios", n)
        contagem <- dplyr::bind_rows(likes, comentarios)
        readr::write_csv(contagem, file)
      }
    )
    output$download_p_xingamentos <- downloadHandler(
      filename = "p_xingamentos.csv",
      content = function(file) {
        contagem <- da_terceiros() %>%
          dplyr::mutate(dia = lubridate::date(data)) %>%
          dplyr::count(dia, xingamento)
        readr::write_csv(contagem, file)
      }
    )
    output$download_p_tx_xingamentos <- downloadHandler(
      filename = "p_tx_xingamentos.csv",
      content = function(file) {
        contagem <- da_terceiros() %>%
          dplyr::mutate(dia = lubridate::date(data)) %>%
          dplyr::count(dia, genero, xingamento) %>%
          tidyr::pivot_wider(names_from = xingamento, values_from = n) %>%
          dplyr::mutate(Sim = tidyr::replace_na(Sim, 0)) %>%
          dplyr::mutate(prop = `Sim`/`Não`) %>%
          dplyr::select(dia, genero, prop)
        readr::write_csv(contagem, file)
      }
    )
    output$download_r_proprios <- downloadHandler(
      filename = "r_proprios.csv",
      content = function(file) {
        contagem <- da_proprios() %>%
          dplyr::count(candidata, partido, uf, cidade, cargo_pretendido, sort = TRUE)
        readr::write_csv(contagem, file)
      }
    )
    output$download_r_comentarios <- downloadHandler(
      filename = "r_comentarios.csv",
      content = function(file) {
        contagem <- da_terceiros() %>%
          dplyr::count(candidata, partido, uf, cidade, cargo_pretendido, xingamento) %>%
          dplyr::arrange(desc(n))
        readr::write_csv(contagem, file)
      }
    )
    output$download_r_curtidos <- downloadHandler(
      filename = "r_curtidos.csv",
      content = function(file) {
        contagem <- da_terceiros() %>%
          dplyr::group_by(candidata, partido, uf, cidade, cargo_pretendido) %>%
          dplyr::summarise(likes = sum(likes), .groups = "drop") %>%
          dplyr::arrange(desc(likes)) %>%
          dplyr::rename(n = likes)
        readr::write_csv(contagem, file)
      }
    )
    output$download_r_tx_xingamentos <- downloadHandler(
      filename = "r_tx_xingamentos.csv",
      content = function(file) {
        contagem <- da_terceiros() %>%
          dplyr::count(candidata, partido, uf, cidade, cargo_pretendido, xingamento) %>%
          tidyr::pivot_wider(names_from = xingamento, values_from = n) %>%
          dplyr::mutate(prop = `Sim`/`Não`) %>%
          dplyr::arrange(desc(prop))
        readr::write_csv(contagem, file)
      }
    )
    output$download_v_posts <- downloadHandler(
      filename = "v_posts.csv",
      content = function(file) {
        contagem <- da_terceiros() %>%
          dplyr::arrange(desc(likes)) %>%
          dplyr::transmute(
            candidata = paste0(candidata, " (", partido, ") | ",
                               cargo_pretendido, " | ", cidade, " - ", uf),
            author,
            link,
            status_id,
            id_texto,
            likes,
            data
          ) %>%
          dplyr::left_join(readr::read_rds("data-raw/da_texto.rds"), "id_texto") %>%
          dplyr::select(-id_texto)
        readr::write_csv(contagem, file)
      }
    )
  })
}


