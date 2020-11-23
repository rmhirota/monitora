#' psj UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_twitter_ui <- function(id){
  ns <- NS(id)
  tagList(

    # infobox ----
    shiny::fluidRow(
      col_3(bs4Dash::bs4ValueBoxOutput(ns("total_tw_publicados"), width = 12)),
      col_3(bs4Dash::bs4ValueBoxOutput(ns("total_tw_terceiros"), width = 12)),
      col_3(bs4Dash::bs4ValueBoxOutput(ns("tx_xingamento"), width = 12)),
      col_3(bs4Dash::bs4ValueBoxOutput(ns("tx_terceiro_publicado"), width = 12))
    ),

    # primeira linha ----
    shiny::fluidRow(

      bs4Dash::tabBox(
        width = 6,
        collapsible = FALSE,
        closable = FALSE,

        id = ns("tw_tempo"),

        title = "Por dia",

        bs4Dash::tabPanel(
          tabName = "Tweets próprios",
          highcharter::highchartOutput(ns("p_tw_proprios")),
          downloadButton(ns("download_p_proprios"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "RTs + likes",
          highcharter::highchartOutput(ns("p_rts_likes")),
          downloadButton(ns("download_p_rts_likes"), "Download dados")
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

        id = ns("tw_ranking"),

        title = "Rankings",

        bs4Dash::tabPanel(
          tabName = "Tweets próprios",
          highcharter::highchartOutput(ns("r_tw_proprios")),
          downloadButton(ns("download_r_tw_proprios"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "RTs + likes",
          highcharter::highchartOutput(ns("r_rts_likes")),
          downloadButton(ns("download_r_rts_likes"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "Mais citados",
          highcharter::highchartOutput(ns("r_citados")),
          downloadButton(ns("download_r_citados"), "Download dados")
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

        shiny::selectInput(
          ns("ordenar_por"),
          label = "Ordenar por",
          c(
            "Likes" = "likes",
            "RTs" = "rts"
          )
        ),

        reactable::reactableOutput(ns("v_tw")),
        downloadButton(ns("download_v_tw"), "Download dados")

      )
    )
  )
}

#' psj Server Functions
#'
#' @noRd
mod_twitter_server <- function(id, app_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # reactives ----

    # bases
    da_proprios <- shiny::reactive({
      app_data()$conteudo %>%
        dplyr::filter(
          rede_social == "twitter",
          author == handle
        )
    })

    da_terceiros <- shiny::reactive({
      app_data()$conteudo %>%
        dplyr::filter(
          rede_social == "twitter",
          author != handle
        )
    })

    da_v <- shiny::reactive({
      da_terceiros() %>%
        dplyr::arrange(desc(!!rlang::sym(input$ordenar_por))) %>%
        head(50)
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

    output$total_tw_publicados <- bs4Dash::renderbs4ValueBox({

      val <- dplyr::n_distinct(da_proprios()$status_id)
      val <- scales::number(val, big.mark = ".", decimal.mark = ",")

      bs4Dash::valueBox(
        subtitle = "Tweets próprios",
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        icon = "twitter",
        status = "primary"

      )
    })

    output$total_tw_terceiros <- bs4Dash::renderbs4ValueBox({

      val <- dplyr::n_distinct(da_terceiros()$status_id)
      val <- scales::number(val, big.mark = ".", decimal.mark = ",")

      bs4Dash::valueBox(
        subtitle = "Tweets de terceiros",
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        icon = "comment-dots",
        status = "primary"
      )
    })

    output$tx_xingamento <- bs4Dash::renderbs4ValueBox({

      val <- validar_infobox_terceiros()

      if (is.null(val)) {
        val1 <- da_terceiros() %>% dplyr::filter(xingamento == "Sim") %>% dplyr::n_distinct()
        val2 <- da_terceiros() %>% dplyr::filter(xingamento == "Não") %>% dplyr::n_distinct()
        val <- val1/val2
        val <- scales::percent(val, .1)
      }

      bs4Dash::valueBox(
        subtitle = "Taxa de tweets ofensivos",
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        icon = "exclamation-circle",
        status = "primary"
      )
    })

    output$tx_terceiro_publicado <- bs4Dash::renderbs4ValueBox({
      val1 <- validar_infobox_proprios()
      val2 <- validar_infobox_terceiros()

      if (is.null(val1) & is.null(val2)) {

        val <- dplyr::n_distinct(da_terceiros()$status_id)/dplyr::n_distinct(da_proprios()$status_id)
        val <- scales::number(val, big.mark = ".", decimal.mark = ",")
      }

      bs4Dash::valueBox(
        subtitle = "Menções por tweet próprio",
        value = tags$p(val, style = "font-size: 2vmax; margin-bottom: 0;"),
        icon = "comments",
        status = "primary"
      )
    })


    # box tweets tempo ----
    output$p_tw_proprios <- highcharter::renderHighchart({
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
    output$p_rts_likes <- highcharter::renderHighchart({
      validar_terceiros()
      contagem <- da_terceiros() %>%
        dplyr::mutate(dia = lubridate::date(data)) %>%
        dplyr::group_by(dia) %>%
        dplyr::summarise(likes = sum(likes), RTs = sum(rts)) %>%
        tidyr::pivot_longer(c(likes, RTs), names_to = "like_rt", values_to = "n")

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
    output$r_tw_proprios <- highcharter::renderHighchart({
      validar_proprios()

      contagem <- da_proprios() %>%
        dplyr::count(candidata, partido, uf, cidade, cargo_pretendido, sort = TRUE) %>%
        head(10)
      contagem %>%
        barras()
    })
    output$r_rts_likes <- highcharter::renderHighchart({
      validar_terceiros()

      contagem <- da_terceiros() %>%
        dplyr::group_by(candidata, partido, uf, cidade, cargo_pretendido) %>%
        dplyr::summarise(likes = sum(likes), RTs = sum(rts), .groups = "drop") %>%
        dplyr::mutate(total = likes + RTs) %>%
        tidyr::pivot_longer(c(likes, RTs), names_to = "categoria", values_to = "n") %>%
        dplyr::arrange(desc(total)) %>%
        head(20)
      contagem %>%
        barras(categ = "categoria")
    })
    output$r_citados <- highcharter::renderHighchart({
      validar_terceiros()

      contagem <- da_terceiros() %>%
        dplyr::count(candidata, partido, uf, cidade, cargo_pretendido, xingamento) %>%
        dplyr::group_by(candidata, partido) %>%
        dplyr::mutate(total = sum(n)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(desc(total)) %>%
        head(20)
      contagem %>%
        barras(categ = "xingamento")
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
    output$v_tw <- reactable::renderReactable({
      validar_terceiros()

      da_v() %>%
        dplyr::transmute(
          candidata = paste0(candidata, " (", partido, ") | ",
                             cargo_pretendido, " | ", cidade, " - ", uf),
          author,
          # url = paste0("https://twitter.com/", author, "/status/", status_id),
          status_id,
          text,
          likes, RTs = rts,
          data
        ) %>%
        reactable::reactable(
          columns = list(
            candidata = reactable::colDef("Candidata", minWidth = 150),
            author = reactable::colDef("Perfil", minWidth = 100),
            status_id = reactable::colDef(
              "link",
              cell = function(value, index) {
                # Render as a link
                link <- sprintf("https://twitter.com/%s/status/%s", .[index, "author"], value)
                htmltools::tags$a(href = link, target = "_blank", "link")
              },
              minWidth = 50
            ),
            text = reactable::colDef("Texto", minWidth = 200),
            likes = reactable::colDef("Likes", minWidth = 50),
            RTs = reactable::colDef("RTs", minWidth = 50),
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
    output$download_p_rts_likes <- downloadHandler(
      filename = "p_rts_likes.csv",
      content = function(file) {
        contagem <- da_terceiros() %>%
          dplyr::mutate(dia = lubridate::date(data)) %>%
          dplyr::group_by(dia) %>%
          dplyr::summarise(likes = sum(likes), RTs = sum(rts)) %>%
          tidyr::pivot_longer(c(likes, RTs), names_to = "like_rt", values_to = "n")
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
    output$download_r_tw_proprios <- downloadHandler(
      filename = "r_tw_proprios.csv",
      content = function(file) {
        contagem <- da_proprios() %>%
          dplyr::count(candidata, partido, uf, cidade, cargo_pretendido, sort = TRUE)
        readr::write_csv(contagem, file)
      }
    )
    output$download_r_rts_likes <- downloadHandler(
      filename = "r_rts_likes.csv",
      content = function(file) {
        contagem <- da_terceiros() %>%
          dplyr::group_by(candidata, partido, uf, cidade, cargo_pretendido) %>%
          dplyr::summarise(likes = sum(likes), RTs = sum(rts), .groups = "drop") %>%
          dplyr::mutate(total = likes + RTs) %>%
          tidyr::pivot_longer(c(likes, RTs), names_to = "categoria", values_to = "n") %>%
          dplyr::arrange(desc(total))
        readr::write_csv(contagem, file)
      }
    )
    output$download_r_citados <- downloadHandler(
      filename = "r_citados.csv",
      content = function(file) {
        contagem <- da_terceiros() %>%
          dplyr::count(candidata, partido, uf, cidade, cargo_pretendido, xingamento) %>%
          dplyr::group_by(candidata, partido) %>%
          dplyr::mutate(total = sum(n)) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(desc(total))
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
    output$download_v_tw <- downloadHandler(
      filename = "v_tw.csv",
      content = function(file) {
        contagem <- da_v() %>%
          dplyr::transmute(
            candidata = paste0(candidata, " (", partido, ") | ",
                               cargo_pretendido, " | ", cidade, " - ", uf),
            author,
            link,
            status_id,
            text,
            likes, RTs = rts,
            data
          )
        readr::write_csv(contagem, file)
      }
    )

  })
}

