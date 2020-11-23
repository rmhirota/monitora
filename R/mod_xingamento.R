##' psj UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_xingamento_ui <- function(id){
  ns <- NS(id)
  tagList(

    # pickerinput ----
    shiny::fluidRow(
      shinyWidgets::pickerInput(
        ns("rede_social"),
        "Rede social",
        c("twitter", "instagram", "youtube"),
        multiple = TRUE,
        selected = c("twitter", "instagram", "youtube"),
        options = list(
          `actions-box` = TRUE,
          `style` = "btn-light text-dark"
        )
      )
    ),

    # primeira linha ----
    shiny::fluidRow(

      bs4Dash::tabBox(
        width = 6,
        collapsible = FALSE,
        closable = FALSE,

        id = ns("p_termos"),

        title = "Termos mais usados",

        bs4Dash::tabPanel(
          tabName = "Por candidato",
          reactable::reactableOutput(ns("p_candidato")),
          downloadButton(ns("download_p_candidato"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "Por partido",
          reactable::reactableOutput(ns("p_partido")),
          downloadButton(ns("download_p_partido"), "Download dados")
        )
      ),


      bs4Dash::tabBox(
        width = 6,
        collapsible = FALSE,
        closable = FALSE,

        id = ns("comparacoes"),

        title = "Comparações",

        bs4Dash::tabPanel(
          tabName = "Gênero",
          highcharter::highchartOutput(ns("c_genero")),
          downloadButton(ns("download_c_genero"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "Raça",
          highcharter::highchartOutput(ns("c_raca")),
          downloadButton(ns("download_c_raca"), "Download dados")
        ),
        bs4Dash::tabPanel(
          tabName = "Cis/Trans/LGBTQIA",
          highcharter::highchartOutput(ns("c_lgbtqia")),
          downloadButton(ns("download_c_lgbtqia"), "Download dados")
        )
      )
    ),

    # segunda linha ----
    shiny::fluidRow(


      bs4Dash::box(
        width = 12,
        collapsible = FALSE,
        closable = FALSE,
        id = ns("p_box"),
        title = "Mensagens ofensivas ao longo do tempo",

        highcharter::highchartOutput(ns("p_tempo_redes")),
        downloadButton(ns("download_p_tempo_redes"), "Download dados")
      )
    )
  )
}

#' psj Server Functions
#'
#' @noRd
mod_xingamento_server <- function(id, app_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # reactives ----

    # bases
    da_termos <- shiny::reactive({
      app_data()$xingamento %>%
        dplyr::filter(
          rede_social %in% input$rede_social
        )
    })

    da <- shiny::reactive({
      app_data()$conteudo
    })

    validar_termos <- shiny::reactive({
      shiny::validate(
        shiny::need(
          nrow(da_termos()) > 0,
          "Não foi possível gerar a visualização com os parâmetros selecionados."
        )
      )
    })
    validar_da <- shiny::reactive({
      shiny::validate(
        shiny::need(
          nrow(da()) > 0,
          "Não foi possível gerar a visualização com os parâmetros selecionados."
        )
      )
    })

    # box termos ----

    output$p_candidato <- reactable::renderReactable({
      validar_termos()

      da_termos() %>%
        dplyr::count(candidata, partido, cargo_pretendido, cidade, uf, lista_xingamentos) %>%
        dplyr::group_by(candidata, partido, cargo_pretendido, cidade, uf) %>%
        dplyr::top_n(1, n) %>%
        dplyr::arrange(desc(n)) %>%
        reactable::reactable(
          columns = list(
            candidata = reactable::colDef("Candidata", minWidth = 150),
            partido = reactable::colDef("Partido", minWidth = 50),
            cargo_pretendido = reactable::colDef("Cargo", minWidth = 100),
            cidade = reactable::colDef("Cidade", minWidth = 100),
            uf = reactable::colDef("uf", minWidth = 50),
            lista_xingamentos = reactable::colDef("Termo mais usado", minWidth = 100),
            n = reactable::colDef("N", minWidth = 50)
          )
        )
    })
    output$p_partido <- reactable::renderReactable({
      validar_termos()

      da_termos() %>%
        dplyr::count(partido, lista_xingamentos) %>%
        dplyr::group_by(partido) %>%
        dplyr::top_n(1, n) %>%
        dplyr::arrange(desc(n)) %>%
        reactable::reactable(
          columns = list(
            partido = reactable::colDef("Partido", minWidth = 50),
            lista_xingamentos = reactable::colDef("Termo mais usado", minWidth = 100),
            n = reactable::colDef("N", minWidth = 50)
          )
        )
    })

    # box comparações ----
    output$c_genero <- highcharter::renderHighchart({
      validar_da()

      contagem <- da() %>%
        dplyr::filter(xingamento != "Não aplicável") %>%
        dplyr::count(genero, xingamento) %>%
        dplyr::group_by(genero) %>%
        dplyr::mutate(prop = n/sum(n)) %>%
        dplyr::ungroup()

      contagem %>%
        dplyr::mutate(lab = scales::percent(prop)) %>%
        highcharter::hchart(
          "bar",
          highcharter::hcaes(genero, prop, group = xingamento),
          stacking = "normal"
        ) %>%
        highcharter::hc_colors(c("#76448a", "#2980b9", "#76d7c4", "#f086cd", "#e6ed7f")) %>%
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = paste0(
            "<b>Quantidade</b>: {point.n}<br>",
            "<b>Propor\u00e7\u00e3o</b>: {point.lab}<br>"
          )
        )
    })
    output$c_raca <- highcharter::renderHighchart({
      validar_da()

      contagem <- da() %>%
        dplyr::filter(xingamento != "Não aplicável") %>%
        dplyr::count(raca, xingamento) %>%
        dplyr::group_by(raca) %>%
        dplyr::mutate(prop = n/sum(n)) %>%
        dplyr::ungroup()

      contagem %>%
        dplyr::mutate(lab = scales::percent(prop)) %>%
        highcharter::hchart(
          "bar",
          highcharter::hcaes(raca, prop, group = xingamento),
          stacking = "normal"
        ) %>%
        highcharter::hc_colors(c("#76448a", "#2980b9", "#76d7c4", "#f086cd", "#e6ed7f")) %>%
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = paste0(
            "<b>Quantidade</b>: {point.n}<br>",
            "<b>Propor\u00e7\u00e3o</b>: {point.lab}<br>"
          )
        )
    })
    output$c_lgbtqia <- highcharter::renderHighchart({
      validar_da()

      contagem <- da() %>%
        dplyr::filter(xingamento != "Não aplicável") %>%
        dplyr::count(lgbt, xingamento) %>%
        dplyr::group_by(lgbt) %>%
        dplyr::mutate(prop = n/sum(n)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(lgbt = dplyr::case_when(
          is.na(lgbt) ~ "?",
          lgbt == TRUE ~ "Sim",
          lgbt == FALSE ~ "Não"
        ))

      contagem %>%
        dplyr::mutate(lab = scales::percent(prop)) %>%
        highcharter::hchart(
          "bar",
          highcharter::hcaes(lgbt, prop, group = xingamento),
          stacking = "normal"
        ) %>%
        highcharter::hc_colors(c("#76448a", "#2980b9", "#76d7c4", "#f086cd", "#e6ed7f")) %>%
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = paste0(
            "<b>Quantidade</b>: {point.n}<br>",
            "<b>Propor\u00e7\u00e3o</b>: {point.lab}<br>"
          )
        )
    })


    # box tempo ----
    output$p_tempo_redes <- highcharter::renderHighchart({
      validar_da()

      contagem <- da() %>%
        dplyr::filter(xingamento == "Sim") %>%
        dplyr::mutate(dia = lubridate::as_date(data)) %>%
        dplyr::count(dia, rede_social)

      contagem %>%
        linha()
    })

    # Botões download ----

    output$download_p_candidato <- downloadHandler(
      filename = "p_candidato.csv",
      content = function(file) {
        contagem <- da_termos() %>%
          dplyr::count(candidata, partido, cargo_pretendido, cidade, uf, lista_xingamentos) %>%
          dplyr::group_by(candidata, partido, cargo_pretendido, cidade, uf) %>%
          dplyr::top_n(1, n) %>%
          dplyr::arrange(desc(n))
        readr::write_csv(contagem, file)
      }
    )
    output$download_p_partido <- downloadHandler(
      filename = "p_partido.csv",
      content = function(file) {
        contagem <- da_termos() %>%
          dplyr::count(partido, lista_xingamentos) %>%
          dplyr::group_by(partido) %>%
          dplyr::top_n(1, n) %>%
          dplyr::arrange(desc(n))
        readr::write_csv(contagem, file)
      }
    )
    output$download_c_genero <- downloadHandler(
      filename = "c_genero.csv",
      content = function(file) {
        contagem <- da() %>%
          dplyr::filter(xingamento != "Não aplicável") %>%
          dplyr::count(genero, xingamento) %>%
          dplyr::group_by(genero) %>%
          dplyr::mutate(prop = n/sum(n)) %>%
          dplyr::ungroup()
        readr::write_csv(contagem, file)
      }
    )
    output$download_c_raca <- downloadHandler(
      filename = "c_raca.csv",
      content = function(file) {
        contagem <- da() %>%
          dplyr::filter(xingamento != "Não aplicável") %>%
          dplyr::count(raca, xingamento) %>%
          dplyr::group_by(raca) %>%
          dplyr::mutate(prop = n/sum(n)) %>%
          dplyr::ungroup()
        readr::write_csv(contagem, file)
      }
    )
    output$download_c_lgbtqia <- downloadHandler(
      filename = "c_lgbtqia.csv",
      content = function(file) {
        contagem <- da() %>%
          dplyr::filter(xingamento != "Não aplicável") %>%
          dplyr::count(lgbt, xingamento) %>%
          dplyr::group_by(lgbt) %>%
          dplyr::mutate(prop = n/sum(n)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(lgbt = dplyr::case_when(
            is.na(lgbt) ~ "?",
            lgbt == TRUE ~ "Sim",
            lgbt == FALSE ~ "Não"
          ))
        readr::write_csv(contagem, file)
      }
    )
    output$download_p_tempo_redes <- downloadHandler(
      filename = "p_tempo_redes.csv",
      content = function(file) {
        contagem <- da() %>%
          dplyr::filter(xingamento == "Sim") %>%
          dplyr::mutate(dia = lubridate::as_date(data)) %>%
          dplyr::count(dia, rede_social)
        readr::write_csv(contagem, file)
      }
    )

  })
}


