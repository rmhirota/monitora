#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  filtros <- purrr::imap(variaveis_cat(), ~{

    op <- as.character(unique(monitora::da_tidy[[.x]]))

    shinyWidgets::pickerInput(
      .x, .y,
      choices = op,
      selected = op,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        container = "body",
        tickIcon = "fa fa-check",
        width = "100%",
        style = "btn-light text-dark",
        selectAllText = "Todos",
        deselectAllText = "Nenhum",
        selectedTextFormat = "count > 1",
        dropupAuto = FALSE
      )
    )
  })

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here

    bs4Dash::dashboardPage(

      # ----
      controlbar = bs4Dash::dashboardControlbar(
        skin = "dark",
        filtros,
        dateRangeInput("data", "data",
                       start = min(lubridate::as_date(monitora::da_tidy$data), na.rm = TRUE),
                       end = max(lubridate::as_date(monitora::da_tidy$data), na.rm = TRUE),
                       language = "pt-BR", separator = "a")
      ),

      navbar = bs4Dash::dashboardHeader(
        # rightUi = auth0::logoutButton(icon = icon("sign-out-alt"))
      ),

      # ----
      sidebar = bs4Dash::dashboardSidebar(
        skin = "light",
        title = "{monitora}",
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarMenuItem(
            "Twitter",
            tabName = "twitter",
            icon = "twitter"
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Instagram",
            tabName = "instagram",
            icon = "instagram"
          ),
          bs4Dash::bs4SidebarMenuItem(
            "YouTube",
            tabName = "youtube",
            icon = "youtube"
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Conteúdo ofensivo",
            tabName = "xingamento",
            icon = "exclamation-circle"
          )
        )
      ),

      # ----
      body = bs4Dash::dashboardBody(
        fresh::use_theme(create_theme_css()),
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
            tabName = "twitter",
            mod_twitter_ui("twitter_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "instagram",
            mod_instagram_ui("instagram_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "youtube",
            mod_youtube_ui("youtube_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "xingamento",
            mod_xingamento_ui("xingamento_ui_1")
          )
        )
      ),

      # ----
      footer = bs4Dash::dashboardFooter(
        copyrights = a(
          href = "https://voltdata.info",
          target = "_blank", "Volt Data Lab"
        ),
        right_text = "2020 | MonitorA"
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'monitora'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

