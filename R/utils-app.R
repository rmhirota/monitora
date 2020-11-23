variaveis_cat <- function() {
  list(
    "Cargo" = "cargo_pretendido",
    "Partido" = "partido",
    "UF" = "uf",
    "Município" = "cidade",
    "Gênero" = "genero",
    "Conteúdo original (não RT)" = "tweet_original",
    "Xingamento" = "xingamento",
    "Turno" = "turno"
  )
}


tempo_lab_one <- function(x) {
  ano <- floor(x)
  mes <- round((x %% 1) * 12)
  ano_s <- dplyr::if_else(ano > 1, "s", "")
  if (mes == 12) {
    ano <- ano + 1
    ano_s <- dplyr::if_else(ano > 1, "s", "")
    stringr::str_glue("{ano} ano{ano_s}")
  } else if (mes == 0) {
    stringr::str_glue("{ano} ano{ano_s}")
  } else {
    mes_es <- dplyr::if_else(mes > 1, "eses", "\u00eas")
    if (ano > 0) {
      stringr::str_glue("{ano} ano{ano_s} e {mes} m{mes_es}")
    } else {
      stringr::str_glue("{mes} m{mes_es}")
    }

  }
}

#' Tempo em label
#'
#' @param x vetor de tempos em anos
#'
#' @export
tempo_lab <- function(x) {
  purrr::map_chr(x, tempo_lab_one)
}

linha <- function(da, tipo = "line") {

  categ <- names(da)[2]

  if (tibble::has_name(da, "prop")) {
    da$lab <- scales::percent(da$prop, accuracy = .1)
    da$n <- da$prop
  } else {
    da$lab <- scales::number(da$n, big.mark = ".", decimal.mark = ",")
  }

  da %>%
    highcharter::hchart(
      tipo,
      highcharter::hcaes(dia, n, group = !!rlang::sym(categ))
    ) %>%
    highcharter::hc_colors(c("#76448a", "#2980b9", "#76d7c4", "#f086cd", "#e6ed7f")) %>%
    highcharter::hc_tooltip(
      headerFormat = "",
      pointFormat = "<b>Dia</b>: {point.dia}<br><b>valor</b>: {point.lab}"
    )
}


barras <- function(da, tipo = "bar", categ = NULL) {

  v <- names(da)[1]

  if (!tibble::has_name(da, "prop")) {
    da$prop <- with(da, n/sum(n))
  }

  if(is.null(categ)) {
    da %>%
      dplyr::mutate(lab = scales::percent(prop)) %>%
      highcharter::hchart(
        tipo,
        highcharter::hcaes(!!rlang::sym(v), n)
      ) %>%
      highcharter::hc_colors("#76d7c4") %>%
      highcharter::hc_tooltip(
        headerFormat = "",
        pointFormat = paste0(
          "<b>Quantidade</b>: {point.n}<br>",
          "<b>Propor\u00e7\u00e3o</b>: {point.lab}<br>",
          "<b>Cargo</b>: {point.cargo_pretendido}<br>",
          "<b>Partido</b>: {point.partido}<br>",
          "<b>Município</b>: {point.cidade}<br>",
          "<b>UF</b>: {point.uf}"
        )
      )
  } else {
    da %>%
      dplyr::mutate(lab = scales::percent(prop)) %>%
      highcharter::hchart(
        tipo,
        highcharter::hcaes(!!rlang::sym(v), n, group = !!rlang::sym(categ)),
        stacking = "normal"
      ) %>%
      highcharter::hc_colors(c("#76448a", "#2980b9", "#76d7c4", "#f086cd", "#e6ed7f")) %>%
      highcharter::hc_tooltip(
        headerFormat = "",
        pointFormat = paste0(
          "<b>Quantidade</b>: {point.n}<br>",
          "<b>Propor\u00e7\u00e3o</b>: {point.lab}<br>",
          "<b>Cargo</b>: {point.cargo_pretendido}<br>",
          "<b>Partido</b>: {point.partido}<br>",
          "<b>Município</b>: {point.cidade}<br>",
          "<b>UF</b>: {point.uf}"
        )
      )
  }
}


pizza <- function(da) {
  v <- names(da)[1]
  cores <- viridis::viridis(nrow(da), begin = .2, end = .8)
  da %>%
    highcharter::hchart(
      "pie",
      highcharter::hcaes(!!rlang::sym(v), prop)
    ) %>%
    highcharter::hc_tooltip(
      headerFormat = "",
      pointFormat = sprintf(
        "<b>{point.%s} <br> aparece em {point.lab} do total</b>",
        v
      )
    ) %>%
    highcharter::hc_colors(cores) %>%
    highcharter::hc_plotOptions(pie = list(
      allowPointSelect = TRUE,
      cursor = "pointer"
    ))

}

histograma <- function(v) {
  highcharter::hchart(v) %>%
    highcharter::hc_colors("#414487FF") %>%
    highcharter::hc_xAxis(plotLines = list(list(
      label = list(text = "Mediana"),
      color = "#FF0000",
      width = 2,
      value = stats::median(v),
      zIndex = 999
    )))
}

prop_agg <- function(x) {
  reactable::JS(stringr::str_glue("
      function(values, rows) {{
        var total = 0.0;
        var ntot = 0.0;
        rows.forEach(function(row) {{
          total += row['n'] * row['{x}'];
          ntot += row['n'];
        }})
        return total / ntot;
      }}
    "))
}
