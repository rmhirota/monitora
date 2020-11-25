

add_xingamentos <- function(base) {
  re <- c("analfabet[oa]", "bandid([:alpha:]*)?", "barang", "burr([:alpha:]*)?", "fei[oa]", "assusta", "macac[oa]",
          "encruzilhada .*frango", "macumb([:alpha:]*)?", "dem[oô]n[ií]([:alpha:]*)?", "satan([:alpha:]*)?",
          "pilantra([:alpha:]*)?", "incompet[eê]n([:alpha:]*)?", "problema(s)? cognitivo(s)?", "piranha", "maconheir[oa]",
          "dar.* rabo", "vagabund([:alpha:]*)?", "louc[oa]", "maluc[oa]", "raça .*inferno", "fdp",
          "figura.* suja", "ser .*internada", "maria .*beco", "mimi(mi|zenta)", "vitimiz([:alpha:]*)?", "chupa",
          "meu.* ovo", "rid[ií]cul([:alpha:]*)?", "porc[ao]", "canh[aã]o", "imund[oa](?![:alpha:])", "descontrolad[oa]",
          "minha.* pica", "inferno", "abortis[td]a", "ovo.* podre", "ir .*cadeleireiro", "conservagata",
          "s[ou]vaco ([:alpha:]*)?", "depila([:alpha:]*)?", "dissimulad([:alpha:]*)?", "desenhe .*v(o)?c([e|ê])?",
          "jument([:alpha:]*)?", "i[mn]becil([:alpha:]*)?", "idiot([:alpha:]*)?", "demente", "d[ou]ente",
          "piranh([:alpha:]*)?", "(?<!d(e|is)|com)puta", "cala.*boca", "quiet[oa]", "manga .*passada", "nojent([:alpha:]*)?",
          "metid.*besta", "t[aá] namorando", "lava.*lou[cç]a", "vai.*tanque", "lacraia", "suj[oa]", "(?<!en)gord([:alpha:]*)?",
          "mequetrefe", "repugnante", "estrume", "mesma merda",
          # LGBTQA
          "sapat[aã]o", "sapatona", "travec([a|ã])?o", "mulher.*barbada", "biboca", "usa.* hipoglos",
          "sauna", " rabo",
          # Manuela
          "manuelanao", "elanao", "comun(a|ist([:alpha:]*)?)(.*merda)?", "crente", "xinxeira", "trambiqueir([:alpha:]*)?",
          # Benedita
          "aposenta", "vov[oóô]", "cheirad([:alpha:]*)?", "comun(a|ist([:alpha:]*)?)(.*nojent([:alpha:]*)?)?",
          "defunt([:alpha:]*)?", "(?<!en)velh([:alpha:]*)?( feia)?", "véi[oa]", "assombra[cç][aã]o", "calabocamaria",
          # Joice
          "pep(p)?a", "porquinha", "🐽","riquinh[oa]", "soberb[oa]", "desesper([:alpha:]*)?", "circo", "gabinete de [oó]dio",
          "escrot([:alpha:]*)?", "(?<![:alpha:])gado", "pamonha",
          # Covas
          "tucanato", "hip[oó]cri([:alpha:]*)?", "mafioso", "ladrão", "máfia .+creche", "fundamentalista", "lgbtfóbico",
          "careca", "extrmista", "arrogante", "mentiros([:alpha:]*)?", "mil[ií]cia([:alpha:]*)?", "d[oó]]rianuncamais",
          "d[oó]riatraidor", "d[oó]rianacadeira", "marionete", "desprepar([:alpha:]*)?",
          # Boulos
          "invadir", "roboulos", "kombador", "bosta", "lixo", "baderneiro", "sem profiss[aã]o", "invasor",
          "lular[aá]pio", "luladr[aã]o", "arruaceiro", "casa .+fogo", "casa.+invadida",
          # Erundina
          "velhota", "asilo",
          # Paes
          "amigo.*cabral", "p[eé]ssim[oa]", "engenh[aã]o", "rombo.*caixa", "obras.*fara[oô]nica(s?)", "nervosinho",
          "corrupto",
          # Crivella
          "decr[eé]pit[oa]", "irrespons[aá]vel", "come.* feno", "guardi[aã]o", "guardi[oẽ]es", "corja", "fariseu",
          "estorvo", "pior prefeit[oa]", "deus.*eleitoreiro", "biruta", "fi[eé]is cegos"
  ) %>%
    stringr::str_c(collapse = ")|(") %>% paste0("(", ., ")") %>% stringr::regex(ignore_case = TRUE)

  proprios <- base %>%
    dplyr::filter(author == handle) %>%
    dplyr::mutate(xingamento = "Não aplicável")

  terceiros <- base %>%
    dplyr::filter(author != handle)

  base_x <- terceiros %>%
    dplyr::mutate(xingamento = stringr::str_detect(text, re)) %>%
    dplyr::mutate(xingamento = dplyr::case_when(
      xingamento == TRUE ~ "Sim",
      xingamento == FALSE ~ "Não"
    )) %>%
    dplyr::bind_rows(proprios)

  base_x
}

add_lista_xingamentos <- function(base) {

  re <- c("analfabet[oa]", "bandid([:alpha:]*)?", "barang", "burr([:alpha:]*)?", "fei[oa]", "assusta", "macac[oa]",
          "encruzilhada .*frango", "macumb([:alpha:]*)?", "dem[oô]n[ií]([:alpha:]*)?", "satan([:alpha:]*)?",
          "pilantra([:alpha:]*)?", "incompet[eê]n([:alpha:]*)?", "problema(s)? cognitivo(s)?", "piranha", "maconheir[oa]",
          "dar.* rabo", "vagabund([:alpha:]*)?", "louc[oa]", "maluc[oa]", "raça .*inferno", "fdp",
          "figura.* suja", "ser .*internada", "maria .*beco", "mimi(mi|zenta)", "vitimiz([:alpha:]*)?", "chupa",
          "meu.* ovo", "rid[ií]cul([:alpha:]*)?", "porc[ao]", "canh[aã]o", "imund[oa](?![:alpha:])", "descontrolad[oa]",
          "minha.* pica", "inferno", "abortis[td]a", "ovo.* podre", "ir .*cadeleireiro", "conservagata",
          "s[ou]vaco ([:alpha:]*)?", "depila([:alpha:]*)?", "dissimulad([:alpha:]*)?", "desenhe .*v(o)?c([e|ê])?",
          "jument([:alpha:]*)?", "i[mn]becil([:alpha:]*)?", "idiot([:alpha:]*)?", "demente", "d[ou]ente",
          "piranh([:alpha:]*)?", "(?<!d(e|is)|com)puta", "cala.*boca", "quiet[oa]", "manga .*passada", "nojent([:alpha:]*)?",
          "metid.*besta", "t[aá] namorando", "lava.*lou[cç]a", "vai.*tanque", "lacraia", "suj[oa]", "(?<!en)gord([:alpha:]*)?",
          "mequetrefe", "repugnante", "estrume", "mesma merda",
          # LGBTQA
          "sapat[aã]o", "sapatona", "travec([a|ã])?o", "mulher.*barbada", "biboca", "usa.* hipoglos",
          "sauna", " rabo",
          # Manuela
          "manuelanao", "elanao", "comun(a|ist([:alpha:]*)?)(.*merda)?", "crente", "xinxeira", "trambiqueir([:alpha:]*)?",
          # Benedita
          "aposenta", "vov[oóô]", "cheirad([:alpha:]*)?", "comun(a|ist([:alpha:]*)?)(.*nojent([:alpha:]*)?)?",
          "defunt([:alpha:]*)?", "(?<!en)velh([:alpha:]*)?( feia)?", "véi[oa]", "assombra[cç][aã]o", "calabocamaria",
          # Joice
          "pep(p)?a", "porquinha", "🐽","riquinh[oa]", "soberb[oa]", "desesper([:alpha:]*)?", "circo", "gabinete de [oó]dio",
          "escrot([:alpha:]*)?", "(?<![:alpha:])gado", "pamonha",
          # Covas
          "tucanato", "hip[oó]cri([:alpha:]*)?", "mafioso", "ladrão", "máfia .+creche", "fundamentalista", "lgbtfóbico",
          "careca", "extrmista", "arrogante", "mentiros([:alpha:]*)?", "mil[ií]cia([:alpha:]*)?", "d[oó]]rianuncamais",
          "d[oó]riatraidor", "d[oó]rianacadeira", "marionete", "desprepar([:alpha:]*)?",
          # Boulos
          "invadir", "roboulos", "kombador", "bosta", "lixo", "baderneiro", "sem profiss[aã]o", "invasor",
          "lular[aá]pio", "luladr[aã]o", "arruaceiro", "casa .+fogo", "casa.+invadida",
          # Erundina
          "velhota", "asilo",
          # Paes
          "amigo.*cabral", "p[eé]ssim[oa]", "engenh[aã]o", "rombo.*caixa", "obras.*fara[oô]nica(s?)", "nervosinho",
          "corrupto",
          # Crivella
          "decr[eé]pit[oa]", "irrespons[aá]vel", "come.* feno", "guardi[aã]o", "guardi[oẽ]es", "corja", "fariseu",
          "estorvo", "pior prefeit[oa]", "deus.*eleitoreiro", "biruta", "fi[eé]is cegos"
  ) %>%
    stringr::str_c(collapse = ")|(") %>% paste0("(", ., ")") %>% stringr::regex(ignore_case = TRUE)

  base_x <- base %>%
    dplyr::mutate(lista_xingamentos = stringr::str_extract_all(text, re)) %>%
    tidyr::unnest(lista_xingamentos) %>%
    dplyr::mutate(lista_xingamentos = stringr::str_squish(stringr::str_to_lower(lista_xingamentos)))
  base_x
}

