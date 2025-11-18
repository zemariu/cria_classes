# -------------------------------------------------------------------
# Curso: Epidemiologia Descritiva Aplicada à Tuberculose
# Função: cria_classes() — Geração de Intervalos de Classes
# Autor: José Mário Nunes da Silva, Ph.D
# Contato: zemariu@usp.br
# -------------------------------------------------------------------
# DESCRIÇÃO
# -------------------------------------------------------------------
# A função cria intervalos de classes para variáveis numéricas usadas 
# em mapas temáticos, permitindo diferentes estilos de classificação
# e criando uma classe exclusiva para valores iguais a zero.
#
# Estilos aceitos (argumento `style`):
#   - "quantile" : quantis
#   - "equal"    : amplitude igual
#   - "sd"       : média ± desvio‐padrão
#   - "pretty"   : intervalos arredondados
#   - "fixed"    : intervalos definidos pelo usuário
#
# Argumentos extras:
#   nome_classe        → nome da coluna com as classes (fator)
#   nome_classe_label  → nome da coluna com labels finalizados
#
# -------------------------------------------------------------------
# EXEMPLO DE USO
# -------------------------------------------------------------------
# mapa_incid <- cria_classes(
#   dados      = mapa_incid,
#   var        = "INCID_2024",
#   n_classes  = 3,
#   style      = "quantile",
#   nome_classe       = "classe_incid",
#   nome_classe_label = "classe_incid_label",
#   sep_decimal  = "," (default)
# )
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Carregar e instalar pacotes automaticamente
# -------------------------------------------------------------------

usar_pacote <- function(pacote) {
  if (!requireNamespace(pacote, quietly = TRUE)) {
    message(paste0("Pacote '", pacote, "' não encontrado. Instalando..."))
    install.packages(pacote)
  }
  library(pacote, character.only = TRUE)
}

usar_pacote("dplyr")
usar_pacote("classInt")
usar_pacote("rlang")

# -------------------------------------------------------------------
# Função principal
# -------------------------------------------------------------------

cria_classes <- function(
    dados,
    var,
    n_classes   = 3,
    style       = "quantile",
    breaks_fixed = NULL,
    nome_classe = "classe",
    sep_decimal = ",",
    contagem    = TRUE        
) {
  # nome da coluna de rótulos
  nome_classe_label <- paste0(nome_classe, "_label")
  
  # impedir sobrescrita de colunas existentes
  col_exist <- intersect(c(nome_classe, nome_classe_label), names(dados))
  if (length(col_exist) > 0) {
    stop(
      "As seguintes colunas já existem no objeto de dados: ",
      paste(col_exist, collapse = ", "),
      ". Renomeie-as ou escolha outro valor para `nome_classe`."
    )
  }
  
  # separar zeros e positivos
  dados_zero <- dados |> dplyr::filter(.data[[var]] == 0)
  dados_pos  <- dados |> dplyr::filter(.data[[var]] > 0)
  
  # caso especial: tudo zero
  if (nrow(dados_pos) == 0) {
    n0 <- nrow(dados_zero)
    rot0 <- if (contagem) paste0("0 [", n0, "]") else "0"
    
    return(
      dados |>
        dplyr::mutate(
          !!nome_classe       := factor("0"),
          !!nome_classe_label := factor(rot0)
        )
    )
  }
  
  # ----------------------------
  # definir intervalos
  # ----------------------------
  if (style == "fixed") {
    if (is.null(breaks_fixed)) {
      stop("Para style = 'fixed', informe breaks_fixed = c(...).")
    }
    
    qs_cut <- sort(unique(breaks_fixed))
    qs_lab <- qs_cut
    
    if (is.infinite(qs_lab[1]) && qs_lab[1] < 0) {
      qs_lab[1] <- min(dados_pos[[var]], na.rm = TRUE)
    }
    if (is.infinite(qs_lab[length(qs_lab)]) && qs_lab[length(qs_lab)] > 0) {
      qs_lab[length(qs_lab)] <- max(dados_pos[[var]], na.rm = TRUE)
    }
    
  } else {
    intervalos <- classInt::classIntervals(
      dados_pos[[var]],
      n     = n_classes,
      style = style
    )
    
    qs_cut <- unique(sort(intervalos$brks))
    qs_lab <- qs_cut
  }
  
  # função para formatar decimal
  format_dec <- function(x) {
    out <- sprintf("%.1f", x)
    if (sep_decimal == ",") out <- gsub("\\.", ",", out)
    out
  }
  
  # rótulos “puros” dos intervalos (sem contagem)
  labels <- paste0(
    format_dec(head(qs_lab, -1)),
    " – ",
    format_dec(tail(qs_lab, -1))
  )
  
  # classificar valores positivos
  dados_pos <- dados_pos |>
    dplyr::mutate(
      !!nome_classe := cut(
        .data[[var]],
        breaks         = qs_cut,
        include.lowest = TRUE,
        labels         = labels,
        right          = TRUE
      )
    ) |>
    dplyr::mutate(
      !!nome_classe := factor(.data[[nome_classe]], levels = labels)
    )
  
  # contagem por classe (incluindo classes vazias)
  tab_n <- as.integer(table(factor(dados_pos[[nome_classe]], levels = labels)))
  names(tab_n) <- labels
  
  # labels com ou sem [n]
  if (contagem) {
    labels_lab <- paste0(labels, " [", tab_n, "]")
  } else {
    labels_lab <- labels
  }
  names(labels_lab) <- labels
  
  # aplicar labels
  dados_pos <- dados_pos |>
    dplyr::mutate(
      !!nome_classe_label := labels_lab[as.character(.data[[nome_classe]])]
    ) |>
    dplyr::mutate(
      !!nome_classe_label := factor(.data[[nome_classe_label]],
                                    levels = unique(labels_lab))
    )
  
  # ----------------------------
  # incluir classe zero se houver zeros
  # ----------------------------
  if (nrow(dados_zero) > 0) {
    n0   <- nrow(dados_zero)
    rot0 <- if (contagem) paste0("0 [", n0, "]") else "0"
    
    dados_zero <- dados_zero |>
      dplyr::mutate(
        !!nome_classe       := factor("0"),
        !!nome_classe_label := factor(rot0)
      )
    
    dados_saida <- dplyr::bind_rows(dados_zero, dados_pos)
  } else {
    dados_saida <- dados_pos
  }
  
  return(dados_saida)
}
