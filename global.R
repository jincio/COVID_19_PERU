bbc_style_adapted <- function (text_size = 1) {
  font <- "Helvetica"
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      family = font,
      size = text_size * 15,
      face = "bold",
      color = "#474785",
      hjust = 0.5
    ),
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = text_size * 11,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    plot.caption = ggplot2::element_text(
      family = font,
      face = "italic",
      size = text_size * 8,
      color = "#222222"
    ),
    plot.margin = unit(c(2,5,2,5), "mm"),
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = font,
      size = text_size * 11,
      color = "#222222"
    ),
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      family = font,
      size = text_size * 7,
      color = "#222222",
      margin = ggplot2::margin(5, b = 10)
    ),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.spacing.x = unit(5, "mm"),
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = text_size * 12, face = "bold", hjust = 0)
  )
}

plot_macro <-
  function(.data,
           macroregion,
           breaks = "7 days",
           ncol = 2) {
    temp <- .data %>%
      dplyr::filter(MACROREG == macroregion) %>%
      dplyr::group_by(REGION, Positivos) %>%
      dplyr::mutate(label = ifelse(dplyr::row_number() == 1, Positivos, NA)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION) %>%
      dplyr::mutate(label = ifelse(dplyr::row_number() == dplyr::n(), Positivos, label))
    
    plot <- temp %>%
      ggplot2::ggplot(ggplot2::aes(x = Fecha, y = Positivos)) +
      ggplot2::geom_line(colour = "#1380A1", size = 1) +
      ggplot2::ylim(0, max(temp$Positivos) * 1.1) +
      ggplot2::scale_x_date(
        labels = scales::date_format("%b-%d"),
        date_breaks = breaks,
        expand = c(0, 1)
      ) +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = label),
        vjust = -0.5,
        size = 2.3,
        box.padding = 0.05
      ) +
      ggplot2::labs(
        title = str_to_upper(paste("Número de casos positivos COVID-19,", macroregion)),
        y = "Número de casos reportados",
        caption = "Fuente: MINSA. Ver (https://jincio.github.io/COVID_19_PERU/Propagacion.html)") +
      ggplot2::facet_wrap( ~ REGION, ncol = ncol) +
      bbc_style_adapted() +
      geom_hline(yintercept = 0, size = 0.5, colour = "#333333")
    
    plot
    
  }


plot_macro2 <-
  function(.data,
           macroregion,
           breaks = "7 days",
           ncol = 2) {
    temp <- .data %>%
      dplyr::filter(MACROREG == macroregion) %>%
      dplyr::group_by(REGION, Fallecidos) %>%
      dplyr::mutate(label = ifelse(dplyr::row_number() == 1, Fallecidos, NA)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION) %>%
      dplyr::mutate(label = ifelse(dplyr::row_number() == dplyr::n(), Fallecidos, label))
    
    plot <- temp %>%
      ggplot2::ggplot(ggplot2::aes(x = Fecha, y = Fallecidos)) +
      ggplot2::geom_line(colour = "#1380A1", size = 1) +
      ggplot2::ylim(0, max(temp$Fallecidos) * 1.1) +
      ggplot2::scale_x_date(
        labels = scales::date_format("%b-%d"),
        date_breaks = breaks,
        expand = c(0, 1)
      ) +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = label),
        vjust = -0.5,
        size = 2.3,
        box.padding = 0.05
      ) +
      ggplot2::labs(
        title = str_to_upper(paste("Número de Fallecidos COVID-19,", macroregion)),
        y = "Número de fallecidos reportados (fecha reportada)",
        caption = "Fuente: MINSA. Ver (https://jincio.github.io/COVID_19_PERU/Propagacion.html)") +
      ggplot2::facet_wrap( ~ REGION, ncol = ncol) +
      bbc_style_adapted() +
      geom_hline(yintercept = 0, size = 0.5, colour = "#333333")
    
    plot
    
  }