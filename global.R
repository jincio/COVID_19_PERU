bbc_style_adapted <- function () {
  font <- "Helvetica"
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      family = font,
      size = 28,
      face = "bold",
      color = "#222222"
    ),
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 22,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    plot.caption = ggplot2::element_blank(),
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = font,
      size = 18,
      color = "#222222"
    ),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(
      family = font,
      size = 18,
      color = "#222222"
    ),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5,
                                                                 b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 22, hjust = 0)
  )
}

plot_macro <-
  function(.data,
           macroregion,
           breaks = "9 days",
           ncol = 3) {
    temp <- .data %>%
      dplyr::filter(MACROREG == macroregion) %>%
      dplyr::group_by(REGION, Positivos_PCR) %>%
      dplyr::mutate(label = ifelse(dplyr::row_number() == 1, Positivos, NA)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION) %>%
      dplyr::mutate(label = ifelse(dplyr::row_number() == dplyr::n(), Positivos, label))
    
    plot <- temp %>%
      ggplot2::ggplot(ggplot2::aes(x = Fecha, y = Positivos)) +
      ggplot2::geom_line(color = "darkred") +
      ggplot2::ylim(0, max(temp$Positivos) * 1.1) +
      ggplot2::scale_x_date(
        labels = scales::date_format("%b-%d"),
        date_breaks = breaks,
        expand = c(0, 1)
      ) +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = label),
        vjust = -0.3,
        size = 2.1,
        box.padding = 0.05
      ) +
      ggplot2::labs(y = "Número de casos reportados") +
      ggplot2::facet_wrap( ~ REGION, ncol = ncol)
    
    ggpubr::ggarrange(
      plot,
      ncol = 1,
      nrow = 1,
      font.label = list(
        size = 10,
        color = "black",
        face = "bold",
        family = NULL
      )
    ) %>%
      ggpubr::annotate_figure(
        top = ggpubr::text_grob(
          paste("Número de casos positivos COVID-19,", macroregion),
          color = "#474785",
          face = "bold",
          size = 14
        ),
        bottom = ggpubr::text_grob(
          "Fuente: MINSA. Ver (https://jincio.github.io/COVID_19_PERU/Propagacion.html)",
          color = "black",
          hjust = 1,
          x = 1,
          face = "italic",
          size = 8
        )
      )
  }