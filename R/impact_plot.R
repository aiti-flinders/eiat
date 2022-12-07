#' Plot Impact Analyses
#'
#' @param data Impact analysis data from `impact_analysis()`
#' @param type Line or column. Single year analyses always use column.
#' @param indicator One of "emp" for employment, "grp" for Gross Regional Product, or "output" for Output
#' @param background_colour Background colour of the graph.
#'
#' @return
#' @export
#'
#' @examples
#'
impact_plot <- function(data, type, indicator, background_colour = "grey") {

  region <- data$region

  plot_title <- switch(indicator,
                       "emp" = glue::glue("Employment Impacts (FTE): {region}"),
                       "grp" = glue::glue("GRP Impacts ($M): {region}"),
                       "output" = glue::glue("Output Impacts ($M): {region}")
  )

  y_axis <- switch(indicator,
                   "emp" = ggplot2::scale_y_continuous(labels = scales::comma_format()),
                   "grp" = ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "M", 0.1))
  )


  if (range(data[[indicator]]$year)[1] == range(data[[indicator]]$year)[2] | type == "col") {

    message("Only 1 year of data. Creating column plot")

    data[[indicator]] %>%
      dplyr::filter(grepl("Direct|Flow on|Total", type)) %>%
      dplyr::group_by(year, type) %>%
      dplyr::summarise(value = sum(value), .groups = "drop")  %>%
      ggplot2::ggplot(ggplot2::aes(x = factor(year), y = value, fill = type, group = type)) +
      ggplot2::geom_col(position = "dodge") +
      aititheme::theme_aiti(colour = background_colour) +
      aititheme::scale_fill_aiti() +
      y_axis +
      ggplot2::labs(title = plot_title,
           x = NULL,
           y = NULL)
  } else {
    data[[indicator]] %>%
      dplyr::filter(grepl("Direct|Flow on|Total", type)) %>%
      dplyr::group_by(year, type) %>%
      dplyr::summarise(value = sum(value), .groups = "drop")  %>%
      ggplot2::ggplot(ggplot2::aes(x = factor(year), y = value, col = type, group = type)) +
      ggplot2::geom_line() +
      aititheme::theme_aiti(colour = background_colour) +
      aititheme::scale_colour_aiti() +
      y_axis +
      ggplot2::labs(title = plot_title,
           x = NULL,
           y = NULL)
  }

}

create_data <- function(n) {
  matrix(round(runif(19*n, 0, 5)), nrow = 19, ncol = n, dimnames = list(eiat:::anzsic_swap$name, 2022:(2022+(n-1))))
}
