#' Plot Impact Analyses
#'
#' @param data Impact analysis data from `impact_analysis()`
#' @param plot_type One of "emp" for employment, "grp" for Gross Regional Product, or "output" for Output
#' @param background_colour Background colour of the graph.
#'
#' @return
#' @export
#'
#' @examples
#'
impact_plot <- function(data,  plot_type = c("emp", "grp", "output"), background_colour = "grey") {

  region <- data$region

  plot_title <- switch(plot_type,
                       "emp" = glue::glue("Employment Impacts (FTE): {region}"),
                       "grp" = glue::glue("GRP Impacts ($M): {region}"),
                       "output" = glue::glue("Output Impacts ($M): {region}")
  )

  y_axis <- switch(plot_type,
                   "emp" = ggplot2::scale_y_continuous(labels = scales::comma_format()),
                   "grp" = ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "M", 0.1))
  )


  if (range(data[[plot_type]]$year)[1] == range(data[[plot_type]]$year)[2]) {
    data[[plot_type]] %>%
      dplyr::filter(grepl("Direct|Flow on|Total", type)) %>%
      dplyr::group_by(year, type) %>%
      dplyr::summarise(value = sum(value), .groups = "drop")  %>%
      ggplot2::ggplot(ggplot2::aes(x = factor(year), y = value, fill = type, group = type)) +
      ggplot2::geom_col() +
      aititheme::theme_aiti(colour = background_colour) +
      y_axis +
      ggplot2::labs(title = plot_title,
           x = NULL,
           y = NULL)
  } else {
    data[[plot_type]] %>%
      dplyr::filter(grepl("Direct|Flow on|Total", type)) %>%
      dplyr::group_by(year, type) %>%
      dplyr::summarise(value = sum(value), .groups = "drop")  %>%
      ggplot2::ggplot(ggplot2::aes(x = factor(year), y = value, col = type, group = type)) +
      ggplot2::geom_line() +
      aititheme::theme_aiti(colour = background_colour) +
      y_axis +
      ggplot2::labs(title = plot_title,
           x = NULL,
           y = NULL)
  }

}

create_data <- function(n) {
  matrix(round(runif(19*n, 0, 5)), nrow = 19, ncol = n, dimnames = list(eiat:::anzsic_swap$name, 2022:(2022+(n-1))))
}
