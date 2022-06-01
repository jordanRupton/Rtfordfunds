#' Hartford Funds branded theme for ggplot
#'
#' This theme borrows heavily from ggthemes::theme_fivethirtyeight()
#' but adds some additional tweaks and HF brand colors.
#'
#' @param base_size The base font size
#' @param base_family The base font family
#'
#' @return A theme
#' @export
#'
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width, color = Petal.Length)) +
#' ggplot2::geom_point() +
#' theme_hf() +
#' ggplot2::scale_color_gradient(low = Rtfordfunds::hf_palette["Domestic Blue"],
#'                               high = Rtfordfunds::hf_palette["Accent Orange"])
theme_hf <- function(base_size = 12, base_family = "sans"){
  (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
     ggplot2::theme(line = ggplot2::element_line(colour = "black"),
           rect = ggplot2::element_rect(fill = Rtfordfunds::hf_palette["Light Gray"],
                               linetype = 0, colour = NA),
           text = ggplot2::element_text(colour = Rtfordfunds::hf_palette["Tagline Gray"]),
           axis.title = ggplot2::element_text(face = "bold"),
           axis.text = ggplot2::element_text(),
           axis.ticks = ggplot2::element_blank(),
           axis.line = ggplot2::element_blank(),
           legend.background = ggplot2::element_rect(),
           legend.position = "bottom",
           legend.direction = "horizontal",
           legend.box = "vertical",
           legend.title = ggplot2::element_text(face = "bold"),
           panel.grid = ggplot2::element_line(colour = NULL),
           panel.grid.major = ggplot2::element_line(colour = Rtfordfunds::hf_palette["Cool Gray 9"]),
           panel.grid.minor = ggplot2::element_blank(),
           plot.title = ggplot2::element_text(hjust = 0, size = ggplot2::rel(1.5), face = "bold"),
           plot.margin = ggplot2::unit(c(1, 1, 1, 1), "lines"),
           strip.background = ggplot2::element_rect(fill = Rtfordfunds::hf_palette["Tagline Gray"]),
           strip.text = ggplot2::element_text(face = "bold", color = "white")))
}
