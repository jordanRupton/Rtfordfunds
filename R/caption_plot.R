#' Insert left & right captions on a plot
#'
#' Take an existing plot and add a horizontal line break at the bottom,
#' with space for a caption on the left and right side underneath.
#'
#' @param plot The existing plot object the caption will be added to
#' @param left_caption Optional caption on the left hand side.  Recommend using for source description.
#' @param right_caption Optional caption on the right hand side.  Recommend using for company name.
#' @param fontsize The fontsize for the captions.
#'
#' @return A grobTree object.  Unlike ggplot objects, this will not automatically be drawn.
#' @export
#'
#' @examples
#' my_plot <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width)) +
#'  ggplot2::geom_point() +
#'  Rtfordfunds::theme_hf()
#'
#' caption_plot(my_plot, "Source: iris dataset", "Hartford Funds")
caption_plot <- function(plot,
                         left_caption = "Source:",
                         right_caption = "Hartford Funds Data Science",
                         fontsize = 12){

    captioned_plot <- grid::grobTree(
      #make the background of the entire plot area match the color of theme_hf
      grid::rectGrob(gp = grid::gpar(fill = Rtfordfunds::hf_palette["Light Gray"], lwd = NA)),

      #Combine the plot, a line break and a caption
      gridExtra::arrangeGrob(
        plot,
        #line break
        grid::rectGrob(height = 0,
                       gp = grid::gpar(fill = Rtfordfunds::hf_palette["Light Gray"],
                                       lwd = 3,
                                       col = Rtfordfunds::hf_palette["Cool Gray 9"])),

        #left caption; typically used to describe the data source
        grid::textGrob(left_caption, just = "left", x = 0.05, y = .5,
                       gp = grid::gpar(fontface = "bold",
                                       fontsize = fontsize,
                                       col = Rtfordfunds::hf_palette["Tagline Gray"],
                                       fontfamily = "sans")),

        #right caption, typically used to name the company or department
        grid::textGrob(right_caption, just = "right", x = 0.95, y = .5,
                       gp = grid::gpar(fontface = "bold",
                                       fontsize = fontsize,
                                       col = Rtfordfunds::hf_palette["Tagline Gray"],
                                       fontfamily = "sans")),

        #layout the plot, line break and captions, with the plot using 95% of the area
        layout_matrix = rbind(c(1, 1),
                              c(2, 2),
                              c(3, 4)),
        heights = c(.95, .01, .04)
      )
    )

    #draw the plot as part of the function, but return the grob
    grid::grid.draw(captioned_plot)
    captioned_plot

}
