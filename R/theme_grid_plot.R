#' Title
#'
#' @return
#' @export
#'
#' @examples
theme_grid_plot <- function() {

  # coord_sf(expand = FALSE) +
    theme_blank_background() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.margin = ggplot2::margin(),
      legend.position = "top"
    ) +
    # theme(text = element_text(size = 20)) +
    # theme(strip.text = element_text(size = 15),
    #       legend.title = element_text(size = 15),
    #       legend.background = element_blank(),
    #       legend.text = element_text(size = 12)) +
    # guides(color = guide_legend(override.aes = list(linewidth = 2))) +
    NULL
}
