#' Delete `output/` directory
#'
#'
#' @importFrom fs dir_exists dir_delete dir_create path_expand
#' @examples
clear_output_dir <- function() {
  if (dir_exists("output/")) {
    dir_delete("output/")
    dir_create("output/")
  } else {
    warning(
      path_expand("output/"),
      "\n",
      " i.e. `output/` doesn't exist"
    )
  }
}
