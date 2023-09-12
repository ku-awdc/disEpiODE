#' Delete `output/` directory
#'
#'
#'
#' @examples
clear_output_dir <- function() {
  if (fs::dir_exists("output/")) {
    fs::dir_delete("output/")
    fs::dir_create("output/")
  } else {
    warning(
      fs::path_expand("output/"),
      "\n",
      " i.e. `output/` doesn't exist"
    )
  }
}
