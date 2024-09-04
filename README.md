
# disEpiODE

<!-- badges: start -->
<!-- badges: end -->

The goal of disEpiODE is to ...

## Installation

The latest stable version of the package can be installed from our drat repository as follows:

```{r eval=FALSE}
install.packages("disEpiODE",
  repos=c("https://cran.rstudio.com/", "https://ku-awdc.github.io/drat/"))
```

Alternatively, the development version can be installed directly from GitHub using the remotes package:

```{r eval=FALSE}
install.packages("remotes")  # if necessary
remotes::install_github("https://github.com/ku-awdc/disEpiODE")
```

Both of these methods should also install the dependent packages, including deSolve (although the remotes package may need to . Binary versions of the package are not provided because the package does not contain compiled code, so should be installable from source on all platforms.

Once the package is installed, it can be loaded as usual and the package vignette can be loaded for further instructions:

```{r}
library("disEpiODE")
vignette("disEpiODE", package="disEpiODE")
```
