local({

  # the renv version to be used
  RENV_VERSION <- "1.0.7"

  # the path to the root directory of the project
  root <- getwd()

  # do nothing if renv is already loaded
  if ("renv" %in% loadedNamespaces()) {
    invisible(renv::load(root))
    return(invisible(NULL))
  }

  # the path to the 'renv' library
  renv_path <- file.path(root, "renv", "library",
                         paste0("R-", getRversion()[, 1:2]),
                         .Platform$r_arch %||% "",
                         "renv")

  if (file.exists(renv_path)) {
    if (!requireNamespace("renv", lib.loc = dirname(renv_path), quietly = TRUE)) {
      install.packages("renv", lib = dirname(renv_path),
                       repos = c(CRAN = "https://cloud.r-project.org"))
    }
    library(renv, lib.loc = dirname(renv_path))
    invisible(renv::load(root))
    return(invisible(NULL))
  }

  # bootstrap renv if not available
  message("* renv não encontrado — instalando versão ", RENV_VERSION, "...")
  install.packages("renv", repos = c(CRAN = "https://cloud.r-project.org"))
  renv::init(bare = TRUE)
  invisible(NULL)
})
