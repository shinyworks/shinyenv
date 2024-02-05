# ---
# repo: shinyworks/shinyenv
# file: standalone-envvars.R
# last-updated: 2023-02-05
# license: https://unlicense.org
# imports: [rlang, shiny]
# ---
#
# Provides a set of functions to set (`.shiny_setenv`), get  (`.shiny_getenv`),
# and unset  (`.shiny_unsetenv`) environment-like variables in Shiny. Exported
# versions of these functions are available without the `.` prefix in the
# installed {shinyenv} package.
#
# ## Changelog
#
# 2023-02-05:
#
# * Initial version.
#
# nocov start

#' Set Shiny User Variables
#'
#' Variables set using [Sys.setenv()] are available to *all* users of a given
#' Shiny app. This is not always desirable. [shiny_setenv()] allows you to set
#' environment-like variables which are only available to the current user, but
#' which are available within any Shiny module within the app.
#'
#' @param ... Named arguments with values coercible to a string.
#' @param session A Shiny session object. Defaults to the current session.
#'
#' @return A logical vector of `TRUE` values the same length as `...`.
#' @noRd
.shiny_setenv <- function(..., session = shiny::getDefaultReactiveDomain()) {
  # TODO: Validate ... (named) before using it.
  dots <- list(...)
  success <- .mapply(
    \(x, i) {
      # TODO: Stabilize x and i to character scalar.
      session$userData[[i]] <- as.character(x)
      return(TRUE)
    },
    list(x = dots, i = names(dots)),
    MoreArgs = list()
  )
  return(invisible(unlist(success)))
}

#' Get Shiny User Variables
#'
#' Get values from the user data environment of a Shiny session.
#'
#' @inheritParams shiny_setenv
#' @param nms A character vector of names.
#' @param unset The value to return for unset variables.
#'
#' @return A character vector of values.
#' @noRd
.shiny_getenv <- function(nms,
                          unset = "",
                          session = shiny::getDefaultReactiveDomain()) {
  # TODO: Validate nms (character) and unset (character scalar).
  unlist(
    .mapply(
      \(name) {
        if (name %in% names(session$userData)) {
          return(session$userData[[name]])
        }
        return(unset)
      },
      dots = list(name = nms),
      MoreArgs = list()
    )
  )
}

#' Unset Shiny User Variables
#'
#' Remove variables from the user data environment of a Shiny session.
#'
#' @inheritParams shiny_getenv
#'
#' @return The modified user data environment, invisibly.
#' @noRd
.shiny_unsetenv <- function(nms, session = shiny::getDefaultReactiveDomain()) {
  return(invisible(rlang::env_unbind(session$userData, nms)))
}

# nocov end
