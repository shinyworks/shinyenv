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
#' @export
#'
#' @examples
#' session <- rlang::env(userData = rlang::env())
#' shiny_setenv(foo = "bar", baz = "foo", session = session)
#' session$userData$foo
#' session$userData[["baz"]]
shiny_setenv <- function(..., session = shiny::getDefaultReactiveDomain()) {
  # TODO: Validate ... (named) before using it.
  success <- purrr::imap_lgl(rlang::list2(...), \(x, i) {
    # TODO: Stabilize x and i to character scalar.
    session$userData[[i]] <- as.character(x)
    return(TRUE)
  })
  return(invisible(unname(success)))
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
#' @export
#'
#' @examples
#' session <- rlang::env(userData = rlang::env(foo = "bar", baz = "foo"))
#' shiny_getenv(c("foo", "baz"), session = session)
#' shiny_getenv("qux", unset = "unset", session = session)
shiny_getenv <- function(nms,
                         unset = "",
                         session = shiny::getDefaultReactiveDomain()) {
  # TODO: Validate nms (character) and unset (character scalar).
  purrr::map_chr(nms, \(name) {
    if (name %in% names(session$userData)) {
      return(session$userData[[name]])
    }
    return(unset)
  })
}

#' Unset Shiny User Variables
#'
#' Remove variables from the user data environment of a Shiny session.
#'
#' @inheritParams shiny_getenv
#'
#' @return The modified user data environment, invisibly.
#' @export
#'
#' @examples
#' session <- rlang::env(
#'   userData = rlang::env(foo = "bar", baz = "foo", bar = "baz")
#' )
#' shiny_unsetenv(c("foo", "baz"), session = session)
#' names(session$userData)
shiny_unsetenv <- function(nms, session = shiny::getDefaultReactiveDomain()) {
  return(invisible(rlang::env_unbind(session$userData, nms)))
}
