baseModelObject <- function(type, id) {
  list(
    model = list(
      type = type,
      id = id,
      attributes = list(
        id = id,
        tags = list(),
        doc = NULL
      )
    ),
    ref = list(
      type = type,
      id = id
    )
  )
}

#' @importFrom digest digest
genId <- function(obj, name = NULL) {
  digest(c(name, obj$time))
}

getJSON <- function(obj) {
  names(obj$plot$attributes$tools) <- NULL
  names(obj$plot$attributes$renderers) <- NULL  
  names(obj) <- NULL
  obj
}

printJSON <- function(obj) {
  cat(toJSON(getJSON(obj$model), pretty = TRUE))
}

underscore2camel <- function(x) {
  x <- gsub("^([a-zA-Z])", "\\U\\1", x, perl = TRUE)
  gsub("_([a-zA-Z])", "\\U\\1", x, perl = TRUE)
}