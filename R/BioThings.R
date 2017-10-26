#' @include utils.R
#' @include biothings_client.R

btversion <- '0.1'

#' @title BioThings
#'
#' @description An S4 Class to access BioThings APIs.
#'
#' @slot client A client configuration list.
#' @slot version The version of the BioThings package.
#' @slot verbose logical.
#' @slot debug logical.
#'
#' @return An S4 class object of the class Biothings.
#' @export BioThings
#' @exportClass BioThings
#' @name BioThings-class
#' @rdname BioThings-class
#'
#' @examples
#' biothings <- BioThings("gene")
#' slot(biothings, "verbose") <- FALSE # default is TRUE
#' biothings
BioThings <- setClass("BioThings",
                      slots = list(client = "list", version = "character",
                                   verbose = "logical", debug = "logical"))

#' @keywords internal
validBiothingsObject <- function(object) {
  errors <- character(0)
  for (sn in c("delay", "step")) {
    if (length(slot(object, sn)) != 1)
      errors[length(errors) + 1] <- sprintf("Slot %s must have length 1", sn)
  }

  if (length(slot(object, "clients")) < 1) {
    errors[length(errors) + 1] <- paste0("client object missing. Necessary ",
                                         "to define API interaction. client ",
                                         "object: ", slot(object, "clients"))
  }

  if (length(errors) > 0) {
    errors
  } else {
    TRUE
  }
}

setValidity("BioThings", validBiothingsObject)

#' @keywords internal
setMethod("initialize",
          signature = "BioThings",
          function(.Object, client, version = version,
                   verbose = TRUE, debug = FALSE) {
  if (is.character(client))
    slot(.Object, "client") <- biothings_clients[[tolower(client)]]
  else if (is.list(client))
    slot(.Object, "client") <- client
  else
    stop("Invalid client argument value.",
         "Available clients from biothings_clients object:\n",
         paste(names(biothings_clients), collapse = "\n"))
  slot(.Object, "version") <- btversion
  slot(.Object, "verbose") <- verbose
  slot(.Object, "debug") <- debug
  return(.Object)
})

#' @keywords internal
setGeneric(".request.get", signature = c("biothings"),
           function(biothings, path, params = list()) {
  standardGeneric(".request.get")
})

#' @keywords internal
setMethod(".request.get", c(biothings = "BioThings"),
          function(biothings, path, params = list()) {
  client_config <- slot(biothings, "client")

  url <- paste(client_config$base_url, path, sep = "/")
  headers <- c('User-Agent' = sprintf('R-httr_biothings_%s/httr.%s',
                                      client_config$user_agent,
                                      version))

  if (exists('params')) {
    if (slot(biothings, "debug")) {
      res <- httr::GET(url, query = params, httr::verbose())
    } else {
      res <- httr::GET(url, query = params,
                       config = httr::add_headers(headers))
    }
  }
  if (res$status_code != 200)
    stop("Request returned unexpected status code:\n",
         paste(capture.output(print(res)), collapse = "\n"))
  httr::content(res, "text")
})

#' @keywords internal
setGeneric(".request.post", signature = c("biothings"),
           function(biothings, path, params = list()) {
  standardGeneric(".request.post")
})

#' @keywords internal
setMethod(".request.post", c(biothings = "BioThings"),
          function(biothings, path, params = list()) {
  client_config <- slot(biothings, "client")

  url <- paste(client_config$base_url, path, sep = "/")
  headers <- c(#'Content-Type' = 'application/x-www-form-urlencoded',
               'User-Agent' = sprintf('R-httr_biothings_%s/httr.%s',
                                      client_config$user_agent,
                                      version))


  if (exists('params')) {
    if (slot(biothings, "debug")) {
      res <- httr::POST(url, body = params,
                        config = httr::add_headers(headers),
                        httr::verbose())
    }
    else {
      res <- httr::POST(url, body = params,
                        config = httr::add_headers(headers))
    }
  }
  result <- httr::content(res, "text")

  if (res$status_code != 200)
    stop("Request returned unexpected status code:\n",
         paste(capture.output(print(res)), collapse = "\n"))
  result
})

#' @keywords internal
.repeated.query <- function(biothings, path, vecparams, params = list()) {
  client_config <- slot(biothings, "client")
  verbose <- slot(biothings, "verbose")
  vecparams.split <- .transpose.nested.list(lapply(vecparams, .splitBySize,
    maxsize = client_config$step))
  if (length(vecparams.split) <= 1) {
    verbose <- FALSE
  }
  vecparams.splitcollapse <- lapply(vecparams.split, lapply, .collapse)
  start <- TRUE
  reslist <- lapply(vecparams.splitcollapse, function(vecparams_) {
    query_params <- c(params, vecparams_)

    if (!start)
      Sys.sleep(client_config$delay)
    else
      assign("start", FALSE, envir = parent.frame())

    .request.post(biothings = biothings, path = path, params = query_params)
  })

  rm(start)
  # This gets the text that would have been returned if we could submit all
  # genes in a single query.
  #restext <- .json.batch.collapse(reslist)
  #return(restext)
  reslist
}
