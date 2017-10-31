#' @include BioThings.R

# btGet ----------------------------------------------------------------

#' @title Get items from BioThings APIs
#'
#' @description
#' Use gene, taxon, variant, chemical and other IDs to get annotation
#' information from BioThings APIs.
#'
#' @param biothings BioThings object or character .
#' @param ids character.
#' @param fields Specify fields to receive from API
#' @param ... Other parameters to pass to API
#' @param return.as character.
#'
#' @return The API response in the form of the provided argument return.as
#'
#' @export
#' @docType methods
#' @rdname btGet-methods
#'
#' @examples
#' #Equivalent:
#'
#' btGet("gene", "1017", fields = c("symbol","name","taxid","entrezgene"),
#'       return.as = "text")
setGeneric("btGet", signature = c("biothings"),
           function(biothings, ids, fields, ..., return.as = "records") {
  standardGeneric("btGet")
})

#' @rdname btGet-methods
setMethod("btGet", signature = c(biothings = "BioThings"),
          function(biothings, ids, fields, ..., return.as = "records") {
  params <- list(...)
  if (!missing(fields))
    params$fields <- .collapse(fields)
  if (!(return.as %in% c("records", "data.frame", "text"))) {
    warning(return.as, " not in in 'records', 'data.frame' or 'text'\n",
            "Defaulting to 'records'")
    return.as <- "records"
  }
  if (is.character(biothings))
    biothings <- BioThings(biothings)
  client_config <- slot(biothings, "client")
  if (length(ids) == 1) {
    res <- .request.get(biothings,
                        paste(client_config$endpoints$annotation$path,
                              ids, sep = "/"), params)
  } else if (length(ids) > 1) {
    params <- lapply(params, .collapse)
    vecparams <- list(ids = .uncollapse(ids))

    res <- .repeated.query(biothings,
                           client_config$endpoints$annotation$path,
                           vecparams = vecparams, params = params)
  }
  .return.as(res, return.as = return.as)
})

#' @rdname btGet-methods
setMethod("btGet", signature = c(biothings = "character"),
          function(biothings, ids, fields, ..., return.as = "records") {
  biothings <- BioThings(biothings)
  btGet(biothings, ids = ids, fields = fields, ..., return.as = return.as)
})

#' @rdname btGet-methods
setMethod("btGet", c(biothings = "missing"),
          function(biothings, ids, fields, ..., return.as = "records") {
  message("No BioThings client object provided.")
  message("Available clients:")
  message(paste(names(biothings_clients), collapse = "\n"))
  client <- readline("Enter a client name: ")
  btclient <- BioThings(client = biothings_clients[[client]])
  btGet(biothings = btclient, ids = ids, fields = fields, ...,
        return.as = return.as)
})
