#' @include BioThings.R

#' @title btQuery
#'
#' @description
#' Retrieve results from the query endpoint of BioThings APIs
#'
#' @param q A query string
#' @param scopes One or more fields (separated by comma) as the search "scopes"
#' @param ... Any parameters to pass to API
#' @param fetch_all This returns a list of _all_ results for a query,
#' regardless of \code{return.as}. See the API documentation.
#' @param return.as Type of return value
#' @param biothings An S4 class BioThings object
#' @return Returns the API result as the provided return.as type
#'
#' @export btQuery
#' @docType methods
#' @rdname btQuery-methods
#'
#' @examples
#' btQuery("gene", q = "NM_013993")
#' gene_client <- BioThings("gene")
#' btQuery(gene_client, "NM_013993")
setGeneric("btQuery", signature = c("biothings"),
           function(biothings, q, ..., fetch_all = FALSE, scopes,
                    return.as = c("records", "data.frame", "text")) {
  standardGeneric("btQuery")
})

#' @rdname btQuery-methods
setMethod("btQuery", c(biothings = "BioThings"),
          function(biothings, q, ..., fetch_all = FALSE, scopes,
                   return.as = c("records", "data.frame", "text")) {
  if (all.equal(return.as, c("records", "data.frame", "text")))
    return.as = "records"
  if (is.character(biothings))
    biothings <- BioThings(biothings)
  client_config <- slot(biothings, "client")
  params <- list(...)
  if (length(q) == 1) {
    params$q <- q
    params$fetch_all <- fetch_all
    res <- .request.get(biothings, client_config$endpoints$query$path,
                        params)

    if (fetch_all) {
      resl <- .return.as(res, "records")[[1]]

      results <- resl$hits

      if ("_scroll_id" %in% names(resl)) {
        if (return.as != "records" & slot(biothings, "verbose"))
          message("fetch_all requires the return type to be records. ",
                  "Returning records.")

        if (slot(biothings, "verbose"))
          message("Getting additional records. Took: ", resl$took)

        scroll_id <- TRUE
        params$scroll_id <- resl[["_scroll_id"]]

        while (scroll_id) {
          scroll <- .request.get(biothings, client_config$endpoints$query$path,
                                 params)
          scroll <- .return.as(scroll, "records")[[1]]

          if (!("error" %in% names(scroll))) {
            if (slot(biothings, "verbose"))
              message("Getting additional records. Took: ", scroll$took)

            params$scroll_id <- scroll[["_scroll_id"]]
            results <- c(results, scroll$hits)
          } else
            scroll_id <- FALSE
        }
      }
      return(results)
    } else {
      if (return.as == "data.frame") {
        return(jsonlite::fromJSON(res))
      } else if (return.as == "text") {
        return(.return.as(res, "text"))
      } else if (return.as == "records") {
        return(.return.as(res, "records"))
      }
    }
  } else if (is.vector(q)) {
    queryMany(biothings = biothings, qterms = q, scopes = scopes, ...,
              return.as = return.as)
  }
})

#' @rdname btQuery-methods
setMethod("btQuery", c(biothings = "missing"),
          function(biothings, q, ...,  fetch_all, scopes, return.as) {
  message("No BioThings client object provided.")
  message("Available clients:")
  message(paste(names(biothings_clients), collapse = "\n"))
  client <- readline("Enter a client name: ")
  btclient <- BioThings(client = biothings_clients[[client]])
  btQuery(biothings, q, client, ..., fetch_all = fetch_all,
          return.as = return.as)
})

#' @rdname btQuery-methods
setMethod("btQuery", c(biothings = "character"),
          function(biothings, q, ...,  fetch_all, scopes, return.as) {
  biothings <- BioThings(biothings)
  btQuery(biothings, q, ..., fetch_all = fetch_all,
          return.as = return.as)
})

# queryMany ---------------------------------------------------------------

#' @keywords internal
setGeneric("queryMany", signature = c("biothings"),
           function(biothings, qterms, scopes = NULL, ...,
                    return.as = c("records", "data.frame", "text")) {
  standardGeneric("queryMany")
})

#' @keywords internal
setMethod("queryMany", c(biothings = "BioThings"),
          function(biothings, qterms, scopes = NULL, ...,
                   return.as = c("records", "data.frame", "text")) {
  client_config <- slot(biothings, "client")
  params <- list(...)
  vecparams <- list(q = .uncollapse(qterms))
  if (exists('scopes')) {
    params <- lapply(params, .collapse)
    params[['scopes']] <- .collapse(scopes)
    returnall <- .pop(params, 'returnall', FALSE)
    params['returnall'] <- NULL
    verbose <- slot(biothings, "verbose")

    if (length(qterms) == 0) {
      return(list())
    }

    out <- .repeated.query(biothings, client_config$endpoints$query$path,
                           vecparams = vecparams, params = params)

    out.li <- .return.as(out, "records")

    found <- sapply(out.li, function(x) is.null(x$notfound))
    li_missing <- as.character(lapply(out.li[!found],
                                      function(x) x[['query']]))
    li_query <- as.character(lapply(out.li[found],
                                    function(x) x[['query']]))

    #check duplication hits
    count <- as.list(table(li_query))
    li_dup <- data.frame(count[count > 1])

    if (verbose) {
      cat("Finished\n")
      if (length('li_dup') > 0) {
        sprintf('%f input query terms found dup hits:   %s', length(li_dup),
                li_dup)
      }
      if (length('li_missing') > 0) {
        sprintf('%f input query terms found dup hits:   %s',
                length(li_missing), li_missing)
      }
    }
    out <- .return.as(out, return.as = return.as)
    if (returnall) {
      return(list("response" = out, 'duplicates' = li_dup,
                  'missing' = li_missing))
    } else {
      if (verbose & ((length(li_dup) >= 1) | (length(li_missing) >= 1))) {
        cat('Pass returnall = TRUE to return lists of duplicate or missing',
            'query terms.\n')
      }
      return(out)
    }
  }
})

#' @keywords internal
setMethod("queryMany", c(biothings = "missing"),
          function(biothings, qterms, scopes = NULL, ...,
                   return.as = c("records", "data.frame", "text")) {

  message("No BioThings client object provided.")
  message("Available clients:")
  message(paste(names(biothings_clients), collapse = "\n"))
  client <- readline("Enter a client name: ")
  btclient <- BioThings(client = biothings_clients[[client]])
  # Should use callGeneric here except that callGeneric gets the variable
  # scoping wrong for the "..." argument
  queryMany(biothings, qterms, scopes, ..., return.as = return.as)
})
