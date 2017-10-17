#' @include BioThings.R

# query -------------------------------------------------------------------

#' @title query
#' Retrieve results from the query endpoint of BioThings APIs
#'
#' @param q A query string
#' @param qterms A vector of query strings
#' @param client A BioThings client name
#' @param ... Any parameters to pass to API
#' @param return.as Type of return value
#' @param biothings An S4 class BioThings object
#' @return Returns the API result as the provided return.as type
#' @name query
#' @exportMethod query
#'
#' @examples
#' query(q="NM_013993", client = "gene")
setGeneric("query", signature = c("biothings"),
           function(q, client, ...,
                    return.as = c("records", "data.frame", "text"), biothings) {
  standardGeneric("query")
})

setMethod("query", c(biothings = "BioThings"),
          function(q, client, ...,
                   return.as = c("records", "data.frame", "text"), biothings) {
  if (all.equal(return.as, c("records", "data.frame", "text")))
    return.as = "records"
  # return.as <- match.arg(return.as)
  params <- list(...)
  params[['q']] <- q
  client_config <- biothings@clients[[client]]
  res <- .request.get(biothings, client,
                      client_config$endpoints$query$path, params)

  if (return.as == "data.frame") {
    return(jsonlite::fromJSON(res))
  } else if (return.as == "text") {
    return(.return.as(res, "text"))
  } else if (return.as == "records") {
    return(.return.as(res, "records"))
  }
})

setMethod("query", c(biothings = "missing"),
          function(q, client, ..., return.as, biothings) {
  biothings <- BioThings()
  query(q, client, ..., return.as = return.as, biothings = biothings)
})

# queryMany ---------------------------------------------------------------

#' @rdname query
#' @exportMethod queryMany
setGeneric("queryMany", signature = c("biothings"),
           function(qterms, client, scopes = NULL, ...,
                    return.as = c("records", "data.frame", "text"), biothings) {
  standardGeneric("queryMany")
})

setMethod("queryMany", c(biothings = "BioThings"),
          function(qterms, client, scopes = NULL, ...,
                   return.as = c("records", "data.frame", "text"),
                   biothings) {
  # return.as <- match.arg(return.as)
  if (all.equal(return.as, c("records", "data.frame", "text")))
    return.as = "records"
  client_config <- biothings@clients[[client]]
  params <- list(...)
  vecparams <- list(q = .uncollapse(qterms))
  if (exists('scopes')) {
    params <- lapply(params, .collapse)
    params[['scopes']] <- .collapse(scopes)
    returnall <- .pop(params, 'returnall', FALSE)
    params['returnall'] <- NULL
    verbose <- biothings@verbose

    if (length(qterms) == 0) {
      return(query(qterms, ...))
    }

    out <- .repeated.query(biothings, client,
                           client_config$endpoints$query$path,
                           vecparams = vecparams, params = params)

    out.li <- .return.as(out, "records")

    found <- sapply(out.li, function(x) is.null(x$notfound))
    li_missing <- as.character(lapply(out.li[!found], function(x) x[['query']]))
    li_query <- as.character(lapply(out.li[found], function(x) x[['query']]))

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
        sprintf('%f input query terms found dup hits:   %s', length(li_missing),
                li_missing)
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

setMethod("queryMany", c(biothings = "missing"),
          function(qterms, client, scopes = NULL, ...,
                   return.as = c("records", "data.frame", "text"),
                   biothings){

  biothings <- BioThings()
  # Should use callGeneric here except that callGeneric gets the variable scoping wrong for the "..." argument
  queryMany(qterms, client, scopes, ..., return.as = return.as,
            biothings = biothings)
})
