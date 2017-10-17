#' @include utils.R

#' @export BioThingsR6
BioThingsR6 <- R6Class("BioThingsR6",
  public = list(
    api = NULL,
    email = NULL,
    verbose = TRUE,
    debug = FALSE,
    version = "0.1",
    initialize = function(api_config, email = NULL, verbose = TRUE,
                          debug = FALSE) {
      self$api <- api_config
      self$email <- email

      if (!missing(verbose))
        self$verbose <- verbose
      if (!missing(debug))
        self$debug <- debug

      if (is.character(api_config) & exists("biothings_clients"))
        if (api_config %in% names(biothings_clients))
          self$api <- biothings_clients[[api_config]]
        else
          stop("The provided key is not available in the biothings_clients config.")
      else
        stop("Error with the provided api_config argument.")
    },
    getAnnotation = function(id,
                             fields = self$api$endpoints[["annotation"]]$fields,
                             ..., return.as = "records") {
      params <- list(...)
      params$fields <- .collapse(fields)
      print(paste(self$api$endpoints[["annotation"]]$path, id, sep = "/"))
      print(params)
      res <- private$.request.get(paste(self$api$endpoints[["annotation"]]$path,
                                        id, sep = "/"), params)
      .return.as(res, return.as = return.as)
    },
    getAnnotations = function(ids,
                              fields =
                                self$api$endpoints[["annotation"]]$fields, ...,
                              return.as = "records") {
      params <- list()
      if (exists('fields')) {
        params <- list(...)
        params[['fields']] <- .collapse(fields)
        params <- lapply(params, .collapse)
      }

      vecparams <- list(ids = .uncollapse(ids))

      res <- private$.repeated.query(self$api$endpoints[["annotation"]]$path,
                                     vecparams = vecparams, params = params)

      .return.as(res, return.as = return.as)
    },
    query = function(q, ..., return.as = "records") {
      # return.as <- match.arg(return.as)
      params <- list(...)
      params[['q']] <- q

      res <- private$.request.get(self$api$endpoints[["query"]]$path, params)

      if (return.as == "data.frame") {
        return(jsonlite::fromJSON(res))
      } else if (return.as == "text") {
        return(.return.as(res, "text"))
      } else if (return.as == "records") {
        return(.return.as(res, "records"))
      }
    },
    queryMany = function(qterms, scopes = NULL, ...,
                         return.as = "records") {
      # return.as <- match.arg(return.as)

      params <- list(...)
      vecparams <- list(q = .uncollapse(qterms))
      if (exists('scopes')) {
        params <- lapply(params, .collapse)
        params[['scopes']] <- .collapse(scopes)
        returnall <- .pop(params, 'returnall', FALSE)
        params['returnall'] <- NULL
        verbose <- self$verbose

        if (length(qterms) == 0) {
          return(query(qterms, ...))
        }

        out <- private$.repeated.query(self$api$endpoints[["query"]]$path,
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
    }
  ),
  private = list(
    .request.get = function(path, params = list()) {
      url <- paste(self$api$base_url, path, sep = "/")
      headers <- c('User-Agent' = sprintf('R-httr_biothings_%s/httr.%s',
                                          self$api$user_agent,
                                          self$version))

      if (exists('params')) {
        if (self$debug) {
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
    },
    .request.post = function(path, params = list()) {
      url <- paste(self$api$base_url, path, sep = "/")
      headers <- c(#'Content-Type' = 'application/x-www-form-urlencoded',
        'User-Agent' = sprintf('R-httr_biothings_%s/httr.%s',
                               self$api$user_agent,
                               version))

      if (exists('params')) {
        if (self$debug) {
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
    },
    .repeated.query = function(path, vecparams, params = list()) {

      verbose <- self$verbose
      vecparams.split <- lapply(vecparams, .splitBySize,
                                maxsize = self$api$step) %>%
        .transpose.nested.list()

      if (length(vecparams.split) <= 1) {
        verbose <- FALSE
      }
      vecparams.splitcollapse <- lapply(vecparams.split, lapply, .collapse)
      start <- TRUE
      reslist <- lapply(vecparams.splitcollapse, function(vecparams_) {
        query_params <- c(params, vecparams_)

        if (!start)
          Sys.sleep(self$api$delay)
        else
          assign("start", FALSE, envir = parent.frame())

        private$.request.post(path = path, params = query_params)
      })

      rm(start)
      # This gets the text that would have been returned if we could submit all
      # genes in a single query.
      #restext <- .json.batch.collapse(reslist)
      #return(restext)
      reslist
    }
  )
)
