version <- '0.5'

MyVariant <- setClass("MyVariant",
    slots=list(base.url="character", delay="numeric", step="numeric", version="character", verbose="logical", debug="logical"),
    prototype=list(base.url="http://myvariant.info/v1", delay=1, step=1000, version=version, verbose=TRUE, debug=FALSE))

validMyVariantObject <- function(object) {
    errors <- character(0)
    for (sn in c("base.url", "delay", "step")) {
        if (length(slot(object, sn)) != 1)
            errors <- c(errors, sprintf("Slot %s must have length 1", sn))
    }
    if (length(errors) > 0){
        errors
    } else {
        TRUE
    }
}

setValidity("MyVariant", validMyVariantObject)

.return.as <- function(gene_obj, return.as=c("DataFrame", "records", "text")) {
    return.as <- match.arg(return.as)
    if (return.as == "DataFrame") {
        df <- .json2df(gene_obj)
        df$`_version` <- NULL
        if("X_id" %in% names(df)){
          names(df)[names(df) == 'X_id'] <- '_id'
        }
        return(DataFrame(df))
    } else if (return.as == "text") {
        return(.json.batch.collapse(gene_obj))
    } else {
        return(fromJSON(.json.batch.collapse(gene_obj), simplifyDataFrame=FALSE))}
}

setGeneric(".request.get", signature=c("myvariant"),
            function(myvariant, path, params=list()) standardGeneric(".request.get"))

setMethod(".request.get", c(myvariant="MyVariant"),
            function(myvariant, path, params=list()){
    url <- paste(myvariant@base.url, path, sep="")
    .headers <- c('User-Agent' = sprintf('R-httr_myvariant.R/httr.%s', version))
    if (exists('params')){
        if (myvariant@debug){
            res <- GET(url, query=params, verbose())
        } else {
            res <- GET(url, query=params, config(add_headers(.headers)))
            }
        }
    if (res$status_code != 200)
        stop("Request returned unexpected status code:\n",
            paste(capture.output(print(res)), collapse="\n"))
    #fromJSON(res)
    #res$`_version` <- NULL
    httr::content(res, "text")
})

setGeneric(".request.post", signature=c("myvariant"),
            function(myvariant, path, params=list()) standardGeneric(".request.post"))

setMethod(".request.post", c(myvariant="MyVariant"),
            function(myvariant, path, params=list()) {
    url <- paste(myvariant@base.url, path, sep="")
    .headers <- c(`Content-Type`='application/x-www-form-urlencoded',
            `User-Agent`=sprintf('R-httr_myvariant.R/httr.%s', version))
    if (exists('params')){
        if (myvariant@debug){
            res <- POST(url, body=params, config(add_headers(.headers), verbose()))
        }
        else{
            res <- POST(url, body=params, config(add_headers(.headers)))
            }
        }
    if (res$status_code != 200)
        stop("Request returned unexpected status code:\n",
             paste(capture.output(print(res)), collapse="\n"))
    httr::content(res, "text")
})


.repeated.query <- function(myvariant, path, vecparams, params=list(), return.as) {
    if (!is.null(params$verbose)) {
        verbose <- params$verbose
    }
    else {
        verbose <- myvariant@verbose
    }
    vecparams.split <- .transpose.nested.list(lapply(vecparams, .splitBySize, maxsize=myvariant@step))
    if (length(vecparams.split) <= 1){
        verbose <- FALSE
    }
    vecparams.splitcollapse <- lapply(vecparams.split, lapply, .collapse)
    n <- length(vecparams.splitcollapse)
    reslist <- character(n)
    i <- 1
    repeat {
        if (verbose) {
          message("Querying chunk ", i, " of ", n)
        }
        params.i <- c(params, vecparams.splitcollapse[[i]])
        reslist[[i]] <- .request.post(myvariant=myvariant, path, params=params.i)
        ## This avoids an extra sleep after the last fragment
        if (i == n){
            message("Concatenating data, please be patient.")
            break()
        }
        Sys.sleep(myvariant@delay)
        i <- i+1
    }
    # This gets the text that would have been returned if we could submit all genes in a single query.
    #restext <- .json.batch.collapse(reslist)
    #return(restext)
   reslist
}

setMethod("metadata", c(x="MyVariant"), function(x, ...) {
    .return.as(.request.get(x, "/metadata"), "records")
})

setGeneric("getVariant", signature=c("myvariant"),
            function(hgvsid, fields=NULL,
            ..., return.as=c("records", "text"), myvariant) standardGeneric("getVariant"))

setMethod("getVariant", c(myvariant="MyVariant"),
            function(hgvsid, fields=NULL,
            ..., return.as=c("records", "text"), myvariant) {
    return.as <- match.arg(return.as)
    params <- list(...)
    params$fields <- .collapse(fields)
    res <- .request.get(myvariant, paste("/variant/", hgvsid, sep=""), params)
    .return.as(res, return.as=return.as)
})

## If nothing is passed for the myvariant argument, just construct a
## default myvariant object and use it.
setMethod("getVariant", c(myvariant="missing"),
            function(hgvsid, fields=NULL,
            ..., return.as=c("records", "text"), myvariant) {

    myvariant <- MyVariant()
    getVariant(hgvsid, fields, ..., return.as=return.as, myvariant=myvariant)
})

setGeneric("getVariants", signature=c("myvariant"),
            function(hgvsids, fields=NULL, verbose=NULL,
            ..., return.as=c("DataFrame", "records", "text"), myvariant) standardGeneric("getVariants"))

setMethod("getVariants", c(myvariant="MyVariant"),
            function(hgvsids, fields=NULL, verbose=NULL,
            ..., return.as=c("DataFrame", "records", "text"), myvariant) {
    return.as <- match.arg(return.as)
    params <- list(...)
    if (exists('fields')) {
        params[['fields']] <- .collapse(fields)
        params <- lapply(params, .collapse)
    }
    if (exists('verbose')) {
        params[['verbose']] <- verbose
    }
    vecparams <- list(ids=.uncollapse(hgvsids))
    res <- .repeated.query(myvariant, '/variant/', vecparams=vecparams, params=params)
    .return.as(res, return.as=return.as)
})

setMethod("getVariants", c(myvariant="missing"),
            function(hgvsids, fields=NULL, verbose=NULL,
            ..., return.as=c("DataFrame", "records", "text"), myvariant) {
    myvariant <- MyVariant()
    getVariants(hgvsids, fields, verbose, ..., return.as=return.as, myvariant=myvariant) #, fields
})

setGeneric("queryVariant", signature=c("myvariant"),
            function(q, ..., return.as=c("DataFrame", "records", "text"), myvariant) standardGeneric("queryVariant"))

setMethod("queryVariant", c(myvariant="MyVariant"),
            function(q, ..., return.as=c("DataFrame", "records", "text"), myvariant) {

    return.as <- match.arg(return.as)
    params <- list(...)
    params[['q']] <- q
    #if(exists('fields')){
    #  params[['fields']] <- .collapse(fields)
      params <- lapply(params, .collapse)
    #}
    res <- .request.get(myvariant, paste("/query/", sep=""), params)
    if (return.as == "DataFrame"){
        return(fromJSON(res))
    } else if (return.as == "text"){
        return(.return.as(res, "text"))
    } else if (return.as == "records"){
        return(.return.as(res, "records"))
    }

})

setMethod("queryVariant", c(myvariant="missing"),
            function(q, ..., return.as=c("DataFrame", "records", "text"), myvariant) {
    myvariant <- MyVariant()
    queryVariant(q, ..., return.as=return.as, myvariant=myvariant)
})

setGeneric("queryVariants", signature=c("myvariant"),
            function(qterms, scopes=NULL, ..., return.as=c("DataFrame",
            "records", "text"), myvariant) standardGeneric("queryVariants"))

setMethod("queryVariants", c(myvariant="MyVariant"),
            function(qterms, scopes=NULL, ..., return.as=c("DataFrame",
            "records", "text"), myvariant){
    return.as <- match.arg(return.as)
    params <- list(...)
    vecparams<-list(q=.uncollapse(qterms))
    if (exists('scopes')){
        params<-lapply(params, .collapse)
        params[['scopes']] <- .collapse(scopes)
        returnall <- .pop(params,'returnall', FALSE)
        params['returnall'] <-NULL
        verbose <- myvariant@verbose

        if (length(qterms) == 0) {
          return(queryVariant(qterms, ...))
        }

        out <- .repeated.query(myvariant, '/query/', vecparams=vecparams, params=params)
        out.li <- .return.as(out, "records")

        found <- sapply(out.li, function(x) is.null(x$notfound))
        li_missing <- as.character(lapply(out.li[!found], function(x) x[['query']]))
        li_query <- as.character(lapply(out.li[found], function(x) x[['query']]))

        #check duplication hits
        count <- as.list(table(li_query))
        li_dup <- data.frame(count[count > 1])

        if (verbose){
            cat("Finished\n")
            if (length('li_dup')>0){
                sprintf('%f input query terms found dup hits:   %s', length(li_dup), li_dup)
            }
            if (length('li_missing')>0){
                sprintf('%f input query terms found dup hits:   %s', length(li_missing), li_missing)
                }
            }
        out <- .return.as(out, return.as=return.as)
        if (returnall){
            return(list("response"=out, 'duplicates'=li_dup, 'missing'=li_missing))
        } else {
            if (verbose & ((length(li_dup)>=1) | (length(li_missing)>=1))){
                cat('Pass returnall=TRUE to return lists of duplicate or missing query terms.\n')
                }
            return(out)
        }
    }
})

setMethod("queryVariants", c(myvariant="missing"),
            function(qterms, scopes=NULL, ...,
            return.as=c("DataFrame", "records", "text"), myvariant){

    myvariant<-MyVariant()
    # Should use callGeneric here except that callGeneric gets the variable scoping wrong for the "..." argument
    queryVariants(qterms, scopes, ..., return.as=return.as, myvariant=myvariant)
})

