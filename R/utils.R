#' @include biothings-package.R

#' @keywords internal
.collapse <- function(...) {
    paste(unlist(list(...)), sep = ",", collapse = ",")
}

#' @keywords internal
.transpose.nested.list <- function(li) {
    ## Assumes that inner names of each element are the same
    if (length(li) == 0)
      return(li)
    inner.i <- seq_along(li[[1]])
    res <- lapply(inner.i, function(i) lapply(li, `[[`, i))
    names(res) <- names(li[[1]])
    res
}

#' @importFrom Hmisc cut2
#' @keywords internal
.splitBySize <- function(x, maxsize) {
    n <- length(x)
    num.chunks <- ceiling(n / maxsize)
    f <- cut2(1:n, g = num.chunks)
    unname(split(x, f))
}

#' @keywords internal
.pop <- function(list, item, default_value = NULL) {
    if (is.null(list[[item]])) {
        return(default_value)
    }
    else{
        value <- list[[item]]
        return(value)}
}

#' @keywords internal
.unnest <- function(list) {
    while(any(vapply(list, is.list, TRUE))) {
      list <- lapply(list, unlist, recursive = FALSE)
    }
    return(list)
}

#' @keywords internal
.unnest.df <- function(df, recursive=TRUE) {
    reslist <- lapply(colnames(df), function(i) {
        if (is(df[[i]], "data.frame")) {
          if (recursive) {
            df[[i]] <- .unnest.df(df[[i]], recursive = TRUE)
          }
            setNames(df[[i]], paste(i, colnames(df[[i]]), sep = "."))
        }
        else {
            df[i]
        }
    })
    res <- do.call(cbind, reslist)
    row.names(res) <- row.names(df)
    res
}

#' @keywords internal
.convertColumn4csv <- function(column){
  needpc <- sapply(column, is, "CharacterList")
  column[needpc] <- lapply(column[needpc], .collapse)
  column
}

#' @keywords internal
.json.batch.collapse <- function(x){
    #stopifnot(all(grepl("^\\s*\\[.*\\]\\s*$", x, perl=TRUE)))
    x <- gsub(pattern = "^\\s*\\[|\\]\\s*$", replacement = "", x, perl = TRUE)
    x <- paste(x, collapse = ",")
    paste("[", x, "]")
}

#' @keywords internal
.json2df <- function(x){
   li <- lapply(x, jsonlite::fromJSON, flatten = TRUE)
   df <- plyr::rbind.fill(li)
   as.data.frame(df, stringsAsFactors = FALSE)
}

#' @keywords internal
.uncollapse <- function(x, sep = ",") {
    x <- as.character(unlist(x))
    unlist(strsplit(x, sep, fixed = TRUE))
}

#' @keywords internal
.splitCols <- function(split.list, colName){
  lapply(sapply(split.list,
                function(i) {
                  strsplit(i[grepl(colName, i)], "=")
                }),
         function(i) {
           tryCatch(i[[2]], error = function(e) e <- NA_integer_)
         }) %>% as.numeric()
}

#' @keywords internal
.return.as <- function(gene_obj,
                       return.as = c("data.frame", "records", "text")) {
  return.as <- match.arg(return.as)
  if (return.as == "data.frame") {
    df <- .json2df(gene_obj)
    df <- plyr::rename(df, c("X_id" = "_id"))
    df$`_version` <- NULL
    return(df)
  } else if (return.as == "text") {
    return(.json.batch.collapse(gene_obj))
  } else {
    return(jsonlite::fromJSON(.json.batch.collapse(gene_obj),
                              simplifyDataFrame = FALSE))}
}
