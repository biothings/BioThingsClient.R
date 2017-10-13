library(GenomicFeatures)
library(IRanges)
library(sqldf)

version <- '0.1'

common_kwargs <- list(
  delay = 1,
  step = 1000,
  max_query = 1000
)

biothings_clients <- list(
  gene = append(list(
    base_url = "http://mygene.info/v3",
    user_agent = "MyGene.R",
    endpoints = c("query" = "query", "gene" = "gene",
                  "gene id" = "gene/:geneid", "metadata" = "metadata")),
    common_kwargs
  ),
  variant = append(list(
    base_url = "http://myvariant.info/v1",
    user_agent = "MyVariant.R",
    endpoints = c("query" = "query", "variant" = "variant",
                  "variant id" = "variant/:variantid",
                  "metadata" = "metadata",
                  "metadata fields" = "metadata/fields")),
    common_kwargs
  ),
  taxon = append(list(
    base_url = "http://t.biothings.io/v1",
    user_agent = "MyTaxon.R",
    endpoints = c("query" = "query", "variant" = "variant",
                  "taxon id" = "taxon/:taxonid",
                  "metadata" = "metadata",
                  "metadata fields" = "metadata/fields")),
    common_kwargs
  ),
  chem = list(
    base_url = "http://mychem.info/v1",
    user_agent = "MyChem.R",
    endpoints = c("query" = "query", "variant" = "variant",
                  "drug id" = "drug/:drugid",
                  "metadata" = "metadata",
                  "metadata fields" = "metadata/fields"),
    delay = 1,
    step = 10,
    max_query = 10
  )
)

BioThings <- setClass("BioThings",
                      slots = list(clients = "list", version="character",
                                   verbose = "logical", debug = "logical"),
                      prototype = list(clients = biothings_clients,
                                       version = version, verbose = TRUE,
                                       debug = FALSE))

validBiothingsObject <- function(object) {
  errors <- character(0)
  for (sn in c("delay", "step")) {
    if (length(slot(object, sn)) != 1)
      errors[length(errors) + 1] <- sprintf("Slot %s must have length 1", sn)
  }

  if (length(slot(object, "clients")) < 1) {
    errors[length(errors) + 1] <- paste0("clients object missing. Necessary to",
                                         "define API interaction. clients ",
                                         "object: ", slot(object, "clients"))
  }

  if (length(errors) > 0){
    errors
  } else {
    TRUE
  }
}

setValidity("BioThings", validBiothingsObject)

.return.as <- function(gene_obj,
                       return.as = c("data.frame", "records", "text")) {
  return.as <- match.arg(return.as)
  if (return.as == "data.frame") {
    df <- .json2df(gene_obj)
    df <- rename(df, c("X_id" = "_id"))
    df$`_version` <- NULL
    return(df)
  } else if (return.as == "text") {
    return(.json.batch.collapse(gene_obj))
  } else {
    return(jsonlite::fromJSON(.json.batch.collapse(gene_obj), simplifydata.frame = FALSE))}
}

setGeneric(".request.get", signature = c("biothings"),
           function(biothings, client, path, params = list()) {
  standardGeneric(".request.get")
})

setMethod(".request.get", c(biothings = "BioThings"),
          function(biothings, client, path, params = list()) {
  client_config <- biothings@clients[[client]]

  url <- paste(client_config$base_url, path, sep = "/")
  headers <- c('User-Agent' = sprintf('R-httr_biothings_%s/httr.%s',
                                      client_config$user_agent,
                                      version))

  if (exists('params')){
    if (biothings@debug){
      res <- httr::GET(url, query = params, httr::verbose())
    } else {
      res <- httr::GET(url, query = params,
                       config = httr::add_headers(headers))
    }
  }
  if (res$status_code != 200)
    stop("Request returned unexpected status code:\n",
         paste(capture.output(print(res)), collapse="\n"))
  httr::content(res, "text")
})

setGeneric(".request.post", signature = c("biothings"),
           function(biothings, client, path, params = list()) {
  standardGeneric(".request.post")
})

setMethod(".request.post", c(biothings = "BioThings"),
          function(biothings, client, path, params = list()) {
  client_config <- biothings@clients[[client]]

  url <- paste(client_config$base_url, path, sep = "/")
  headers <- c(#'Content-Type' = 'application/x-www-form-urlencoded',
               'User-Agent' = sprintf('R-httr_biothings_%s/httr.%s',
                                      client_config$user_agent,
                                      version))


  if (exists('params')) {
    if (biothings@debug) {
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
  print(result)

  if (res$status_code != 200)
    stop("Request returned unexpected status code:\n",
         paste(capture.output(print(res)), collapse = "\n"))
  result
})

.repeated.query <- function(biothings, client, path, vecparams,
                            params = list()) {
  client_config <- biothings@clients[[client]]
  verbose <- biothings@verbose
  vecparams.split <- .transpose.nested.list(lapply(vecparams, .splitBySize,
    maxsize = client_config$step))
  if (length(vecparams.split) <= 1) {
    verbose <- FALSE
  }
  vecparams.splitcollapse <- lapply(vecparams.split, lapply, .collapse)
  start <- TRUE
  reslist <- lapply(vecparams.splitcollapse, function (vecparams_) {
    query_params <- c(params, vecparams_)

    if (!start)
      Sys.sleep(client_config$delay)
    else
      start <<- FALSE

    .request.post(biothings = biothings, client = client, path = path,
                  params = query_params)
  })

  rm(start)
  # This gets the text that would have been returned if we could submit all
  # genes in a single query.
  #restext <- .json.batch.collapse(reslist)
  #return(restext)
  reslist
}

setMethod("metadata", c(x="BioThings"), function(x, ...) {
  .return.as(.request.get(x, client, "/metadata"), "records")
})

# Ancillary Functions -----------------------------------------------------

# tx.id is a foreign key. matches tx.id from transcripts.
index.tx.id <- function(transcripts, splicings){#, genes){
  transcripts$tx_id <- as.integer(seq_len(nrow(transcripts)))
  new.splicings <- sqldf("SELECT tx_id,
                         exon_rank,
                         exon_start,
                         exon_end,
                         cds_start,
                         cds_end
                         FROM transcripts
                         NATURAL JOIN splicings")
  genes <- sqldf("SELECT tx_id,
                 gene_id
                 FROM transcripts")
  transcripts$num_exons <- NULL
  transcripts$gene_id <- NULL
  transcripts$unique_tx_name <- NULL
  transcripts$cdsstart <- NULL
  transcripts$cdsend <- NULL
  chrominfo <- data.frame(chrom=as.character(unique(transcripts$tx_chrom)),
                          length=rep(NA, length(unique(transcripts$tx_chrom))),
                          is_circular=rep(NA, length(unique(transcripts$tx_chrom))))
  mygene.version <- tryCatch(installed.packages()["mygene", "Version"], error=function(...) "unknown")
  name <- c("mygene version at creation time",
            "Resource URL",
            "mygene API URL")#,
  #"Data source")
  value <- c(mygene.version,
             "http://mygene.info",
             "http://mygene.info/v3")#,
  #"mygene")
  makeTxDb(transcripts, new.splicings, genes, chrominfo,
           metadata=data.frame(name,
                               value,
                               stringsAsFactors=FALSE))
}

# merges like data.frames to single dataframe
merge.df <- function(df.list){
  transcript.list <- lapply(df.list, `[[`, "transcripts")
  splicing.list <- lapply(df.list, `[[`, "splicings")
  transcripts <- do.call(rbind, transcript.list)
  splicings <- do.call(rbind, splicing.list)
  index.tx.id(transcripts, splicings)
}

#initiates data.frames from "records" query
extract.tables.for.gene <- function(query) {
  if (!is.null(names(query$exons[[1]]))){
    query.exons <- query$exons
    txs <- sapply(query.exons, `[[`, "transcript")
    txdf <- data.frame(tx_name=txs, unique_tx_name=txs,
                       num_exons=sapply(query.exons, function(x) nrow(x$position)),
                       sapply(c("chr", "strand", "txstart", "cdsstart", "cdsend", "txend"),
                              function(i) sapply(query.exons, `[[`, i), simplify=FALSE),
                       gene_id=query$`_id`)
    txdf$strand <- factor(ifelse(txdf$strand == 1, "+", "-"), levels=c("+", "-", "*"))
    txdf <- rename(txdf, c(txstart="tx_start", txend="tx_end",
                           chr="tx_chrom", strand="tx_strand"))
    splicings <- data.frame(
      do.call(rbind,
              lapply(seq(1, length(txs), 1), function(txpos) {
                start.end.table <- data.frame(query.exons[[txpos]]$position)
                names(start.end.table)[1:2] <- c("exon_start", "exon_end")
                start.end.table <- start.end.table[order(start.end.table$exon_start, start.end.table$exon_end),]
                eranks <- seq(nrow(start.end.table))
                if (txdf[txpos,]$tx_strand == "-")
                  eranks <- rev(eranks)
                df <- data.frame(start.end.table,
                                 exon_rank=eranks,
                                 unique_tx_name=query.exons[[txpos]]$transcript)
                cds.start <- query.exons[[txpos]]$cdsstart
                cds.end <- query.exons[[txpos]]$cdsend
                if (!is.null(cds.start) && !is.null(cds.end)) {
                  coding <- df$exon_end >= cds.start & df$exon_start <= cds.end
                  df <- data.frame(df, cds_start=NA_integer_, cds_end=NA_integer_)
                  df$cds_start[coding] <- pmax(cds.start, df$exon_start[coding])
                  df$cds_end[coding] <- pmin(cds.end, df$exon_end[coding])
                }
                df
              })))
    df.list <- list(transcripts=txdf, splicings=splicings)
    df.list
  } else {
    query.exons.list <- .transpose.nested.list(query$exons)
    df.list.nested <- mapply(query.exons.list, seq_len(length(query.exons.list)),
                             SIMPLIFY=FALSE,
                             FUN=function(query.exons, dup.num) {
                               txdf <- data.frame(tx_name=names(query.exons),
                                                  unique_tx_name=sprintf("%s.%s", names(query.exons), dup.num),
                                                  num_exons=sapply(query.exons, function(x) nrow(x$exons)),
                                                  sapply(c("chr", "strand", "txstart", "cdsstart", "cdsend", "txend"),
                                                         function(i) sapply(query.exons, `[[`, i), simplify=FALSE),
                                                  gene_id=query$`_id`)
                               txdf$strand <- factor(ifelse(txdf$strand == 1, "+", "-"), levels=c("+", "-", "*"))
                               txdf <- rename(txdf, c(txstart="tx_start", txend="tx_end",
                                                      chr="tx_chrom", strand="tx_strand"))
                               splicings <- data.frame(
                                 do.call(rbind,
                                         lapply(txdf$tx_name, function(txname) {
                                           start.end.table <- data.frame(query.exons[[txname]]$exons)
                                           names(start.end.table)[1:2] <- c("exon_start", "exon_end")
                                           start.end.table <- start.end.table[order(start.end.table$exon_start, start.end.table$exon_end),]
                                           eranks <- seq(nrow(start.end.table))
                                           if (txdf[txname,]$tx_strand == "-")
                                             eranks <- rev(eranks)
                                           df <- data.frame(start.end.table,
                                                            exon_rank=eranks,
                                                            unique_tx_name=sprintf("%s.%s", txname, dup.num))
                                           cds.start <- query.exons[[txname]]$cdsstart
                                           cds.end <- query.exons[[txname]]$cdsend
                                           if (!is.null(cds.start) && !is.null(cds.end)) {
                                             coding <- df$exon_end >= cds.start & df$exon_start <= cds.end
                                             df <- data.frame(df, cds_start=NA_integer_, cds_end=NA_integer_)
                                             df$cds_start[coding] <- pmax(cds.start, df$exon_start[coding])
                                             df$cds_end[coding] <- pmin(cds.end, df$exon_end[coding])
                                           }
                                           df
                                         })))
                               df.list <- list(transcripts=txdf, splicings=splicings)
                               df.list
                             })
    df.list <- lapply(.transpose.nested.list(df.list.nested), do.call, what=rbind)
  }
  df.list
}

# passes gene list to query or queryMany, converts response to txdb
makeTxDbFromMyGene <- function(gene.list, scopes, species, returnall=FALSE){
  if (length(gene.list) == 1) {
    res <- query(gene.list,
                 scopes=scopes,
                 fields="exons",
                 species=species,
                 size=1,
                 return.as="records")$hits
  } else {
    mygene <- MyGene(verbose=FALSE)
    res <- queryMany(gene.list,
                     scopes=scopes,
                     fields="exons",
                     species=species,
                     return.as="records",
                     mygene=mygene)
  }
  has.exons <- sapply(res, function(x) is.null(x$notfound))
  if (all(!has.exons)) {
    stop("No genes from your gene list have available exons annotations")
  } else if (any(!has.exons)) {
    warning("Some genes do not have available exons annotations")
    notfound <- as.character(lapply(res[!has.exons], function(x) x[['query']]))
  }
  res <- res[has.exons]
  txdb <- merge.df(lapply(res, function(i) extract.tables.for.gene(i)))
  if (returnall){
    return(c(txdb, notfound))
  } else {
    txdb
  }
}
