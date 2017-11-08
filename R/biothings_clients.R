common_kwargs <- list(
  delay = 1,
  step = 1000,
  max_query = 1000
)

#' BioThings API Client Configuration
#'
#' This is a list that containts configuration information for each Biothings
#' API:
#'
#' \href{http://biothings.io/}{BioThings} APIs:
#' \itemize{
#'   \item \href{http://mygene.info/}{MyGene}
#'   \item \href{http://myvariant.info/}{MyVariant}
#'   \item \href{http://t.biothings.io/}{MyTaxon}
#'   \item \href{http://mychem.info/}{MyChem}
#' }
#'
#' To integrate a new API into the package, be sure to update this in addition
#' to generating the associated methods from getThing and getThings.
#' @export
biothings_clients <- list(
  gene = append(list(
    entity = "gene",
    base_url = "http://mygene.info/v3",
    user_agent = "MyGene.R",
    endpoints = list("query" = list(path = "query"),
                     "annotation" = list(path = "gene",
                                         return_types = c("records", "text",
                                                          "data.frame"),
                                         fields = c("symbol", "name", "taxid",
                                                    "entrezgene")),
                     "metadata" = list(path = "metadata"),
                     "metadata_fields" = list(path = "metadata/fields"))),
    common_kwargs
  ),
  variant = append(list(
    entity = "variant",
    base_url = "http://myvariant.info/v1",
    user_agent = "MyVariant.R",
    endpoints = list("query" = list(path = "query"),
                  "annotation" = list(path = "variant"),
                  "metadata" = list(path = "metadata"),
                  "metadata_fields" = list(path = "metadata/fields"))),
    common_kwargs
  ),
  taxon = append(list(
    entity = "taxon",
    base_url = "http://t.biothings.io/v1",
    user_agent = "MyTaxon.R",
    endpoints = list("query" = list(path = "query"),
                  "annotation" = list(path = "taxon"),
                  "metadata" = list(path = "metadata"),
                  "metadata_fields" = list(path = "metadata/fields"))),
    common_kwargs
  ),
  chem = list(
    entity = "chemical",
    base_url = "http://mychem.info/v1",
    user_agent = "MyChem.R",
    endpoints = list("query" = list(path = "query"),
                  "annotation" = list(path = "drug"),
                  "metadata" = list(path = "metadata"),
                  "metadata_fields" = list(path = "metadata/fields")),
    delay = 1,
    step = 10,
    max_query = 10
  )
)
