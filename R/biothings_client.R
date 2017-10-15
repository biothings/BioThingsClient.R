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
#'
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
    endpoints = c("query" = "query", "taxon" = "taxon",
                  "taxon id" = "taxon/:taxonid",
                  "metadata" = "metadata",
                  "metadata fields" = "metadata/fields")),
    common_kwargs
  ),
  chem = list(
    base_url = "http://mychem.info/v1",
    user_agent = "MyChem.R",
    endpoints = c("query" = "query", "drug" = "drug",
                  "drug id" = "drug/:drugid",
                  "metadata" = "metadata",
                  "metadata fields" = "metadata/fields"),
    delay = 1,
    step = 10,
    max_query = 10
  )
)
