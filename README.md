# `BioThingsClient`
[![Travis-CI Build Status](https://travis-ci.org/biothings/BioThingsClient.R.svg?branch=master)](https://travis-ci.org/biothings/biothings_client.R)

This is an R package that provides access to the [BioThings APIs](biothings.io):
* [Chemicals](http://mychem.info/)
* [Taxons](http://t.biothings.io/)
* [Variants](http://myvariant.info/)
* [Genes](http://mygene.info/)

As new APIs are added, this package will be updated to provide access to them. If there is a new API and the package still hasn't been updated, you can create a new configuration object that will allow you to access such new API.

## Installation

### Installing from GitHub
```r
install.packages("devtools")
library(devtools)
install_github("biothings/BioThingsClient.R")
```

## Usage
The package is intended to be simple to use, without the need to instantiate a class object (though you can! Particularly if you have an updated API configuration). For a given API and relevant id, the user can make requests from the API as follows:

### Accessing the MyGene API
```r
library(BioThingsClient.R)
gene_client <- BioThingsClient("gene")
btGet(gene_client, "1017")
# or:
btGet("gene", "1017")
# or:
btGet("gene", c("9606", "10030"))
```

### Accessing the Taxonomy API
```r
library(BioThingsClient.R)
taxon_client <- BioThingsClient("taxon")
btGet(taxon_client, "9606")
# or:
btGet("taxon", "9606")
```
### Accessing the MyVariant API
```r
library(BioThingsClient.R)
variant_client <- BioThingsClient("variant")
btGet(variant_client, "chr7:g.140453134T>C")
```
### Accessing the MyChem API
```r
library(BioThingsClient.R)
chem_client <- BioThingsClient("chem")
```

### Viewing available clients and endpoints

```r
library(BioThingsClient.R)
biothings_clients
```

### Accessing endpoints of the APIs with the query method:
```r
btQuery(chem_client, "drugbank.name:celecoxib")

btQuery(variant_client, c("rs58991260", "rs2500"))
```

## Existing Issues
There are some issues with requesting data frames for certain APIs and endpoints. Some chemical responses are so large that fromJSON will hang. Others have so many fields that the data frame will be heavily nested and hard to use. We recommend avoid requesting data frames without specifying certain fields.
