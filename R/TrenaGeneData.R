#----------------------------------------------------------------------------------------------------
#'
#' @import methods
#'
#' @name TrenaGeneData-class
#' @rdname TrenaGeneData-class
#' @aliases TrenaGeneData
#' @exportClass TrenaGeneData

.TrenaGeneData <- setClass ("TrenaGeneData",
                     slots = c(geneSymbol="character",
                               genomeName="character",
                               footprintDatabaseNames="character",
                               expressionDatasetDirectory="character",
                               expressionDatasetNames="character")
                     )

#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#----------------------------------------------------------------------------------------------------
setGeneric("getGeneSymbol",                 signature="obj", function(obj) standardGeneric ("getGeneSymbol"))
setGeneric("getGenomeName",                 signature="obj", function(obj) standardGeneric ("getGenomeName"))
setGeneric("getFootprintDatabaseNames",     signature="obj", function(obj) standardGeneric ("getFootprintDatabaseNames"))
setGeneric("getExpressionDatasetDirectory", signature="obj", function(obj) standardGeneric ("getExpressionDatasetDirectory"))
setGeneric("getExpressionDatasetNames",     signature="obj", function(obj) standardGeneric ("getExpressionDatasetNames"))
#----------------------------------------------------------------------------------------------------
#' Define an object of class TrenaGeneData
#'
#' @description
#' an abstract base class
#'
#' @rdname TrenaGeneData-class
#'
#' @param geneSymbol character, the HUGO gene name
#' @param footprintDatabaseNames, character vector, zero or more Postgres databases
#' @param expressionDatasetNames, character vector, zero or more RData filenames
#'
#' @return An object of the TrenaGeneData class
#'
#' @export
#'

TrenaGeneData <- function(geneSymbol,
                          genomeName,
                          footprintDatabaseNames=NA_character_,
                          expressionDatasetDirectory=NA_character_,
                          expressionDatasetNames=NA_character_)

{
   .TrenaGeneData(geneSymbol=geneSymbol,
                  genomeName=genomeName,
                  footprintDatabaseNames=footprintDatabaseNames,
                  expressionDatasetDirectory=expressionDatasetDirectory,
                  expressionDatasetNames=expressionDatasetNames)

} # TrenaGeneData, the constructor
#----------------------------------------------------------------------------------------------------
#' the HUGO gene symbol
#'
#' @rdname getGeneSymbol
#'
#' @param obj a TrenaGeneData (or subclass) object
#'
#' @return character, the HUGO symbol for this gene
#'
#' @export
#'
setMethod(getGeneSymbol, "TrenaGeneData",

      function(obj){
         obj@geneSymbol
         })

#----------------------------------------------------------------------------------------------------
#' for instance, hg38
#'
#' @rdname getGenomeName
#'
#' @param obj a TrenaGeneData (or subclass) object
#'
#' @return character, the standard code for the genome build for the data offered here
#'
#' @export
#'
setMethod(getGenomeName, "TrenaGeneData",

      function(obj){
         obj@genomeName
         })

#----------------------------------------------------------------------------------------------------
#' one or more postgress database names, assumed for now to be on khaleesi
#'
#' @rdname getFootprintDatabaseNames
#'
#' @param obj a TrenaGeneData (or subclass) object
#'
#' @return character vector, a list of one or more postgres database names
#'
#' @export
#'
setMethod(getFootprintDatabaseNames, "TrenaGeneData",

      function(obj){
          obj@footprintDatabaseNames
          })

#----------------------------------------------------------------------------------------------------
#' one or more descriptive filenames, .RData suffix stripped off
#'
#' @rdname getExpressionDatasetNames
#'
#' @param obj a TrenaGeneData (or subclass) object
#'
#' @return character vector, a list of one or more expression data sets
#'
#' @export
#'
setMethod(getExpressionDatasetNames, "TrenaGeneData",

      function(obj){
         sub(".RData", "", obj@expressionDatasetNames, fixed=TRUE)
         })

#----------------------------------------------------------------------------------------------------
#' local directory where expression data RData files are foudn
#'
#' @rdname getExpressionDatasetDirectory
#'
#' @param obj a TrenaGeneData (or subclass) object
#'
#' @return character vector, the full path to the direcotyr
#'
#' @export
#'
setMethod(getExpressionDatasetDirectory, "TrenaGeneData",

      function(obj){
         sub(".RData", "", obj@expressionDatasetDirectory, fixed=TRUE)
         })

#----------------------------------------------------------------------------------------------------
