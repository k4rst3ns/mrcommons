#' @title calcBioenergyFeedstocksMAgPIE
#' @description Returns biomass supply and/or potentials for the energy sector extracted
#'   from MAgPIE scenario runs (SSP1-5, SDP) at ISO country level (PJ/yr).
#'
#' @param version  MAgPIE version folder name inside the MAgPIEBiomass source directory,
#'   e.g. `"MAgPIE_4.14.0"`.
#' @param category  Which data to return: `"supply"` (wood fuel + manure fuel +
#'   traditional crop residue fuel), `"potential"` (crop residue + wood residue +
#'   biogas potentials), or `"all"` (both combined).
#' @param cropresScen  Crop residue collection scenario — one of: 
#'                     - `"cf0p3_md0"` (30\% collection, no min. density), 
#'                     - `"cf0p3_md2"` (30\%, 2 tDM/ha),
#'                     - `"cf0p3_md4"` (30\%, 4 tDM/ha), 
#'                     - `"cf0p1_md4"` (10\%, 4 tDM/ha).
#' @param woodresFrac  Numeric 0–1. Share of the wood processing residue potential to
#'                     include (default 0.5, i.e. full potential as estimated at 
#'                     30\% of industrial roundwood demand).
#' @param biogasFrac   Numeric 0–1. Share of the biogas feedstock potential to include
#'                     (default 1).
#' @param zeroPast  Numeric year or `FALSE`. If a year is given (e.g. `2025`), crop
#'   residue and wood residue potentials are set to zero for all years up to and
#'   including that year. If `FALSE`, no zeroing is applied. Does not affect supply
#'   or biogas.
#'
#' @return List with a magpie object (ISO × year × scenario.variable), weight NULL,
#'   unit `"PJ/yr"`, and a description string.
#'
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#'   calcOutput("BioenergyFeedstocksMAgPIE", category = "supply")
#'   calcOutput("BioenergyFeedstocksMAgPIE", category = "potential", cropresScen = "cf0p3_md4")
#'   calcOutput("BioenergyFeedstocksMAgPIE", category = "all", woodresFrac = 0.5, zeroPast = TRUE)
#' }
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass mbind collapseNames add_dimension getYears getNames<- getSets<-
#' @importFrom magpiesets findset

calcBioenergyFeedstocksMAgPIE <- function(version     = "MAgPIE_4.14.0",
                                          category     = "all",
                                          cropresScen = "cf0p3_md4",
                                          woodresFrac = 0.5,
                                          biogasFrac  = 1,
                                          zeroPast    = 2025) {

  if (!category %in% c("supply", "potential", "all")) {
    stop("category must be one of: 'supply', 'potential', 'all'")
  }

  validScens <- c("cf0p3_md0", "cf0p3_md2", "cf0p3_md4", "cf0p1_md4")
  if (!cropresScen %in% validScens) {
    stop("cropresScen must be one of: ", paste(validScens, collapse = ", "))
  }

  # Flatten read-function output from scenario.type[.category].variable
  # → scenario.variable by keeping only the first (scenario) and last (item) parts.
  # This drops the type/category prefix used for internal hierarchy in the read function.
  flattenDims <- function(x) {
    n     <- getNames(x)
    parts <- strsplit(n, "\\.", fixed = TRUE)
    getNames(x) <- vapply(parts, function(p) paste(p[1L], p[length(p)], sep = "."),
                          character(1L))
    getSets(x) <- c(getSets(x)[1:2], "scenario", "variable")
    x
  }

  out <- NULL

  # --- Supply ---
  if (category %in% c("supply", "all")) {

    supply <- flattenDims(readSource("MAgPIEBiomass", subtype = paste0(version, ":supply")))

    # Traditional crop residue burning from calc1stBioDem, mapped to SSP scenarios
    # via gms$c60_1stgen_biodem in magpie/config/scenario_config.csv
    scenMap1stBio <- c(SSP1 = "phaseout2020", SSP2 = "const2020",  SSP3 = "const2030",
                       SSP4 = "const2020",   SSP5 = "phaseout2020", SDP  = "phaseout2020")
    kres    <- findset("kres")
    resDem  <- calcOutput("1stBioDem", aggregate = FALSE)[, , kres]

    residuefuel <- do.call(mbind, lapply(names(scenMap1stBio), function(ssp) {
      x <- dimSums(resDem[, , scenMap1stBio[[ssp]]], dim = "ItemCodeItem")
      getNames(x, dim = "scenario") <- ssp
      x
    }))
    residuefuel <- add_dimension(residuefuel, dim = 3.2, add = "variable", nm = "residuefuel")
    getSets(residuefuel)[3:4] <- c("scenario", "variable")

    out <- mbind(out, supply, residuefuel)
  }

  # --- Potentials ---
  if (category %in% c("potential", "all")) {

    cropres <- readSource("MAgPIEBiomass", subtype = paste0(version, ":cropresPot"))
    cropres <- flattenDims(collapseNames(cropres[, , cropresScen], collapsedim = "scen"))

    woodres <- flattenDims(readSource("MAgPIEBiomass", subtype = paste0(version, ":woodresPot"))) * woodresFrac

    biogas  <- flattenDims(readSource("MAgPIEBiomass", subtype = paste0(version, ":biogasPot"))) * biogasFrac

    if (!isFALSE(zeroPast)) {
      histYears <- getYears(cropres)[getYears(cropres, as.integer = TRUE) <= as.integer(zeroPast)]
      cropres[, histYears, ] <- 0
      woodres[, histYears, ] <- 0
    }

    out <- mbind(out, cropres, woodres, biogas)
  }

  # Reorder to variable.scenario — matching calcResFor2ndBioengery convention for REMIND
  # (feedstock type at 3.1, SSP scenario at 3.2)
  n     <- getNames(out)
  parts <- strsplit(n, "\\.", fixed = TRUE)
  getNames(out) <- vapply(parts, function(p) paste(p[2L], p[1L], sep = "."), character(1L))
  getSets(out)  <- c(getSets(out)[1:2], "variable", "scenario")

  return(list(x           = out,
              weight      = NULL,
              unit        = "PJ/yr",
              description = "Biomass supply and potentials for the energy sector from MAgPIE scenario runs"))
}
