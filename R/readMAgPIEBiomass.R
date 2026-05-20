#' @title readMAgPIEBiomass
#' @description Read versioned biomass-for-energy datasets extracted from MAgPIE model
#' runs for use in REMIND. Data are organised by MAgPIE version in subfolders of the
#' MAgPIEBiomass source directory. Each subfolder contains four CSV files produced by
#' the magpie4 extraction functions (`extractWoodFuel`, `extractCropResidues2ndBE`,
#' `extractManureFuel`, `extractProcessingWoodResidues`, `extractBiogasFeedstock`) via
#' the MAgPIE output script `scripts/output/extra/biomassFromMAgPIE.R`, run across
#' scenarios SSP1-SSP5 and SDP (NPi2025 policy baseline).
#'
#' @param subtype Character string of the form `"<version>:<variable>"`, where
#'   `<version>` is the MAgPIE version folder (e.g. `"MAgPIE_4.14.0"`) and
#'   `<variable>` is one of:
#'   \describe{
#'     \item{`supply`}{**Biomass supply**: wood fuel and manure fuel at ISO country
#'       level (PJ/yr). Variables: `woodfuel`, `manurefuel`.
#'       Sources: `p73_forestry_demand_prod_specific[wood_fuel]` and
#'       `ManureExcretion[fuel]`.}
#'     \item{`cropresPot`}{**Crop residue potential**: sustainably harvestable crop
#'       residues for 2nd generation bioenergy (PJ/yr). Four collection scenarios
#'       (scen dimension): `cf0p3_md0`, `cf0p3_md2`, `cf0p3_md4`, `cf0p1_md4`.
#'       Variables: `straw`, `fibrous`, `nonfibrous`.
#'       Source: `ResidueBiomass` + `ResidueUsage[recycling]` share.}
#'     \item{`woodresPot`}{**Wood residue potential**: wood processing residues at ISO
#'       country level (PJ/yr), estimated at 30\% of industrial roundwood demand.
#'       Variable: `woodres`.
#'       Source: `p73_forestry_demand_prod_specific[industrial_roundwood]`.}
#'     \item{`biogasPot`}{**Biogas potential**: biogas feedstock at ISO country level
#'       (PJ/yr). Variables: `manure` (confinement manure to digesters),
#'       `forage` (placeholder, all zeros).
#'       Source: `ov_manure_confinement[digester]`.}
#'   }
#'
#' @return A magpie object at ISO country level.
#'
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#'   x <- readSource("MAgPIEBiomass", subtype = "MAgPIE_4.14.0:supply")
#'   x <- readSource("MAgPIEBiomass", subtype = "MAgPIE_4.14.0:cropresPot")
#'   x <- readSource("MAgPIEBiomass", subtype = "MAgPIE_4.14.0:woodresPot")
#'   x <- readSource("MAgPIEBiomass", subtype = "MAgPIE_4.14.0:biogasPot")
#' }
#'
#' @importFrom madrat toolSplitSubtype
#' @importFrom magclass read.report collapseNames getSets<- getNames<-

readMAgPIEBiomass <- function(subtype) {

  subtypeList <- toolSplitSubtype(subtype, list(
    version  = NULL,
    variable = c("supply", "cropresPot", "woodresPot", "biogasPot")
  ))
  ver      <- subtypeList$version
  variable <- subtypeList$variable

  fileMap <- c(
    supply     = "biomass_supply.csv",
    cropresPot = "biomass_potential_cropres.csv",
    woodresPot = "biomass_potential_woodres.csv",
    biogasPot  = "biomass_potential_biogas.csv"
  )

  # Short variable names replacing long MIF reporting strings
  varNames <- list(
    supply     = c("Biomass supply\\|Wood fuel \\(PJ/yr\\)"                                    = "tradfuel.woodfuel",
                   "Biomass supply\\|Manure Collected As Fuel \\(PJ/yr\\)"                     = "tradfuel.manurefuel"),
    cropresPot = c("Biomass potential\\|Crop residues\\|\\+\\|Straw \\(PJ/yr\\)"                     = "pot2ndBE.cropres.straw",
                   "Biomass potential\\|Crop residues\\|\\+\\|Other fibrous crop residues \\(PJ/yr\\)" = "pot2ndBE.cropres.fibrous",
                   "Biomass potential\\|Crop residues\\|\\+\\|Non fibrous crop residues \\(PJ/yr\\)" = "pot2ndBE.cropres.nonfibrous"),
    woodresPot = c("Biomass potential\\|Wood processing residues \\(PJ/yr\\)"                  = "pot2ndBE.woodres"),
    biogasPot  = c("Biomass potential\\|Biogas feedstock\\|\\+\\|Manure - Anaerobic Digester \\(PJ/yr\\)" = "biogas.manure",
                   "Biomass potential\\|Biogas feedstock\\|\\+\\|Forage \\(PJ/yr\\)"                 = "biogas.forage")
  )

  path <- file.path(ver, fileMap[[variable]])
  if (!file.exists(path)) {
    stop("File not found: ", path, "\n",
         "Expected MAgPIE version folder '", ver, "' inside the MAgPIEBiomass source directory.")
  }

  x <- as.magpie(read.csv(path))

  # Preserve dim1/dim2 set names (e.g. "iso", "t")
  sets12 <- getSets(x)[1:2]

  # Build new dim-3 names: replace long MIF strings with short hierarchical names.
  # For cropresPot: read.report auto-splits the dot in variable names producing
  # scenario.variable.scen; we move scen to 3.2 → scenario.scen.type.category.variable
  getNames(x) <- stringr::str_replace_all(getNames(x), varNames[[variable]])

  if (variable == "cropresPot") {
    # parts = c(ssp, mif_variable, cf0p3_md4) — reorder to ssp.cf0p3_md4.<short_var>
    getSets(x)  <- c(sets12, "scenario", "scen", "type", "category", "variable")
  } else {
    # parts = c(ssp, mif_variable) → ssp.<short_var>#
    getSets(x)  <- c(sets12, "scenario", "type", "variable")
  }

  return(x)
}
