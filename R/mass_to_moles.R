mgL_M <- function(conc, mol_wt) {
  gL <- conc / 1000 # grams/L
  M <- gL / mol_wt
  return(M)
}
