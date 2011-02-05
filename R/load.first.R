.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "mi"))
  ver <- packageDescription("mi", lib = mylib)$Version
  builddate <- packageDescription("mi", lib.loc = mylib)$Date
  cat(paste("\nmi (Version ", ver, ", built: ", builddate, ")\n", sep = ""))
  if(!any(search()=="package:abind"))
    require(abind)
  if(!any(search()=="package:car"))
    require(car)
  if(!any(search()=="package:arm"))
    require(arm)
}
