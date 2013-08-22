# ==============================================================================
# convergence plot
# ==============================================================================
convergence.plot <- function( mi.object, ... ) {
  traceplot( as.bugs.array(mi.object@mcmc), ... )
  invisible( as.bugs.array(mi.object@mcmc) )
}


conv.plot <- function( mi.object, ... ) {
  invisible( convergence.plot( mi.object = mi.object, ... ) )
}
