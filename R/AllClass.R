# ==============================================================================
# class definition 
# ==============================================================================
methods::setOldClass("lm")
methods::setOldClass("glm")

setClass("mi.info")
        


setClass("mi",
            representation(
                call      = "call",
                data      = "data.frame" ,
                m         = "numeric",
                mi.info   = "mi.info",
                imp       = "list",
                mcmc      = "ANY",
                converged = "logical",
                coef.mcmc      = "ANY",
                coef.converged = "logical",
                add.noise = "logical",
                total.iters  = "numeric"),
            contains  = "list"
)





#setClass("mi.initialize",
#            representation(
#              data          = "data.frame",
#              mi.data       = "list",
#              VarName       = "vector",
#              start.val     = "list",
#              coef.val      = "list",
#              mi.object     = "list",
#              length.list   = "list",
#              col.mis       = "logical",
#              ncol.mis      = "numeric",
#              iteration.idx = "integer"),            
#              contains  = "list"
#)



setClass("mi.method",
        representation(
          model     = "list",
          expected  = "ANY",
          random    = "ANY"),
          contains  = "list"
)


setClass("mi.continuous",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "ANY"),
            contains = "mi.method"
)

setClass("mi.count",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "ANY"),
            contains = "mi.method"
)




setClass("mi.binary",
        representation(
            model    = "list", 
            expected = "ANY", 
            random   = "ANY"),
            contains = "mi.method"
)

setClass("mi.categorical",
        representation(
            model     = "list", 
            expected  = "ANY", 
            random    = "ANY"),
            contains  = "mi.method"
)

setClass("mi.polr",
        representation(
            model     = "list", 
            expected  = "ANY", 
            random    = "ANY"),
            contains  = "mi.method"
)


setClass("mi.fixed",
        representation(
            model    = "list", 
            expected = "ANY", 
            random   = "ANY"),
            contains = "mi.method"
)


setClass("mi.pmm",
        representation(
            model     = "list", 
            expected  = "ANY", 
            random    = "ANY",
            residuals = "numeric"),
            contains  = "mi.method"
)

setClass("mi.copy",
        representation(
            model    = "list", 
            expected = "ANY", 
            random   = "ANY"),
            contains = "mi.method"
)


setClass("mi.pooled",
            representation(
                call       = "call",
                mi.pooled  = "list" ,
                mi.fit     = "list"),
                contains   = "list"
)


setClass("mi.preprocessed",
            representation(
                data      = "data.frame" ,
                mi.info   = "mi.info"),
            contains  = "list"
)
