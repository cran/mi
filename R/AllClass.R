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
                converged = "logical",
                coef.conv = "ANY",
                bugs      = "ANY",
                preprocess = "logical",
                mi.info.preprocessed = "ANY"),
            contains  = "list"
)

setClass("mi.glm",
            representation(
                call           = "call",
                glm.mi.pooled  = "list" ,
                glm.mi.fit     = "list"),
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



setClass("mi.lm",
            representation(
                call          = "call",
                lm.mi.pooled  = "list" ,
                lm.mi.fit     = "list"),
                contains      = "list"
)


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




setClass("mi.dichotomous",
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
