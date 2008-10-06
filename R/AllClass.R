# ==============================================================================
# class definition 
# ==============================================================================
methods::setOldClass("lm")
methods::setOldClass("glm")

setClass("mi.info",
            representation()#,
            #contains = "list"
)
setClass("mi",
            representation(
                call      = "call",
                data      = "data.frame" ,
                m         = "numeric",
                mi.info   = "mi.info",
                imp       = "list",
                converged = "logical",
                coef.conv = "ANY",
                bugs      = "ANY")#,
            #contains  = "list"
)

setClass("mi.method",
            representation ="VIRTUAL",
            contains = "list"
)
setClass("mi.dichotomous",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)
setClass("mi.categorical",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric",
            residual = "numeric"),
            contains = "mi.method"
)
setClass("mi.polr",
        representation(
            model = "list", 
            expected = "numeric", 
            random = "numeric"),
            contains = "mi.method"
)
setClass("mi.continuous",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)
setClass("mi.fixed",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)
setClass("mi.mixed",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)

setClass("mi.sqrtcontinuous",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)
setClass("mi.logcontinuous",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)
setClass("mi.pmm",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric",
            residual = "numeric"),
            contains = "mi.method"
)
setClass("mi.copy",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)
