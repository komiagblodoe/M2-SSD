program <-
function(input_learn, input_test) {
    ##
    ## YOUR CODE BEGINS HERE
    ##
    
    for ( v in c("sex", "tissue_status", "histology", "dead", "dead_at_24_months", "t", "n", "m", "tnm_stage") ) {
        input_learn[[ v ]] <- as.factor(x = input_learn[[ v ]])
    }
    ## summary(object = input_learn)

    ## We remove constant variables :
    idx <- apply(
        X      = input_learn,
        MARGIN = 2,
        FUN    = function(x) {
            length(x = table(x = x) ) > 1
        } )
    input_learn   <- input_learn[ , idx, drop = FALSE]

    ##
    ## YOUR CODE BEGINS HERE
    ##

    input_learn$os_months <- NULL
    input_learn$dead      <- NULL
    
    ## summary(object = model)

    virulence_levels <- levels(x = input_learn$dead_at_24_months)

    pred <- predict.glm(object = model, newdata = input_test, type = "response")

    idx  <- pred <= 0.5
    pred[  idx ] <- virulence_levels[ 1 ]
    pred[ !idx ] <- virulence_levels[ 2 ]
    table(pred, useNA = "ifany")
    
    return( pred )
    
    ##
    ## YOUR CODE ENDS HERE
    ##
}
