print(x = "Session info :")
print(x = sessionInfo(), RNG = TRUE, locale = TRUE)

## define input/output/ref/res from command line args (should in principle never be changed)
args <- commandArgs(TRUE)

if (!exists("input"))  input  <- args[1]
if (!exists("output")) output <- args[2]
if (!exists("ref"))    ref <- "/ref/"
if (!exists("res"))    res <- "/res/"  

## Load submited results from participant
est <- readRDS(paste0(input, res, "results.rds") )

## Load reference (ground truth)
ref <- readRDS(paste0(input, ref, "/data_reference_sex.rds") )

## define the scores files
score <- est == ref
score[ is.na(x = score) ] <- FALSE
score <- mean(x = as.numeric(score) )

estUnique <- unique(x = est)
refUnique <- unique(x = ref)

if ( sum( ! {estUnique %in% refUnique} ) > 0 ) {
    print(x = paste0("Warning : the estimated variable has the values ", paste(estUnique, collapse = "/"), " whereas the reference has the values ", paste(refUnique, collapse = "/") ) )
}

output_file <- paste0(output,"/scores.txt")
cat(paste0("TPR: ", score), file = output_file, append = FALSE)
