common.default <-
function(..., overlap = "all", equal.or = "equal"){
	LIS <- list(...)
	return(common.list(LIS, overlap))
}
