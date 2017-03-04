# earnPremium.R
#
require(plyr)
earnPremium <- function(data, asof)
  as.matrix(
    adply(.data = data, 
          .margins = 1, 
          .fun = function(x) diff(approxfun(c(x[["effectivedate"]], x[["expirationdate"]]), c(0, x[["writtenpremium"]]), rule = 2)(asof))
          )[-(1:length(data))]
    )

