require(dplyr)
require(purrr)
require(rpivotTable)
# require(gt)

moh_pivot <- function(d){
  
  pivot <- d %>%
    purrr::pluck("analytics")
  
  rpivotTable(data = pivot,
              rows = c("indicator"),
              vals = "Weighted difference",
              aggregatorName = "Sum",
              rendererName = "Table",
              width="70%",
              height="700px")
  
}
