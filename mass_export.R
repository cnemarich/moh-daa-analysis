require(config)
require(futile.logger)
require(glue)
require(dplyr)
require(tibble)
require(jsonlite)
require(httr)
require(tidyr)
require(stringr)
require(DT)
require(datapackr)
source("./utils.R")

secrets <- "/Users/cnemarich001/.secrets/datim.json"
datapackr::loginToDATIM(secrets)

config <- config::get()
options("baseurl" = config$baseurl)
flog.appender(appender.file(config$log_path), name="cop_memo")

ou_list <- getUserOperatingUnits("ybg3MO3hcf4")

ou_df <- data.frame(ou = names(ou_list),
                    id = ou_list,
                    row.names = NULL,
                    stringsAsFactors = FALSE)

get_ou_name <- ou_df$ou

names(get_ou_name) <- ou_df$id

for (ou_uid in ou_list) {
  
  ou_name <- unname(get_ou_name[ou_uid])
  
  print(ou_name)
  
  period <- "2018Oct"
  
  #Countries with facility level at organization unit level 7
  level_sevens <- c("Qh4XMQJhbk8", "ds0ADyc9UCU", "IH1kchw86uA", "JTypsdEUNPw",
                    "HfVjCurKxh2", "lZsCb6y0KDX", "XtxUYCsDWrR", "cDGPF739ZZr",
                    "WLG0z5NxQs8", "mdXu6iCbn2G", "FETQ6OmnsKB")
  
  ou_lvl <- ifelse(ou_uid %in% level_sevens, 7, 6)
  
  base_url <- config$baseurl
  
  lvl_url <- ifelse(ou_lvl == 7, "LEVEL-10;", "")
  
  print("fetching mer data...")
  
  url_mer <- glue::glue("{base_url}api/29/analytics.json?
                dimension=SH885jaRe0o:mXjFJEexCHJ;t6dWOH7W5Ml&
                dimension=ou:LEVEL-5;LEVEL-6;LEVEL-7;
                LEVEL-8;LEVEL-9;{lvl_url}{ou_uid}&
                dimension=dx:xNTzinnVgba;yEQ5FoXJWAx;aeCf1jJWE1x;sdarqD1J8fb;
                GxUQu72i38n;Mon8vQgC9qg;l697bKzFRSv;J1E7eh1CyA0;LZbeWYZEkYL&
                filter=pe:{period}&
                displayProperty=SHORTNAME&
                tableLayout=true&
                columns=SH885jaRe0o&
                rows=ou;dx&
                showHierarchy=true&
                skipData=false&
                includeMetadataDetails=false") %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    URLencode(.)
  
  mer_data <- d2_analyticsresponse(url_mer)
  
  print("got the mer data.")
  
  print("fetching emr data...")
  
  url_emr <- glue::glue("{base_url}api/29/analytics.json?
                dimension=dx:mFvVvrRvZgo&dimension=co&
                dimension=ou:LEVEL-5;LEVEL-6;LEVEL-7;
                LEVEL-8;LEVEL-9;{lvl_url}{ou_uid}&
                filter=pe:{period}&
                displayProperty=SHORTNAME&
                tableLayout=true&
                columns=dx;co&
                rows=ou&
                showHierarchy=true&
                skipData=false&
                includeMetadataDetails=false") %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    URLencode(.)
  
  emr_data <- d2_analyticsresponse(url_emr)
  
  print("got the emr data.")
  
  suffix <- "analysis_workbook"
  date <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file <- paste0(paste(ou_name, suffix, date, sep = "_"), ".xlsx")
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "RawData")
  openxlsx::addWorksheet(wb, "EMRData")
  openxlsx::writeDataTable(wb = wb, sheet = "RawData", x = mer_data)
  openxlsx::writeDataTable(wb = wb, sheet = "EMRData", x = emr_data)
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  
}
