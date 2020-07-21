require(config)
require(futile.logger)
require(glue)
require(dplyr)
require(purrr)
require(tibble)
require(jsonlite)
require(httr)
require(tidyr)
require(stringr)
require(DT)
require(datapackr)

config <- config::get()
options("baseurl" = config$baseurl)
options("geourl" = config$geourl)
flog.appender(appender.file(config$log_path), name = "cop_memo")


dhis_login <- function(baseurl, username, password) {
  httr::set_config(httr::config(http_version = 0))
  url <- URLencode(URL = paste0(config$baseurl, "api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url,
                 httr::authenticate(username, password),
                 httr::timeout(60))
  if (r$status != 200L) {
    return(FALSE)
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return(TRUE)
  }
}

geoalign_login <- function(geourl, username, password) {
  httr::set_config(httr::config(http_version = 0))
  url <- URLencode(URL = paste0(config$geourl, "api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url,
                 httr::authenticate(username, password),
                 httr::timeout(60))
  if (r$status != 200L) {
    return(FALSE)
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return(TRUE)
  }
}

get_user_operating_units <- function(uid) {

  country_uid <- c("l1KFEXKI4Dg", "Qh4XMQJhbk8", "bQQJe0cC1eD", "ds0ADyc9UCU",
                   "ANN4YCOufcP", "V0qMZH29CtN", "IH1kchw86uA", "JTypsdEUNPw",
                   "HfVjCurKxh2", "qllxzIjjurr", "lZsCb6y0KDX", "h11OyvlPxpJ",
                   "FFVkaV9Zk1S", "PqlFzhuPcF1", "XtxUYCsDWrR", "cDGPF739ZZr",
                   "WLG0z5NxQs8", "mdXu6iCbn2G", "FETQ6OmnsKB", "ligZVIYs2rL",
                   "YM6xn5QxNpY", "f5RoebaDLMx", "a71G4Gtcttv")

  ous <- datapackr::configFile %>%
    dplyr::select(countryName, countryUID) %>%
    dplyr::filter(!stringr::str_detect(countryName, "_Military")) %>%
    dplyr::distinct() %>%
    dplyr::arrange(countryName) %>%
    dplyr::filter(countryUID %in% country_uid)

  if (is.null(uid)) {
    return("")
  }

  # If user does not have global access, only return their assigned country.
  if (uid != "ybg3MO3hcf4") {
    ous %<>%
      dplyr::filter(countryUID == uid)
  }
  setNames(ous$countryUID, ous$countryName)
}

get_ou_name <- function(ou_uid) {

  ous <- get_user_operating_units(ou_uid) %>%
    data.frame()

  idx <- which(ous$. == ou_uid)[1]

  name <- rownames(ous)[idx]

  return(name)

}

get_ou_facility_level <- function(ou_uid) {

  # List of all countries by UID where the facilities are at level 7
  level_sevens <- c("Qh4XMQJhbk8", "ds0ADyc9UCU", "IH1kchw86uA", "JTypsdEUNPw",
                    "HfVjCurKxh2", "lZsCb6y0KDX", "XtxUYCsDWrR", "cDGPF739ZZr",
                    "WLG0z5NxQs8", "mdXu6iCbn2G", "FETQ6OmnsKB")

  # Returns the level at which facilities are listed for the country
  if (ou_uid %in% level_sevens) {
    return(7)
  } else {
    return(6)
  }

}

d2_analyticsresponse <- function(url, remap_cols = TRUE) {
  d <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
  if (NROW(d$rows) > 0) {
    metadata <- do.call(rbind,
                        lapply(d$metaData$items,
                               data.frame, stringsAsFactors = FALSE)) %>%
      mutate(., from = row.names(.))
    remap_meta <-
      function(x) {
        plyr::mapvalues(x, metadata$from, metadata$name, warn_missing = FALSE)
      }

    d <- tibble::as_tibble(d$rows,
                           .name_repair = "minimal") %>%
      `names<-`(., d$headers$column)
    if (remap_cols == TRUE) {
      d <- plyr::colwise(remap_meta)(d)
    }
    return(d)
    } else {
      return(NULL)
    }
}

get_org_unit_lvls <- function() {

  # Assembles URL for API call
  base_url <- config$baseurl

  url_hie <- glue::glue("{base_url}api/dataStore/
                        dataSetAssignments/orgUnitLevels") %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    URLencode(.)

  # Fetches data from the server
  df <- jsonlite::fromJSON(httr::content(httr::GET(url_hie), as = "text"))

  if (is.null(df)) {
    return(NULL)
  }

}

get_indicators_table <- function(ou_uid = "cDGPF739ZZr") {

  # Assembles URL for API call based on the facility level of the OU
  ou_lvl <- get_ou_facility_level(ou_uid)

  base_url <- config$baseurl

  lvl_url <- ifelse(ou_lvl == 7, "LEVEL-10;", "")

  url <- glue::glue("{base_url}api/29/analytics.json?
                dimension=SH885jaRe0o:mXjFJEexCHJ;t6dWOH7W5Ml&
                dimension=ou:LEVEL-5;LEVEL-6;LEVEL-7;
                LEVEL-8;LEVEL-9;{lvl_url}{ou_uid}&
                dimension=dx:xNTzinnVgba;yEQ5FoXJWAx;aeCf1jJWE1x;sdarqD1J8fb;
                GxUQu72i38n;Mon8vQgC9qg;l697bKzFRSv;J1E7eh1CyA0;LZbeWYZEkYL&
                dimension=pe:2017Oct;2018Oct&
                displayProperty=SHORTNAME&
                tableLayout=true&
                columns=SH885jaRe0o&
                rows=ou;dx;pe&
                showHierarchy=true&
                skipData=false&
                includeMetadataDetails=false") %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    URLencode(.)

  # Fetches data from the server
  df <- d2_analyticsresponse(url)

  # Returns null if API returns nothing
  if (is.null(df)) {
    return(NULL)
    }

  # Renames organization unit level columns and adds Site Hierachy column
  if (ou_lvl == 7) {

     df %<>%
      dplyr::select("namelevel3" = orgunitlevel3,
                    "namelevel4" = orgunitlevel4,
                    "namelevel5" = orgunitlevel5,
                    "namelevel6" = orgunitlevel6,
                    "namelevel7" = orgunitlevel7,
                    "indicator" = dataname,
                    "period" = periodname,
                    "MOH" = "00100 - PEPFAR-MOH align: MOH Data",
                    "PEPFAR" = "00200 - PEPFAR-MOH align: PEPFAR Data") %>%
      dplyr::filter(!is.na(namelevel7) & (namelevel7 != "")) %>%
      dplyr::mutate("Site hierarchy" = paste(namelevel3, namelevel4,
                                             namelevel5, namelevel6,
                                             namelevel7, sep = " / "))

  } else{

    df %<>%
      dplyr::select("namelevel3" = orgunitlevel3,
                    "namelevel4" = orgunitlevel4,
                    "namelevel5" = orgunitlevel5,
                    "namelevel6" = orgunitlevel6,
                    "indicator" = dataname,
                    "period" = periodname,
                    "MOH" = "00100 - PEPFAR-MOH align: MOH Data",
                    "PEPFAR" = "00200 - PEPFAR-MOH align: PEPFAR Data") %>%
      dplyr::filter(!is.na(namelevel6) & (namelevel6 != "")) %>%
      dplyr::mutate("Site hierarchy" = paste(namelevel3, namelevel4, namelevel5,
                                            namelevel6, sep = " / "))

  }

  # Cleans up indicator, MOH, and PEPFAR columns
  df %<>%
    tidyr::separate(indicator, c("indicator"), sep = " ", extra = "drop") %>%
    dplyr::mutate(period = case_when(
      period == "Oct 2018 to Sep 2019" ~ 2019,
      period == "Oct 2017 to Sep 2018" ~ 2018,
      TRUE ~ NA_real_
    )) %>%
    dplyr::mutate("MOH" = as.numeric(MOH)) %>%
    dplyr::mutate("PEPFAR" = as.numeric(PEPFAR))

  # Summarizes MOH and PEPFAR data up from coarse and fine disaggregates
  if (ou_lvl == 7) {

    df %<>%
      dplyr::group_by(namelevel3, namelevel4, namelevel5, namelevel6,
                      namelevel7, indicator, period, `Site hierarchy`) %>%
      dplyr::summarise(MOH = sum(MOH, na.rm = any(!is.na(MOH))),
                       PEPFAR = max(PEPFAR, na.rm = any(!is.na(PEPFAR)))) %>%
      dplyr::ungroup()

  } else {

    df %<>%
      dplyr::group_by(namelevel3, namelevel4, namelevel5, namelevel6,
                    indicator, period, `Site hierarchy`) %>%
      dplyr::summarise(MOH = sum(MOH, na.rm = any(!is.na(MOH))),
                       PEPFAR = max(PEPFAR, na.rm = any(!is.na(PEPFAR)))) %>%
      dplyr::ungroup()

    }

  # Creates basic summary columns about reporting institutions and figures
  df %<>%
    dplyr::mutate("Reported on by both" =
                    ifelse(is.na(MOH) | is.na(PEPFAR),
                           ifelse(is.na(MOH) & is.na(PEPFAR), "neither", "one"),
                           "both")) %>%
    dplyr::mutate("Reported by" =
                    ifelse(!is.na(MOH),
                           ifelse(!is.na(PEPFAR), "Both", "MOH"),
                           ifelse(!is.na(PEPFAR), "Neither", "PEPFAR"))) %>%
    dplyr::mutate("Difference" =
                    ifelse(`Reported by` == "Both", MOH - PEPFAR, NA)) %>%
    dplyr::mutate("Reported higher" = case_when(
      is.na(MOH) ~ "Only PEPFAR reported",
      is.na(PEPFAR) ~ "Only MOH reported",
      Difference > 0 ~ "MOH reported higher",
      Difference < 0 ~ "PEPFAR reported higher",
      Difference == 0 ~ "Same result reported",
      TRUE ~ "Neither reported"
    ))

  # Groups rows by indicator and calculates indicator-specific summaries
  df %<>%
    dplyr::group_by(indicator, period) %>%
    dplyr::mutate("Count of sites reporting both" =
                    sum(ifelse(`Reported on by both` == "both", 1, 0))) %>%
    dplyr::mutate("PEPFAR sum of sites reporting both" =
                    sum(ifelse(`Reported on by both` == "both", PEPFAR, 0))) %>%
    dplyr::ungroup()

  # Calculates weighting variables
  df %<>%
    dplyr::mutate("Weighting" =
                    ifelse(`Reported by` == "Both",
                           PEPFAR / `PEPFAR sum of sites reporting both`,
                           NA)) %>%
    dplyr::mutate("Average" = rowMeans(cbind(MOH, PEPFAR), na.rm = F)) %>%
    dplyr::mutate("Weighted difference" =
                    ifelse(`Reported by` == "Both",
                           Weighting * abs(Difference) / Average, NA))

  # Reorganizes table for export
  df %<>%
    dplyr::select(starts_with("namelevel"), indicator, period, MOH, PEPFAR,
                  `Reported on by both`, `Reported by`, `Reported higher`,
                  Difference, Weighting, `Weighted difference`,
                  `Count of sites reporting both`,
                  `PEPFAR sum of sites reporting both`, `Site hierarchy`)

}

get_emr_table <- function(ou_uid = "cDGPF739ZZr") {

  # Assembles URL for API call based on the facility level of the OU
  ou_lvl <- get_ou_facility_level(ou_uid)

  base_url <- config$baseurl

  lvl_url <- ifelse(ou_lvl == 7, "LEVEL-10;", "")

  url_emr <- glue::glue("{base_url}api/29/analytics.json?
                dimension=dx:mFvVvrRvZgo&dimension=co&
                dimension=ou:LEVEL-5;LEVEL-6;LEVEL-7;
                LEVEL-8;LEVEL-9;{lvl_url}{ou_uid}&
                dimension=pe:2017Oct;2018Oct&
                displayProperty=SHORTNAME&
                tableLayout=true&
                columns=dx;co&
                rows=ou;pe&
                showHierarchy=true&
                skipData=false&
                includeMetadataDetails=false") %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    URLencode(.)

  # Fetches data from the server
  df <- d2_analyticsresponse(url_emr)

  if (is.null(df)) {
    return(NULL)
    }

  # Renames organization unit level columns and adds Site Hierachy column
  if (ou_lvl == 7) {

    df %<>%
      dplyr::select("namelevel3" = orgunitlevel3,
                    "namelevel4" = orgunitlevel4,
                    "namelevel5" = orgunitlevel5,
                    "namelevel6" = orgunitlevel6,
                    "namelevel7" = orgunitlevel7,
                    "period" = periodname,
                    starts_with("EMR_SITE")) %>%
      dplyr::mutate("Site hierarchy" = paste(namelevel3, namelevel4,
                                             namelevel5, namelevel6,
                                             namelevel7, sep = " / "))

  } else {

    df %<>%
      dplyr::select("namelevel3" = orgunitlevel3,
                    "namelevel4" = orgunitlevel4,
                    "namelevel5" = orgunitlevel5,
                    "namelevel6" = orgunitlevel6,
                    "period" = periodname,
                    starts_with("EMR_SITE")) %>%
      dplyr::mutate("Site hierarchy" = paste(namelevel3, namelevel4, namelevel5,
                                            namelevel6, sep = " / "))

  }

  # Cleans up period names and renames EMR columns with shorter names
  df %<>%
    dplyr::mutate(period = case_when(
      period == "Oct 2018 to Sep 2019" ~ 2019,
      period == "Oct 2017 to Sep 2018" ~ 2018,
      TRUE ~ NA_real_
    )) %>%
    dplyr::mutate("EMR - HIV Testing Services" =
                    as.numeric(`EMR_SITE (N, NoApp, Serv Del Area) Service Delivery Area - HIV Testing Services`),
                  "EMR - Care and Treatment" =
                    as.numeric(`EMR_SITE (N, NoApp, Serv Del Area) Service Delivery Area - Care and Treatment`),
                  "EMR - ANC and/or Maternity" =
                    as.numeric(`EMR_SITE (N, NoApp, Serv Del Area) Service Delivery Area - ANC and/or Maternity`),
                  "EMR - EID" =
                    as.numeric(`EMR_SITE (N, NoApp, Serv Del Area) Service Delivery Area - Early Infant Diagnosis (not Ped ART)`),
                  "EMR - HIV/TB" =
                    as.numeric(`EMR_SITE (N, NoApp, Serv Del Area) Service Delivery Area - HIV/TB`)) %>%
    dplyr::select(-starts_with("EMR_SITE"))

  # Transforms EMR indicators into boolean values
  if (ou_lvl == 7) {

    df %<>%
      dplyr::group_by(namelevel3, namelevel4, namelevel5, namelevel6,
                      namelevel7, `Site hierarchy`, period) %>%
      dplyr::summarise("Has EMR - HIV Testing Services" =
                         ifelse(!is.na(sum(`EMR - HIV Testing Services`)),
                                TRUE, FALSE),
                       "Has EMR - Care and Treatment" =
                         ifelse(!is.na(sum(`EMR - Care and Treatment`)),
                                TRUE, FALSE),
                       "Has EMR - ANC and/or Maternity" =
                         ifelse(!is.na(sum(`EMR - ANC and/or Maternity`)),
                                TRUE, FALSE),
                       "Has EMR - EID" =
                         ifelse(!is.na(sum(`EMR - EID`)),
                                TRUE, FALSE),
                       "Has EMR - HIV/TB" =
                         ifelse(!is.na(sum(`EMR - HIV/TB`)),
                                TRUE, FALSE)) %>%
    dplyr::ungroup()

} else {

  df %<>%
    dplyr::group_by(namelevel3, namelevel4, namelevel5, namelevel6,
                    `Site hierarchy`, period) %>%
    dplyr::summarise("Has EMR - HIV Testing Services" =
                       ifelse(!is.na(sum(`EMR - HIV Testing Services`)),
                              TRUE, FALSE),
                     "Has EMR - Care and Treatment" =
                       ifelse(!is.na(sum(`EMR - Care and Treatment`)),
                              TRUE, FALSE),
                     "Has EMR - ANC and/or Maternity" =
                       ifelse(!is.na(sum(`EMR - ANC and/or Maternity`)),
                              TRUE, FALSE),
                     "Has EMR - EID" =
                       ifelse(!is.na(sum(`EMR - EID`)),
                              TRUE, FALSE),
                     "Has EMR - HIV/TB" =
                       ifelse(!is.na(sum(`EMR - HIV/TB`)),
                              TRUE, FALSE)) %>%
    dplyr::ungroup()

}

  # Reorganizes table for export
  df %<>%
    dplyr::select(starts_with("namelevel"), period,
                  starts_with("Has EMR"), `Site hierarchy`)

}

get_geoalign_table <- function() {

  # Assembles URL for API call
  geo_url <- config$geourl

  url_geo <- glue::glue("{geo_url}api/dataStore/
                        MOH_country_indicators/2019") %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    URLencode(.)

  # Fetches data from the server
  df <- jsonlite::fromJSON(httr::content(httr::GET(url_geo), as = "text"))

  if (is.null(df)) {
    return(NULL)
  }

  df %<>%
    pivot_longer(-c(CountryName, CountryCode, generated),
                 names_sep = "_(?=[^_]*$)",
                 names_to = c("indicator", ".value")) %>%
    select(CountryName, CountryCode, indicator,
           hasDisagMapping, hasResultsData, generated)

}

combine_data <- function(indicators, emr) {

  emr %<>%
    select(-starts_with("namelevel"))

  df <- indicators %>%
    dplyr::left_join(emr, by = c("Site hierarchy", "period"))

  }

wb_filename <- function(ou, my_indicator) {

  suffix <- "analysis_workbook"
  date <- format(Sys.time(), "%Y%m%d_%H%M%S")
  name <- paste0(paste(ou, my_indicator, suffix, date, sep = "_"), ".xlsx")
  return(name)

}

wb_filecontent <- function(d, my_indicator, file) {

  analytics <- d %>%
    purrr::pluck(., "analytics") %>%
    dplyr::filter(indicator == my_indicator) %>%
    dplyr::select(-indicator)

  wb <- openxlsx::loadWorkbook(file = file.path("templates", "template.xlsx"))

  openxlsx::removeTable(wb = wb, sheet = "RawData", table = "Table2")

  openxlsx::writeDataTable(
    wb = wb,
    sheet = "RawData",
    x = analytics,
    xy = c(1, 1),
    colNames = T,
    rowNames = F,
    tableStyle = "TableStyleMedium16",
    tableName = "Table2",
    withFilter = TRUE,
    bandedRows = TRUE
    )

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

  return(wb)

}

raw_filename <- function(ou) {

  suffix <- "raw_data"
  date <- format(Sys.time(), "%Y%m%d_%H%M%S")
  name <- paste0(paste(ou, suffix, date, sep = "_"), ".xlsx")
  return(name)

}

raw_filecontent <- function(d, file) {

  analytics <- d %>%
    purrr::pluck("analytics")

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "RawData")

  openxlsx::writeDataTable(wb = wb,
                           sheet = "RawData",
                           x = analytics,
                           xy = c(1, 1),
                           colNames = T,
                           rowNames = F,
                           tableStyle = "TableStyleMedium16",
                           tableName = "RawData",
                           withFilter = TRUE,
                           bandedRows = TRUE
                           )

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  return(wb)

}
