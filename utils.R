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


config <- config::get()
options("baseurl" = config$baseurl)
flog.appender(appender.file(config$log_path), name="cop_memo")

getUserOperatingUnits<-function(uid) {
  
  country_uid <- c('l1KFEXKI4Dg','Qh4XMQJhbk8','bQQJe0cC1eD','ds0ADyc9UCU',
                   'ANN4YCOufcP','V0qMZH29CtN','IH1kchw86uA','JTypsdEUNPw',
                   'HfVjCurKxh2','qllxzIjjurr','lZsCb6y0KDX','h11OyvlPxpJ','FFVkaV9Zk1S',
                   'PqlFzhuPcF1','XtxUYCsDWrR','cDGPF739ZZr','WLG0z5NxQs8','mdXu6iCbn2G',
                   'FETQ6OmnsKB','ligZVIYs2rL','YM6xn5QxNpY','f5RoebaDLMx','a71G4Gtcttv')
  
  ous<-datapackr::configFile %>% 
    dplyr::select(countryName,countryUID) %>% 
    dplyr::filter(!stringr::str_detect(countryName,"_Military")) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(countryName) %>%
    dplyr::filter(countryUID %in% country_uid)
  
  if ( is.null(uid) ) {return("")} 
  
  if ( uid != "ybg3MO3hcf4") {
    ous %<>% 
      dplyr::filter(model_uid == uid) 
  }
  setNames(ous$countryUID,ous$countryName)
}

DHISLogin <- function(baseurl, username, password) {
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

indicatorSelector <- function(df){
  
  if (!inherits(df,"error") & !is.null(df)){
    
    df  %>% 
      purrr::pluck(.,"indicators") %>%
      purrr::pluck(.,"indicator") %>% 
      unique()
    
  } else {
    NULL
  }
  
}

d2_analyticsResponse <- function(url,remapCols=TRUE) {
  d <- jsonlite::fromJSON(content(GET(url), "text"))
  if ( NROW(d$rows) > 0 ) {
    metadata <- do.call(rbind,
                        lapply(d$metaData$items,
                               data.frame, stringsAsFactors = FALSE)) %>% mutate(., from = row.names(.))
    remapMeta <-
      function(x) {
        plyr::mapvalues(x, metadata$from, metadata$name, warn_missing = FALSE)
      }
    
    d<-tibble::as_tibble(d$rows) %>% `names<-`(., d$headers$column)
    if(remapCols == TRUE) {
      d<-plyr::colwise(remapMeta)(d)
    }
    return(d) } else {
      return(NULL)
    }
}

analysis_getIndicatorsTable<-function(ou_uid="cDGPF739ZZr",period="2018Oct") {
  
  #Countries with facility level at organization unit level 7 
  level_sevens <- c('Qh4XMQJhbk8','ds0ADyc9UCU','IH1kchw86uA','JTypsdEUNPw',
                    'HfVjCurKxh2','lZsCb6y0KDX','XtxUYCsDWrR','cDGPF739ZZr',
                    'WLG0z5NxQs8','mdXu6iCbn2G','FETQ6OmnsKB')
  
  ou_lvl <- ifelse(ou_uid %in% level_sevens,7,6)
    
  base_url<-config$baseurl
  
  lvl_url<- ifelse(ou_lvl==7,"LEVEL-10;","")
  
  url<-glue::glue("{base_url}api/29/analytics.json?dimension=SH885jaRe0o:mXjFJEexCHJ;t6dWOH7W5Ml&
                dimension=ou:LEVEL-5;LEVEL-6;LEVEL-7;LEVEL-8;LEVEL-9;{lvl_url}{ou_uid}&
                dimension=dx:xNTzinnVgba;yEQ5FoXJWAx;aeCf1jJWE1x;sdarqD1J8fb;GxUQu72i38n;
                Mon8vQgC9qg;l697bKzFRSv;J1E7eh1CyA0;LZbeWYZEkYL&
                filter=pe:{period}&
                displayProperty=SHORTNAME&
                tableLayout=true&
                columns=SH885jaRe0o&
                rows=ou;dx&
                showHierarchy=true&
                skipData=false&
                includeMetadataDetails=false") %>% 
    stringr::str_replace_all( "[\r\n]" , "") %>% 
    URLencode(.)
  
  df <- d2_analyticsResponse(url)
  
  if (is.null(df)) { return(NULL)}
  
  if (ou_lvl==7) {
   
     df %<>%
      dplyr::select("namelevel3"=orgunitlevel3,
                    "namelevel4"=orgunitlevel4,
                    "namelevel5"=orgunitlevel5,
                    "namelevel6"=orgunitlevel6,
                    "namelevel7"=orgunitlevel7,
                    "indicator"=dataname,
                    "MOH"="00100 - PEPFAR-MOH align: MOH Data",
                    "PEPFAR"="00200 - PEPFAR-MOH align: PEPFAR Data") %>%
#      dplyr::filter(!is.na(namelevel7) & (namelevel7 != "")) %>%
      dplyr::mutate("Site_hierarchy"= paste(namelevel3,namelevel4,namelevel5,namelevel6,namelevel7,sep=" / "))
    
  } else{
    
    df %<>%
      dplyr::select("namelevel3"=orgunitlevel3,
                    "namelevel4"=orgunitlevel4,
                    "namelevel5"=orgunitlevel5,
                    "namelevel6"=orgunitlevel6,
                    "indicator"=dataname,
                    "MOH"="00100 - PEPFAR-MOH align: MOH Data",
                    "PEPFAR"="00200 - PEPFAR-MOH align: PEPFAR Data") %>%
#      dplyr::filter(!is.na(namelevel6) & (namelevel6 != "")) %>% 
      dplyr::mutate("Site_hierarchy"= paste(namelevel3,namelevel4,namelevel5,namelevel6,sep=" / ")) %>%
      dplyr::mutate("namelevel7" = "") 
    
  }
  
  df %<>%
    tidyr::separate(indicator,c("indicator"),sep=" ",extra='drop') %>%
    dplyr::mutate("MOH"=as.numeric(MOH)) %>%
    dplyr::mutate("PEPFAR"=as.numeric(PEPFAR)) %>%
    dplyr::group_by(namelevel3,namelevel4,namelevel5,namelevel6,namelevel7,indicator,Site_hierarchy) %>%
    dplyr::summarise(MOH = sum(MOH),PEPFAR = sum(PEPFAR)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("Reported_on_by_both" = ifelse(is.na(MOH) | is.na(PEPFAR),"one","both")) %>%
    dplyr::mutate("Reported_by" = ifelse(!is.na(MOH),ifelse(!is.na(PEPFAR),"Both","MOH"),"PEPFAR")) %>%
    dplyr::group_by(indicator) %>%
    dplyr::mutate("Count_of_sites_reporting_both" = sum(ifelse(Reported_on_by_both=="both",1,0))) %>%
    dplyr::mutate("PEPFAR_sum_of_sites_reporting_both" = sum(ifelse(Reported_on_by_both=="both",PEPFAR,0))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("Difference" = ifelse(Reported_by == "Both",MOH - PEPFAR,NA)) %>%
    dplyr::mutate("Reported_higher" = case_when(
      is.na(MOH) ~ "Only PEPFAR reported",
      is.na(PEPFAR) ~ "Only MOH reported",
      Difference > 0 ~ "MOH",
      Difference < 0 ~ "PEPFAR",
      Difference == 0 ~ "Same result reported",
      TRUE ~ "Neither reported"
    )) %>%
    dplyr::mutate("Weighting" = ifelse(Reported_by=="Both",PEPFAR/PEPFAR_sum_of_sites_reporting_both,NA)) %>%
    dplyr::mutate("Average" = rowMeans(cbind(MOH, PEPFAR),na.rm=F)) %>%
    dplyr::mutate("Weighted_diff" = ifelse(Reported_by=="Both",Weighting*abs(Difference)/Average,NA)) %>%
    dplyr::select(starts_with("namelevel"),indicator,
                  MOH,PEPFAR,Reported_on_by_both,Reported_by,Reported_higher,
                  Difference,Weighting,Weighted_diff,Count_of_sites_reporting_both,
                  PEPFAR_sum_of_sites_reporting_both,Site_hierarchy)
  
}

analysis_getEMRTable<-function(ou_uid="cDGPF739ZZr",period="2018Oct") {
  
  #Countries with facility level at organization unit level 7 
  level_sevens <- c('Qh4XMQJhbk8','ds0ADyc9UCU','IH1kchw86uA','JTypsdEUNPw',
                    'HfVjCurKxh2','lZsCb6y0KDX','XtxUYCsDWrR','cDGPF739ZZr',
                    'WLG0z5NxQs8','mdXu6iCbn2G','FETQ6OmnsKB')
  
  ou_lvl <- ifelse(ou_uid %in% level_sevens,7,6)
  
  base_url<-config$baseurl
  
  lvl_url<- ifelse(ou_lvl==7,"LEVEL-10;","")
  
  url_emr <- glue::glue("{base_url}api/29/analytics.json?dimension=dx:mFvVvrRvZgo&dimension=co&
                dimension=ou:LEVEL-5;LEVEL-6;LEVEL-7;LEVEL-8;LEVEL-9;{lvl_url}{ou_uid}&
                filter=pe:{period}&
                displayProperty=SHORTNAME&
                tableLayout=true&
                columns=dx;co&
                rows=ou&
                showHierarchy=true&
                skipData=false&
                includeMetadataDetails=false") %>% 
    stringr::str_replace_all( "[\r\n]" , "") %>% 
    URLencode(.)
  
  df <- d2_analyticsResponse(url_emr)
  
  if (is.null(df)) { return(NULL)}
  
  if (ou_lvl==7) {
    
    df %<>%
      dplyr::select("namelevel3"=orgunitlevel3,
                    "namelevel4"=orgunitlevel4,
                    "namelevel5"=orgunitlevel5,
                    "namelevel6"=orgunitlevel6,
                    "namelevel7"=orgunitlevel7,
                    starts_with("EMR_SITE")) %>%
#      dplyr::filter(!is.na(namelevel7) & (namelevel7 != "")) %>%
      dplyr::mutate("Site_hierarchy"= paste(namelevel3,namelevel4,namelevel5,namelevel6,namelevel7,sep=" / "))
    
  } else{
    
    df %<>%
      dplyr::select("namelevel3"=orgunitlevel3,
                    "namelevel4"=orgunitlevel4,
                    "namelevel5"=orgunitlevel5,
                    "namelevel6"=orgunitlevel6,
                    starts_with("EMR_SITE")) %>%
#       dplyr::filter(!is.na(namelevel6) & (namelevel6 != "")) %>%
      dplyr::mutate("Site_hierarchy"= paste(namelevel3,namelevel4,namelevel5,namelevel6,sep=" / ")) %>%
      dplyr::mutate("namelevel7" = "")
    
  }
  
  df %<>%
    dplyr::group_by(namelevel3,namelevel4,namelevel5,namelevel6,namelevel7,Site_hierarchy) %>%
    dplyr::mutate("EMR - HIV Testing Services" = ifelse(sum(`EMR_SITE (N, NoApp, Serv Del Area) Service Delivery Area - HIV Testing Services`) > 0, "yes","no")) %>%
    dplyr::ungroup() %>%
    dplyr::select(starts_with("namelevel"),starts_with("EMR_SITE"),Site_hierarchy)
  
}

analysis_combineData <- function(indicators,emr){
  
  emr %<>%
    select(-starts_with("namelevel"))
  
  df <- indicators %>%
    dplyr::left_join(emr,by="Site_hierarchy")

  }

wb_filename <- function(ou, my_indicator){
  
  suffix <- "analysis_workbook"
  date<-format(Sys.time(),"%Y%m%d_%H%M%S")
  name <- paste0(paste(ou,my_indicator,suffix,date,sep="_"),".xlsx")
  return (name)
  
}

wb_filecontent <- function(d, my_indicator,file){
  
  d %<>%
    purrr::pluck("indicators") %>%
    dplyr::filter(indicator == my_indicator) %>%
    dplyr::select(-indicator)
  
  
  wb <- openxlsx::loadWorkbook(file=file.path("templates","template.xlsx"))
  openxlsx::writeData(wb = wb,
                      sheet = "RawData",
                      x = d,
                      xy = c(1,2),
                      colNames = F, rowNames = F, withFilter = FALSE)
  openxlsx::saveWorkbook(wb,file=file,overwrite = TRUE)
  return (wb)
  
}
