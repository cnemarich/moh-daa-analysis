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
  
  countryUID <- c('l1KFEXKI4Dg','Qh4XMQJhbk8','bQQJe0cC1eD','ds0ADyc9UCU',
                   'ANN4YCOufcP','V0qMZH29CtN','IH1kchw86uA','JTypsdEUNPw',
                   'HfVjCurKxh2','qllxzIjjurr','lZsCb6y0KDX','h11OyvlPxpJ','FFVkaV9Zk1S',
                   'PqlFzhuPcF1','XtxUYCsDWrR','cDGPF739ZZr','WLG0z5NxQs8','mdXu6iCbn2G',
                   'FETQ6OmnsKB','ligZVIYs2rL','YM6xn5QxNpY','f5RoebaDLMx','a71G4Gtcttv')
  
  facilityLevel <- c(6,7,6,7,6,6,7,7,7,6,7,6,6,6,7,7,7,7,7,6,6,6,6)
  
  df <- data.frame(countryUID,facilityLevel)
  
  ous<-datapackr::configFile %>% 
    dplyr::select(countryName,countryUID) %>% 
    dplyr::filter(!stringr::str_detect(countryName,"_Military")) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(countryName) %>%
    dplyr::inner_join(df)
  
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

analysis_getSitesTable<-function(ou_uid="cDGPF739ZZr") {

  return(NULL)

}

analysis_getIndicatorsTable<-function(ou_uid="cDGPF739ZZr") {
  
  base_url<-config$baseurl
  
  url<-glue::glue("{base_url}api/29/analytics.json?dimension=SH885jaRe0o:mXjFJEexCHJ;t6dWOH7W5Ml&
                dimension=ou:LEVEL-5;LEVEL-6;LEVEL-7;LEVEL-8;LEVEL-9;LEVEL-10;{ou_uid}&
                dimension=dx:xNTzinnVgba;yEQ5FoXJWAx;aeCf1jJWE1x;sdarqD1J8fb;GxUQu72i38n;
                Mon8vQgC9qg;l697bKzFRSv;J1E7eh1CyA0;LZbeWYZEkYL&
                filter=pe:2018Oct&
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
  
  df %<>%
    dplyr::select("namelevel3"=orgunitlevel3,
                  "namelevel4"=orgunitlevel4,
                  "namelevel5"=orgunitlevel5,
                  "namelevel6"=orgunitlevel6,
                  "namelevel7"=orgunitlevel7,
                  "indicator"=dataname,
                  "MOH"="00100 - PEPFAR-MOH align: MOH Data",
                  "PEPFAR"="00200 - PEPFAR-MOH align: PEPFAR Data") %>%
    tidyr::separate(indicator,c("indicator"),sep=" ",extra='drop') %>%
    dplyr::mutate("MOH"=as.numeric(MOH)) %>%
    dplyr::mutate("PEPFAR"=as.numeric(PEPFAR)) %>%
    dplyr::mutate("Reported_on_by_both" = ifelse(is.na(MOH) | is.na(MOH),"one","both")) %>%
    dplyr::mutate("Reported_by" = ifelse(!is.na(MOH),ifelse(!is.na(PEPFAR),"Both","MOH"),"PEPFAR")) %>%
    dplyr::mutate("Difference" = ifelse(Reported_by == "Both",MOH - PEPFAR,NA)) %>%
    dplyr::mutate("Reported_higher" = ifelse(!is.na(Difference),ifelse(Difference>0,"MOH",ifelse(Difference<0,"PEPFAR","Same result reported")),ifelse(is.na(MOH),"Only PEPFAR reported","Only MOH Reported"))) %>%
    dplyr::mutate("Count_of_sites_reporting_both" = sum(ifelse(Reported_by=="Both",1,0))) %>%
    dplyr::mutate("PEPFAR_sum_of_sites_reporting_both" = sum(ifelse(Reported_by=="Both",PEPFAR,0))) %>%
    dplyr::mutate("Weighting" = ifelse(Reported_by=="Both",PEPFAR/PEPFAR_sum_of_sites_reporting_both,NA)) %>%
    dplyr::mutate("Average" = rowMeans(cbind(MOH, PEPFAR),na.rm=F)) %>%
    dplyr::mutate("Weighted_diff" = ifelse(Reported_by=="Both",Weighting*abs(Difference)/Average,NA)) %>%
    dplyr::mutate("Site_hierarchy"= paste(namelevel3,namelevel4,namelevel5,namelevel6,namelevel7,sep=" / ")) %>%
    dplyr::select(namelevel3,namelevel4,namelevel5,namelevel6,namelevel7,indicator,
                  MOH,PEPFAR,Reported_on_by_both,Reported_by,Reported_higher,
                  Difference,Weighting,Weighted_diff,Count_of_sites_reporting_both,
                  PEPFAR_sum_of_sites_reporting_both,Site_hierarchy)

}
