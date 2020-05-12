require(dplyr)
require(purrr)
require(ggplot2)
library(scales)
library(RColorBrewer)
require(rpivotTable)

site_table_data <- function(df, ou_uid) {

  ou_lvl <- get_ou_facility_level(ou_uid)

  if (ou_lvl == 7) {

    df %<>%
      dplyr::filter(`Reported on by both` == "both") %>%
      dplyr::group_by(namelevel3, namelevel4, namelevel5,
                      namelevel6, namelevel7, indicator,
                      period, `Site hierarchy`) %>%
      dplyr::summarise(PEPFAR = sum(PEPFAR),
                       MOH = sum(MOH),
                       Difference = sum(Difference),
                       `Weighted difference` = sum(`Weighted difference`)) %>%
      dplyr::ungroup()

  } else {

    df %<>%
      dplyr::filter(`Reported on by both` == "both") %>%
      dplyr::group_by(namelevel3, namelevel4, namelevel5,
                      namelevel6, indicator,
                      period, `Site hierarchy`) %>%
      dplyr::summarise(PEPFAR = sum(PEPFAR),
                       MOH = sum(MOH),
                       Difference = sum(Difference),
                       `Weighted difference` = sum(`Weighted difference`)) %>%
      dplyr::ungroup()

    }

  return(df)

}

indicator_table_data <- function(df) {

  df %>%
    dplyr::filter(`Reported on by both` == "both") %>%
    dplyr::group_by(indicator, period) %>%
    dplyr::summarise(Sites = n(),
                     PEPFAR = sum(PEPFAR),
                     MOH = sum(MOH),
                     Difference = sum(Difference),
                     `Weighted difference` = sum(`Weighted difference`)) %>%
    dplyr::ungroup()

}

discordance_chart <- function(df) {

  df %>%
    ggplot2::ggplot(aes(x = period,
                        y = `Weighted difference`,
                        group = indicator,
                        color = indicator)) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::scale_x_discrete(name = "Year") +
    ggplot2::scale_y_continuous(name = "Weighted Average Discordance",
                                breaks = seq(from = 0, to = 1, by = .1),
                                labels = scales::percent) +
    ggplot2::labs(title = "Overall Weighted Average Discordance",
                  subtitle = "Changes in Discordance, FY2018-FY2019") +
    ggplot2::theme_bw() +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 22),
                   plot.subtitle = element_text(hjust = 0.5, size = 14),
                   legend.position = "bottom")

}

moh_pivot <- function(d) {

  pivot <- d %>%
    purrr::pluck("analytics")

  rpivotTable(data = pivot,
              rows = c("indicator"),
              vals = "Weighted difference",
              aggregatorName = "Sum",
              rendererName = "Table",
              width = "70%",
              height = "700px")

}
