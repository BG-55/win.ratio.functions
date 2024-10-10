#' Make nice tables for Win Ratio model output
#'
#' @importFrom dplyr %>%
#'
#' @param model_name A list output from a win.stat model, obtained by setting summary.print to false.
#' @param text_size A numeric, which is the size of the font for the text in the table. Default is 9.
#' @param footnote_text A character, which is the text that the footnote consists of. Default is no footnote.
#'
#' @return A flextable with the win ratio, net benefit, and win odds, along with their subsequent 95% CIs and P-Values
#' @export
#'
#' @examples
#' data_binary <- WINS::data_binary
#' res_binary <- WINS::win.stat(data = data_binary, ep_type = "binary", priority = c(1:3),
#' stratum.weight = "unstratified", arm.name = c("A","B"), alpha=0.05,
#' digit = 3,pvalue = "two-sided", summary.print = FALSE)
#' nice_table(res_binary,9,"test")
#'
nice_table <- function(model_name,text_size=9,footnote_text="") {
  #Setup size
  if(class(text_size) == "character") {
    text_size <- 9
  }
  #Missing model
  if(missing(model_name)) {
    stop("You must supply a model in the model_name function")
  }
#v <-
#Turn the numbers into dataframes
lapply(model_name$Win_statistic, tibble::enframe) %>%
  #Binds all of the dataframes together into one
  dplyr::bind_rows(., .id = "column_label") %>%
  #Chnage values in name for to be same so we can pivot
  dplyr::mutate(name = dplyr::case_when(
    stringr::str_detect(name,"_L") ~ "Lower",
    stringr::str_detect(name,"_U") ~ "Upper",
    TRUE ~ "Ratio"
  )) %>%
  #Pivot to be in good table form
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  #Renaming so we can join with p-value
  dplyr::rename(stat = column_label) %>%
  dplyr::mutate(stat = dplyr::case_when(
    stat == "Win_Ratio" ~ "wr",
    stat == "Net_Benefit" ~ "nb",
    TRUE ~ "wo"
  )) %>%
  #Join with p-value table
  dplyr::left_join(.,model_name$p_value %>% tidyr::pivot_longer(., cols = everything(), names_to = "stat") %>%
                     dplyr::mutate(stat = dplyr::case_when(
                       stat ==  "pvalue_WR" ~ "wr",
                       stat == "pvalue_NB" ~ "nb",
                       TRUE ~ "wo")) %>% dplyr::rename(p_value = value),by = "stat") %>%
  #P-Value Formatting
  dplyr::mutate(p_value = dplyr::case_when(
    p_value < 0.001 ~ "< 0.001",
    TRUE ~ as.character(signif(p_value,3))
  )) %>%
  #Other number formatting
  dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(.,3))) %>%
  dplyr::mutate(stat = dplyr::case_when(
    stat == "wr" ~ "Win Ratio",
    stat == "nb" ~ "Net Benefit",
    TRUE ~ "Win Odds"
  )) %>%
  dplyr::rename("Statistic Type" = stat, "Statistic Value" = Ratio, "Lower Limit of 95% CI" = Lower,
         "Upper Limit of 95% CI" = Upper, "P-Value" = p_value) %>%
  #Build tables
  flextable::flextable() %>%
  flextable::border_inner(border = officer::fp_border(color = "#D3D3D3")) %>%
  flextable::border_outer(border = officer::fp_border(color = "#D3D3D3"), part = "body") %>%
  flextable::border_outer(border = officer::fp_border(color = "#D3D3D3"), part = "header") %>%
  flextable::fontsize(size = text_size) %>% flextable::fontsize(size = text_size, part = "header") %>%
  flextable::set_table_properties(layout = "autofit") %>%
  flextable::add_footer_lines(footnote_text) %>%
  flextable::fontsize(size = text_size, part = "footer") %>%
  flextable::align(align = "center", part = "header")
}
