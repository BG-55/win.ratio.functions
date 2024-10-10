#' Make a table that shows the components of a Win Ratio Model
#'
#' @importFrom dplyr %>%
#'
#' @param model_name A list output from a win.stat model, obtained by setting summary.print to false.
#' @param tot_comp A numeric, which is the total number of person to person comparisons. Calculated by multiplying the number of people in the treatment arm and the number of people in the control arm
#' @param endpoint_names A vector or character, which is the names of the endpoints in the model
#' @param number_endpoints A numeric, which is the number of endpoints in the model. Must be specified of endpoint_names is not specified
#' @param text_size A numeric, which is the size of the font for the text in the table. Default is 9.
#' @param footnote_text A character, which is the text that the footnote consists of. Default is no footnote.
#'
#' @return A flextable, which consists of the win ratio, net benefit, and win odds at each endpoint.
#' @export
#'
#' @examples
#' data_binary <- WINS::data_binary
#' res_binary <- WINS::win.stat(data = data_binary, ep_type = "binary", priority = c(1:3),
#' stratum.weight = "unstratified", arm.name = c("A","B"), alpha=0.05,
#' digit = 3,pvalue = "two-sided", summary.print = FALSE)
#' #Get the total number of comparisons
#' tot_comps <- sum(data_binary$arm == "A") * sum(data_binary$arm == "B")
#' inter_table(res_binary, tot_comp = tot_comps, number_endpoints = 3)
#'


inter_table <- function(model_name,tot_comp,endpoint_names=NULL, number_endpoints=NULL,
                        text_size=9,footnote_text="") {
  #If both missing give an error
  if(is.null(endpoint_names) && is.null(number_endpoints)) {
    stop("Either number_endpoints or endpoint_names must have an arguement")
  }
  #Create endpoints if no names entered
if (is.null(endpoint_names)) {
  endpoint_names <- paste("Endpoint",rep(1:number_endpoints))
}
  #Missing tot combos gives an error
if(base::missing(tot_comp)) {
  stop("tot_comp must have an argument")
}
  #tot_comp being not a number is an error
if(class(tot_comp) == "character") {
  stop("tot_comp must be a numeric")
}
  #Setup size
  if(class(text_size) == "character") {
    text_size <- 9
  }
  #Missing model
  if(missing(model_name)) {
    stop("You must supply a model in the model_name function")
  }
#ii <-
dplyr::bind_rows(model_name$summary_ep, .id = "column_label") %>%
  dplyr::mutate(trt_type = sub("_.*","",column_label)) %>%
  #Get endpoint number
  dplyr::mutate(ep_number = as.character(sub('.*Endpoint', '', column_label))) %>%
  #Group by endpoint number
    dplyr::group_by(ep_number, trt_type) %>%
    dplyr::summarize(total_count = sum(Count)) %>%
  #Reformat for math
  tidyr::pivot_wider(names_from = trt_type, values_from = total_count) %>%
  #Get total counts
    dplyr::mutate(ov_count = rowSums(dplyr::across(where(is.numeric)), na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
  #CumSum for total counts
    dplyr::mutate(cum_sum_ov_count = cumsum(ov_count)) %>%
  #Win Ratio (ratio)
    dplyr::mutate("Win Ratio" = signif(Trt / Con,3)) %>%
  #Net Benefit (not condtional)
  #(tot_comp - cum_sum_ov_count) is the number of ties at that point
  #Get total number of combos for denom here
    dplyr::mutate("Net Benefit" = signif((Trt - Con) / tot_comp),3) %>%
  #verifier
  #391-254(23 events prior + 173+58 events at this endpoint) = 137 ties at endpoint 2
  #58-173 / 58+173+137 = -0.3125
    dplyr::mutate("Win Odds" =
           signif((Trt + (0.5*(tot_comp - cum_sum_ov_count))) / (Con + (0.5*(tot_comp - cum_sum_ov_count)))),3) %>%
  #137 ties * 0.5 = 68.5
  #58 + 68.5 / 173 + 68.5 = 0.523
    dplyr::mutate("Endpoint Name" = endpoint_names) %>%
  #Add overall row
  tibble::add_row(`Endpoint Name` = "Overall",
          `Win Ratio` = signif(unname(model_name$Win_statistic$Win_Ratio[1]),3),
          `Net Benefit` = signif(unname(model_name$Win_statistic$Net_Benefit[1]),3),
          `Win Odds` = signif(unname(model_name$Win_statistic$Win_Odds[1])),3) %>%
    dplyr::select(c(`Endpoint Name`, `Win Ratio`, `Net Benefit`, `Win Odds`)) %>%
    dplyr::mutate(dplyr::across(c(`Win Ratio`, `Net Benefit`, `Win Odds`), ~ as.character(.))) %>%
    dplyr::mutate(dplyr::across(c(`Win Ratio`, `Net Benefit`, `Win Odds`), ~ dplyr::case_when(
    . == "NaN" | . == "Inf" ~ "N/A",
    TRUE ~ .
  ))) %>%
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
