# Slightly modified `broom:::exponentiate()` function
#
# Uses updated dplyr semantics of `dplyr::across` within `dplyr::mutate`
# instead of `dplyr::mutate_at`
exponentiate <- function(data) {
  data <- dplyr::mutate(data, "estimate" = exp(eval(str2expression("estimate"))))
  if ("conf.low" %in% colnames(data)) {
    data <- dplyr::mutate(
      data,
      dplyr::across(.cols = c("conf.low", "conf.high"), .fns = exp)
    )
  }
  data
}
