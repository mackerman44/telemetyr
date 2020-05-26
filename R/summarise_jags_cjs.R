#' @title Summarise Bayesian CJS Model
#'
#' @description From the mcmc.list object, summarise the parameter estimates and include site names where appropriate.
#'
#' @author Kevin See and Mike Ackerman
#'
#' @inheritParams postpack::post_summ
#'
#' @importFrom postpack post_summ
#' @export
#' @return tibble

summarise_jags_cjs = function(post = NULL,
                              p = NULL,
                              ...) {

  stopifnot(!is.null(post),
            class(post) == 'mcmc.list')


  if(is.null(p)) {
    p = colnames(post[[1]])
  }

  param_summ = post_summ(post,
                         p,
                         ...) %>%
    t() %>%
    as_tibble(rownames = "param") %>%
    mutate(cv = abs(sd / mean)) %>%
    mutate(site_num = str_extract(param, "[:digit:]+"),
           site_num = as.integer(site_num),
           param_grp = str_extract(param, "[:alpha:]+")) %>%
    select(param_grp, site_num, everything())

  return(param_summ)

}
