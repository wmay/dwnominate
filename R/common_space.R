## calculate common space scores 


get_scale_mat = function(wnoms, id_name = NULL, dim = NULL) {
  ## organize the scale matrix
  if (is.null(dim)) {
    wnom_dims = 1:wnoms[[1]]$dimensions
  } else {
    wnom_dims = dim
  }
  if (is.null(id_name)) {
    get_ids = function(x) row.names(x$legislators)
  } else {
    get_ids = function(x) x$legislators[, id_name]
  }
  leg_ids = unique(unlist(lapply(wnoms, get_ids)))
  scales = expand.grid(session = 1:length(wnoms), dim = wnom_dims)
  scale_names = paste0('s', scales$session, 'd', scales$dim)
  smat = matrix(NA, nrow = length(leg_ids), ncol = length(scale_names),
                dimnames = list(leg_ids, scale_names))
  for (s in 1:length(wnoms)) {
    legs = wnoms[[s]]$legislators
    for (d in wnom_dims) {
      scale_name = paste0('s', s, 'd', d)
      coord_name = paste0('coord', d, 'D')
      smat[as.character(legs[, id_name]), scale_name] = legs[, coord_name]
    }
  }
  smat
}


#' Calculate common space scores
#'
#' The common space function extracts latent dimensions from a
#' collection of roll call scaling results using the procedure
#' outlined in Poole (1998) and Poole (2005).
#'
#' @param nom_list A list of \code{nomObject} results from the
#'   \code{wnominate} package. The number of estimated dimensions in
#'   each \code{nomObject} should be greater than or equal to
#'   \code{dims}.
#' @param id Column name in the \code{nomObject}s' \code{legislators}
#'   data frames providing a unique legislator ID. If not specified
#'   row names will be used.
#' @param dims The number of dimensions to estimate.
#' @param minscale The minimum number of W-NOMINATE scores a
#'   legislator must have to be used for the latent dimension
#'   estimates. Common space scores for legislators with fewer than
#'   \code{minscale} scores will be derived from the estimated
#'   dimensions.
#' @param polarity A vector of ID's of length \code{dims} of
#'   legislators who should have relatively positive scores on the
#'   corresponding dimension. If only one ID is provided, it will be
#'   recycled for all dimensions.
#' @return A list of class \code{common space} containing: \itemize{
#'   \item{legislators} {A data frame of legislator coordinates}
#'   \item{blackbox} {A list of blackbox results} \item{nom_list} {The
#'   provided list of \code{wnominate} results} }
#' @references Keith T. Poole. 1998. 'Recovering a Basic Space From a
#'   Set of Issue Scales.' New York: Cambridge University
#'   Press. American Journal of Political Science, Vol. 42, No. 3
#'   (Jul., 1998), pp. 954-993
#' 
#' Keith T. Poole. 2005. 'Spatial Models of Parliamentary Voting.' New
#'   York: Cambridge University Press.
#' @export
common_space = function(nom_list, id = NULL, dims = 2, minscale = 5,
                        polarity = NULL) {
  ## check the dimensions
  min_dim = min(sapply(nom_list, function(x) x$dimensions))
  if (dims > min_dim) stop("All wnominate results must have at least 'dims' dimensions.")
  if (!is.null(polarity)) {
    if (length(polarity) == 1 && dims > 1) {
      polarity = rep(polarity, dims)
    } else if (length(polarity) != dims) {
      stop("Polarity should be of length 1 or 'dims'.")
    }
  }
  bb = list()
  legislators = data.frame()
  for (n in 1:dims) {
    ## run blackbox
    smat = get_scale_mat(nom_list, id, n)
    bb[[n]] = basicspace::blackbox(smat, dims = 1, minscale = minscale)
    ## get missing legislator scores
    new_legs = bb[[n]]$individuals[[1]]
    scales = bb[[n]]$stimuli[[1]]
    missing = which(is.na(new_legs$c1))
    new_legs$c1[missing] = apply(smat[row.names(new_legs)[missing], ], 1,
                                 function(x) mean((x - scales$c) / scales$w1, na.rm = T))
    ## scale results to fit within the range [-1, 1]
    new_legs$c1 = scale(new_legs$c1, center = mean(range(new_legs$c1)),
                        scale = (max(new_legs$c1) - min(new_legs$c1)) / 2)
    ## reverse the scores if needed to match the polarity
    if (!is.null(polarity)) {
      if (new_legs[polarity[n], 'c1'] < median(new_legs$c1, na.rm = TRUE)) {
        new_legs$c1 = -new_legs$c1
      }
    }
    
    names(new_legs) = paste0('coord', n, 'D')
    legislators = merge(legislators, new_legs, by = 'row.names', all = TRUE)
    ## fix the extra Row.names column
    row.names(legislators) = legislators$Row.names
    legislators$Row.names = NULL
  }
  ## scale results to fit in unit hypersphere
  distances = apply(legislators, 1, function(x) sqrt(sum(x ^ 2)))
  legislators[, ] = scale(legislators, center = FALSE,
                          scale = rep(max(distances, na.rm=T), ncol(legislators)))
  
  res = list(legislators = legislators,
             blackbox = bb, nom_list = nom_list)
  class(res) = 'common space'
  res
}
