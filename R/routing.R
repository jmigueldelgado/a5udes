#' Calculate the water balance and route downstream. Order must be given previously by rank with `arrange(desc(rank))`.
#'
#' @param neigh is a dataframe describing the neighborhood upstream of the reservoir
#' @param state is a dataframe describing the current state of the reservoirs -- volume, incoming runoff, maximum volume and overspill
#' @return state the state of the reservoir after water balance concluded
#' @export
water_balance = function(neigh,state) {
  for(i in seq(1,nrow(neigh))) {
    state$overspill[i]=max(
      state$volume[i]+state$incoming_runoff[i]+sum(state$overspill[neigh$neighborhood[i][[1]]])-state$evap[i]-state$max_volume[i],
      0)
    state$volume[i]=min(state$max_volume[i],
      state$volume[i]+state$incoming_runoff[i]+sum(state$overspill[neigh$neighborhood[i][[1]]])-state$evap[i]
    )
  }
  return(state)
}
