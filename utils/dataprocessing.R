grid_maker = function(variable) {
  lims = range(variable)
  return(seq(from = lims[1], to = lims[2], length.out = length(variable)))
}
