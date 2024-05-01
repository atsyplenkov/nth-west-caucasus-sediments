# An approach to estimate saturated sediment density
# using diameter and settling velocity from
# 1) Reservoir sedimentation handbook: design and management of dams,
# reservoirs, and watersheds for sustainable use
# and
# 2) Речные наносы: учебник

sed_dens <-
  function(
      .w,
      .d,
      .mu = 10^-3) {
    # convert diam in mm to mm
    .d <- .d / 1000

    1000 + (((500 * .d * .w + 3 * .mu)^2 - 9 * .mu^2) / (1636 * .d^3))
  }

sed_dens(0.072, 0.6)
