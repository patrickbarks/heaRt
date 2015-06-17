
# Create two heart-shaped polygons, one representing relaxed heart and the
#  other a contracted heart. Animate a transition between the two polygons
#  according to a 'heartbeat function' that represents the intensity of contraction
#  versus time. The heartbeat function here is just a series of normal distributions.


# Required libraries
library(animation)
library(ggplot2)


# Set x and y coordinates for relaxed heart and contracted heart
z <- seq(0, 2 * pi, 0.001)
heart_relax_x <- 15.5 * sin(z)^3
heart_relax_y <- 12 * cos(z) - 3 * cos(2 * z) - 2 * cos(3 * z) - 0.3 * cos(4 * z)
heart_contract_x <- 15 * sin(z)^3
heart_contract_y <- 11.7 * cos(z) - 3 * cos(2.03 * z) - 2 * cos(2.9 * z) - 0.3 * cos(4.1 * z)


# Plot to check heart polygons
ggplot() +
  geom_polygon(aes(x = heart_relax_x, y = heart_relax_y), fill = rgb(0, 0, 1, 0.6)) +
  geom_polygon(aes(x = heart_contract_x, y = heart_contract_y), fill = rgb(1, 0, 0, 0.6))


# Set parameters for heartbeat function. The heartbeat vector describes contraction
#  intensity over time Min contraction intensity is simply the 'relaxed' heart, and
#  max contraction intensity is the 'contracted' heart. Intermediate contraction
#  intensities yield heart shapes intermediate between the relaxed and contracted heart.
n_frames <- 200                         # number of frames in gif
frames <- 1:n_frames                    # frame vector
n_beats <- 5                            # number of heartbeats to depict
frames_per_beat <- n_frames / n_beats   # number of frames per hearbeat
beat_duration <- 4                      # becomes stdev of normal distributions


# Heartbeat function
heartbeat <- rep(dnorm(1:frames_per_beat, frames_per_beat / 2, beat_duration), n_beats)
heartbeat <- heartbeat / max(heartbeat)


# Plot to check heartbeat function
plot(heartbeat ~ frames, type = "l")


# Create gif
saveGIF({
  # create two-panel plot matrix
  layout(matrix(c(1, 2)), widths = 6, heights = c(4, 1))
  par(oma = c(1, 1, 1, 1), mar = c(0.5, 0, 0, 0))
	
  for (i in frames) {
    # get current heart coords at given contraction intensity
    heart_x <- (1 - heartbeat[i]) * heart_relax_x + heartbeat[i] * heart_contract_x
    heart_y <- (1 - heartbeat[i]) * heart_relax_y + heartbeat[i] * heart_contract_y

    # plot heart polygon (fill transparency proportional to contraction intensity)
    plot(NA, NA, xlim = c(-16, 16), ylim = c(-14, 11), ann = F, axes = F)
    polygon(heart_x, heart_y, col = rgb(1, 0, 0, 0.3 + 0.5 * heartbeat[i]), lwd = 6)
    box()

    # plot heartbeat function
    plot(heartbeat ~ frames, type = "l", ylim=c(0, 1.4 * max(heartbeat)), lwd = 2, ann = F, axes=F)
    box()

    # plot series of points tracking progress of heartbeat function
    points(frames[i], heartbeat[i], pch = 19, cex = 2, col = rgb(0, 0, 0, 1))
    if (i > 1) points(frames[i-1], heartbeat[i-1], pch = 19, cex = 1.9, col = rgb(0, 0, 0, 0.8))
    if (i > 2) points(frames[i-2], heartbeat[i-2], pch = 19, cex = 1.8, col = rgb(0, 0, 0, 0.6))
    if (i > 3) points(frames[i-3], heartbeat[i-3], pch = 19, cex = 1.7, col = rgb(0, 0, 0, 0.4))
    if (i > 4) points(frames[i-4], heartbeat[i-4], pch = 19, cex = 1.6, col = rgb(0, 0, 0, 0.2))
  }
}, movie.name = "heaRt.gif", interval = 0.03, ani.width = 400, ani.height = 400, loop = TRUE)

