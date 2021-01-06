#MATH S-139
#Toolkit for drawing diagrams in Euclidean geometry
#Be sure this file is in your default directory (set in Tools/Global Options)
#install.packages("beepr")
library(beepr)

#Create a blank plot that extends from -xylim to xylim in both x and y
blank.euclid.plot <- function(xylim, show.axes = FALSE)
{
  if(show.axes) {par(mar=c(3,3,1,1))} else {par(mar=c(1,1,1,1))}
  plot(NULL, xlim = c(-xylim,xylim), ylim = c(-xylim,xylim), axes = show.axes, asp = 1, pch = 20)             
}

#Draws a segment from satrt to end on a line where P is at 0, Q is at 1
draw.euclid.line <- function(P, Q, start = 0, end = 1, color = par("fg"), style = par("lty"), width = par("lwd")){
  A <- P+start*(Q-P)
  B <- P + end*(Q-P)
  segments(A[1],A[2],B[1],B[2], col = color, lty = style, lwd = width)
}

#Draws a circle
#radius can be either a number or the endpoints of a segment.
draw.euclid.circle <- function(center, radius, color = par("fg"), style = par("lty"), width = par("lwd")){
  if(length(radius) >1 ){radius = sqrt(sum((radius-center)*(radius-center))) }
  angles = (0:360)*pi/180
  x <- center[1]+radius*cos(angles)
  y <- center[2]+radius*sin(angles)
  points(x,y, type = "l", col = color, lty = style, lwd = width)
}

#Erases a previously drawn circle
erase.euclid.circle <- function(center, radius){
  draw.euclid.circle(center, radius, color = "white" )
}

#These are enough letters for the Pythagorean Theorem
chLatin <- c("A","B","C","D","E","F","G","H","K","L")
chGreek <- c(expression(Alpha),expression(Beta),expression(Gamma),expression(Delta),
             expression(Epsilon),expression(Zeta),expression(Eta),expression(Theta),
             expression(Kappa),expression(Lambda))

#Prints a label next to a single point, with offset being a clock face number
label.euclid.point <- function(pt, name, offset, scale = 0.1, greek = FALSE){
  x <- pt[1] + scale*sin(offset*pi/6)
  y <- pt[2] + scale*cos(offset*pi/6)
  points(pt[1],pt[2])
  if (!greek)
    text(x,y,name)
  else {
    pos <- which(chLatin == name)[1]
    grName <- chGreek[pos]
    text(x,y,grName)
  }
}

#Labels multiple points, which are columns of the matrix pts
label.euclid.points <- function(pts, names, offsets, scale = 0.1, greek = FALSE){
  x <- pts[1,] + scale*sin(offsets*pi/6)
  y <- pts[2,] + scale*cos(offsets*pi/6)
  points(pts[1,],pts[2,])
  if (!greek)
    text(x,y,names)
  else {
    grNames <- character(0)
    for (i in 1:length(names)) {
      pos <- which(chLatin == names[i])[1]
      grNames <- c(grNames,chGreek[pos])
    }
    text(x,y,grNames)
  }
}

#Returns a point specified by a mouse click
locate.euclid.point <- function(){
  beep()
  pt <- locator(1)
  thePoint <- c(pt$x, pt$y)
  points(pt$x,pt$y)
  return (thePoint)
}

#Returns the third vertex of an equilateral triangle in which ABC go counterclockwise
complete.euclid.triangle <- function(ptA, ptB){
  v <- ptB - ptA
  len <- sqrt(sum(v^2)) #length of the segment
  angle <- atan2(v[2],v[1])
  theta <- angle + 2*pi/3
  ptC <- c(ptB[1]+len*cos(theta), ptB[2]+len*sin(theta))
  return(ptC)
}








