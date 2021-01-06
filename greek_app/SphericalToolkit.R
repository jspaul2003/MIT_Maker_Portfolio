#MATH S-139
#Toolkit for drawing diagrams in spherical geometry

require(plotrix)
#install.packages("beepr")
require(beepr)

#Utility functions
#Make a vector on the unit sphere, given latitude and longitude in degrees.
makeUnitVec <- function(lat, long){
  c(cos(lat*pi/180)*cos(long*pi/180),cos(lat*pi/180)*sin(long*pi/180),sin(lat*pi/180))
}

#The dot product as a binary operator
"%.%" <- function(v,w) sum(v*w)

#The cross product as a binary operator
"%x%" <- function(v,w) c(v[2]*w[3]-v[3]*w[2], v[3]*w[1]-v[1]*w[3], v[1]*w[2]-v[2]*w[1])

#Distance in radians between points P and Q
spherical.distance <- function(P,Q) acos(P%.%Q)

#Draw a blank plot, showing the bounding circle
blank.spherical.plot <- function(xylim, show.axes = FALSE)
{
  if(show.axes) {par(mar=c(3,3,1,1))} else {par(mar=c(1,1,1,1))}
  plot(NULL, xlim = c(-xylim,xylim), ylim = c(-xylim,xylim), axes = show.axes, asp = 1, pch = 20)
  draw.circle(0,0,1, border = "cyan")
}

#Draws a segment from start to end on a line where P is at 0, Q is at 1
#nplot is the number of segments between P and Q
draw.spherical.line <- function(P, Q, start = 0, end = 1, nplot = 20, color = par("fg"), style = par("lty"), width = par("lwd"))
{distPQ <-spherical.distance(P,Q)
  vecPQ<- (Q - P*cos(distPQ))/sin(distPQ)
  Nstart <- floor(start*nplot)
  Nend <- ceiling(end*nplot)
  
angles <- (Nstart:Nend)*distPQ/nplot; angles
#Work out the y and z coordinates for the corresponding points
y <- P[2]*cos(angles)+vecPQ[2]*sin(angles)
z <- P[3]*cos(angles)+vecPQ[3]*sin(angles)
points(y,z,type="l",col = color)
}

#Draws a circle
#radius can be either a number or the endpoints of a segment.
draw.spherical.circle <- function(center, radius, color = par("fg"), style = par("lty"), width = par("lwd"))
{if(length(radius) >1 ){radius <- spherical.distance(center,radius) }
angles = (0:360)*pi/180
vecN <- c(0,0,1)   #the North Pole
distAN <- spherical.distance(center,vecN)   #distance in radians to the North Pole
#Make a direction vector that points North
vecAN <- (vecN-cos(distAN)*center)/sin(distAN)
#A direction vector that points East is perpendicular to one that points North and to vecA.
vecAE <- vecAN%x%center;  
y <- center[2]*cos(radius)+(vecAN[2]*cos(angles)+vecAE[2]*sin(angles))*sin(radius)
z <- center[3]*cos(radius)+(vecAN[3]*cos(angles)+vecAE[3]*sin(angles))*sin(radius)
points(y,z, type="l", col = color,lty = style, lwd = width)   
}

#Erases a previously drawn circle
erase.spherical.circle <- function(center, radius)
{draw.spherical.circle(center, radius, color = "white" )}

#These are more than enough letters for Propositions 1-26
chLatin <- c("A","B","C","D","E","F","G","H","K","L")
chGreek <- c(expression(Alpha),expression(Beta),expression(Gamma),expression(Delta),
             expression(Epsilon),expression(Zeta),expression(Eta),expression(Theta),
             expression(Kappa),expression(Lambda))

#Prints a label next to a single point, with offset being a clock face number
label.spherical.point <- function(pt, name, offset, scale = 0.07,greek = FALSE){
  x <- pt[2] + scale*sin(offset*pi/6)
  y <- pt[3] + scale*cos(offset*pi/6)
  points(pt[2],pt[3])
  if (!greek)
    text(x,y,name)
  else {
    pos <- which(chLatin == name)[1]
    grName <- chGreek[pos]
    text(x,y,grName)
  }
}


#Labels multiple points, which are columns of the matrix pts
label.spherical.points <- function(pts, names, offsets, scale = 0.05, greek = FALSE){
  x <- pts[2,] + scale*sin(offsets*pi/6)
  y <- pts[3,] + scale*cos(offsets*pi/6)
  points(pts[2,],pts[3,])
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
locate.spherical.point <- function(){
  beep()
  pt <- locator(1)
  thePoint <- c(sqrt(1-pt$x^2-pt$y^2),pt$x, pt$y)
  points(pt$x,pt$y)
  return (thePoint)
}

#Returns the third vertex of an equilateral triangle in which ABC go counterclockwise
complete.spherical.triangle <- function(ptA, ptB){
  d <- spherical.distance(ptA, ptB)
  angle <- acos(cos(d)/(1+cos(d)))    #vertex angle for equilateral triangle
  vecAB <- (ptB - ptA*cos(d))/sin(d) #direction vector from A to B
  cross <- -vecAB%x%ptA      #perpendicular to vecAB and ptA
  poleAB <- cross/sqrt(cross%.%cross)  #unit vector perpendicular to both
  w <- vecAB*cos(angle)+poleAB*sin(angle)  #direction vector for side AC
  ptC <- ptA*cos(d)+w*sin(d)    
  points(ptC[2],ptC[3])
  return(ptC)
}

