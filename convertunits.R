convertunits <- function(val,inputstr,outputstr,dimension) {
  inputstr <- tolower(inputstr)
  outputstr <- tolower(outputstr)
  dimension <- tolower(dimension)
  g <- 9.80665 # gravity, m/s2
  if (dimension =="length") {
    val <- switch(EXPR = inputstr,
                  "in"=val,
                  "ft"=val*12,
                  "yd"=val*12*3,
                  "mm"=val*1/10/2.54,
                  "cm"=val*1/2.54,
                  "m"=val*100/2.54)
    val <- switch(EXPR = outputstr,
                  "in"=val,
                  "ft"=val*1/12,
                  "yd"=val*1/(12*3),
                  "mm"=val*10*2.54,
                  "cm"=val*2.54,
                  "m"=val*1/100*2.54)
  } else if (dimension == "area") {
    val <- switch(EXPR = inputstr,
                  "in2"=val,
                  "ft2"=val*(12)^2,
                  "yd2"=val*(12*3)^2,
                  "mm2"=val*(1/10/2.54)^2,
                  "cm2"=val*(1/2.54)^2,
                  "m2"=val*(100/2.54)^2)
    val <- switch(EXPR = outputstr,
                  "in2"=val,
                  "ft2"=val*(1/12)^2,
                  "yd2"=val*(1/(12*3)^2),
                  "mm2"=val*(10*2.54)^2,
                  "cm2"=val*(2.54)^2,
                  "m2"=val*(1/100*2.54)^2)
  } else if (dimension == "volume") {
    val <- switch(EXPR = inputstr,
                  "in3"=val,
                  "ft3"=val*(12)^3,
                  "yd3"=val*(12*3)^3,
                  "mm3"=val*(1/10/2.54)^3,
                  "cm3"=val*(1/2.54)^3,
                  "m3"=val*(100/2.54)^3,
                  "ml"=val*(1/2.54)^3,
                  "l" =val*(1/2.54)^3*1000,
                  "us gallons"=val*231,
                  "bbls"=val*42*231)
    val <- switch(EXPR = outputstr,
                  "in3"=val,
                  "ft3"=val*(1/12)^3,
                  "yd3"=val*(1/(12*3)^3),
                  "mm3"=val*(10*2.54)^3,
                  "cm3"=val*(2.54)^3,
                  "m3"=val*(1/100*2.54)^3,
                  "ml"=val*(2.54)^3,
                  "l" =val*(2.54)^3/1000,
                  "us gallons"=val*1/231,
                  "bbls"=val*1/(42*231))
  } else if (dimension == "time") {
    val <- switch(EXPR=inputstr,
                  "s"=val,
                  "min"=val*60,
                  "hr"=val*60*60,
                  "days"=val*60*60*24,
                  "weeks"=val*60*60*24*7,
                  "months"=val*60*60*24*7*4,
                  "years"=val*60*60*24*365.25)
    val <- switch(EXPR=outputstr,
                  "s"=val,
                  "min"=val*1/60,
                  "hr"=val*1/(60*60),
                  "days"=val*1/(60*60*24),
                  "weeks"=val*1/(60*60*24*7),
                  "months"=val*1/(60*60*24*7*4),
                  "years"=val*1/(60*60*24*365.25))
  } else if (dimension == "temperature") {
    val <- switch(EXPR=inputstr,
                  "f"=val,
                  "c"=val*9/5+32,
                  "k"=val*9/5-459.67,
                  "r"=val-459.67)
    val <- switch(EXPR=outputstr,
                  "f"=val,
                  "c"=(val-32)*5/9,
                  "k"=(val+459.67)*5/9,
                  "r"=val+459.67)
  } else if (dimension == "mass") {
    val <- switch(EXPR=inputstr,
                  "lb"=val,
                  "slugs"=val*(g*100/2.54/12),
                  "g"=val*1/453.5924277,
                  "kg"=val*1/0.4535924277)
    val <- switch(EXPR=outputstr,
                  "lb"=val,
                  "slugs"=val*1/(g*100/2.54/12),
                  "g"=val*453.5924277,
                  "kg"=val*0.4535924277)
  } else if (dimension == "force") {
     val <- switch(EXPR=inputstr,
                   "lb"=val,
                   "kip"=val*1000,
                   "n"=val*100/2.54/12/0.4535924277/(g*100/2.54/12),
                   "kn"=val*1000*100/2.54/12/0.4535924277/(g*100/2.54/12))
     val <- switch(EXPR=outputstr,
                   "lb"=val,
                   "kip"=val*1/1000,
                   "n"=val*1/(100/2.54/12/0.4535924277/(g*100/2.54/12)),
                   "kn"=val*1/(1000*100/2.54/12/0.4535924277/(g*100/2.54/12)))
  }
  return(val)
}