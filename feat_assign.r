feat_assign <- function(input, use.rounding = FALSE) {
 # require(stringr)

  place.lab <-  "bfmpvw"
  place.vel <-  "gkNJ"
  place.dor <-  "gkNJ"
  place.glot <-  "?h"
  place.cor <-  "CdjnstzrlTDSZLR<"
  
  place <-
    c(
      lab = place.lab,
      dor = place.dor,
      glot = place.glot,
      cor = place.cor
    )
  
  manner.stop <- "bdgkpt?<"
  manner.fricative <- "CfjszvTDSZh"
  manner.nas <- "mnN"
  manner.approx <- "rlLRJw"
  
  manner <-
    c(
      stop = manner.stop,
      fricative = manner.fricative,
      nas = manner.nas,
      approx = manner.approx
    )
  
  voicing.voi <- "bdgjmnzvNrlDZLRJw<"
  voicing.unvoi <- "Cfkpst?TSh"
  
  voicing <- c(voi = voicing.voi, unvoi = voicing.unvoi)
  
  height.low <- "aA"
  height.mid <- "eEo>^&"
  height.high <- "iIuU"
  height.low_high <- "yW"
  height.mid_high <- "O"
  
  height <-
    c(
      low = height.low,
      mid = height.mid,
      high = height.high,
      low_high = height.low_high,
      mid_high = height.mid_high
    )
  
  backness.back <- "auUo>^W"
  backness.front <- "AeEiI"
  backness.central <- "&"
  backness.back_front <- "yO"
  
  backness <-
    c(
      back = backness.back,
      front = backness.front,
      central = backness.central,
      back_front = backness.back_front
    )
  
  quality.tense <- "aeiuoyWO"
  quality.lax <- "AEIU>^&"
  
  quality <- c(tense = quality.tense, lax = quality.lax)
  
  rounding.unrou <- "a"
  rounding.round <- "uUo>"
  
  rounding <- c(unrou = rounding.unrou, round = rounding.round)
  
  if (use.rounding) {
    allfeatures <-
      c(
        place = place,
        manner = manner,
        voicing = voicing,
        height = height,
        backness = backness,
        quality = quality,
        rounding = rounding
      )
  } else {
    allfeatures <-
      c(
        place = place,
        manner = manner,
        voicing = voicing,
        height = height,
        backness = backness,
        quality = quality
      )
  }
  
  df <- data.frame( names(allfeatures))
  colnames(df) <- "feature"
  for (let in strsplit(input, split = "")[[1]]) {
    tmp <-
      data.frame(names(allfeatures), sapply(allfeatures,function(x) length(grep(let,x, fixed=TRUE) > 0)))
    colnames(tmp) <- c("feature", let)
    df <- merge(df, tmp, by = "feature")
  }
  rownames(df) <- df[, 1]
  df[, 1] <- NULL
  
  return(df)
  
}
