#' @export correct
#'
#' @title
#' Spelling Corrector for Bahasa Indonesia
#'
#' @description
#' Provides function to correct spelling of a word in
#' Bahasa Indonesia using Naive Bayes algorithm.
#'
#' @author Aisyah Zakiah
#'
#' @param kalimat, kamus
#'
#' @examples correct("tidakk")
#'

correct <- function(kalimat, kamus=NULL) {
  kalimat <- as.character(kalimat)
  kalimat_split = strsplit(kalimat, split = " ")
  kalimat <- ""

  for(i in kalimat_split[[1]]){
    kata <- i
    if(nchar(trimws(kata))>0){
      if(is_katadasar(kata, kamus = kamus)) {
        kalimat <- c(kalimat, kata)
      } else{
        if(!is.null(kata)){
          if(kata %in% daftar_perubahan$lama) {
            idx <- which(daftar_perubahan$lama == kata)
            kalimat <- c(kalimat, daftar_perubahan$baru[idx])
          } else{
            edit_dist <- adist(kata, daftar_kata)
            min_edit_dist <- min(edit_dist)
            proposals_by_prob <- c(daftar_kata[ edit_dist <= min_edit_dist])
            a <- data.frame(lama=kata, baru=proposals_by_prob[1])
            daftar_perubahan <- rbind(daftar_perubahan, a)
            kalimat <- c(kalimat, proposals_by_prob[1])
          }
        }
      }
    }
    m <- write.csv(daftar_perubahan,row.names=FALSE,  file="daftar_perubahan.csv")
  }

  return(paste(kalimat, collapse=" "))
}

is_katadasar <- function(kata, kamus=NULL) {
  if ( is.null(kamus) ) {
    kata_dasar <- unique(kamus_katadasar)
  } else if ( is.vector(kamus) ) {
    kata_dasar <- unique(c(kamus_katadasar, kamus))
  } else {
    stop("Kamus harus berupa vektor!")
  }

  b <- !is.na(match(kata, kata_dasar))
  return(b)
}
