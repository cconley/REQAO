#'df <- SchoolLoad(3, YYYY, 66060, "DDSB", datadir)
#'
#'Use:   df <- SchoolLoad(3, 2017, 66060, "DDSB", datadir)
#'grades = 3, 6, 9, 10
#'year = YYYY (school year in June)
#'
#' - identify the grade, year and board id: used to construct EQAO file names
#' - identify the school board: used to label the board row in the file which is NA by default
#' - for the DDSB use v and c to access network and c data directories
#'   otherwise pass a character string or vector with the directory containing the EQAO files
#

SchoolLoad <- function(grade, year,bident,board, datadir) {
  datadir.txt <- ifelse(datadir == "v", "V:/Programs/Accountability & Assessment/AA_MasterData/",
         ifelse(datadir == "c", "C:/Data-Local/", datadir))

  ifelse(grade == 10, {
    OSSLT.1 <- readr::read_csv(paste0(datadir.txt,"G10_",year,"_FTE_B01",bident,"_1.csv"))
    OSSLT.1$SchoolMident[is.na(OSSLT.1$SchoolMident)] <- bident
    OSSLT.1$SchoolName[is.na(OSSLT.1$SchoolName)] <- board

    OSSLT.2 <- readr::read_csv(paste0(datadir.txt,"G10_", year,"_FTE_B01",bident,"_2.csv"))
    OSSLT.2$SchoolMident[is.na(OSSLT.2$SchoolMident)] <- bident
    OSSLT.2$SchoolName[is.na(OSSLT.2$SchoolName)] <- board

    OSSLT.3 <- readr::read_csv(paste0(datadir.txt,"G10_", year, "_FTE_B01",bident,"_3.csv"))
    OSSLT.3$SchoolMident[is.na(OSSLT.3$SchoolMident)] <- bident
    OSSLT.3$SchoolName[is.na(OSSLT.3$SchoolName)] <- board

    x <- merge(OSSLT.1, OSSLT.2, by = "SchoolMident")
    x <- merge(x, OSSLT.3, by = "SchoolMident")

    x$year <- year

    return(x)
    },
    ifelse(grade %in% 9, {
      G9.1 <- readr::read_csv(paste0(datadir.txt,"G9_",year,"_B01",bident,"_1.csv"))
      G9.1$SchoolMident[is.na(G9.1$SchoolMident)] <- bident
      G9.1$SchoolName[is.na(G9.1$SchoolName)] <- board

      G9.2 <- readr::read_csv(paste0(datadir.txt,"G9_",year,"_B01",bident,"_2.csv"))
      G9.2$SchoolMident[is.na(G9.2$SchoolMident)] <- bident
      G9.2$SchoolName[is.na(G9.2$SchoolName)] <- board

      G9.3 <- readr::read_csv(paste0(datadir.txt,"G9_",year,"_B01",bident,"_3.csv"))
      G9.3$SchoolMident[is.na(G9.3$SchoolMident)] <- bident
      G9.3$SchoolName[is.na(G9.3$SchoolName)] <- board

      x <- merge(G9.1, G9.2, by = "SchoolMident")
      x <- merge(x, G9.3, by = "SchoolMident")

      x$year <- year

      return(x)
      },
      ifelse(grade %in% c(3,6), {
        x.1 <- readr::read_csv(paste0(datadir.txt,"G",grade,"_",year,"_B01",bident,"_1.csv"))
        x.1$SchoolMident[is.na(x.1$SchoolMident)] <- bident
        x.1$SchoolName[is.na(x.1$SchoolName)] <- board

        x.2 <- readr::read_csv(paste0(datadir.txt,"G",grade,"_",year,"_B01",bident,"_2.csv"))
        x.2$SchoolMident[is.na(x.2$SchoolMident)] <- bident
        x.2$SchoolName[is.na(x.2$SchoolName)] <- board

        x.3 <- readr::read_csv(paste0(datadir.txt,"G",grade,"_",year,"_B01",bident,"_3.csv"))
        x.3$SchoolMident[is.na(x.3$SchoolMident)] <- bident
        x.3$SchoolName[is.na(x.3$SchoolName)] <- board

        x.4 <- readr::read_csv(paste0(datadir.txt,"G",grade,"_",year,"_B01",bident,"_4.csv"))
        x.4$SchoolMident[is.na(x.4$SchoolMident)] <- bident
        x.4$SchoolName[is.na(x.4$SchoolName)] <- board

        x <- merge(x.1, x.2, by = "SchoolMident")
        x <- merge(x, x.3, by = "SchoolMident")
        x <- merge(x, x.4, by = "SchoolMident")

        x$year <- year

        return(x)
      }, "Unable to locate files, check the grade, bident, or file name.")))

}




#'df <- StudentLoad(3, YYYY, bident, datadir)
#'
#'Use:   df <- StudentLoad(3, 2017, bident, datadir)
#'grades = 3, 6, 9, 10
#'
#' - identify the grade, year and board id: used to construct EQAO file names
#' - for the DDSB use v and c to access network and c data directories
#'   otherwise pass a character string or vector with the directory containing the EQAO files

StudentLoad <- function(grade, year, bident, datadir){
  datadir.txt <- ifelse(datadir == "v", "V:/Programs/Accountability & Assessment/AA_MasterData/",
                        ifelse(datadir == "c", "C:/Data-Local/", datadir))
  ifelse(grade == 10, {
    x <- readr::read_csv(paste0(datadir.txt,"OSSLT_",year,"_ISD_SQ_B0E",bident,".csv"))
    x$IEPcode <- paste0(x$IEP,
                        x$IPRCExBehaviour,
                        x$IPRCExAutism,
                        x$IPRCExDeaf,
                        x$IPRCExLanguage,
                        x$IPRCExSpeech,
                        x$IPRCExLearning,
                        x$IPRCExGiftedness,
                        x$IPRCExMildIntellectual,
                        x$IPRCExDevelopmental,
                        x$IPRCExPhysical,
                        x$IPRCExBlind,
                        x$IPRCExMultiple)
    x$IEPcode <- ifelse(x$IEPcode == "0000000000000", "No IEP",
                        ifelse(x$IEPcode == "1000000000000", "IEP no IPRC",
                               ifelse(x$IEPcode == "1100000000000", "Behaviour",
                                      ifelse(x$IEPcode == "1010000000000", "Autism",
                                             ifelse(x$IEPcode == "1001000000000", "Deaf",
                                                    ifelse(x$IEPcode == "1000100000000", "Language",
                                                           ifelse(x$IEPcode == "1000010000000", "Speech",
                                                                  ifelse(x$IEPcode == "1000001000000","Learning",
                                                                         ifelse(x$IEPcode == "1000000100000","Giftedness",
                                                                                ifelse(x$IEPcode == "1000000010000","MildIntellectual",
                                                                                       ifelse(x$IEPcode == "1000000001000","Developmental",
                                                                                              ifelse(x$IEPcode == "1000000000100","Physical",
                                                                                                     ifelse(x$IEPcode == "1000000000010","Blind",
                                                                                                            ifelse(x$IEPcode == "1000000000001","Multiple","BadCode")
                                                                                                     )
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
    )

    x$Gender <- ifelse(x$Gender == 1, "Male",
                       ifelse(x$Gender == 2, "Female", NA))

    x$EligibilityStatus <- ifelse(x$EligibilityStatus == 2, "PE",
                                  ifelse(x$EligibilityStatus == 1, "FTE", NA))

    x$LevelOfStudyLanguage <- ifelse(x$LevelOfStudyLanguage == 5, "Other",
                                     ifelse(x$LevelOfStudyLanguage == 4, "ESL/ELD",
                                            ifelse(x$LevelOfStudyLanguage == 3, "Loc. Dev.",
                                                   ifelse(x$LevelOfStudyLanguage == 2, "Applied",
                                                          ifelse(x$LevelOfStudyLanguage == 1, "Academic", NA)))))
    x$ESLELD_ALFPDF <- ifelse(x$ESLELD_ALFPDF == 1, "ELL",
                              ifelse(x$ESLELD_ALFPDF == 0, "non-ELL", NA))

    x$AccScribing <- ifelse(x$AccScribing == 1, "Scribe",
                            ifelse(x$AccScribing == 0, "no Scribe", NA))

    x$AccAssistiveTech <- ifelse(x$AccAssistiveTech == 1, "Assist. Tech.",
                                 ifelse(x$AccAssistiveTech == 0, "no Assist. Tech", NA))
    return(x)
    },
    ifelse(grade == 9, {
      x <- readr::read_csv(paste0(datadir.txt,"G9_",year,"_B0E", bident,"_ISD_SQ.csv"))
      x$IEP <- paste0(x$SIF_IEP,
                      x$SIF_IPRCBehaviour,
                      x$SIF_IPRCAutism,
                      x$SIF_IPRCDeaf,
                      x$SIF_IPRCLanguage,
                      x$SIF_IPRCSpeech,
                      x$SIF_IPRCLearning,
                      x$SIF_IPRCGifted,
                      x$SIF_IPRCIntellectual,
                      x$SIF_IPRCDevelopmental,
                      x$SIF_IPRCPhysical,
                      x$SIF_IPRCBlind,
                      x$SIF_IPRCMultiple)
      x$IEP <- ifelse(x$IEP == "0000000000000", "No IEP",
                      ifelse(x$IEP == "1000000000000", "IEP no IPRC",
                             ifelse(x$IEP == "1100000000000", "Behaviour",
                                    ifelse(x$IEP == "1010000000000", "Autism",
                                           ifelse(x$IEP == "1001000000000", "Deaf",
                                                  ifelse(x$IEP == "1000100000000", "Language",
                                                         ifelse(x$IEP == "1000010000000", "Speech",
                                                                ifelse(x$IEP == "1000001000000","Learning",
                                                                       ifelse(x$IEP == "1000000100000","Giftedness",
                                                                              ifelse(x$IEP == "1000000010000","MildIntellectual",
                                                                                     ifelse(x$IEP == "1000000001000","Developmental",
                                                                                            ifelse(x$IEP == "1000000000100","Physical",
                                                                                                   ifelse(x$IEP == "1000000000010","Blind",
                                                                                                          ifelse(x$IEP == "1000000000001","Multiple","BadCode")
                                                                                                   )
                                                                                            )
                                                                                     )
                                                                              )
                                                                       )
                                                                )
                                                         )
                                                  )
                                           )
                                    )
                             )
                      )
      )
      x$Gender <- ifelse(x$Gender == 1, "Male",
                         ifelse(x$Gender == 2, "Female", NA))

      x$Program <- ifelse(x$Program == 2, "Academic",
                                    ifelse(x$EligibilityStatus == 1, "Applied", NA))

      x$MathClassWhen <- ifelse(x$MathClassWhen == 1, "Semester 1",
                                       ifelse(x$MathClassWhen == 2, "Semester 2",
                                              ifelse(x$MathClassWhen == 3, "Full Year",NA)))
      x$ESLELD_ALFPDF <- ifelse(x$ESLELD_ALFPDF == 1, "ELL",
                                ifelse(x$ESLELD_ALFPDF == 0, "non-ELL", NA))
      return(x)
      },
    ifelse(grade %in% c(3,6), {
      x <- readr::read_csv(paste0(datadir.txt,year-1, year-2000,"_B",bident,"_1G",grade,".csv"))
      x$IEP <- paste0(x$SIF_IEP,
                      x$SIF_IPRC_Behaviour,
                      x$SIF_IPRC_Autism,
                      x$SIF_IPRC_Deaf,
                      x$SIF_IPRC_Language,
                      x$SIF_IPRC_Speech,
                      x$SIF_IPRC_Learning,
                      x$SIF_IPRC_Giftedness,
                      x$SIF_IPRC_MildIntellectual,
                      x$SIF_IPRC_Developmental,
                      x$SIF_IPRC_Physical,
                      x$SIF_IPRC_Blind,
                      x$SIF_IPRC_Multiple)
      x$IEP <- ifelse(x$IEP == "0000000000000", "No IEP",
                      ifelse(x$IEP == "1000000000000", "IEP no IPRC",
                             ifelse(x$IEP == "1100000000000", "Behaviour",
                                    ifelse(x$IEP == "1010000000000", "Autism",
                                           ifelse(x$IEP == "1001000000000", "Deaf",
                                                  ifelse(x$IEP == "1000100000000", "Language",
                                                         ifelse(x$IEP == "1000010000000", "Speech",
                                                                ifelse(x$IEP == "1000001000000","Learning",
                                                                       ifelse(x$IEP == "1000000100000","Giftedness",
                                                                              ifelse(x$IEP == "1000000010000","MildIntellectual",
                                                                                     ifelse(x$IEP == "1000000001000","Developmental",
                                                                                            ifelse(x$IEP == "1000000000100","Physical",
                                                                                                   ifelse(x$IEP == "1000000000010","Blind",
                                                                                                          ifelse(x$IEP == "1000000000001","Multiple","BadCode")
                                                                                                   )
                                                                                            )
                                                                                     )
                                                                              )
                                                                       )
                                                                )
                                                         )
                                                  )
                                           )
                                    )
                             )
                      )
      )
      x$Gender <- ifelse(x$Gender == 1, "Male",
                         ifelse(x$Gender == 2, "Female", NA))

      x$Background_FrenchImmersion <- ifelse(x$Background_FrenchImmersion == 1, "FI",
                                ifelse(x$Background_FrenchImmersion == 0, "not FI", NA))

      x$Background_ESLELD_ALFPDF <- ifelse(x$Background_ESLELD_ALFPDF == 1, "ELL",
                                ifelse(x$Background_ESLELD_ALFPDF == 0, "not ELL", NA))

      return(x)
      },"Unable to locate files, check the grade, bident, or file name.")))
  }


#Examples
#G3ISD <- StudentLoad(3, 2017, 66060,"v")
#G6ISD <- StudentLoad(6, 2017, 66060, "v")
#G9ISD <- StudentLoad(9, 2017, 66060, "v")
#G10ISD <- StudentLoad(10, 2017, 66060, "v")


