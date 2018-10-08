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
    OSSLT.2 <- dplyr::select(OSSLT.2, -OrgType, -Lang, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType, -Eligibility)

    OSSLT.3 <- readr::read_csv(paste0(datadir.txt,"G10_", year, "_FTE_B01",bident,"_3.csv"))
    OSSLT.3$SchoolMident[is.na(OSSLT.3$SchoolMident)] <- bident
    OSSLT.3$SchoolName[is.na(OSSLT.3$SchoolName)] <- board
    OSSLT.3 <- dplyr::select(OSSLT.3, -OrgType, -Lang, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType, -Eligibility)

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
      G9.2 <- dplyr::select(G9.2, -OrgType, -Language, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType)

      G9.3 <- readr::read_csv(paste0(datadir.txt,"G9_",year,"_B01",bident,"_3.csv"))
      G9.3$SchoolMident[is.na(G9.3$SchoolMident)] <- bident
      G9.3$SchoolName[is.na(G9.3$SchoolName)] <- board
      G9.3 <- dplyr::select(G9.3, -OrgType, -Language, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType)

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
        x.2 <- dplyr::select(x.2, -OrgType, -Language, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType)

        x.3 <- readr::read_csv(paste0(datadir.txt,"G",grade,"_",year,"_B01",bident,"_3.csv"))
        x.3$SchoolMident[is.na(x.3$SchoolMident)] <- bident
        x.3$SchoolName[is.na(x.3$SchoolName)] <- board
        x.3 <- dplyr::select(x.3, -OrgType, -Language, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType)

        x.4 <- readr::read_csv(paste0(datadir.txt,"G",grade,"_",year,"_B01",bident,"_4.csv"))
        x.4$SchoolMident[is.na(x.4$SchoolMident)] <- bident
        x.4$SchoolName[is.na(x.4$SchoolName)] <- board
        x.4 <- dplyr::select(x.4, -OrgType, -Language, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType)

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

    x$IEPcode <- ifelse(x$IPRCExBehaviour == 1, "Behaviour",
                        ifelse(x$IPRCExAutism == 1, "Autism",
                               ifelse(x$IPRCExDeaf == 1, "Deaf",
                                      ifelse(x$IPRCExLanguage == 1, "Language",
                                             ifelse(x$IPRCExSpeech == 1, "Speech",
                                                    ifelse(x$IPRCExLearning == 1, "Learning",
                                                           ifelse(x$IPRCExGiftedness == 1, "Gifted",
                                                                  ifelse(x$IPRCExMildIntellectual == 1, "Mild Intellectual",
                                                                         ifelse(x$IPRCExDevelopmental == 1, "Developmental",
                                                                                ifelse(x$IPRCExPhysical == 1, "Physical",
                                                                                       ifelse(x$IPRCExBlind == 1, "Blind",
                                                                                              ifelse(x$IPRCExMultiple == 1, "Multiple", "No IEP")
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

      x$IEPcode <- ifelse(x$SIF_IPRCBehaviour == 1, "Behaviour",
                          ifelse(x$SIF_IPRCAutism == 1, "Autism",
                                 ifelse(x$SIF_IPRCDeaf == 1, "Deaf",
                                        ifelse(x$SIF_IPRCLanguage == 1, "Language",
                                               ifelse(x$SIF_IPRCSpeech == 1, "Speech",
                                                      ifelse(x$SIF_IPRCLearning == 1, "Learning",
                                                             ifelse(x$SIF_IPRCGifted == 1, "Gifted",
                                                                    ifelse(x$SIF_IPRCIntellectual == 1, "Mild Intellectual",
                                                                           ifelse(x$SIF_IPRCDevelopmental == 1, "Developmental",
                                                                                  ifelse(x$SIF_IPRCPhysical == 1, "Physical",
                                                                                         ifelse(x$SIF_IPRCBlind == 1, "Blind",
                                                                                                ifelse(x$SIF_IPRCMultiple == 1, "Multiple", "No IEP")
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
                                    ifelse(x$Program == 1, "Applied", NA))

      x$MathClassWhen <- ifelse(x$MathClassWhen == 1, "Semester 1",
                                       ifelse(x$MathClassWhen == 2, "Semester 2",
                                              ifelse(x$MathClassWhen == 3, "Full Year",NA)))
      x$ESLELD_ALFPDF <- ifelse(x$ESLELD_ALFPDF == 1, "ELL",
                                ifelse(x$ESLELD_ALFPDF == 0, "non-ELL", NA))
      return(x)
      },
    ifelse(grade %in% c(3,6), {
      x <- readr::read_csv(paste0(datadir.txt,year-1, year-2000,"_B",bident,"_1G",grade,".csv"))

      x$IEPcode <- ifelse(x$SIF_IPRC_Behaviour == 1, "Behaviour",
                          ifelse(x$SIF_IPRC_Autism == 1, "Autism",
                                 ifelse(x$SIF_IPRC_Deaf == 1, "Deaf",
                                        ifelse(x$SIF_IPRC_Language == 1, "Language",
                                               ifelse(x$SIF_IPRC_Speech == 1, "Speech",
                                                      ifelse(x$SIF_IPRC_Learning == 1, "Learning",
                                                             ifelse(x$SIF_IPRC_Giftedness == 1, "Gifted",
                                                                    ifelse(x$SIF_IPRC_MildIntellectual == 1, "Mild Intellectual",
                                                                           ifelse(x$SIF_IPRC_Developmental == 1, "Developmental",
                                                                                  ifelse(x$SIF_IPRC_Physical == 1, "Physical",
                                                                                         ifelse(x$SIF_IPRC_Blind == 1, "Blind",
                                                                                                ifelse(x$SIF_IPRC_Multiple == 1, "Multiple", "No IEP")
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



#'AchieveLabel(df, grade, type) (char/num) - Apply Labels to Achievement Levels EQAO 3 6 9 and choose type of label
#'
#'Use:   AchLabel(df, grade, type)
#'
#'grade: 3 (recodes ROverallLevel, WOVerallLevel and MOverallLevel)
#'grade: 6 (recodes R...W...MOverallLevel, Prior_G3_R...Prior_G3_W...Prior_G3_MOverallLevel)
#'grade: 9 (recodes OverallOutcomeLevel, Prior_G6_R...W...M, Prior_G3_R...W...MOverallLevel)
#'grade: 10 (recodes OSSLTOutcome, Prior G6_R...W, Prior_G3_R...WOverallLevel)
#'
#'type: char = character labels (Level 1, Level 2, Witheld etc.)
#'type: num = numeric (NE1 = 0, all non-levels are NA - like fully participating)
#'
#' - pass a variable and recode with labels for achievement levels

AchieveLabel <- function(x, grade, type){
  ifelse(grade ==3, {
    ifelse(type == "char", {
      x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(ROverallLevel,WOverallLevel, MOverallLevel),
                            .funs= dplyr::funs(dplyr::recode(.,`1` = "Level 1",
                                                             `2` = "Level 2",
                                                             `3` = "Level 3",
                                                             `4` = "Level 4",
                                                             `0` = "NE1",
                                                             `W` = "Witheld",
                                                             `R` = "Witheld",
                                                             `P` = "Pending",
                                                             `X` = "Exempt",
                                                             `Q` = "Not Required",
                                                             `-1` = "No Data",
                                                             `B` = "No Data")
                                               )
                            )
      },
      ifelse(type == "num", {
        x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(ROverallLevel,WOverallLevel, MOverallLevel),
                              .funs= dplyr::funs(dplyr::recode(.,`1` = 1,
                                                               `2` = 2,
                                                               `3` = 3,
                                                               `4` = 4,
                                                               `0` = 0,
                                                               `W` = as.numeric(NA),
                                                               `R` = as.numeric(NA),
                                                               `P` = as.numeric(NA),
                                                               `X` = as.numeric(NA),
                                                               `Q` = as.numeric(NA),
                                                               `-1` = as.numeric(NA),
                                                               `B` = as.numeric(NA))
                                                 )
                              )
        }, "Check the type of recoding selected")
      )
    },
    ifelse(grade == 6, {
      ifelse(type == "char", {
        x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(ROverallLevel,WOverallLevel, MOverallLevel, Prior_G3_ROverallLevel, Prior_G3_WOverallLevel, Prior_G3_MOverallLevel),
                              .funs= dplyr::funs(dplyr::recode(.,`1` = "Level 1",
                                                               `2` = "Level 2",
                                                               `3` = "Level 3",
                                                               `4` = "Level 4",
                                                               `0` = "NE1",
                                                               `W` = "Witheld",
                                                               `R` = "Witheld",
                                                               `P` = "Pending",
                                                               `X` = "Exempt",
                                                               `Q` = "Not Required",
                                                               `-1` = "No Data",
                                                               `B` = "No Data")
                              )
        )
      },
      ifelse(type == "num", {
        x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(ROverallLevel,WOverallLevel, MOverallLevel, Prior_G3_ROverallLevel, Prior_G3_WOverallLevel, Prior_G3_MOverallLevel),
                              .funs= dplyr::funs(dplyr::recode(.,`1` = 1,
                                                               `2` = 2,
                                                               `3` = 3,
                                                               `4` = 4,
                                                               `0` = 0,
                                                               `W` = as.numeric(NA),
                                                               `R` = as.numeric(NA),
                                                               `P` = as.numeric(NA),
                                                               `X` = as.numeric(NA),
                                                               `Q` = as.numeric(NA),
                                                               `-1` = as.numeric(NA),
                                                               `B` = as.numeric(NA))
                              )
        )
        x$change_g63_read <- ifelse(!is.na(x$ROverallLevel) & !is.na(x$Prior_G3_ROverallLevel),
                    x$ROverallLevel - x$Prior_G3_ROverallLevel, NA)
        x$change_g63_write <- ifelse(!is.na(x$WOverallLevel) & !is.na(x$Prior_G3_WOverallLevel),
                                    x$WOverallLevel - x$Prior_G3_WOverallLevel, NA)
        x$change_g63_math <- ifelse(!is.na(x$MOverallLevel) & !is.na(x$Prior_G3_MOverallLevel),
                                    x$MOverallLevel - x$Prior_G3_MOverallLevel, NA)


        x$change_g63_read_label <- ifelse(!is.na(x$change_g63_read),
                                          ifelse(x$change_g63_read > 0 , "increase",
                                                 ifelse(x$change_g63_read <0, "decrease", "same")
                                                 ), NA
        )
        x$change_g63_write_label <- ifelse(!is.na(x$change_g63_write),
                                          ifelse(x$change_g63_write > 0 , "increase",
                                                 ifelse(x$change_g63_write <0, "decrease", "same")
                                          ), NA
        )
        x$change_g63_math_label <- ifelse(!is.na(x$change_g63_math),
                                          ifelse(x$change_g63_math > 0 , "increase",
                                                 ifelse(x$change_g63_math <0, "decrease", "same")
                                          ), NA
        )
      }, "Check the type of recoding selected")
      )
    },
    ifelse(grade == 9 , {
      ifelse(type == "char", {
        x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(OverallOutcomeLevel, Prior_G3_ROverallLevel, Prior_G3_WOverallLevel, Prior_G3_MOverallLevel, Prior_G6_ROverallLevel, Prior_G6_WOverallLevel, Prior_G6_MOverallLevel),
                              .funs= dplyr::funs(dplyr::recode(.,`1` = "Level 1",
                                                               `2` = "Level 2",
                                                               `3` = "Level 3",
                                                               `4` = "Level 4",
                                                               `0` = "NE1",
                                                               `W` = "Witheld",
                                                               `R` = "Witheld",
                                                               `P` = "Pending",
                                                               `X` = "Exempt",
                                                               `V` = "Vulgar",
                                                               `Q` = "Not Required",
                                                               `-1` = "No Data",
                                                               `B` = "No Data")
                                                 )
                              )
        },
        ifelse(type == "num", {
          x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(OverallOutcomeLevel, Prior_G3_ROverallLevel, Prior_G3_WOverallLevel, Prior_G3_MOverallLevel, Prior_G6_ROverallLevel, Prior_G6_WOverallLevel, Prior_G6_MOverallLevel),
                                .funs= dplyr::funs(dplyr::recode(.,`1` = 1,
                                                                 `2` = 2,
                                                                 `3` = 3,
                                                                 `4` = 4,
                                                                 `0` = 0,
                                                                 `W` = as.numeric(NA),
                                                                 `R` = as.numeric(NA),
                                                                 `P` = as.numeric(NA),
                                                                 `X` = as.numeric(NA),
                                                                 `V` = as.numeric(NA),
                                                                 `Q` = as.numeric(NA),
                                                                 `-1` = as.numeric(NA),
                                                                 `B` = as.numeric(NA)
                                                                 )
                                                   )
                                )
          x$change_g96_math <- ifelse(!is.na(x$OverallOutcomeLevel) & !is.na(x$Prior_G6_MOverallLevel),
                                      x$OverallOutcomeLevel - x$Prior_G6_MOverallLevel, NA)
          x$change_g93_math <- ifelse(!is.na(x$OverallOutcomeLevel) & !is.na(x$Prior_G3_MOverallLevel),
                                      x$OverallOutcomeLevel - x$Prior_G3_MOverallLevel, NA)


          x$change_g96_math_label <- ifelse(!is.na(x$change_g96_math),
                                            ifelse(x$change_g96_math > 0 , "increase",
                                                   ifelse(x$change_g96_math <0, "decrease", "same")
                                            ), NA)
          x$change_g93_math_label <- ifelse(!is.na(x$change_g93_math),
                                            ifelse(x$change_g93_math > 0 , "increase",
                                                   ifelse(x$change_g93_math <0, "decrease", "same")
                                            ), NA)

          }, "Check the type of recoding selected"))},
      ifelse(grade == 10, {
        ifelse(type == "char", {
          x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(Prior_G6_ROverallLevel, Prior_G6_WOverallLevel, Prior_G3_ROverallLevel, Prior_G3_WOverallLevel),
                                .funs= dplyr::funs(dplyr::recode(.,`1` = "Level 1",
                                                                 `2` = "Level 2",
                                                                 `3` = "Level 3",
                                                                 `4` = "Level 4",
                                                                 `0` = "NE1",
                                                                 `W` = "Witheld",
                                                                 `R` = "Witheld",
                                                                 `P` = "Pending",
                                                                 `X` = "Exempt",
                                                                 `Q` = "Not Required",
                                                                 `-1` = "No Data",
                                                                 `B` = "No Data")
                                                   )
                                )
          x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(OSSLTOutcome),
                              .funs= dplyr::funs(dplyr::recode(.,`0` = "Pending",
                                                               `1` = "Successful",
                                                               `2` = "Unsuccessful",
                                                               `3` = "Absent",
                                                               `4` = "OSSLC",
                                                               `5` = "Deferred",
                                                               `6` = "Exempt",
                                                               `10` = "Witheld")
                                                 )
                              )
        },
        ifelse(type == "num", {
          x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(OSSLTOutcome),
                                .funs= dplyr::funs(dplyr::recode(.,`0` = as.numeric(NA),
                                                                 `1` = 1,
                                                                 `2` = 0,
                                                                 `3` = as.numeric(NA),
                                                                 `4` = 1,
                                                                 `5` = as.numeric(NA),
                                                                 `6` = as.numeric(NA),
                                                                 `10` = as.numeric(NA))
                                                   )
                                )
          }, "Check the type of recoding selected")
        )},"Check the grade selected")
      )
    )
    )
  return(x)
  }



###Provincial Data Load
#'df <- ProvLoad(3, YYYY, datadir)
#'
#'Use:   df <- SchoolLoad(3, 2017, datadir)
#'grades = 3, 6, 9, 10
#'year = YYYY (school year in June)
#'
#' - identify the grade, year: used to construct EQAO file names
#' - for the DDSB use v and c to access network and c data directories
#'   otherwise pass a character string or vector with the directory containing the EQAO files
#

ProvLoad <- function(grade, year, datadir) {
  datadir.txt <- ifelse(datadir == "v", "V:/Programs/Accountability & Assessment/AA_MasterData/",
                        ifelse(datadir == "c", "C:/Data-Local/", datadir))

  ifelse(grade == 10, {
    OSSLT.1 <- readr::read_csv(paste0(datadir.txt,"G10_",year,"_FTE_P01_1.csv"))
    OSSLT.1$SchoolMident[is.na(OSSLT.1$SchoolMident)] <- 1
    OSSLT.1$SchoolName[is.na(OSSLT.1$SchoolName)] <- "Prov"

    OSSLT.2 <- readr::read_csv(paste0(datadir.txt,"G10_", year,"_FTE_P01_2.csv"))
    OSSLT.2$SchoolMident[is.na(OSSLT.2$SchoolMident)] <- 1
    OSSLT.2$SchoolName[is.na(OSSLT.2$SchoolName)] <- "Prov"
    OSSLT.2 <- dplyr::select(OSSLT.2, -OrgType, -Lang, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType, -Eligibility)

    OSSLT.3 <- readr::read_csv(paste0(datadir.txt,"G10_", year, "_FTE_P01_3.csv"))
    OSSLT.3$SchoolMident[is.na(OSSLT.3$SchoolMident)] <- 1
    OSSLT.3$SchoolName[is.na(OSSLT.3$SchoolName)] <- "Prov"
    OSSLT.3 <- dplyr::select(OSSLT.3, -OrgType, -Lang, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType, -Eligibility)

    x <- merge(OSSLT.1, OSSLT.2, by = "SchoolMident")
    x <- merge(x, OSSLT.3, by = "SchoolMident")

    x$year <- year

    return(x)
  },
  ifelse(grade %in% 9, {
    G9.1 <- readr::read_csv(paste0(datadir.txt,"G9_",year,"_P01_1.csv"))
    G9.1$SchoolMident[is.na(G9.1$SchoolMident)] <- 1
    G9.1$SchoolName[is.na(G9.1$SchoolName)] <- "Prov"

    G9.2 <- readr::read_csv(paste0(datadir.txt,"G9_",year,"_P01_2.csv"))
    G9.2$SchoolMident[is.na(G9.2$SchoolMident)] <- 1
    G9.2$SchoolName[is.na(G9.2$SchoolName)] <- "Prov"
    G9.2 <- dplyr::select(G9.2, -OrgType, -Language, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType)

    G9.3 <- readr::read_csv(paste0(datadir.txt,"G9_",year,"_P01_3.csv"))
    G9.3$SchoolMident[is.na(G9.3$SchoolMident)] <- 1
    G9.3$SchoolName[is.na(G9.3$SchoolName)] <- "Prov"
    G9.3 <- dplyr::select(G9.3, -OrgType, -Language, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType)

    x <- merge(G9.1, G9.2, by = "SchoolMident")
    x <- merge(x, G9.3, by = "SchoolMident")

    x$year <- year

    return(x)
  },
  ifelse(grade %in% c(3,6), {
    x.1 <- readr::read_csv(paste0(datadir.txt,"G",grade,"_",year,"_P01_1.csv"))
    x.1$SchoolMident[is.na(x.1$SchoolMident)] <- 1
    x.1$SchoolName[is.na(x.1$SchoolName)] <- "Prov"

    x.2 <- readr::read_csv(paste0(datadir.txt,"G",grade,"_",year,"_P01_2.csv"))
    x.2$SchoolMident[is.na(x.2$SchoolMident)] <- 1
    x.2$SchoolName[is.na(x.2$SchoolName)] <- "Prov"
    x.2 <- dplyr::select(x.2, -OrgType, -Language, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType)

    x.3 <- readr::read_csv(paste0(datadir.txt,"G",grade,"_",year,"_P01_3.csv"))
    x.3$SchoolMident[is.na(x.3$SchoolMident)] <- 1
    x.3$SchoolName[is.na(x.3$SchoolName)] <- "Prov"
    x.3 <- dplyr::select(x.3, -OrgType, -Language, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType)

    x.4 <- readr::read_csv(paste0(datadir.txt,"G",grade,"_",year,"_P01_4.csv"))
    x.4$SchoolMident[is.na(x.4$SchoolMident)] <- 1
    x.4$SchoolName[is.na(x.4$SchoolName)] <- "Prov"
    x.4 <- dplyr::select(x.4, -OrgType, -Language, -ApplySuppression, -BoardMident, -BoardName, -SchoolName, -Suppressed, -FundingType)

    x <- merge(x.1, x.2, by = "SchoolMident")
    x <- merge(x, x.3, by = "SchoolMident")
    x <- merge(x, x.4, by = "SchoolMident")

    x$year <- year

    return(x)
  }, "Unable to locate files, check the grade, or file name.")))

}


