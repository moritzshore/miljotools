#' Soil Texture ClassifieR
#'
#' Reads in soil clay, silt, sand fractions and returns soil
#' classification based on USDA or Norwegian definitions.
#'
#' - Automatically Compatible with csv or xlsx.
#'
#' - Tries to automatically guess format of data, and acts accordingly.
#'
#' - Automatically rounds total fraction to 100%, if differences are under 1%.
#'
#' - Exports classification based on user settings
#'
#' - Creates an interactive diagnostic plot, as well as saving one to output directory.
#'
#' Note: the script will automatically try to identify which columns contain
#' the clay, silt, sand fractions and also try to identify which format
#' they are in. If this does not work, you might get incorrect or flawed
#' results, so make sure to READ what THE CONSOLE prints out, to make sure
#' it did the correct things.
#'
#' If the script cannot read your file automatically, then the following
#' format is recommended:
#'
#' Clay column: "clay" in percent format (%), decimal = "."
#' Silt column: "silt" in percent format (%), decimal = "."
#' Sand column: "sand" in percent format (%), decimal = "."
#'
#' @author Moritz Shore, Nov.2 2022
#' @param input path to input file (supported: *.xlsx/*.csv)
#' @param output file name of output (select between csv or excel) (ie: *.xlsx/*.csv)
#' @param wd path to desired working directory. Default is current.
#' @param version "NOR" for Norwegian, "USDA" for USDA.
#' @param plot_result TRUE/FALSE display diagnostic plot?
#' @param interactive if plot is true, do you want an interactive or static plot?
#' @param sheet If using excel, specify which sheet the data is in. 1 by default
#' @param decimal Specify decimal separator ("," or "."). By default ","
#' @param append append results to existing dataframe? (TRUE) or return single column (FALSE). default TRUE.
#' @param new_sums append new "rounded sums" to input dataframe? Default FALSE
#' @param write optional flag to stop the function from writing any files. Default FALSE
#'
#' @examples
#' # for demonstration purposes, use path of package
#'  example_file_path <- system.file(package = "miljotools", "/extdata/soil_classify/example.csv")
#'
#' classify_soil(
#'   input = example_file_path,
#'   output = "output_file.csv",
#'   version = "NOR",
#'   plot_result = TRUE,
#'   interactive = FALSE,
#'   decimal = ".",
#'   write = FALSE # FALSE just for demonstration
#' )
#'
#' @return Returns path to written file
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom grDevices colorRampPalette
#' @importFrom plotly plot_ly layout
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggtern ggtern
#' @importFrom ggplot2 aes
#' @importFrom stringr str_split
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom utils write.csv read.table
#'
classify_soil <-
  function(input,
           output,
           wd = NULL,
           version = "NOR",
           plot_result = TRUE,
           interactive = TRUE,
           sheet = 1,
           decimal = ",",
           append = TRUE,
           new_sums = FALSE,
           write = FALSE){

    # REQUIRED to fill these out!
    PATH = input
    if(wd %>% is.null()){
      OUTPATH = getwd()
    }else{
      OUTPATH = wd
    }

    if(output %>% is.null()){
      OUTPUT_FILE = paste0("classfied_soil_", Sys.getpid(), ".csv")
    }else{
      OUTPUT_FILE = output
    }

    VERSION = version
    SHEET = sheet
    DECIMAL_SEPERATOR = decimal
    APPEND = append
    NEW_SUMS = new_sums

    ### Supporting Functions ----
    # Classify soils based of the USDA texture triangle
    # Following:
    # Soil Science Division Staff. 2017. Soil survey manual. C. Ditzler, K. Scheffe,
    # and H.C. Monger (eds.). USDA Handbook 18. Government Printing Office,
    # Washington, D.C. (Chapter 3, pages 122-124)

    # This way of doing it is slow. maybe use matricies (n x 3)
    classify_texture_USDA <- function(dataframe) {
      # Sub functions ----

      # Sand: Material has more than 85 percent sand, and the percentage of silt
      # plus 1.5 times the percentage of clay is less than 15. (USDA, 2017).
      isSand <- function(sand, silt, clay) {
        return(sand > 85 & (silt + 1.5 * clay) < 15)
      }
      # Loamy sand: Material has between 70 and 90 percent sand, the percentage of
      # silt plus 1.5 times the percentage of clay is 15 or more, and the percentage
      # of silt plus twice the percentage of clay is less than 30. (USDA, 2017).
      isLoamySand <- function(sand, silt, clay) {
        return(sand >= 70 & # should it include 70 or not? I am not sure
                 sand <= 90 & # should it include 90 or not? I am not sure
                 (silt + 1.5 * clay) >= 15 & (silt + 2 * clay) < 30)
      }
      # Sandy loams: Material has 7 to less than 20 percent clay and more than
      # 52 percent sand, and the percentage of silt plus twice the percentage of
      # clay is 30 or more; OR material has less than 7 percent clay and less than
      # 50 percent silt, and the percentage of silt plus twice the percentage of
      # clay is 30 or more. (USDA, 2017).
      isSandyLoam <- function(sand, silt, clay) {
        return((clay >= 7 &
                  clay < 20 & sand > 52 & (silt + 2 * clay) >= 30) | # OR
                 (clay < 7 & silt < 50 & (silt + 2 * clay) >= 30))
      }
      # Loam: Material has 7 to less than 27 percent clay, 28 to less than 50
      # percent silt, and 52 percent or less sand. (USDA, 2017).
      isLoam <- function(sand, silt, clay) {
        return(clay >= 7 & clay < 27 & silt >= 28 &
                 silt < 50 & sand <= 52)
      }
      # Clay: Material has 40 percent or more clay, 45 percent or less sand, and
      # less than 40 percent silt. (USDA, 2017).
      isClay <- function(sand, silt, clay) {
        return(clay >= 40 & sand <= 45 & silt < 40)
      }
      # Silty clay: Material has 40 percent or more clay and 40 percent or more silt
      # . (USDA, 2017).
      isSiltyClay <- function(sand, silt, clay) {
        return(clay >= 40 & silt >= 40)
      }
      # Sandy clay: Material has 35 percent or more clay and more than 45 percent
      # sand. (USDA, 2017).
      isSandyClay <- function(sand, silt, clay) {
        return(clay >= 35 & sand > 45)
      }
      # Clay loam: Material has 27 to less than 40 percent clay and more than 20
      # to 45 percent sand. (USDA, 2017).
      isClayLoam <- function(sand, silt, clay) {
        return(clay < 40 & clay >= 27 & sand > 20 & sand <= 45)
      }
      # Sandy clay loam: Material has 20 to less than 35 percent clay, less than 28
      # percent silt, and more than 45 percent sand. (USDA, 2017).
      isSandyClayLoam <- function(sand, silt, clay) {
        return(clay < 35 & clay >= 20 & sand > 45 & silt < 28)
      }
      # Silty clay loam: Material has 27 to less than 40 percent clay and 20 percent
      # or less sand. (USDA, 2017).
      isSiltyClayLoam <- function(sand, silt, clay) {
        return(clay < 40 & clay >= 27 & sand <= 20)
      }
      # Silt: Material has 80 percent or more silt and less than 12 percent clay.
      # (USDA, 2017).
      isSilt <- function(sand, silt, clay) {
        return(clay < 12 & silt >= 80)
      }
      # Silt loam. Material has 50 percent or more silt and 12 to less than 27
      # percent clay; OR material has 50 to less than 80 percent silt and less than
      # 12 percent clay. (USDA, 2017)
      isSiltLoam <- function(sand, silt, clay) {
        return((silt >= 50 & clay >= 12 & clay < 27) | # OR
                 (silt >= 50 & silt < 80 & clay < 12))
      }

      #  Vector Classification with just one data entry
      classifyUSDA <- function(sand, silt, clay) {

        # if any of the fractuins are NA, return NA
        if (sum(is.na(c(sand, silt, clay))) != 0) return(NA)

        # skip if sum is too far off 100
        if (sand + silt + clay > 101 | sand + silt + clay < 99) return(NA)

        # find matching classification
        vector_evaluation = c(
          isSand         (sand, silt, clay),
          isLoamySand    (sand, silt, clay),
          isSandyLoam    (sand, silt, clay),
          isLoam         (sand, silt, clay),
          isSiltLoam     (sand, silt, clay),
          isSilt         (sand, silt, clay),
          isSandyClayLoam(sand, silt, clay),
          isClayLoam     (sand, silt, clay),
          isSiltyClayLoam(sand, silt, clay),
          isSandyClay    (sand, silt, clay),
          isSiltyClay    (sand, silt, clay),
          isClay         (sand, silt, clay)
        )

        # vector to match the functions to the names
        vector_names = c(
          "sand",
          "loamy_sand",
          "sandy_loam",
          "loam",
          "silt_loam",
          "silt",
          "sandy_clay_loam",
          "clay_loam",
          "silty_clay_loam",
          "sandy_clay",
          "silty_clay",
          "clay"
        )

        # if no functions return true
        if (sum(vector_evaluation) == 0) {
          stop("Not classifiable!",
               paste0(" sand=", sand,
                      " silt=", silt,
                      " clay=", clay))
        }

        # if more than one function is true
        if(sum(vector_evaluation) > 1) {
          stop("More than one classification!",
               paste0(" sand=", sand,
                      " silt=", silt,
                      " clay=", clay), "\n", vector_names[which(vector_evaluation)])
        }

        # return the correct classification if only 1
        return(vector_names[which(vector_evaluation)])
      }

      # this is an ugly wrapper to get this style of function to work
      # I hope to improve it someday soon
      # it basically lets you pass a dataframe, instead of individual triplets
      # extract the needed columns from the dataframe
      apply(
        X = dataframe[, c('sand', 'silt', 'clay')],

        # a vector giving the subscripts which the function will be applied over
        # E.g., for a matrix 1 indicates rows, 2 indicates columns,
        # c(1, 2) indicates rows and columns. Where X has named dimnames,
        # it can be a character vector selecting dimension names.
        MARGIN = 1,

        # A wrapped function for the classify function
        FUN = function(xyz)
          classifyUSDA(xyz['sand'], xyz['silt'], xyz['clay'])
      )
    }

    # classifies the norwegian texture classes
    # following Sveistrup and Njoes, 1984

    classify_Texture_NOR <- function(dataframe) {
      # subfunctions ----

      # Sand inneholder 85% eller mer sand og mindre enn 10% leir
      is_Sand_NOR <-
        function(sand, silt, clay) {
          return(
            sand >= 85 &
              clay <=  10
          )
        }

      # Siltig sand inneholder mindre enn 10% leir, mer enn 40 og opp til 85% sand
      # og mindre enn 50% silt.
      # !! "opp til 85% sand? <=? <?"
      is_SiltigSand_NOR <-
        function(sand, silt, clay) {
          return(
            clay <  10 &
              silt <  50 &
              sand >  40 &
              sand <= 85
          )
        }

      # Sandig silt inneholder fra 50 til 80%  silt, mer enn 8 og opp til 50% sand
      # og minde en 12% leir
      is_sandigSilt_NOR <-
        function(sand, silt, clay) {
          return(
            silt >= 50 &
              silt <= 80 &
              sand >  8  &
              sand <=  50 &
              clay <  12
          )
        }

      # Silt inneholder 80% eller mer silt og mindre enn 12% leir.
      is_Silt_NOR <-
        function(sand, silt, clay) {
          return(
            silt >= 80 &
              clay <  12
          )
        }

      # Sandig lettleire inneholder fra 10 til 25% leir, mindre enn 25% silt og mer
      # enn 50 til og med 90% sand.
      is_sandigLettleire_NOR <-
        function(sand, silt, clay) {
          return(
            clay >=  10 &
              clay <  25 &
              silt <=  25 &
              sand >  50 &
              sand <= 90
          )
        }

      # lettleire inneholder fra 10 til 25% leir og fra 25 til 50% silt.
      is_lettLeire_NOR <-
        function(sand, silt, clay) {
          return(
            clay >=  10 &
              clay <=  25 &
              silt >  25 &
              silt <=  50
          )
        }

      # Siltig lettleire inneholder fra 12 til 25% leir og fra 50 til og med 88% silt.
      is_siltigLettleire_NOR <-
        function(sand, silt, clay) {
          return(
            clay >= 12 &
              clay <=  25 &
              silt >  50 &
              silt <= 88
          )
        }

      # Sandig mellom/eire inneholder fra 25 til 40% leir, mindre enn 25% silt og
      # mer enn 35 til og med 75% sand.
      is_sandig_Mellomleire_NOR <-
        function(sand, silt, clay) {
          return(
            clay >=  25 &
              clay <  40 &
              silt <=  25 &
              sand >  35 &
              sand <= 75
          )
        }

      # Mellomleire inneholder fra 25 til 40% leir og fra 25 til 50% silt.
      is_mellomLeire_NOR <-
        function(sand, silt, clay) {
          return(
            clay >  25 &
              clay <  40 &
              silt >  25 &
              silt <  50
          )
        }

      # Siltig mellomLeire inneholder fra 25 til og med 50% leir og fra 50 til og
      # med 75%  silt.
      is_siltigMellomleire_NOR <-
        function(sand, silt, clay) {
          return(
            clay >=  25 &
              clay <= 50 &
              silt >= 50 &
              silt <= 75
          )
        }

      # Stiv leire inneholder fra 40---60% leir og inntil 50% silt.
      is_stivLeire_NOR <-
        function(sand, silt, clay) {
          return(
            clay >= 40 &
              clay < 60 &
              silt <  50
          )
        }

      # Svaert stiv leire, 60% eller mer leir.
      is_svaertStivleire_NOR <-
        function(sand, silt, clay) {
          return(
            clay >= 60
          )
        }

      # classify ----

      # identifies the right texture
      classifyNOR <-function(sand, silt, clay){
        # if the sum is too high or low, skip, return NA
        if (sand+silt+clay > 101 | sand+silt+clay < 99){return(NA)}

        classification_vector <- c(
          is_Sand_NOR              (sand, silt, clay),
          is_SiltigSand_NOR        (sand, silt, clay),
          is_sandigSilt_NOR        (sand, silt, clay),
          is_Silt_NOR              (sand, silt, clay),
          is_sandigLettleire_NOR   (sand, silt, clay),
          is_lettLeire_NOR         (sand, silt, clay),
          is_siltigLettleire_NOR   (sand, silt, clay),
          is_sandig_Mellomleire_NOR(sand, silt, clay),
          is_mellomLeire_NOR       (sand, silt, clay),
          is_siltigMellomleire_NOR (sand, silt, clay),
          is_stivLeire_NOR         (sand, silt, clay),
          is_svaertStivleire_NOR   (sand, silt, clay)
        )

        name_list =   c(
          "sand",
          "siltig_sand",
          "sandig_silt",
          "silt",
          "sandig_lettleire",
          "lettleire",
          "siltig_lettleire",
          "sandig_mellomleire",
          "mellom_leire",
          "siltig_mellomleire",
          "stiv_leire",
          "svaert_stiv_leire"
        )

        # if no functions return true
        if (sum(classification_vector) == 0) {
          stop("Not classifiable!",
               paste0(" sand=", sand,
                      " silt=", silt,
                      " clay=", clay))
        }

        # # if more than one function is true
        # if(sum(classification_vector) > 1) {
        #   print("doubleclass")
        #   # stop("More than one classification!",
        #   #      paste0(" sand=", sand,
        #   #             " silt=", silt,
        #   #             " clay=", clay), "\n", name_list[which(classification_vector)])
        # }


        # bandaid fix for more than one classification: take the first one.
        return(name_list[which(classification_vector) %>% min()])
      }

      # executed function code:
      # this is a wrapper to get it to work. ugly, should be improved sometime
      apply(X = dataframe[, c('sand', 'silt', 'clay')],

            # a vector giving the subscripts which the function will be applied over
            # E.g., for a matrix 1 indicates rows, 2 indicates columns,
            # c(1, 2) indicates rows and columns. Where X has named dimnames,
            # it can be a character vector selecting dimension names.
            MARGIN = 1,

            # A wrapped function for the classify function
            FUN = function(xyz)
              classifyNOR(sand = xyz['sand'], silt = xyz['silt'], clay = xyz['clay'])
      ) %>% return()
    }


    # custom function to detect the file extension
    detect_file_ext <- function(PATH) {
      # detect file type:
        # split based on "/"
        # return the last element of the path (i.e. the file name)
        part1 <- stringr::str_split(PATH, pattern = "/", simplify = T)[length(stringr::str_split(PATH, pattern = "/", simplify = T))]
        # split the file name based on "."
        part2 <- stringr::str_split(part1, pattern = "\\.", simplify = T)
        # return the second part of the string
        part3 <- as.vector(x = part2, mode = "any")

        return_value <- part3[2]

      # stop the function of no file extension could be found
      if (is.na(return_value)) {
        stop("\nno file extension found")
      }

      # return the file extension as a string
      return(return_value)
    }

    # Read input function
    custom_read <- function(PATH,
                            DECIMAL_SEPERATOR = ",",
                            SHEET = 1) {
      # read input file
      file_ext = detect_file_ext(PATH)

      # if an excel file:
      if (file_ext == "xlsx") {
        input.df <- readxl::read_excel(PATH, sheet = SHEET)

        # if a CSV file:
      } else if (file_ext == "csv") {
        # try first with separator ","
        input.df = try(read.table(
          PATH,
          sep = ",",
          dec = DECIMAL_SEPERATOR,
          check.names = F,
          header = T
        ),
        silent = T)

        # if that failed, try doing it with the other separator
        if (is.data.frame(input.df) == FALSE) {
          input.df = try(read.table(
            PATH,
            sep = ";",
            dec = DECIMAL_SEPERATOR,
            check.names = F,
            header = T
          ),
          silent = T)
        }

        # if that failed as well, then throw an error
        if (is.data.frame(input.df) == FALSE) {
          stop("\ncannot read this csv! maybe check the decimal seperator?")
        }

        # if neither of those are the file type, stop the script.
      } else
        (stop("INVALID FILE TYPE. Suppported file types are .csv, or .xlsx"))

      # check if everything worked. if it dint, STOP
      if (is.data.frame(input.df) == FALSE) {
        stop("error reading dataframe. please check file!")
      }
      # otherwise, return the dataframe.
      else
        (return(input.df))
    }

    # finds out which column of the dataframe belongs to the passed fraction type
    # then checks if there are any out of range values
    # then guesses if the values are in percent or fraction form
    # then returns the desired vector in PERCENT FORM
    # type = a STRING of either "CLAY, "SAND", or "SILT"
    findFraction <- function(type, dataframe) {
      cat("Identifying column: ", type, "\n")

      # checking if at least one matching column was found, if not STOP


        part1 <- dataframe %>%
        colnames() %>% # grab column names
        toupper()  # make them uppercase

        part2 <- grepl(x = part1, pattern = toupper(type)) # find a matching column

        matched_column_index =  which(part2 == TRUE) %>% min() # grab the first one

      # if the name cannot be matched to a column, throw and error
      if (length(matched_column_index) == 0 |
          is.infinite(matched_column_index)) {
        stop(
          "Column could not be identified: \n searching:\n",
          dataframe %>% colnames(),
          "\n for ",
          type,
          call. = F
        )
      }

      # print to console  which column was identified
      cat(type," column identified as '",colnames(dataframe)[matched_column_index],"' index=",matched_column_index, "\n")

      # returns the matching vector
      # as.numeric converts every non numeric thing into NA, which is.na() can detect
      return.v <-  dataframe[[matched_column_index]] %>% as.numeric()


      # all non numeric entries are replace with NA
      if (length(which(is.na(return.v))) > 0) {
        warning(length(which(is.na(return.v))), " NA values\n")
      }

      # checking to see if values are in fraction
      if (mean(return.v, na.rm = T) < 1) {
        cat("> mean value of ",type," column is ", round(mean(return.v, na.rm = T), 1)," \nAssuming values are in FRACTION form. ie. 0.431 (instead of 43.1%)\n")
        # and adjusting the returning vector
        return.v = return.v * 100
      }

      # checking to see if values are in percent
      if (mean(return.v, na.rm=T) > 1) {
        cat("> mean value of ",type," column is ", round(mean(return.v, na.rm = T), 1)," \nAssuming values are in PERCENT form. ie. 43.1% (instead of 0.431)\n")
        return.v = return.v
      }
      return(return.v)
    }

    # if any of the fractions dont add up to 100%, this function will fix that
    # by  adjusting the sand fraction.
    # the threshold for maximum auto adjust is set in line [+5 this one], and is
    # currently set to 1%
    roundTo100 <- function(sand, silt, clay) {
      # check if any overlap/underlap is too high to manually adjust
      too_high = which(abs(round(100 - (silt + sand + clay), 1)) > 1) # ">1" is the error margin

      # throw an error if the margin is surpassed
      if (length(too_high) > 0) {
        warning(
          length(too_high),
          " fractions are greater than 100% by over 1%. \n  Gap too large to automatically adjust, will be skipped. \n  First Index: ",
          min(too_high)
        )
      }

      cat("\n\n Checking for fraction sums over 100 %:")

      # identify which entries have a sum too high
      too_high_index = which(round(silt + sand + clay, 1) > 100.0)

      # identify which entries have a sum too low
      too_low_index = which(round(silt + sand + clay, 1) < 100.0)

      # Print to console
      cat("\n> ",
          length(too_high_index),
          " entries have a fractional sum of over 100.")
      cat("\n> ",
          length(too_low_index),
          " entries have a fractional sum of under 100.")

      cat("\n adjusting the sand fraction and rounding...\n")

      # calculate the overshoot amount per overshoot
      overshoot_amount = (sand[too_high_index] + silt[too_high_index] +
                            clay[too_high_index]) - 100

      # calculate the undershoot amount per undershot
      undershoot_amount = 100 - (sand[too_low_index] + silt[too_low_index] +
                                   clay[too_low_index])

      # reset to 0 any change over 1
      # these are practically "Skipped"
      overshoot_amount[which(overshoot_amount > 1)] = 0
      undershoot_amount[which(undershoot_amount > 1)] = 0

      # adjust the respective indicies by that amount
      sand[too_high_index] = sand[too_high_index] - overshoot_amount
      sand[too_low_index] = sand[too_low_index] + undershoot_amount

      return(data.frame(clay, silt, sand))
    }

    # interactive diagnostic plot
    diagnostic_plot <- function(input.df) {
      # reusable function for creating annotation object
      label <- function(txt) {
        list(
          text = txt,
          x = 0.1,
          y = 1,
          ax = 0,
          ay = 0,
          xref = "paper",
          yref = "paper",
          align = "center",
          font = list(
            family = "serif",
            size = 15,
            color = "white"
          ),
          bgcolor = "#b3b3b3",
          bordercolor = "black",
          borderwidth = 2
        )
      }

      # reusable function for axis formatting
      axis <- function(txt) {
        list(
          title = txt,
          tickformat = ".0%",
          tickfont = list(size = 10)
        )
      }

      ternaryAxes <- list(
        aaxis = axis("clay"),
        baxis = axis("sand"),
        caxis = axis("silt")
      )

      # number of colors needed.. (This might break with different classification
      # systems!)
      nb.cols <- 13

      mycolors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(nb.cols)

      plot <- plotly::plot_ly(
        input.df,
        a = ~ clay,
        b = ~ sand,
        c = ~ silt,
        color = texture_class,
        colors = mycolors,
        type = "scatterternary",
        mode = "markers"
      ) %>% plotly::layout(annotations = label(paste0("texture classification:\n", VERSION)),
                           ternary = ternaryAxes)

      return(plot)
    }

    ### Main function ----
    # reading input file
    input.df <- custom_read(PATH, DECIMAL_SEPERATOR, SHEET)

    # identify and extract the salt sit and clay columns
    sand <- findFraction(type = "sand", dataframe =  input.df)
    silt <- findFraction(type = "silt", dataframe =  input.df)
    clay <- findFraction(type = "clay", dataframe =  input.df)

    # Rounding sums to 100
    rounded_sums = roundTo100(silt = silt, sand = sand, clay = clay)

    # Routine if the Norwegian system is to be used
    if (VERSION == "NOR") {
      cat("\n> classifiying NORWEGIAN textures")
      texture_class = classify_Texture_NOR(rounded_sums)
    }

    # Routine if the USDA system is to be used
    if (VERSION == "USDA") {
      cat("\n> classifiying USDA textures")
      texture_class = classify_texture_USDA(rounded_sums)
    }

    # bind both the new sums and the texture class to the dataframe (ALTERNATIVE)
    if(APPEND & NEW_SUMS) {input.df <- cbind(input.df, rounded_sums, texture_class)}

    # bind only the texture class to the dataframe (NORMAL)
    if (APPEND & !NEW_SUMS) {input.df <- cbind(input.df, texture_class)}

    # only bind the new sums to the dataframe (UNSUAL)
    if (!APPEND & NEW_SUMS) {input.df <- cbind(rounded_sums, texture_class)}

    # only returns the vector of the textures, in order (ALTERNATIVE)
    if (!APPEND & !NEW_SUMS) {input.df <- texture_class}

    if(write){
      # Write the output file in either csv or xlsx form:
      cat("\n writing output file to", paste0(OUTPATH, "/", OUTPUT_FILE), "\n")

      # xlsx form
      if(detect_file_ext(OUTPUT_FILE)=="xlsx") {
        writexl::write_xlsx(input.df,
                            path = paste0(OUTPATH, "/", OUTPUT_FILE),
                            col_names = T)
      }
      # CSV form
      if(detect_file_ext(OUTPUT_FILE)=="csv") {
        utils::write.csv(
          x = input.df,
          file = paste0(OUTPATH, "/", OUTPUT_FILE),
          quote = F,
          row.names = F
        )
      }
    }



    if(plot_result){
      plot_data <- cbind(rounded_sums, texture_class)

      if(interactive){
        # interactive diagnostic blot
        # maybe include the border, with that package.
        plotly_plot <- diagnostic_plot(plot_data)

        plotly_plot %>% print()
      }else{
        # non interactive ggplot (seems to be pretty broken now that ggtern is no longer
        # supported.. cannot use ggsave)
        outplot <- plot_data %>% ggtern::ggtern(ggplot2::aes(
          y = clay,
          z = silt,
          x = sand,
          color = texture_class
        )) + ggplot2::geom_point(shape = 3, size = 4)

        outplot %>% print()
      }
    }
    return(paste0(OUTPATH, "/", OUTPUT_FILE))
  }
