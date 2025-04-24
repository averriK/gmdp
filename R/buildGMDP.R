#' @title Build Ground-Motion Data Products
#'
#' @description
#' Reads hazard curves via `importModel.oqAEP()` and direct UHS via `importModel.oqUHS()`,
#' optionally does param expansions (`buildParamHaz/buildParamUHS`) if `param=TRUE`,
#' optionally does site amplification (`applySiteAmp/mergeAF`) if `vs30` is given.
#' Returns final tables. All internal helpers are defined below with `@noRd`.
#'
#' @param path Character. Folder with OQ hazard & UHS CSV/zip
#' @param vref Numeric. Reference Vs30
#' @param vs30 Numeric vector. If not empty & vref in \{760,3000\}, site amp
#' @param IDo Character. Label
#' @param param Logical. If TRUE => param expansions
#' @param quantile_AF For site amp factor
#' @param TRmin,TRmax Param-fitting bounds
#'
#' @return list with \code{AEPTable, UHSTable, SaTRmodel, AFmodel_AEP, AFmodel_UHS}
#'
#' @import data.table
#' @importFrom dsra fitModel.AF.TR   # (Only if you really call dsra::fitModel.AF.TR)
#' @export
buildGMDP <- function(path,
                      vref,
                      vs30         = NULL,
                      IDo          = "gmdp",
                      param        = FALSE,
                      quantile_AF  = "mean",
                      TRmin        = 100,
                      TRmax        = 10000)
{
  # 1) Import hazard
  AEP_in <- importModel.oqAEP(path=path, vref=vref)
  # 2) Import UHS (internal function below)
  UHS_in <- importModel.oqUHS(path)

  # Tag them if you want ID/AF
  AEP_in[, `:=`(ID=IDo, AF=1, Vref=vref)]
  UHS_in[, `:=`(ID=IDo, AF=1, Vref=vref)]

  AEPTable <- copy(AEP_in)
  UHSTable <- copy(UHS_in)

  SaTRmodel    <- data.table()
  AFmodel_AEP  <- data.table()
  AFmodel_UHS  <- data.table()

  # If param=TRUE => expansions
  if (param) {
    groupCols <- intersect(c("lat","lon","depth","p","Tn"), names(AEP_in))
    SaTRmodel <- AEP_in[
      ,
      fitModel.Sa.TR(.SD, TRmin=TRmin, TRmax=TRmax),
      by = groupCols
    ]
    if (nrow(SaTRmodel)) {
      # build param hazard
      newHaz <- buildParamHaz(SaTRmodel, AEP_in)
      if (nrow(newHaz)) {
        newHaz[, `:=`(ID=IDo, AF=1, Vref=vref)]
        AEPTable <- rbind(AEPTable, newHaz, fill=TRUE)
      }
      # build param UHS
      newUHS <- buildParamUHS(SaTRmodel, AEP_in)
      if (nrow(newUHS)) {
        newUHS[, `:=`(ID=IDo, AF=1, Vref=vref)]
        UHSTable <- rbind(UHSTable, newUHS, fill=TRUE)
      }
    }
  }

  # If site amp
  doAmp <- (!is.null(vs30) && length(vs30)>0 && vref %in% c(760,3000))
  if (doAmp) {
    AF_AEP <- applySiteAmp(AEPTable, vs30, vref, quantile_AF)
    if (nrow(AF_AEP)) {
      AEPTable <- mergeAF(AEPTable, AF_AEP)
      AFmodel_AEP <- AF_AEP
    }
    if (nrow(UHSTable)) {
      AF_UHS <- applySiteAmp(UHSTable, vs30, vref, quantile_AF)
      if (nrow(AF_UHS)) {
        UHSTable <- mergeAF(UHSTable, AF_UHS)
        AFmodel_UHS <- AF_UHS
      }
    }
  }

  return(list(
    AEPTable    = AEPTable,
    UHSTable    = UHSTable,
    SaTRmodel   = SaTRmodel,
    AFmodel_AEP = AFmodel_AEP,
    AFmodel_UHS = AFmodel_UHS
  ))
}

#### Internal helpers below ####

utils::globalVariables(c("uhs_label"))  # hush data.table meltdown note

#' @noRd
importModel.oqUHS <- function(path) {
  # minimal meltdown for uhs.*.csv
  tmp_dir <- file.path(path, paste0(".temp_oqUHS_", as.integer(Sys.time())))
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
  }
  dir.create(tmp_dir, showWarnings=FALSE)

  zip_files <- list.files(path, pattern="uhs-csv\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zipfile=zf, exdir=tmp_dir, junkpaths=TRUE)
    }
  }
  search_dir <- if (length(zip_files)) tmp_dir else path
  uhs_files <- list.files(search_dir, pattern="uhs.*\\.csv$", full.names=TRUE)
  if (!length(uhs_files)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
    message("No UHS CSV files in: ", path)
    return(data.table())
  }

  out_list <- list()
  iCount   <- 0
  for (f_ in uhs_files) {
    header_line <- tryCatch(readLines(f_, n=1L), error=function(e)"")
    p_val <- .extractQuantileFromHeader(header_line)
    ITo   <- .extractInvestigationTime(header_line)

    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]

    id_cols <- c("lon","lat")
    if ("depth" %in% col_names) {
      id_cols <- c(id_cols, "depth")
    }
    measure_cols <- setdiff(col_names, id_cols)
    if (!length(measure_cols)) next

    dt_long <- data.table::melt(
      dt_raw,
      id.vars=id_cols,
      measure.vars=measure_cols,
      variable.name="uhs_label",
      value.name="Sa"
    )
    dt_long[, Sa := as.numeric(Sa)]
    dt_long[, Tn := .parseUHS_TnOnly(uhs_label)]
    dt_long[, uhs_label := NULL]

    dt_long[, `:=`(p=p_val, ITo=ITo)]
    out_list[[ iCount <- iCount+1 ]] <- dt_long
  }
  unlink(tmp_dir, recursive=TRUE, force=TRUE)
  DT <- data.table::rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(DT)) return(DT)

  final_cols <- c("lon","lat","depth","Tn","Sa","p","ITo")
  keepC <- intersect(final_cols, names(DT))
  data.table::setcolorder(DT, keepC)
  return(DT[])
}

#' @noRd
buildParamHaz <- function(fitDT, AEP_in) {
  ITo <- if ("ITo" %in% names(AEP_in)) unique(AEP_in$ITo)[1] else 50
  TRseq <- seq(100,10000,25)

  fitDT[
    ,
    {
      Sa_calc <- exp(a + b*log(TRseq) + c*(1/TRseq))
      AEP_approx <- 1 / TRseq
      POE_approx <- 1 - exp(-ITo * (1/TRseq))
      data.table(
        TR  = TRseq,
        Sa  = Sa_calc,
        AEP = AEP_approx,
        POE = POE_approx,
        ITo = ITo
      )
    },
    by = .(lat, lon, depth, p, Tn, a, b, c, sdLnA, R2, MSE, RMSE, fit)
  ]
}

#' @noRd
buildParamUHS <- function(fitDT, AEP_in) {
  ITo <- if ("ITo" %in% names(AEP_in)) unique(AEP_in$ITo)[1] else 50
  TRo <- unique(c(seq(100,10000,25), 475,975,2475,5000,10000))

  fitDT[
    ,
    {
      Sa_calc <- exp(a + b*log(TRo) + c*(1/TRo))
      AEP_approx <- 1 / TRo
      POE_approx <- 1 - exp(-ITo*(1/TRo))
      data.table(
        TR  = TRo,
        Sa  = Sa_calc,
        AEP = AEP_approx,
        POE = POE_approx,
        ITo = ITo
      )
    },
    by=.(lat, lon, depth, p, Tn)
  ]
}

#' @noRd
applySiteAmp <- function(dt, vs30vec, vref, quantAF) {
  # call dsra::fitModel.AF.TR or fitModel.AF.TR from dsra
  grouping <- c()
  for (cc in c("ID","lat","lon","depth","p","TR")) {
    if (cc %in% names(dt)) grouping <- c(grouping, cc)
  }

  resAF <- data.table()
  for (Vs in vs30vec) {
    pgaKey <- dt[Tn==0, .(PGA = Sa[which.max(Sa)]), by=grouping]
    tmp    <- merge(dt, pgaKey, by=grouping, all.x=TRUE)

    fullG <- c(grouping, "Tn")
    AFdt <- tmp[
      ,
      fitModel.AF.TR(
        .x   = .SD,
        pga  = PGA,
        q    = quantAF,
        Tn   = Tn,
        vs30 = Vs,
        vref = vref
      ),
      by=fullG
    ]
    resAF <- rbind(resAF, AFdt, fill=TRUE)
  }
  return(resAF)
}

#' @noRd
mergeAF <- function(dt, AFdt) {
  keepC <- c("ID","Vref","Vs30","lat","lon","depth","p","Tn","AF","sdLnAF","PGA")
  keepC <- intersect(keepC, names(AFdt))
  AFuniq <- unique(AFdt[, ..keepC])

  joinC <- intersect(names(dt), names(AFuniq))
  dt2 <- AFuniq[dt, on=joinC][
    ,
    `:=`(Sa = AF * Sa)
  ]
  return(dt2)
}
