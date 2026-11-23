# GGCMI Phase 3 SSA / RiskAssess quality-check functions
# Adapted and substantially rewritten on 2026-11-03 from:
# https://github.com/AgMIP-GGCMI/phase3_quality_check
# Upstream reference: ae30290a89f2e330d7983aa69ede6fc6c8380a7c
# SPDX-License-Identifier: AGPL-3.0-only
# The full AGPL-3.0 license is included at the end of this file.
#
# Read-only QC: no NetCDF is renamed, rewritten, interpolated or repaired.
# Dependency: ncdf4. Calendar/mask: FIRST SEASON ONLY.
# Coordinates and their data are always indexed together. Opposite latitude
# orders are supported; changing labels alone would NOT repair a flipped map.
# Revision: core/optional variables, monthly soil moisture, irrigation-specific
# requirements, and no compression-level checks.
# Report layout revised 2026-11-05: two main reports, with supporting files in details/.

ssa_default_config <- function() {
  ranges <- list(
    yield=c(0,100), biom=c(0,100), cnyield=c(0,100),
    plantday=c(1,366), plantyear=c(1600,2100), harvyear=c(1600,2100),
    anthday=c(0,365), matyday=c(-365,365), pirnreq=c(0,1e6),
    aet=c(0,1e6), soilmoist1m=c(0,1e6), soilmoistmat=c(0,1e6),
    transp=c(0,1e6), soilevap=c(0,1e6), runoff=c(0,1e6),
    rootm=c(0,100), tnrup=c(0,1e4), tnrin=c(0,1e4),
    tnrloss=c(0,1e4), n2oemis=c(0,1e4), n2emis=c(0,1e4),
    nleach=c(0,1e4), tcemis=c(0,1e4), ch4emis=c(0,1e4),
    maturitystatus=c(0,1), maturityindex=c(-5,1))
  units <- c(
    yield="t ha-1 gs-1 (dry matter)", biom="t ha-1 gs-1 (dry matter)",
    rootm="t ha-1 gs-1 (dry matter)", cnyield="", maturitystatus="",
    maturityindex="", plantday="day of year", plantyear="calendar year",
    harvyear="calendar year", anthday="days from planting",
    matyday="days from planting", pirnreq="kg m-2 gs-1",
    aet="kg m-2 gs-1", transp="kg m-2 gs-1", soilevap="kg m-2 gs-1",
    runoff="kg m-2 gs-1", soilmoist1m="kg m-3", soilmoistmat="kg m-3",
    tnrup="kgN ha-1 gs-1", tnrin="kgN ha-1 gs-1",
    tnrloss="kgN ha-1 gs-1", n2oemis="gN m-2 gs-1",
    n2emis="gN m-2 gs-1", nleach="gN m-2 gs-1",
    tcemis="gC m-2 gs-1", ch4emis="gC m-2 gs-1")
  core_vars <- c("yield","biom","cnyield","plantday","plantyear","anthday",
                 "matyday","harvyear","pirnreq","aet","soilmoist1m",
                 "tnrup","n2oemis","nleach")
  timesteps <- setNames(rep("annual",length(ranges)),names(ranges))
  timesteps[["soilmoist1m"]] <- "monthly"
  list(
    output_root="", report_root="", calendar_file="", models=NULL,
    expected_prefixes=c("APSIMX-PIK"="apsim-pik"),
    n_levels=c("nc","n0","n20","n40","n60","n90","n120","n180"),
    core_forcing="gswp-w5e5",
    core_climate_scenario="obsclim", gcm_climate_scenario="historical",
    optional_gcms=c("ukesm1-0-ll","mri-esm2-0","mpi-esm1-2-hr",
                    "ipsl-cm6a-lr","gfdl-esm4"),
    gcm_check_scope="submitted_only", # submitted_only / all / none
    calendar_code="ssacld", crop="mai", region="ssa",
    start_year=1978L, end_year=2019L,
    ranges=ranges, units=units, allowed_vars=names(ranges),
    core_vars=core_vars, optional_vars=setdiff(names(ranges),core_vars),
    core_missing_severity="ERROR", variable_timesteps=timesteps,
    firr_only_vars="pirnreq",
    fail_value=-1, expected_fill=1e20,
    coordinate_tolerance=1e-6, resolution=0.5,
    save_reports=TRUE, report_date=format(Sys.Date(), "%Y-%m-%d"),
    max_examples_per_file=100L)
}

ssa_bind <- function(xs, empty=data.frame()) {
  xs <- Filter(function(x) is.data.frame(x) && nrow(x)>0L, xs)
  if (!length(xs)) return(empty)
  rownames_out <- do.call(rbind, xs)
  rownames(rownames_out) <- NULL
  rownames_out
}

ssa_empty_issues <- function() {
  as.data.frame(setNames(rep(list(character()),6),
    c("model","file","stage","severity","code","message")),
    stringsAsFactors=FALSE)
}

ssa_logger <- function(model, file, stage) {
  rows <- list()
  list(
    add=function(severity, code, message) {
      rows[[length(rows)+1L]] <<- data.frame(
        model=model, file=file, stage=stage, severity=severity,
        code=code, message=as.character(message), stringsAsFactors=FALSE)
    },
    get=function() ssa_bind(rows, ssa_empty_issues()))
}

ssa_attr <- function(nc, var, key, default=NULL) {
  x <- ncdf4::ncatt_get(nc, var, key)
  if (isTRUE(x$hasatt)) x$value else default
}

ssa_base_variable <- function(name) {
  strsplit(tolower(name), "[-_]")[[1L]][1L]
}

target_units <- function(variable, cfg=ssa_default_config()) {
  base <- ssa_base_variable(variable)
  if (!base %in% names(cfg$units)) stop("No units configured for ", base)
  unname(cfg$units[[base]])
}

ssa_expected_timestep <- function(variable, cfg) {
  if (!variable %in% names(cfg$variable_timesteps)) return(NA_character_)
  unname(cfg$variable_timesteps[[variable]])
}

ssa_variable_class <- function(variable, cfg) {
  ifelse(variable %in% cfg$core_vars,"core",
    ifelse(variable %in% cfg$optional_vars,"optional","unconfigured"))
}

ssa_axis <- function(name) {
  switch(tolower(name), lon="lon", longitude="lon",
         lat="lat", latitude="lat", time="time", "unknown")
}

ssa_longitude <- function(x) ((x+180) %% 360)-180

# Match coordinate values, not array positions. No interpolation.
ssa_match_coordinates <- function(reference, candidate, tolerance) {
  if (!length(candidate) || any(!is.finite(candidate)))
    return(rep(NA_integer_, length(reference)))
  ord <- order(candidate)
  v <- candidate[ord]
  left <- pmax(1L, pmin(length(v), findInterval(reference, v)))
  right <- pmin(length(v), left+1L)
  nearest <- ifelse(abs(reference-v[left]) <= abs(reference-v[right]),
                    left, right)
  result <- ord[nearest]
  result[abs(reference-candidate[result]) > tolerance] <- NA_integer_
  result
}

# Read one explicitly named first-season array into R order [lon, lat].
ssa_read_calendar_array <- function(nc, name) {
  if (!name %in% names(nc$var)) stop("Calendar variable missing: ", name)
  v <- nc$var[[name]]
  axes <- vapply(v$dim, function(d) ssa_axis(d$name), character(1))
  if (length(axes)!=2L || !setequal(axes,c("lon","lat")))
    stop("Calendar variable must have exactly longitude and latitude dimensions.")
  a <- ncdf4::ncvar_get(nc, name, collapse_degen=FALSE)
  aperm(a, match(c("lon","lat"), axes))
}

readmask.nc <- function(filename, tolerance=1e-6) {
  if (!requireNamespace("ncdf4", quietly=TRUE))
    stop("Please install the R package ncdf4.")
  nc <- ncdf4::nc_open(filename)
  on.exit(ncdf4::nc_close(nc))
  name <- "optimal_sowing_doy" # FIRST SEASON ONLY; never infer first variable.
  if (!name %in% names(nc$var)) stop("First-season sowing variable not found.")
  vd <- nc$var[[name]]$dim
  axes <- vapply(vd, function(d) ssa_axis(d$name), character(1))
  if (length(axes)!=2L || !setequal(axes,c("lon","lat")))
    stop("Invalid calendar dimensions.")
  lon <- ssa_longitude(vd[[match("lon",axes)]]$vals)
  lat <- vd[[match("lat",axes)]]$vals
  if (any(!is.finite(c(lon,lat))) ||
      any(diff(sort(lon))<=tolerance) ||
      any(diff(sort(lat))<=tolerance))
    stop("Calendar coordinates contain missing or duplicate positions.")
  if (any(abs(lat)>90)) stop("Calendar latitude is outside [-90,90].")
  xorder <- order(lon); yorder <- order(lat)
  read_first <- function(variable) {
    a <- ssa_read_calendar_array(nc,variable)
    a[xorder,yorder,drop=FALSE] # reorder COORDINATES AND DATA together
  }
  sow <- read_first(name)
  if (any(is.finite(sow) & (sow<1 | sow>366 | abs(sow-round(sow))>tolerance)))
    stop("First-season sowing days must be integers between 1 and 366.")
  mask <- is.finite(sow)
  if (!any(mask)) stop("First-season calendar has no valid target cells.")
  mat <- if ("optimal_sowing_mat" %in% names(nc$var))
    read_first("optimal_sowing_mat") else matrix(NA_real_,nrow(mask),ncol(mask))
  # This is a diagnostic flag, NOT permission to ignore missing output.
  crossing <- mask & is.finite(mat) & mat>365
  list(mask=mask, lon=lon[xorder], lat=lat[yorder], sowing_day=sow,
       reference_cross_year=crossing, source=normalizePath(filename,winslash="/"),
       variable=name, n_target=sum(mask))
}

ssa_expected_prefix <- function(model, cfg) {
  index <- match(tolower(model),tolower(names(cfg$expected_prefixes)))
  if (!is.na(index)) tolower(unname(cfg$expected_prefixes[index]))
  else tolower(model)
}

ssa_empty_manifest <- function() {
  cols <- c("file","name","model","forcing","scenario","soc","variable","crop",
            "irrigation","region","timestep","start_year","end_year","n_level")
  d <- as.data.frame(setNames(rep(list(character()),length(cols)),cols),
                     stringsAsFactors=FALSE)
  d$parsed <- logical()
  d
}

test.filename <- function(file_path, model_dir, cfg) {
  model <- basename(model_dir)
  log <- ssa_logger(model,file_path,"filename")
  meta <- as.list(setNames(rep("",14L),names(ssa_empty_manifest())[1:14]))
  meta$file <- file_path; meta$name <- basename(file_path)
  meta$parsed <- FALSE
  if (!grepl("\\.nc4?$",meta$name,ignore.case=TRUE)) {
    log$add("ERROR","FILE_EXTENSION","Expected .nc (or .nc4 with a warning).")
    return(list(meta=as.data.frame(meta,stringsAsFactors=FALSE),issues=log$get()))
  }
  if (!endsWith(meta$name,".nc"))
    log$add("WARNING","FILE_EXTENSION","Preferred extension is lowercase .nc.")
  fields <- strsplit(sub("\\.[^.]+$","",meta$name),"_",fixed=TRUE)[[1]]
  if (length(fields)!=11L) {
    log$add("ERROR","FILENAME_FIELDS",paste("Expected 11 underscore-separated fields; found",length(fields)))
    return(list(meta=as.data.frame(meta,stringsAsFactors=FALSE),issues=log$get()))
  }
  keys <- c("model","forcing","scenario","soc","variable","crop","irrigation",
            "region","timestep","start_year","end_year")
  for (i in seq_along(keys)) meta[[keys[i]]] <- tolower(fields[i])
  meta$parsed <- TRUE
  if (any(fields!=tolower(fields)))
    log$add("WARNING","FILENAME_CASE","Filename fields should be lowercase.")
  if (meta$model!=tolower(model))
    log$add("WARNING","MODEL_FOLDER_MISMATCH",
      paste("Model folder:",model,"; filename prefix:",fields[1]))
  wanted <- ssa_expected_prefix(model,cfg)
  if (meta$model!=wanted)
    log$add("WARNING","MODEL_PREFIX",paste("Expected model prefix",wanted,"but found",meta$model))
  if (!meta$forcing %in% c(cfg$core_forcing,cfg$optional_gcms))
    log$add("ERROR","CLIMATE_FORCING",paste("Unknown climate forcing:",meta$forcing))
  expected_scenario <- if (meta$forcing==cfg$core_forcing)
    cfg$core_climate_scenario else cfg$gcm_climate_scenario
  if (meta$scenario!=expected_scenario)
    log$add("ERROR","CLIMATE_SCENARIO",paste(meta$forcing,"requires",expected_scenario))
  soc <- strsplit(meta$soc,"-",fixed=TRUE)[[1]]
  if (length(soc)==3L && soc[1]=="soc" && soc[2]==cfg$calendar_code &&
      soc[3] %in% cfg$n_levels) meta$n_level <- soc[3] else
    log$add("ERROR","SOC_SCENARIO","Expected soc-ssacld-<configured N level>.")
  if (!meta$variable %in% cfg$allowed_vars)
    log$add("ERROR","VARIABLE",paste("Variable is not configured:",meta$variable))
  if (meta$crop!=cfg$crop) log$add("ERROR","CROP",paste("Expected",cfg$crop))
  irrigation <- if (meta$forcing==cfg$core_forcing) c("firr","noirr") else "noirr"
  if (!meta$irrigation %in% irrigation)
    log$add("ERROR","IRRIGATION",paste("Allowed irrigation for this forcing:",paste(irrigation,collapse=", ")))
  if (meta$region!=cfg$region) log$add("ERROR","REGION",paste("Expected",cfg$region))
  expected_timestep <- ssa_expected_timestep(meta$variable,cfg)
  if (!is.na(expected_timestep) && meta$timestep!=expected_timestep)
    log$add("ERROR","TIMESTEP",paste("Expected",expected_timestep,"for",meta$variable))
  years_valid <- all(grepl("^[0-9]{4}$",c(meta$start_year,meta$end_year)))
  if (!years_valid || as.integer(meta$start_year)>as.integer(meta$end_year)) {
    log$add("ERROR","FILENAME_YEARS","Invalid start/end year fields.")
  } else if (as.integer(meta$start_year)!=cfg$start_year ||
             as.integer(meta$end_year)!=cfg$end_year) {
    log$add("ERROR","FILENAME_YEARS",paste0("Expected ",cfg$start_year,"_",cfg$end_year,
      "; found ",meta$start_year,"_",meta$end_year,"."))
  }
  root <- normalizePath(model_dir,winslash="/",mustWork=TRUE)
  full <- normalizePath(file_path,winslash="/",mustWork=TRUE)
  relative <- substring(full,nchar(root)+2L)
  parts <- strsplit(relative,"/",fixed=TRUE)[[1]]
  if (length(parts)!=3L)
    log$add("WARNING","DIRECTORY_STRUCTURE","Expected model/forcing/N-level/file.nc.")
  if (length(parts)>=3L) {
    if (tolower(parts[1])!=meta$forcing)
      log$add("WARNING","FORCING_FOLDER",paste("Folder:",parts[1],"; filename:",meta$forcing))
    if (tolower(parts[2])!=meta$n_level)
      log$add("WARNING","N_LEVEL_FOLDER",paste("Folder:",parts[2],"; filename N level:",meta$n_level))
  }
  list(meta=as.data.frame(meta,stringsAsFactors=FALSE),issues=log$get())
}

do_test.filenames <- function(files, model_dir, cfg) {
  results <- lapply(files,function(f) test.filename(f,model_dir,cfg))
  list(manifest=ssa_bind(lapply(results, function(x) x$meta),ssa_empty_manifest()),
       issues=ssa_bind(lapply(results, function(x) x$issues),ssa_empty_issues()))
}

ssa_simulation_key <- function(d) {
  fields <- c("forcing","scenario","soc","variable","crop","irrigation","region","timestep")
  if (!nrow(d)) return(character())
  do.call(paste,c(d[fields],sep="|"))
}

do_test.file_set <- function(manifest, model_dir, cfg, filename_issues=ssa_empty_issues()) {
  model <- basename(model_dir)
  log <- ssa_logger(model,"","completeness")
  dirs <- tolower(basename(list.dirs(model_dir,recursive=FALSE,full.names=TRUE)))
  present <- unique(c(dirs,manifest$forcing[manifest$parsed]))
  optional <- switch(cfg$gcm_check_scope, all=cfg$optional_gcms,
    submitted_only=intersect(cfg$optional_gcms,present), none=character())
  forcings <- c(cfg$core_forcing,optional)
  expected <- ssa_bind(lapply(forcings,function(forcing) {
    irrigation <- if (forcing==cfg$core_forcing) c("firr","noirr") else "noirr"
    d <- expand.grid(n_level=cfg$n_levels,irrigation=irrigation,
                     variable=c(cfg$core_vars,cfg$optional_vars),stringsAsFactors=FALSE)
    # Irrigation requirements are only expected for irrigated simulations.
    d <- d[!d$variable %in% cfg$firr_only_vars | d$irrigation=="firr",,drop=FALSE]
    d$variable_class <- ssa_variable_class(d$variable,cfg)
    d$forcing <- forcing
    d$scenario <- if (forcing==cfg$core_forcing)
      cfg$core_climate_scenario else cfg$gcm_climate_scenario
    d$soc <- paste("soc",cfg$calendar_code,d$n_level,sep="-")
    d$crop <- cfg$crop; d$region <- cfg$region
    d$timestep <- unname(cfg$variable_timesteps[d$variable])
    d
  }))
  prefix <- ssa_expected_prefix(model,cfg)
  expected$expected_file <- with(expected,paste(prefix,forcing,scenario,soc,variable,
    crop,irrigation,region,timestep,cfg$start_year,
    paste0(cfg$end_year,".nc"),sep="_"))
  expected$expected_relative_path <- with(expected,
    file.path(forcing,n_level,expected_file))
  expected$status <- "missing"; expected$found_files <- ""
  observed <- manifest[manifest$parsed,,drop=FALSE]
  okeys <- ssa_simulation_key(observed); ekeys <- ssa_simulation_key(expected)
  issue_files <- unique(filename_issues$file[filename_issues$severity %in% c("ERROR","WARNING")])
  for (i in seq_len(nrow(expected))) {
    hit <- which(okeys==ekeys[i])
    if (length(hit)) {
      expected$found_files[i] <- paste(observed$file[hit],collapse=" | ")
      # A wrong model prefix/year is NOT counted as a wholly absent experiment.
      expected$status[i] <- if (length(hit)>1L) "duplicate" else if
        (observed$file[hit] %in% issue_files) "present_with_naming_issues" else "present"
    }
  }
  core <- expected$variable_class=="core"
  missing_core <- sum(core & expected$status=="missing")
  missing_optional <- sum(!core & expected$status=="missing")
  if (missing_core) log$add(cfg$core_missing_severity,"MISSING_CORE_FILES",
    paste(missing_core,"of",sum(core),"core-variable files missing; see simulations_missing.txt and expected_files.csv in details/ (same run prefix)."))
  if (missing_optional) log$add("INFO","OPTIONAL_FILES_NOT_PROVIDED",
    paste(missing_optional,"of",sum(!core),"optional-variable files not provided. This is informational only."))
  if (any(expected$status=="duplicate"))
    log$add("ERROR","DUPLICATE_OUTPUTS","Multiple files represent the same experiment/variable. See expected_files.csv in details/ (same run prefix).")
  if (length(okeys) && any(!okeys %in% ekeys))
    log$add("INFO","UNREQUESTED_OUTPUTS",
      "Some parsed files are outside the configured experiment/variable combinations; they still receive content checks.")
  log$add("INFO","GCM_CHECK_SCOPE",paste("Optional GCM check scope:",cfg$gcm_check_scope,
    "; checked GCMs:",if(length(optional)) paste(optional,collapse=", ") else "none",
    "; climate scenarios:",cfg$core_climate_scenario,"(reanalysis),",
    cfg$gcm_climate_scenario,"(GCMs)."))
  list(expected=expected,issues=log$get(),optional=optional)
}

# Decode years and months from declared units, not array indices.
# Supports modern Gregorian calendars and fixed 365/366/360-day calendars.
ssa_decode_time <- function(values, units, calendar="standard") {
  if (!length(values) || any(!is.finite(values))) stop("Time coordinates are empty/non-finite.")
  pattern <- "^\\s*(growing seasons?|years?|months?|days?|hours?|minutes?|seconds?)\\s+since\\s+([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})(.*)$"
  groups <- regmatches(tolower(units),regexec(pattern,tolower(units)))[[1]]
  if (!length(groups)) stop("Unsupported time units: ",units)
  unit <- groups[2]; yy <- as.integer(groups[3]); mm <- as.integer(groups[4]); dd <- as.integer(groups[5])
  if (mm<1 || mm>12 || dd<1 || dd>31) stop("Invalid time origin.")
  if (grepl("^(growing season|year)",unit)) {
    if (any(abs(values-round(values))>1e-6)) stop("Annual season/year offsets must be integers.")
    return(data.frame(year=as.integer(yy+round(values)),month=NA_integer_))
  }
  if (grepl("^month",unit)) {
    if (any(abs(values-round(values))>1e-6)) stop("Month offsets must be integers.")
    index <- mm-1+round(values)
    return(data.frame(year=as.integer(yy+floor(index/12)),
                      month=as.integer(index %% 12+1)))
  }
  multiplier <- if(grepl("^day",unit)) 1 else if(grepl("^hour",unit)) 1/24 else
    if(grepl("^minute",unit)) 1/1440 else 1/86400
  suffix <- trimws(sub("^\\s*,","",groups[6]))
  if (!nzchar(suffix)) suffix <- "00:00:00"
  suffix <- sub("(Z| UTC)$","",suffix,ignore.case=TRUE)
  origin <- sprintf("%04d-%02d-%02d %s",yy,mm,dd,suffix)
  if (calendar %in% c("standard","gregorian","proleptic_gregorian")) {
    origin <- as.POSIXct(origin,tz="UTC")
    if (is.na(origin)) stop("Cannot decode time origin.")
    dates <- origin+values*multiplier*86400
    return(data.frame(year=as.integer(format(dates,"%Y",tz="UTC")),
                      month=as.integer(format(dates,"%m",tz="UTC"))))
  }
  nday <- switch(calendar, noleap=365L, "365_day"=365L,
    all_leap=366L, "366_day"=366L, "360_day"=360L, NA_integer_)
  if (is.na(nday)) stop("Unsupported calendar: ",calendar)
  months <- if(nday==360L) rep(30L,12L) else c(31L,if(nday==366L)29L else 28L,31L,30L,31L,30L,31L,31L,30L,31L,30L,31L)
  if (dd>months[mm]) stop("Invalid origin day for declared calendar.")
  offset <- sum(months[seq_len(mm-1L)])+dd-1
  # Fixed-calendar date decoding currently supports midnight origins.
  if (!grepl("^00:00(:00(\\.0+)?)?$",suffix)) stop("Non-midnight fixed-calendar origins are not supported.")
  days <- offset+values*multiplier
  year_offset <- floor(days/nday)
  day_of_year <- days-year_offset*nday
  data.frame(year=as.integer(yy+year_offset),
             month=as.integer(findInterval(day_of_year,c(0,cumsum(months)))))
}

ssa_decode_years <- function(values, units, calendar="standard") {
  ssa_decode_time(values,units,calendar)$year
}

ssa_period_labels <- function(years, months, timestep) {
  if (timestep=="monthly")
    ifelse(is.na(years)|is.na(months),NA_character_,sprintf("%04d-%02d",years,months))
  else as.character(years)
}

ssa_expected_periods <- function(first_year, last_year, timestep) {
  years <- seq.int(first_year,last_year)
  if (timestep=="monthly")
    ssa_period_labels(rep(years,each=12L),rep(seq_len(12L),length(years)),timestep)
  else as.character(years)
}

ssa_check_time <- function(nc, dim, meta, cfg, log) {
  values <- dim$vals; n <- length(values)
  units <- ssa_attr(nc,dim$name,"units","")
  calendar <- tolower(ssa_attr(nc,dim$name,"calendar","standard"))
  timestep <- ssa_expected_timestep(meta$variable,cfg)
  if (any(!is.finite(values)) || (n>1L && any(diff(values)<=0)))
    log$add("ERROR","TIME_ORDER","Time coordinates must be finite, strictly increasing, and non-duplicated.")
  standard_units <- paste0(if(timestep=="monthly")"months" else "growing seasons",
                           " since ",cfg$start_year,"-01-01")
  if (units!=standard_units)
    log$add(if(timestep=="monthly")"INFO" else "WARNING","TIME_ENCODING",
      paste("Alternative time encoding:",units,"; decoded periods are checked separately."))
  decoded <- tryCatch(ssa_decode_time(values,units,calendar),error=function(e) {
    log$add("ERROR","TIME_DECODE",conditionMessage(e))
    data.frame(year=rep(NA_integer_,n),month=rep(NA_integer_,n))
  })
  y <- decoded$year; months <- decoded$month
  if (timestep=="monthly" && !anyNA(y) && anyNA(months))
    log$add("ERROR","TIME_MONTH_UNRESOLVED",
      "Monthly output needs month-resolving time coordinates; integer year/season offsets are insufficient.")
  periods <- ssa_period_labels(y,months,timestep)
  y1 <- suppressWarnings(as.integer(meta$start_year)); yn <- suppressWarnings(as.integer(meta$end_year))
  if (!is.na(y1) && !is.na(yn) && yn>=y1) {
    wanted <- ssa_expected_periods(y1,yn,timestep)
    if (n!=length(wanted)) log$add("ERROR","TIME_COUNT_FILENAME",
      paste("Time length",n,"does not match",length(wanted),timestep,
            "steps for filename span",y1,yn))
    if (!anyNA(periods) && !identical(periods,wanted))
      log$add("ERROR","TIME_FILENAME_MISMATCH",
        paste("Decoded",timestep,"periods do not match the continuous filename year span."))
  }
  wanted <- ssa_expected_periods(cfg$start_year,cfg$end_year,timestep)
  show_periods <- function(x) paste(c(head(x,24L),
    if(length(x)>24L) paste0("... (",length(x)," total)")),collapse=",")
  if (!anyNA(periods) && !identical(periods,wanted))
    log$add("ERROR",if(timestep=="monthly")"TIME_PROJECT_MONTHS" else "TIME_PROJECT_YEARS",paste(
      "Expected",length(wanted),timestep,"steps from",wanted[1],"through",tail(wanted,1),
      "; missing:",show_periods(setdiff(wanted,periods)),
      "; extra:",show_periods(setdiff(periods,wanted)),
      "; duplicates:",show_periods(unique(periods[duplicated(periods)]))))
  list(years=y,months=months,periods=periods,timestep=timestep,
       values=values,units=units,calendar=calendar)
}

ssa_empty_coverage <- function() {
  data.frame(file=character(),variable=character(),forcing=character(),n_level=character(),
    irrigation=character(),variable_class=character(),timestep=character(),
    time_index=integer(),year=integer(),month=integer(),period=character(),time_value=double(),
    expected_cells=integer(),valid_inside=integer(),missing_inside=integer(),
    failed_inside=integer(),unrepresented_target_cells=integer(),
    valid_outside=integer(),out_of_range=integer(),missing_crossyear_reference=integer())
}

ssa_empty_examples <- function() {
  data.frame(file=character(),type=character(),year=integer(),month=integer(),
             period=character(),lon=double(),lat=double(),value=double())
}

test.file <- function(file_path, meta, reference, cfg, model) {
  log <- ssa_logger(model,file_path,"data")
  coverage <- list(); examples <- list(); sample_count <- 0L
  stats <- data.frame()
  result <- function() list(issues=log$get(),coverage=ssa_bind(coverage,ssa_empty_coverage()),
                            examples=ssa_bind(examples,ssa_empty_examples()),stats=stats)
  if (!isTRUE(meta$parsed)) {
    log$add("ERROR","CONTENT_SKIPPED","Cannot associate content with a malformed filename; content checks skipped.")
    return(result())
  }
  if (!meta$variable %in% cfg$allowed_vars) {
    log$add("ERROR","CONTENT_SKIPPED","Variable has no configured QC rules.")
    return(result())
  }
  nc <- ncdf4::nc_open(file_path)
  on.exit(ncdf4::nc_close(nc))
  candidate <- names(nc$var)[vapply(names(nc$var),ssa_base_variable,character(1))==meta$variable]
  # Coordinates, CRS and other auxiliary variables are never selected by position.
  candidate <- candidate[vapply(candidate,function(n) nc$var[[n]]$ndims>0L,logical(1))]
  if (length(candidate)!=1L) {
    log$add("ERROR","DATA_VARIABLE",paste("Expected one identifiable",meta$variable,
      "data variable; found",length(candidate),paste(candidate,collapse=", ")))
    return(result())
  }
  var <- candidate[1]; v <- nc$var[[var]]
  if (var!=meta$variable) {
    log$add("WARNING","VARIABLE_NAME_SUFFIX",paste("Stored variable:",var,
      "; base variable used for QC:",meta$variable))
    bits <- strsplit(tolower(var),"[-_]")[[1]]
    if (length(bits)==3L && (bits[2]!=meta$crop || bits[3]!=meta$irrigation))
      log$add("WARNING","VARIABLE_SUFFIX_MISMATCH","Stored crop/irrigation suffix differs from filename.")
  }
  units <- ssa_attr(nc,var,"units","")
  want <- target_units(meta$variable,cfg)
  normalize_units <- function(s) trimws(gsub("\\s+"," ",s))
  if (normalize_units(units)!=normalize_units(want)) {
    dry_unspecified <- meta$variable %in% c("yield","biom","rootm") &&
      normalize_units(units)=="t ha-1 gs-1"
    dimensionless <- want=="" && units %in% c("-","1","dimensionless")
    if (dry_unspecified) log$add("WARNING","DRY_MATTER_UNSPECIFIED",
      "Mass/area/season units match, but dry-matter basis is not stated.")
    else if (!dimensionless) log$add("ERROR","UNITS",
      paste0("Observed units '",units,"'; expected '",want,"'. No numerical conversion applied."))
  }
  fill <- ssa_attr(nc,var,"_FillValue",ssa_attr(nc,var,"missing_value",NULL))
  if (is.null(fill) || length(fill)!=1L || !is.finite(fill) ||
      abs(as.numeric(fill)/cfg$expected_fill-1)>1e-6)
    log$add("WARNING","FILL_VALUE",
      paste("Nonstandard missing-value marker:",paste(fill,collapse=","),
        "; declared missing values are still excluded from statistics."))
  axes <- vapply(v$dim,function(d) ssa_axis(d$name),character(1))
  if (length(axes)!=3L || !setequal(axes,c("lon","lat","time"))) {
    log$add("ERROR","DATA_DIMENSIONS","Data variable must have exactly one longitude, latitude and time dimension.")
    return(result())
  }
  pos <- match(c("lon","lat","time"),axes)
  dx <- v$dim[[pos[1]]]; dy <- v$dim[[pos[2]]]; dt <- v$dim[[pos[3]]]
  x <- ssa_longitude(dx$vals); y <- dy$vals
  if (dx$name!="lon" || dy$name!="lat")
    log$add("INFO","COORDINATE_ALIASES","Recognized longitude/latitude dimension aliases.")
  if (any(dx$vals!=x, na.rm=TRUE))
    log$add("INFO","LONGITUDE_CONVENTION","Longitude labels mapped to [-180,180); data remain paired with their original coordinates.")
  if (any(!is.finite(c(x,y))) || any(abs(y)>90) ||
      any(diff(sort(x))<=cfg$coordinate_tolerance) ||
      any(diff(sort(y))<=cfg$coordinate_tolerance)) {
    log$add("ERROR","COORDINATE_VALUES","Missing, duplicated or invalid coordinate values prevent safe matching.")
    return(result())
  }
  for (d in list(dx,dy)) {
    unit <- ssa_attr(nc,d$name,"units","")
    wanted_unit <- if (ssa_axis(d$name)=="lon") "degrees_east" else "degrees_north"
    if (unit!=wanted_unit) log$add("ERROR","COORDINATE_UNITS",
      paste(d$name,"has units",unit,"rather than",wanted_unit))
  }
  if ((length(x)>1L && any(abs(diff(sort(x))-cfg$resolution)>cfg$coordinate_tolerance)) ||
      (length(y)>1L && any(abs(diff(sort(y))-cfg$resolution)>cfg$coordinate_tolerance)))
    log$add("ERROR","GRID_RESOLUTION","Grid spacing differs from the configured resolution.")
  if (length(y)>1L && all(diff(y)<0))
    log$add("INFO","LATITUDE_DESCENDING","Latitude is descending. Data are matched by coordinate; no standalone coordinate flip is performed.")
  ix <- ssa_match_coordinates(reference$lon,x,cfg$coordinate_tolerance)
  iy <- ssa_match_coordinates(reference$lat,y,cfg$coordinate_tolerance)
  gx <- which(!is.na(ix)); gy <- which(!is.na(iy))
  represented <- outer(!is.na(ix),!is.na(iy),"&")
  absent <- sum(reference$mask & !represented)
  if (absent) log$add("ERROR","TARGET_COORDINATES_MISSING",
    paste(absent,"target cells have no matching output coordinates."))
  if (length(x)!=length(reference$lon) || length(y)!=length(reference$lat) ||
      anyNA(ix) || anyNA(iy))
    log$add("WARNING","GRID_EXTENT_DIFFERENT",
      paste("Output grid:",length(x),"x",length(y),"; reference:",length(reference$lon),
        "x",length(reference$lat),". Coverage is evaluated at matched coordinates."))
  output_target <- matrix(FALSE,length(x),length(y))
  if (length(gx) && length(gy))
    output_target[ix[gx],iy[gy]] <- reference$mask[gx,gy,drop=FALSE]
  time <- ssa_check_time(nc,dt,meta,cfg,log)
  bounds <- cfg$ranges[[meta$variable]] # NAME lookup, never list position
  nvalid <- nmissing <- nfail <- nbad <- nnonfinite <- 0
  lower <- Inf; upper <- -Inf
  add_examples <- function(condition, values, lons, lats, kind, year, month, period) {
    remaining <- cfg$max_examples_per_file-sample_count
    if (remaining<=0L || !any(condition)) return(invisible(NULL))
    id <- head(which(condition,arr.ind=TRUE),remaining)
    examples[[length(examples)+1L]] <<- data.frame(file=file_path,type=kind,year=year,
      month=month,period=period,
      lon=lons[id[,1]],lat=lats[id[,2]],value=values[id],stringsAsFactors=FALSE)
    sample_count <<- sample_count+nrow(id)
  }
  for (k in seq_len(dt$len)) {
    start <- rep(1L,3); count <- vapply(v$dim,function(d) d$len,integer(1))
    start[pos[3]] <- k; count[pos[3]] <- 1L
    a <- ncdf4::ncvar_get(nc,var,start=start,count=count,collapse_degen=FALSE)
    a <- aperm(a,pos)
    a <- matrix(a,nrow=length(x),ncol=length(y))
    # ncvar_get handles declared fill values; also honor missing_value if supplied.
    mv <- ssa_attr(nc,var,"missing_value",NULL)
    if (length(mv)) for (m in mv) if (is.finite(m)) a[!is.na(a)&a==m] <- NA_real_
    failed <- is.finite(a) & a==cfg$fail_value
    valid <- is.finite(a) & !failed
    bad <- valid & (a<bounds[1] | a>bounds[2])
    nvalid <- nvalid+sum(valid); nmissing <- nmissing+sum(is.na(a))
    nfail <- nfail+sum(failed); nbad <- nbad+sum(bad)
    nnonfinite <- nnonfinite+sum(is.infinite(a))
    if (any(valid)) { lower <- min(lower,a[valid]); upper <- max(upper,a[valid]) }
    aligned <- matrix(NA_real_,nrow(reference$mask),ncol(reference$mask))
    if (length(gx) && length(gy))
      aligned[gx,gy] <- a[ix[gx],iy[gy],drop=FALSE]
    aligned_failed <- is.finite(aligned) & aligned==cfg$fail_value
    aligned_valid <- is.finite(aligned) & !aligned_failed
    missing <- reference$mask & !aligned_valid
    year <- time$years[k]
    month <- if(time$timestep=="monthly") time$months[k] else NA_integer_
    period <- time$periods[k]
    coverage[[length(coverage)+1L]] <- data.frame(
      file=file_path,variable=meta$variable,forcing=meta$forcing,n_level=meta$n_level,
      irrigation=meta$irrigation,variable_class=ssa_variable_class(meta$variable,cfg),
      timestep=time$timestep,time_index=k,year=year,month=month,period=period,
      time_value=time$values[k],
      expected_cells=reference$n_target,valid_inside=sum(reference$mask&aligned_valid),
      missing_inside=sum(missing),failed_inside=sum(reference$mask&aligned_failed),
      unrepresented_target_cells=absent,valid_outside=sum(valid&!output_target),
      out_of_range=sum(bad),missing_crossyear_reference=sum(missing&reference$reference_cross_year))
    add_examples(missing,aligned,reference$lon,reference$lat,"target_missing_or_failed",year,month,period)
    add_examples(valid&!output_target,a,x,y,"outside_mask",year,month,period)
    add_examples(bad,a,x,y,"out_of_range",year,month,period)
  }
  cov <- ssa_bind(coverage,ssa_empty_coverage())
  if (!nvalid) log$add("ERROR","NO_VALID_DATA",
    "No valid numeric observations in the entire variable (missing values/failure markers excluded).")
  if (any(cov$valid_inside==0L))
    log$add("ERROR","EMPTY_TARGET_PERIODS",paste("No valid target-cell data at time indices:",
      paste(cov$time_index[cov$valid_inside==0L],collapse=", ")))
  if (any(cov$missing_inside>0L))
    log$add("WARNING","MASK_COVERAGE_MISSING",paste(
      sum(cov$missing_inside>0L),"time steps have missing/failed target cells; maximum",
      max(cov$missing_inside),"of",reference$n_target,". See yearly_coverage.csv in details/ (same run prefix)."))
  if (any(cov$valid_outside>0L))
    log$add("WARNING","MASK_COVERAGE_EXTRA",paste(
      sum(cov$valid_outside>0L),"time steps have values outside the first-season mask; maximum",
      max(cov$valid_outside),"."))
  if (nbad) log$add("WARNING","DATA_RANGE",paste(nbad,"values outside [",
    bounds[1],",",bounds[2],"]; observed non-failure range:",lower,"to",upper))
  if (nfail) log$add("WARNING","FAILURE_VALUES",paste(nfail,"values equal failure marker",cfg$fail_value))
  if (nnonfinite) log$add("ERROR","INFINITE_VALUES",paste(nnonfinite,"infinite values found."))
  last <- which(!is.na(cov$year)&cov$year==cfg$end_year)
  if (length(last) && any(cov$missing_crossyear_reference[last]>0L))
    log$add("INFO","FINAL_YEAR_CROSS_YEAR_DIAGNOSTIC",
      paste("Per time step in the final year,",min(cov$missing_crossyear_reference[last]),
        "to",max(cov$missing_crossyear_reference[last]),"missing target cells have first-season",
        "reference maturity >365. This is a diagnostic association, not a confirmed cause or exemption."))
  stats <- data.frame(file=file_path,variable=var,variable_class=ssa_variable_class(meta$variable,cfg),
    timestep=time$timestep,units=units,
    nlon=length(x),nlat=length(y),ntime=dt$len,valid_values=nvalid,
    missing_values=nmissing,failed_values=nfail,infinite_values=nnonfinite,
    out_of_range_values=nbad,min=if(nvalid)lower else NA_real_,
    max=if(nvalid)upper else NA_real_,stringsAsFactors=FALSE)
  result()
}

do_test.files <- function(manifest, reference, cfg, model) {
  results <- vector("list",nrow(manifest))
  for (i in seq_len(nrow(manifest))) { # ALL files; no 1,000-file limit
    cat(sprintf("[%s] %d/%d %s\n",model,i,nrow(manifest),manifest$name[i]))
    results[[i]] <- tryCatch(
      test.file(manifest$file[i],as.list(manifest[i,,drop=FALSE]),reference,cfg,model),
      error=function(e) {
        log <- ssa_logger(model,manifest$file[i],"data")
        log$add("ERROR","READ_OR_CHECK_FAILURE",conditionMessage(e))
        list(issues=log$get(),coverage=ssa_empty_coverage(),
             examples=ssa_empty_examples(),stats=data.frame())
      })
  }
  list(issues=ssa_bind(lapply(results,function(x)x$issues),ssa_empty_issues()),
       coverage=ssa_bind(lapply(results,function(x)x$coverage),ssa_empty_coverage()),
       examples=ssa_bind(lapply(results,function(x)x$examples),ssa_empty_examples()),
       stats=ssa_bind(lapply(results,function(x)x$stats)))
}

setup_reports <- function(report_root, model, report_date) {
  # Keep the two main reports in the date folder; place supporting files in details/.
  directory <- file.path(report_root,model,report_date)
  details_directory <- file.path(directory,"details")
  dir.create(details_directory,recursive=TRUE,showWarnings=FALSE)
  if (!dir.exists(details_directory)) stop("Cannot create report directory: ",details_directory)
  # Atomic reservation prevents overwriting even for concurrent same-day runs.
  stem <- paste0("run_",format(Sys.time(),"%H%M%S"))
  count <- 0L
  repeat {
    id <- paste0(stem,if(count) sprintf("_%03d",count) else "")
    lock <- file.path(directory,paste0(".",id,".reserved"))
    existing <- c(Sys.glob(file.path(directory,paste0(id,"_*"))),
                  Sys.glob(file.path(details_directory,paste0(id,"_*"))))
    if (!length(existing)) {
      if (dir.create(lock,showWarnings=FALSE)) break
      if (!dir.exists(lock)) stop("Cannot reserve a report filename in: ",directory)
    }
    count <- count+1L
  }
  list(directory=directory,details_directory=details_directory,id=id,path=function(suffix) {
    folder <- if (suffix %in% c("summary.txt","issues.csv")) directory else details_directory
    file.path(folder,paste0(id,"_",suffix))
  })
}

ssa_issue_lines <- function(issues) {
  if (!nrow(issues)) return("No issues detected.")
  unlist(lapply(seq_len(nrow(issues)),function(i) c(
    paste0("[",issues$severity[i],"] ",issues$code[i]),
    paste0("File: ",issues$file[i]),issues$message[i],"")),use.names=FALSE)
}

ssa_issue_summary <- function(issues) {
  # Count unique submitted files separately from issue rows. Model-level issues
  # have no input filename; their affected_files value is not applicable.
  groups <- unique(issues[c("severity","stage","code")])
  groups$affected_files <- rep(NA_integer_,nrow(groups))
  groups$messages <- integer(nrow(groups))
  for (i in seq_len(nrow(groups))) {
    rows <- issues$severity==groups$severity[i] & issues$stage==groups$stage[i] &
      issues$code==groups$code[i]
    files <- unique(issues$file[rows])
    files <- files[!is.na(files) & nzchar(files)]
    if (length(files)) groups$affected_files[i] <- length(files)
    groups$messages[i] <- sum(rows)
  }
  groups[order(match(groups$severity,c("ERROR","WARNING","INFO")),
               groups$stage,groups$code),,drop=FALSE]
}

ssa_write_reports <- function(reports, model, cfg, reference, manifest, expected, data, issues) {
  write <- function(lines,suffix) writeLines(enc2utf8(lines),reports$path(suffix),useBytes=TRUE)
  csv <- function(d,suffix) utils::write.csv(d,reports$path(suffix),row.names=FALSE,na="",fileEncoding="UTF-8")
  header <- c("GGCMI Phase 3 SSA / RiskAssess quality check",paste("Model:",model),
    paste("Run:",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
    paste("Reference:",reference$source),
    paste("Mask:",reference$variable,"(FIRST SEASON ONLY); target cells:",reference$n_target),"")
  counts <- table(factor(issues$severity,levels=c("ERROR","WARNING","INFO")))
  core <- expected$variable_class=="core"
  summary <- c(header,
    "START HERE",
    paste("1. Read this summary; then filter",basename(reports$path("issues.csv")),"by severity/code."),
    "2. Open details/ only when you need coverage, statistics or coordinates.",
    paste("Use files with the same run prefix:",reports$id),"",
    paste("Files inventoried:",nrow(manifest)),
    paste("Files with completed numerical statistics:",nrow(data$stats)),
    paste("Core-variable files required:",sum(core),
          "; missing:",sum(core & expected$status=="missing"),
          "; missing-file severity:",cfg$core_missing_severity),
    paste("Optional-variable files in scope:",sum(!core),
          "; not provided (INFO only):",sum(!core & expected$status=="missing")),
    paste("Files present across both classes:",sum(expected$status!="missing"),
          "; duplicate combinations:",sum(expected$status=="duplicate")),
    paste("Errors:",counts[1],"; warnings:",counts[2],"; information:",counts[3]),
    "These are message counts, not counts of files or missing grid cells.",
    "ERROR = failed requirement; WARNING = investigate; INFO = context.",
    "", "ISSUES BY TYPE (one row per severity / stage / code)",
    "affected_files = unique submitted files; '-' = model-level issue.",
    "messages = issue rows. File counts across categories must not be added together.")
  if (nrow(issues)) {
    grouped <- ssa_issue_summary(issues)
    grouped$affected_files <- ifelse(is.na(grouped$affected_files),"-",
                                     as.character(grouped$affected_files))
    summary <- c(summary,capture.output(print(grouped,row.names=FALSE)))
  } else summary <- c(summary,"No issues detected.")
  summary <- c(summary,"", "DETAILS (filenames below omit the run prefix)",
    "Missing/duplicate outputs: details/expected_files.csv or details/simulations_missing.txt.",
    "Grid coverage by year/month: details/yearly_coverage.csv.",
    "Value statistics and locations: details/file_statistics.csv and details/coordinate_examples.csv.",
    "No simulation data were modified. Passing QC does not validate model accuracy.")
  write(summary,"summary.txt")
  write(c(header,ssa_issue_lines(issues[issues$stage=="filename",,drop=FALSE])),"filename_issues.txt")
  missing_core <- expected[core & expected$status=="missing",,drop=FALSE]
  missing_optional <- expected[!core & expected$status=="missing",,drop=FALSE]
  duplicates <- expected[expected$status=="duplicate",,drop=FALSE]
  write(c(header,ssa_issue_lines(issues[issues$stage=="completeness",,drop=FALSE]),
    "MISSING CORE-VARIABLE FILES",
    if(nrow(missing_core)) missing_core$expected_relative_path else "None.",
    "", "OPTIONAL-VARIABLE FILES NOT PROVIDED (INFO ONLY)",
    if(nrow(missing_optional)) capture.output(print(
      stats::aggregate(list(files_not_provided=rep(1L,nrow(missing_optional))),
        missing_optional[c("forcing","variable","timestep")],sum),row.names=FALSE)) else "None.",
    "See expected_files.csv for individual optional-file combinations.",
    "", "DUPLICATE FILE COMBINATIONS",
    if(nrow(duplicates)) duplicates$expected_relative_path else "None."),
    "simulations_missing.txt")
  write(c(header,ssa_issue_lines(issues[issues$stage=="data",,drop=FALSE])),"data_issues.txt")
  csv(issues,"issues.csv"); csv(manifest,"file_inventory.csv")
  csv(expected,"expected_files.csv"); csv(data$coverage,"yearly_coverage.csv")
  csv(data$stats,"file_statistics.csv"); csv(data$examples,"coordinate_examples.csv")
  write(capture.output(dput(cfg)),"configuration.R")
  write(capture.output(utils::sessionInfo()),"session_info.txt")
}

ssa_validate_config <- function(cfg) {
  if (!requireNamespace("ncdf4",quietly=TRUE)) stop("Install ncdf4 first: install.packages('ncdf4')")
  if (!dir.exists(cfg$output_root)) stop("Output root does not exist: ",cfg$output_root)
  if (!file.exists(cfg$calendar_file)) stop("Calendar file does not exist: ",cfg$calendar_file)
  if (!is.null(cfg$required_vars) || !is.null(cfg$optional_mode) || !is.null(cfg$timestep))
    stop("Outdated configuration: use core_vars/optional_vars, gcm_check_scope and variable_timesteps from the revised main script.")
  if (!cfg$gcm_check_scope %in% c("submitted_only","all","none"))
    stop("gcm_check_scope must be submitted_only, all or none; it is not a climate scenario.")
  if (!identical(cfg$core_climate_scenario,"obsclim") ||
      !identical(cfg$gcm_climate_scenario,"historical"))
    stop("This project uses obsclim for reanalysis and historical for GCMs.")
  classes <- c(cfg$core_vars,cfg$optional_vars)
  if (!length(cfg$core_vars) || anyDuplicated(classes) ||
      !setequal(classes,cfg$allowed_vars))
    stop("core_vars and optional_vars must be disjoint and together cover allowed_vars.")
  if (!cfg$core_missing_severity %in% c("ERROR","WARNING"))
    stop("core_missing_severity must be ERROR or WARNING.")
  if (!all(cfg$allowed_vars %in% names(cfg$variable_timesteps)) ||
      anyNA(cfg$variable_timesteps[cfg$allowed_vars]) ||
      !all(cfg$variable_timesteps[cfg$allowed_vars] %in% c("annual","monthly")))
    stop("Every allowed variable needs an annual or monthly entry in variable_timesteps.")
  if (!all(cfg$firr_only_vars %in% cfg$allowed_vars)) stop("Invalid firr_only_vars.")
  if (!all(cfg$allowed_vars %in% intersect(names(cfg$ranges),names(cfg$units))))
    stop("Every allowed variable must have named range and unit rules.")
  if ("evap" %in% cfg$allowed_vars) stop("evap is excluded from this project.")
  for (v in cfg$allowed_vars) {
    b <- cfg$ranges[[v]]
    if (length(b)!=2L || anyNA(b) || b[1]>b[2]) stop("Invalid bounds for ",v)
  }
  if (!is.finite(cfg$coordinate_tolerance) || cfg$coordinate_tolerance<=0 ||
      cfg$coordinate_tolerance>=cfg$resolution/2) stop("Invalid coordinate tolerance.")
  if (cfg$start_year>cfg$end_year) stop("Invalid project year range.")
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",cfg$report_date)) stop("report_date must use YYYY-MM-DD.")
  if (cfg$save_reports && !nzchar(cfg$report_root)) stop("Set report_root.")
  invisible(TRUE)
}

run_ssa_qc <- function(cfg) {
  ssa_validate_config(cfg)
  reference <- readmask.nc(cfg$calendar_file,cfg$coordinate_tolerance)
  cat("First-season reference target cells:",reference$n_target,"\n")
  models <- cfg$models
  if (is.null(models)) {
    models <- basename(list.dirs(cfg$output_root,full.names=TRUE,recursive=FALSE))
    models <- models[!grepl("^[._]",models)]
    reportpath <- normalizePath(cfg$report_root,winslash="/",mustWork=FALSE)
    models <- models[vapply(models,function(m)
      normalizePath(file.path(cfg$output_root,m),winslash="/",mustWork=FALSE)!=reportpath,logical(1))]
  }
  if (!length(models)) stop("No model folders found. Set cfg$models or check output_root.")
  if (any(grepl("[/\\\\]",models)) || any(models %in% c(".","..")))
    stop("models must contain folder basenames, not paths.")
  results <- list()
  for (model in models) {
    model_dir <- file.path(cfg$output_root,model)
    if (!dir.exists(model_dir)) stop("Model directory does not exist: ",model_dir)
    files <- list.files(model_dir,pattern="\\.nc4?$",recursive=TRUE,full.names=TRUE,ignore.case=TRUE)
    files <- normalizePath(files,winslash="/",mustWork=TRUE)
    named <- do_test.filenames(files,model_dir,cfg)
    named$manifest$variable_class <- ssa_variable_class(named$manifest$variable,cfg)
    complete <- do_test.file_set(named$manifest,model_dir,cfg,named$issues)
    data <- do_test.files(named$manifest,reference,cfg,model)
    issues <- ssa_bind(list(named$issues,complete$issues,data$issues),ssa_empty_issues())
    if (!length(files)) {
      log <- ssa_logger(model,"","data")
      log$add("ERROR","NO_NETCDF_FILES","No .nc/.nc4 files were found in this model folder.")
      issues <- ssa_bind(list(issues,log$get()),ssa_empty_issues())
    }
    reports <- NULL
    if (cfg$save_reports) {
      reports <- setup_reports(cfg$report_root,model,cfg$report_date)
      ssa_write_reports(reports,model,cfg,reference,named$manifest,complete$expected,data,issues)
      cat("Start here:",reports$path("summary.txt"),"\n")
    }
    cat(sprintf("%s: %d errors, %d warnings, %d information messages.\n",model,
      sum(issues$severity=="ERROR"),sum(issues$severity=="WARNING"),sum(issues$severity=="INFO")))
    if (!cfg$save_reports) cat(paste(ssa_issue_lines(issues),collapse="\n"),"\n")
    results[[model]] <- list(issues=issues,manifest=named$manifest,
      expected=complete$expected,coverage=data$coverage,statistics=data$stats,
      examples=data$examples,reports=if(is.null(reports))NULL else reports$directory)
  }
  invisible(results)
}


# ===== FULL LICENSE (verbatim text, R comment prefix added) =====
#                     GNU AFFERO GENERAL PUBLIC LICENSE
#                        Version 3, 19 November 2007
# 
#  Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>
#  Everyone is permitted to copy and distribute verbatim copies
#  of this license document, but changing it is not allowed.
# 
#                             Preamble
# 
#   The GNU Affero General Public License is a free, copyleft license for
# software and other kinds of works, specifically designed to ensure
# cooperation with the community in the case of network server software.
# 
#   The licenses for most software and other practical works are designed
# to take away your freedom to share and change the works.  By contrast,
# our General Public Licenses are intended to guarantee your freedom to
# share and change all versions of a program--to make sure it remains free
# software for all its users.
# 
#   When we speak of free software, we are referring to freedom, not
# price.  Our General Public Licenses are designed to make sure that you
# have the freedom to distribute copies of free software (and charge for
# them if you wish), that you receive source code or can get it if you
# want it, that you can change the software or use pieces of it in new
# free programs, and that you know you can do these things.
# 
#   Developers that use our General Public Licenses protect your rights
# with two steps: (1) assert copyright on the software, and (2) offer
# you this License which gives you legal permission to copy, distribute
# and/or modify the software.
# 
#   A secondary benefit of defending all users' freedom is that
# improvements made in alternate versions of the program, if they
# receive widespread use, become available for other developers to
# incorporate.  Many developers of free software are heartened and
# encouraged by the resulting cooperation.  However, in the case of
# software used on network servers, this result may fail to come about.
# The GNU General Public License permits making a modified version and
# letting the public access it on a server without ever releasing its
# source code to the public.
# 
#   The GNU Affero General Public License is designed specifically to
# ensure that, in such cases, the modified source code becomes available
# to the community.  It requires the operator of a network server to
# provide the source code of the modified version running there to the
# users of that server.  Therefore, public use of a modified version, on
# a publicly accessible server, gives the public access to the source
# code of the modified version.
# 
#   An older license, called the Affero General Public License and
# published by Affero, was designed to accomplish similar goals.  This is
# a different license, not a version of the Affero GPL, but Affero has
# released a new version of the Affero GPL which permits relicensing under
# this license.
# 
#   The precise terms and conditions for copying, distribution and
# modification follow.
# 
#                        TERMS AND CONDITIONS
# 
#   0. Definitions.
# 
#   "This License" refers to version 3 of the GNU Affero General Public License.
# 
#   "Copyright" also means copyright-like laws that apply to other kinds of
# works, such as semiconductor masks.
# 
#   "The Program" refers to any copyrightable work licensed under this
# License.  Each licensee is addressed as "you".  "Licensees" and
# "recipients" may be individuals or organizations.
# 
#   To "modify" a work means to copy from or adapt all or part of the work
# in a fashion requiring copyright permission, other than the making of an
# exact copy.  The resulting work is called a "modified version" of the
# earlier work or a work "based on" the earlier work.
# 
#   A "covered work" means either the unmodified Program or a work based
# on the Program.
# 
#   To "propagate" a work means to do anything with it that, without
# permission, would make you directly or secondarily liable for
# infringement under applicable copyright law, except executing it on a
# computer or modifying a private copy.  Propagation includes copying,
# distribution (with or without modification), making available to the
# public, and in some countries other activities as well.
# 
#   To "convey" a work means any kind of propagation that enables other
# parties to make or receive copies.  Mere interaction with a user through
# a computer network, with no transfer of a copy, is not conveying.
# 
#   An interactive user interface displays "Appropriate Legal Notices"
# to the extent that it includes a convenient and prominently visible
# feature that (1) displays an appropriate copyright notice, and (2)
# tells the user that there is no warranty for the work (except to the
# extent that warranties are provided), that licensees may convey the
# work under this License, and how to view a copy of this License.  If
# the interface presents a list of user commands or options, such as a
# menu, a prominent item in the list meets this criterion.
# 
#   1. Source Code.
# 
#   The "source code" for a work means the preferred form of the work
# for making modifications to it.  "Object code" means any non-source
# form of a work.
# 
#   A "Standard Interface" means an interface that either is an official
# standard defined by a recognized standards body, or, in the case of
# interfaces specified for a particular programming language, one that
# is widely used among developers working in that language.
# 
#   The "System Libraries" of an executable work include anything, other
# than the work as a whole, that (a) is included in the normal form of
# packaging a Major Component, but which is not part of that Major
# Component, and (b) serves only to enable use of the work with that
# Major Component, or to implement a Standard Interface for which an
# implementation is available to the public in source code form.  A
# "Major Component", in this context, means a major essential component
# (kernel, window system, and so on) of the specific operating system
# (if any) on which the executable work runs, or a compiler used to
# produce the work, or an object code interpreter used to run it.
# 
#   The "Corresponding Source" for a work in object code form means all
# the source code needed to generate, install, and (for an executable
# work) run the object code and to modify the work, including scripts to
# control those activities.  However, it does not include the work's
# System Libraries, or general-purpose tools or generally available free
# programs which are used unmodified in performing those activities but
# which are not part of the work.  For example, Corresponding Source
# includes interface definition files associated with source files for
# the work, and the source code for shared libraries and dynamically
# linked subprograms that the work is specifically designed to require,
# such as by intimate data communication or control flow between those
# subprograms and other parts of the work.
# 
#   The Corresponding Source need not include anything that users
# can regenerate automatically from other parts of the Corresponding
# Source.
# 
#   The Corresponding Source for a work in source code form is that
# same work.
# 
#   2. Basic Permissions.
# 
#   All rights granted under this License are granted for the term of
# copyright on the Program, and are irrevocable provided the stated
# conditions are met.  This License explicitly affirms your unlimited
# permission to run the unmodified Program.  The output from running a
# covered work is covered by this License only if the output, given its
# content, constitutes a covered work.  This License acknowledges your
# rights of fair use or other equivalent, as provided by copyright law.
# 
#   You may make, run and propagate covered works that you do not
# convey, without conditions so long as your license otherwise remains
# in force.  You may convey covered works to others for the sole purpose
# of having them make modifications exclusively for you, or provide you
# with facilities for running those works, provided that you comply with
# the terms of this License in conveying all material for which you do
# not control copyright.  Those thus making or running the covered works
# for you must do so exclusively on your behalf, under your direction
# and control, on terms that prohibit them from making any copies of
# your copyrighted material outside their relationship with you.
# 
#   Conveying under any other circumstances is permitted solely under
# the conditions stated below.  Sublicensing is not allowed; section 10
# makes it unnecessary.
# 
#   3. Protecting Users' Legal Rights From Anti-Circumvention Law.
# 
#   No covered work shall be deemed part of an effective technological
# measure under any applicable law fulfilling obligations under article
# 11 of the WIPO copyright treaty adopted on 20 December 1996, or
# similar laws prohibiting or restricting circumvention of such
# measures.
# 
#   When you convey a covered work, you waive any legal power to forbid
# circumvention of technological measures to the extent such circumvention
# is effected by exercising rights under this License with respect to
# the covered work, and you disclaim any intention to limit operation or
# modification of the work as a means of enforcing, against the work's
# users, your or third parties' legal rights to forbid circumvention of
# technological measures.
# 
#   4. Conveying Verbatim Copies.
# 
#   You may convey verbatim copies of the Program's source code as you
# receive it, in any medium, provided that you conspicuously and
# appropriately publish on each copy an appropriate copyright notice;
# keep intact all notices stating that this License and any
# non-permissive terms added in accord with section 7 apply to the code;
# keep intact all notices of the absence of any warranty; and give all
# recipients a copy of this License along with the Program.
# 
#   You may charge any price or no price for each copy that you convey,
# and you may offer support or warranty protection for a fee.
# 
#   5. Conveying Modified Source Versions.
# 
#   You may convey a work based on the Program, or the modifications to
# produce it from the Program, in the form of source code under the
# terms of section 4, provided that you also meet all of these conditions:
# 
#     a) The work must carry prominent notices stating that you modified
#     it, and giving a relevant date.
# 
#     b) The work must carry prominent notices stating that it is
#     released under this License and any conditions added under section
#     7.  This requirement modifies the requirement in section 4 to
#     "keep intact all notices".
# 
#     c) You must license the entire work, as a whole, under this
#     License to anyone who comes into possession of a copy.  This
#     License will therefore apply, along with any applicable section 7
#     additional terms, to the whole of the work, and all its parts,
#     regardless of how they are packaged.  This License gives no
#     permission to license the work in any other way, but it does not
#     invalidate such permission if you have separately received it.
# 
#     d) If the work has interactive user interfaces, each must display
#     Appropriate Legal Notices; however, if the Program has interactive
#     interfaces that do not display Appropriate Legal Notices, your
#     work need not make them do so.
# 
#   A compilation of a covered work with other separate and independent
# works, which are not by their nature extensions of the covered work,
# and which are not combined with it such as to form a larger program,
# in or on a volume of a storage or distribution medium, is called an
# "aggregate" if the compilation and its resulting copyright are not
# used to limit the access or legal rights of the compilation's users
# beyond what the individual works permit.  Inclusion of a covered work
# in an aggregate does not cause this License to apply to the other
# parts of the aggregate.
# 
#   6. Conveying Non-Source Forms.
# 
#   You may convey a covered work in object code form under the terms
# of sections 4 and 5, provided that you also convey the
# machine-readable Corresponding Source under the terms of this License,
# in one of these ways:
# 
#     a) Convey the object code in, or embodied in, a physical product
#     (including a physical distribution medium), accompanied by the
#     Corresponding Source fixed on a durable physical medium
#     customarily used for software interchange.
# 
#     b) Convey the object code in, or embodied in, a physical product
#     (including a physical distribution medium), accompanied by a
#     written offer, valid for at least three years and valid for as
#     long as you offer spare parts or customer support for that product
#     model, to give anyone who possesses the object code either (1) a
#     copy of the Corresponding Source for all the software in the
#     product that is covered by this License, on a durable physical
#     medium customarily used for software interchange, for a price no
#     more than your reasonable cost of physically performing this
#     conveying of source, or (2) access to copy the
#     Corresponding Source from a network server at no charge.
# 
#     c) Convey individual copies of the object code with a copy of the
#     written offer to provide the Corresponding Source.  This
#     alternative is allowed only occasionally and noncommercially, and
#     only if you received the object code with such an offer, in accord
#     with subsection 6b.
# 
#     d) Convey the object code by offering access from a designated
#     place (gratis or for a charge), and offer equivalent access to the
#     Corresponding Source in the same way through the same place at no
#     further charge.  You need not require recipients to copy the
#     Corresponding Source along with the object code.  If the place to
#     copy the object code is a network server, the Corresponding Source
#     may be on a different server (operated by you or a third party)
#     that supports equivalent copying facilities, provided you maintain
#     clear directions next to the object code saying where to find the
#     Corresponding Source.  Regardless of what server hosts the
#     Corresponding Source, you remain obligated to ensure that it is
#     available for as long as needed to satisfy these requirements.
# 
#     e) Convey the object code using peer-to-peer transmission, provided
#     you inform other peers where the object code and Corresponding
#     Source of the work are being offered to the general public at no
#     charge under subsection 6d.
# 
#   A separable portion of the object code, whose source code is excluded
# from the Corresponding Source as a System Library, need not be
# included in conveying the object code work.
# 
#   A "User Product" is either (1) a "consumer product", which means any
# tangible personal property which is normally used for personal, family,
# or household purposes, or (2) anything designed or sold for incorporation
# into a dwelling.  In determining whether a product is a consumer product,
# doubtful cases shall be resolved in favor of coverage.  For a particular
# product received by a particular user, "normally used" refers to a
# typical or common use of that class of product, regardless of the status
# of the particular user or of the way in which the particular user
# actually uses, or expects or is expected to use, the product.  A product
# is a consumer product regardless of whether the product has substantial
# commercial, industrial or non-consumer uses, unless such uses represent
# the only significant mode of use of the product.
# 
#   "Installation Information" for a User Product means any methods,
# procedures, authorization keys, or other information required to install
# and execute modified versions of a covered work in that User Product from
# a modified version of its Corresponding Source.  The information must
# suffice to ensure that the continued functioning of the modified object
# code is in no case prevented or interfered with solely because
# modification has been made.
# 
#   If you convey an object code work under this section in, or with, or
# specifically for use in, a User Product, and the conveying occurs as
# part of a transaction in which the right of possession and use of the
# User Product is transferred to the recipient in perpetuity or for a
# fixed term (regardless of how the transaction is characterized), the
# Corresponding Source conveyed under this section must be accompanied
# by the Installation Information.  But this requirement does not apply
# if neither you nor any third party retains the ability to install
# modified object code on the User Product (for example, the work has
# been installed in ROM).
# 
#   The requirement to provide Installation Information does not include a
# requirement to continue to provide support service, warranty, or updates
# for a work that has been modified or installed by the recipient, or for
# the User Product in which it has been modified or installed.  Access to a
# network may be denied when the modification itself materially and
# adversely affects the operation of the network or violates the rules and
# protocols for communication across the network.
# 
#   Corresponding Source conveyed, and Installation Information provided,
# in accord with this section must be in a format that is publicly
# documented (and with an implementation available to the public in
# source code form), and must require no special password or key for
# unpacking, reading or copying.
# 
#   7. Additional Terms.
# 
#   "Additional permissions" are terms that supplement the terms of this
# License by making exceptions from one or more of its conditions.
# Additional permissions that are applicable to the entire Program shall
# be treated as though they were included in this License, to the extent
# that they are valid under applicable law.  If additional permissions
# apply only to part of the Program, that part may be used separately
# under those permissions, but the entire Program remains governed by
# this License without regard to the additional permissions.
# 
#   When you convey a copy of a covered work, you may at your option
# remove any additional permissions from that copy, or from any part of
# it.  (Additional permissions may be written to require their own
# removal in certain cases when you modify the work.)  You may place
# additional permissions on material, added by you to a covered work,
# for which you have or can give appropriate copyright permission.
# 
#   Notwithstanding any other provision of this License, for material you
# add to a covered work, you may (if authorized by the copyright holders of
# that material) supplement the terms of this License with terms:
# 
#     a) Disclaiming warranty or limiting liability differently from the
#     terms of sections 15 and 16 of this License; or
# 
#     b) Requiring preservation of specified reasonable legal notices or
#     author attributions in that material or in the Appropriate Legal
#     Notices displayed by works containing it; or
# 
#     c) Prohibiting misrepresentation of the origin of that material, or
#     requiring that modified versions of such material be marked in
#     reasonable ways as different from the original version; or
# 
#     d) Limiting the use for publicity purposes of names of licensors or
#     authors of the material; or
# 
#     e) Declining to grant rights under trademark law for use of some
#     trade names, trademarks, or service marks; or
# 
#     f) Requiring indemnification of licensors and authors of that
#     material by anyone who conveys the material (or modified versions of
#     it) with contractual assumptions of liability to the recipient, for
#     any liability that these contractual assumptions directly impose on
#     those licensors and authors.
# 
#   All other non-permissive additional terms are considered "further
# restrictions" within the meaning of section 10.  If the Program as you
# received it, or any part of it, contains a notice stating that it is
# governed by this License along with a term that is a further
# restriction, you may remove that term.  If a license document contains
# a further restriction but permits relicensing or conveying under this
# License, you may add to a covered work material governed by the terms
# of that license document, provided that the further restriction does
# not survive such relicensing or conveying.
# 
#   If you add terms to a covered work in accord with this section, you
# must place, in the relevant source files, a statement of the
# additional terms that apply to those files, or a notice indicating
# where to find the applicable terms.
# 
#   Additional terms, permissive or non-permissive, may be stated in the
# form of a separately written license, or stated as exceptions;
# the above requirements apply either way.
# 
#   8. Termination.
# 
#   You may not propagate or modify a covered work except as expressly
# provided under this License.  Any attempt otherwise to propagate or
# modify it is void, and will automatically terminate your rights under
# this License (including any patent licenses granted under the third
# paragraph of section 11).
# 
#   However, if you cease all violation of this License, then your
# license from a particular copyright holder is reinstated (a)
# provisionally, unless and until the copyright holder explicitly and
# finally terminates your license, and (b) permanently, if the copyright
# holder fails to notify you of the violation by some reasonable means
# prior to 60 days after the cessation.
# 
#   Moreover, your license from a particular copyright holder is
# reinstated permanently if the copyright holder notifies you of the
# violation by some reasonable means, this is the first time you have
# received notice of violation of this License (for any work) from that
# copyright holder, and you cure the violation prior to 30 days after
# your receipt of the notice.
# 
#   Termination of your rights under this section does not terminate the
# licenses of parties who have received copies or rights from you under
# this License.  If your rights have been terminated and not permanently
# reinstated, you do not qualify to receive new licenses for the same
# material under section 10.
# 
#   9. Acceptance Not Required for Having Copies.
# 
#   You are not required to accept this License in order to receive or
# run a copy of the Program.  Ancillary propagation of a covered work
# occurring solely as a consequence of using peer-to-peer transmission
# to receive a copy likewise does not require acceptance.  However,
# nothing other than this License grants you permission to propagate or
# modify any covered work.  These actions infringe copyright if you do
# not accept this License.  Therefore, by modifying or propagating a
# covered work, you indicate your acceptance of this License to do so.
# 
#   10. Automatic Licensing of Downstream Recipients.
# 
#   Each time you convey a covered work, the recipient automatically
# receives a license from the original licensors, to run, modify and
# propagate that work, subject to this License.  You are not responsible
# for enforcing compliance by third parties with this License.
# 
#   An "entity transaction" is a transaction transferring control of an
# organization, or substantially all assets of one, or subdividing an
# organization, or merging organizations.  If propagation of a covered
# work results from an entity transaction, each party to that
# transaction who receives a copy of the work also receives whatever
# licenses to the work the party's predecessor in interest had or could
# give under the previous paragraph, plus a right to possession of the
# Corresponding Source of the work from the predecessor in interest, if
# the predecessor has it or can get it with reasonable efforts.
# 
#   You may not impose any further restrictions on the exercise of the
# rights granted or affirmed under this License.  For example, you may
# not impose a license fee, royalty, or other charge for exercise of
# rights granted under this License, and you may not initiate litigation
# (including a cross-claim or counterclaim in a lawsuit) alleging that
# any patent claim is infringed by making, using, selling, offering for
# sale, or importing the Program or any portion of it.
# 
#   11. Patents.
# 
#   A "contributor" is a copyright holder who authorizes use under this
# License of the Program or a work on which the Program is based.  The
# work thus licensed is called the contributor's "contributor version".
# 
#   A contributor's "essential patent claims" are all patent claims
# owned or controlled by the contributor, whether already acquired or
# hereafter acquired, that would be infringed by some manner, permitted
# by this License, of making, using, or selling its contributor version,
# but do not include claims that would be infringed only as a
# consequence of further modification of the contributor version.  For
# purposes of this definition, "control" includes the right to grant
# patent sublicenses in a manner consistent with the requirements of
# this License.
# 
#   Each contributor grants you a non-exclusive, worldwide, royalty-free
# patent license under the contributor's essential patent claims, to
# make, use, sell, offer for sale, import and otherwise run, modify and
# propagate the contents of its contributor version.
# 
#   In the following three paragraphs, a "patent license" is any express
# agreement or commitment, however denominated, not to enforce a patent
# (such as an express permission to practice a patent or covenant not to
# sue for patent infringement).  To "grant" such a patent license to a
# party means to make such an agreement or commitment not to enforce a
# patent against the party.
# 
#   If you convey a covered work, knowingly relying on a patent license,
# and the Corresponding Source of the work is not available for anyone
# to copy, free of charge and under the terms of this License, through a
# publicly available network server or other readily accessible means,
# then you must either (1) cause the Corresponding Source to be so
# available, or (2) arrange to deprive yourself of the benefit of the
# patent license for this particular work, or (3) arrange, in a manner
# consistent with the requirements of this License, to extend the patent
# license to downstream recipients.  "Knowingly relying" means you have
# actual knowledge that, but for the patent license, your conveying the
# covered work in a country, or your recipient's use of the covered work
# in a country, would infringe one or more identifiable patents in that
# country that you have reason to believe are valid.
# 
#   If, pursuant to or in connection with a single transaction or
# arrangement, you convey, or propagate by procuring conveyance of, a
# covered work, and grant a patent license to some of the parties
# receiving the covered work authorizing them to use, propagate, modify
# or convey a specific copy of the covered work, then the patent license
# you grant is automatically extended to all recipients of the covered
# work and works based on it.
# 
#   A patent license is "discriminatory" if it does not include within
# the scope of its coverage, prohibits the exercise of, or is
# conditioned on the non-exercise of one or more of the rights that are
# specifically granted under this License.  You may not convey a covered
# work if you are a party to an arrangement with a third party that is
# in the business of distributing software, under which you make payment
# to the third party based on the extent of your activity of conveying
# the work, and under which the third party grants, to any of the
# parties who would receive the covered work from you, a discriminatory
# patent license (a) in connection with copies of the covered work
# conveyed by you (or copies made from those copies), or (b) primarily
# for and in connection with specific products or compilations that
# contain the covered work, unless you entered into that arrangement,
# or that patent license was granted, prior to 28 March 2007.
# 
#   Nothing in this License shall be construed as excluding or limiting
# any implied license or other defenses to infringement that may
# otherwise be available to you under applicable patent law.
# 
#   12. No Surrender of Others' Freedom.
# 
#   If conditions are imposed on you (whether by court order, agreement or
# otherwise) that contradict the conditions of this License, they do not
# excuse you from the conditions of this License.  If you cannot convey a
# covered work so as to satisfy simultaneously your obligations under this
# License and any other pertinent obligations, then as a consequence you may
# not convey it at all.  For example, if you agree to terms that obligate you
# to collect a royalty for further conveying from those to whom you convey
# the Program, the only way you could satisfy both those terms and this
# License would be to refrain entirely from conveying the Program.
# 
#   13. Remote Network Interaction; Use with the GNU General Public License.
# 
#   Notwithstanding any other provision of this License, if you modify the
# Program, your modified version must prominently offer all users
# interacting with it remotely through a computer network (if your version
# supports such interaction) an opportunity to receive the Corresponding
# Source of your version by providing access to the Corresponding Source
# from a network server at no charge, through some standard or customary
# means of facilitating copying of software.  This Corresponding Source
# shall include the Corresponding Source for any work covered by version 3
# of the GNU General Public License that is incorporated pursuant to the
# following paragraph.
# 
#   Notwithstanding any other provision of this License, you have
# permission to link or combine any covered work with a work licensed
# under version 3 of the GNU General Public License into a single
# combined work, and to convey the resulting work.  The terms of this
# License will continue to apply to the part which is the covered work,
# but the work with which it is combined will remain governed by version
# 3 of the GNU General Public License.
# 
#   14. Revised Versions of this License.
# 
#   The Free Software Foundation may publish revised and/or new versions of
# the GNU Affero General Public License from time to time.  Such new versions
# will be similar in spirit to the present version, but may differ in detail to
# address new problems or concerns.
# 
#   Each version is given a distinguishing version number.  If the
# Program specifies that a certain numbered version of the GNU Affero General
# Public License "or any later version" applies to it, you have the
# option of following the terms and conditions either of that numbered
# version or of any later version published by the Free Software
# Foundation.  If the Program does not specify a version number of the
# GNU Affero General Public License, you may choose any version ever published
# by the Free Software Foundation.
# 
#   If the Program specifies that a proxy can decide which future
# versions of the GNU Affero General Public License can be used, that proxy's
# public statement of acceptance of a version permanently authorizes you
# to choose that version for the Program.
# 
#   Later license versions may give you additional or different
# permissions.  However, no additional obligations are imposed on any
# author or copyright holder as a result of your choosing to follow a
# later version.
# 
#   15. Disclaimer of Warranty.
# 
#   THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
# APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
# HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
# OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
# THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
# IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
# ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
# 
#   16. Limitation of Liability.
# 
#   IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
# WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
# THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
# GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
# USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
# DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
# PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
# EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGES.
# 
#   17. Interpretation of Sections 15 and 16.
# 
#   If the disclaimer of warranty and limitation of liability provided
# above cannot be given local legal effect according to their terms,
# reviewing courts shall apply local law that most closely approximates
# an absolute waiver of all civil liability in connection with the
# Program, unless a warranty or assumption of liability accompanies a
# copy of the Program in return for a fee.
# 
#                      END OF TERMS AND CONDITIONS
# 
#             How to Apply These Terms to Your New Programs
# 
#   If you develop a new program, and you want it to be of the greatest
# possible use to the public, the best way to achieve this is to make it
# free software which everyone can redistribute and change under these terms.
# 
#   To do so, attach the following notices to the program.  It is safest
# to attach them to the start of each source file to most effectively
# state the exclusion of warranty; and each file should have at least
# the "copyright" line and a pointer to where the full notice is found.
# 
#     <one line to give the program's name and a brief idea of what it does.>
#     Copyright (C) <year>  <name of author>
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU Affero General Public License as published
#     by the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU Affero General Public License for more details.
# 
#     You should have received a copy of the GNU Affero General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.
# 
# Also add information on how to contact you by electronic and paper mail.
# 
#   If your software can interact with users remotely through a computer
# network, you should also make sure that it provides a way for users to
# get its source.  For example, if your program is a web application, its
# interface could display a "Source" link that leads users to an archive
# of the code.  There are many ways you could offer source, and different
# solutions will be better for different programs; see section 13 for the
# specific requirements.
# 
#   You should also get your employer (if you work as a programmer) or school,
# if any, to sign a "copyright disclaimer" for the program, if necessary.
# For more information on this, and how to apply and follow the GNU AGPL, see
# <https://www.gnu.org/licenses/>.
# 
