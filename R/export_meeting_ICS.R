
#' Create a minimal iCalendar VEVENT
#' Using the lubridate::with_tz() function convert in the expected timezone
#' using R local, see Sys.timezone()
#'
#' @param start,end start and end times of the event, can be a list to create
#' a serie. This will be converted to GMT time zone (expected by iCalendar)
#' @param meeting_title a meeting title of the event.
#' @param outfile output file name. If NULL, the iCalendar is returned as a string
#' @param domain a domain name. Concatenated to the UUID if given
#'
#' @importFrom uuid UUIDgenerate
#' @importFrom lubridate with_tz
#'
#' @export
#'
#' @examples
#' dates <- c("2024-09-14", "2024-09-21", "2024-09-28")
#' starts <- paste0(dates, paste0(" 10:00:00 ", Sys.timezone()))
#' ends <- paste0(dates, paste0(" 11:30:00 ", Sys.timezone()))
#' export_meeting_ICS(starts, ends, "Shopping")
#'
#' @return 0, but either save to a file or write into terminal

export_meeting_ICS <- function(start, end, meeting_title, outfile = NULL, domain = NULL) {
	stopifnot(length(start) == length(end))
	stopifnot(length(meeting_title) == 1)
	time_zone = "GMT" # the calender expect this time zone, do not change
	start <- with_tz(start, time_zone) #as_datetime
	end <- with_tz(end, time_zone)
	date_now <- Sys.time()
	HEADER <- "BEGIN:VCALENDAR\nVERSION:2.0" # default
	HEADER <- paste0(HEADER, "\nPRODID:-//R//vCalendar 1.0//EN") # This property specifies the identifier for the product that created the iCalendar object.
	EVENTS <- sapply(1:length(start), function(i) {
		uid <- UUIDgenerate()
		if (!is.null(domain)) uid <- paste0(uid, "@", domain)
		dtstamp <- format(date_now, "%Y%m%dT%H%M%SZ") # , tz = time_zone
		dtstart <- format(start[i], "%Y%m%dT%H%M%SZ")
		dtend <- format(end[i], "%Y%m%dT%H%M%SZ")
		paste0("BEGIN:VEVENT\nUID:", uid,
					 "\nDTSTAMP:", dtstamp,
					 "\nDTSTART:", dtstart, "\nDTEND:", dtend,
					 "\nSUMMARY:", meeting_title, "\nEND:VEVENT")
	})
	END <- "END:VCALENDAR"
	if (is.null(outfile)) {
		final_data <- paste0(HEADER, paste(EVENTS, collapse = "\n"), END)
		sprintf(final_data)
		return(0)
	} else {
		writeLines(c(HEADER, EVENTS, END), con = outfile)
		return(0)
	}
}
