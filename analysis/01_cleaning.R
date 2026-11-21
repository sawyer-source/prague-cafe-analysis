# ===================================================================
# 01_cleaning.R -- CLEAN THE RAW CAFE DATA (topic: Cafe Size)
#
# what this script does:
#   1. reads analysis/raw_data.csv (the sheet as downloaded, never edited by hand)
#   2. inspects it before changing anything
#   3. removes empty rows and cafes that were entered twice
#   4. cleans the columns our ten questions use
#   5. creates the new variables the questions need
#   6. checks the result and writes analysis/dataset.csv
#
# open prague-cafe-analysis.Rproj first, so the working directory is the
# repository root and the relative paths below work on any computer
# ===================================================================


# ============================================================
# 1. LOAD THE RAW DATA
# ============================================================
# check.names = FALSE keeps the column names exactly as in the sheet
# (without it, R would replace every space and bracket with a dot)
df_raw <- read.csv(file.path("analysis", "raw_data.csv"),
                   header = TRUE, check.names = FALSE, encoding = "UTF-8")


# ============================================================
# 2. INSPECT BEFORE CHANGING ANYTHING
# ============================================================
str(df_raw)              # the type R gave each column (chr = text, int/num = numbers)
nrow(df_raw)             # 299 rows
colSums(is.na(df_raw))   # missing values (NA) per column

# the price columns are "chr" (text): some cells hold several prices like
# "85, 95, 105". the distance column is text too: the empty rows at the
# bottom of the sheet contain "#VALUE!". both are fixed below

# the categorical columns: list their actual values to spot typos
table(df_raw[["District (number)"]], useNA = "ifany")
table(df_raw[["Cafe Type"]], useNA = "ifany")
table(df_raw[["Payment"]], useNA = "ifany")
# district has typos ("Prage 7", "prauge 1"), lower-case "prague 5" and
# stray spaces ("Prague 7 ") -- cleaned in section 4.
# cafe type and payment are not used by any Cafe Size question, so we only
# look at them and do not clean them


# ============================================================
# 3. EMPTY ROWS AND REPEATED CAFES
# ============================================================

## 3a. empty rows
# rows 202-300 of the sheet have no student name and no cafe: they only
# hold a default "Cash & Card" and "#VALUE!" from the sheet's formulas
sum(trimws(df_raw[["Name"]]) == "")   # 99 empty rows

df_rows <- df_raw[trimws(df_raw[["Name"]]) != "", ]   # keep rows that have a student name
nrow(df_rows)   # 200 cafes (10 students x 20)

## 3b. keep only the columns our questions use, with short names
# trimws() removes stray spaces at the start and end of the text
df <- data.frame(
  student        = trimws(df_rows[["Name"]]),
  cafe           = trimws(df_rows[["Cafe name"]]),
  address        = df_rows[["Cafe address (Google Maps)"]],
  district_raw   = df_rows[["District (number)"]],
  espresso_raw   = df_rows[["Espresso Price in CZK (dine-in)"]],
  cappuccino_raw = df_rows[["Cappuccino Price in CZK (normal milk, dine-in)"]],
  flat_white_raw = df_rows[["Flat White Price in CZK (normal milk, dine-in)"]],
  seats_inside   = df_rows[["Seating Places Inside"]],
  seats_outside  = df_rows[["Seating Places Outside"]],
  rating         = df_rows[["Rating (Google)"]],
  distance_raw   = df_rows[["Distance from Pražský Orloj"]]
)

## 3c. repeated cafes
# exact copies of a whole row?
sum(duplicated(df))   # 0

# but 11 cafes were visited by TWO students, so they appear twice with
# slightly different spellings and values. we found them by comparing the
# addresses: each pair has the same address and (almost) the same number
# of Google ratings. same-looking names are not always the same cafe: the
# three "EMA espresso bar" rows have different addresses (three branches),
# so they all stay.
# DECISION: keep the FIRST entry of each pair in the sheet, drop the second
repeated <- data.frame(
  student = c("Anthony George", "Eden Rae Warner", "Sawyer Gatdula",
              "Keira McManus",  "Keira McManus",   "Anthony George",
              "Keira McManus",  "Sawyer Gatdula",  "Sawyer Gatdula",
              "Sawyer Gatdula", "Tyler Bannon"),
  cafe    = c("cafe louvre",    "Starbucks Reserve", "Caffé Milani",
              "Café Savoy",     "Kampa Cafe",        "caffe vescovi",
              "Duo cafe & bistro Plzeňská", "Cafe-Cafe", "MONDIEU",
              "Republica Coffee", "No26 Espresso")
)

# TRUE for the rows whose student + cafe pair is in the list above
is_repeat <- paste(df$student, df$cafe) %in% paste(repeated$student, repeated$cafe)
sum(is_repeat)                                  # should be 11
df[is_repeat, c("student", "cafe", "address")]  # look at them before removing

df <- df[!is_repeat, ]
nrow(df)   # 189 unique cafes


# ============================================================
# 4. CLEAN THE DISTRICT
# ============================================================
# NEW column, the original stays for comparison. keep only the digits of
# the label and rebuild it: "prauge 1" -> "1" -> "Prague 1"
df$district <- paste("Prague", gsub("[^0-9]", "", df$district_raw))

table(df$district_raw, df$district)   # check: every raw label landed in the right district

# five cafes have a district that contradicts their own address: four Mala
# Strana cafes (postcode 118 00 / 110 00, which is Prague 1) were entered
# as Prague 2, and one Kosire cafe (150 00 Praha 5) as Prague 7
df[df$cafe %in% c("Bistro Monk", "The Miners Charles Bridge", "Stranou",
                  "Dobra Trafika", "Tomcat coffee Košíře"),
   c("cafe", "address", "district")]   # look first

df$district[df$cafe %in% c("Bistro Monk", "The Miners Charles Bridge",
                           "Stranou", "Dobra Trafika")] <- "Prague 1"
df$district[df$cafe == "Tomcat coffee Košíře"] <- "Prague 5"

table(df$district)


# ============================================================
# 5. CLEAN THE PRICES
# ============================================================
# first, list every price cell that is not a plain number
df$espresso_raw[grepl("[^0-9]", df$espresso_raw)]
df$cappuccino_raw[grepl("[^0-9]", df$cappuccino_raw)]
df$flat_white_raw[grepl("[^0-9]", df$flat_white_raw)]

# the problems: several sizes in one cell ("85, 95, 105", "65,85"), a
# dine-in/take-away pair ("55/30"), and "na" in lower case.
# DECISION (the default in the guidelines): keep the FIRST listed price
first_price <- function(x) {
  x <- trimws(tolower(x))
  x[x %in% c("", "na")] <- NA   # an empty cell or "na" becomes a real missing value
  x <- gsub("/", ",", x)        # "55/30" -> "55,30", so "/" works like a comma
  x <- sub(",.*", "", x)        # delete the first comma and everything after it: "85, 95, 105" -> "85"
  as.numeric(x)                 # text -> number
}

# NEW columns, the _raw originals stay
df$espresso   <- first_price(df$espresso_raw)
df$cappuccino <- first_price(df$cappuccino_raw)
df$flat_white <- first_price(df$flat_white_raw)

# check: print the same problem rows again, before (_raw) and after
had_problem <- grepl("[^0-9]", df$espresso_raw) |
               grepl("[^0-9]", df$cappuccino_raw) |
               grepl("[^0-9]", df$flat_white_raw)
df[had_problem, c("cafe", "espresso_raw", "espresso", "cappuccino_raw",
                  "cappuccino", "flat_white_raw", "flat_white")]


# ============================================================
# 6. CLEAN THE DISTANCE FROM THE ORLOJ
# ============================================================
# stored as text only because of "#VALUE!" in the empty rows, which are gone
df$distance_raw[grepl("[^0-9.]", df$distance_raw)]   # nothing left that is not a number
df$distance_km <- as.numeric(df$distance_raw)        # NEW column, in km


# ============================================================
# 7. CREATE THE VARIABLES THE QUESTIONS NEED
# ============================================================
# 0/1 dummies (1 = yes). a missing value stays missing: if seats_outside
# is NA, has_outdoor is NA too (we never turn NA into 0)

# A2, A9: does the cafe have outdoor seating? (at least one seat outside)
df$has_outdoor <- ifelse(df$seats_outside > 0, 1L, 0L)

# A5: small cafe = 15 or fewer seats inside
df$small_cafe <- ifelse(df$seats_inside <= 15, 1L, 0L)

# A5: top-rated = Google rating of 4.7 or higher
df$top_rated <- ifelse(df$rating >= 4.7, 1L, 0L)

# A6-A10: Price Level = average of the espresso, cappuccino and flat white
# prices. if one of the three is missing the sum is NA, so the Price Level
# is computed only for cafes with all three prices, as the guidelines say
df$price_level <- (df$espresso + df$cappuccino + df$flat_white) / 3

# A10: above-median Price Level. the median is computed here, never typed in
median_price_level <- median(df$price_level, na.rm = TRUE)
median_price_level
df$above_median_price <- ifelse(df$price_level > median_price_level, 1L, 0L)


# ============================================================
# 8. CHECK THE RESULT
# ============================================================
c(rows_raw = nrow(df_raw), rows_clean = nrow(df))   # 299 -> 189

# keep the cleaned columns only (the _raw originals were for checking)
dataset <- df[c("student", "cafe", "district",
                "seats_inside", "seats_outside", "has_outdoor", "small_cafe",
                "rating", "top_rated",
                "espresso", "cappuccino", "flat_white",
                "price_level", "above_median_price", "distance_km")]

colSums(is.na(dataset))   # missing values per column, after cleaning
summary(dataset)          # ranges: seats >= 0, ratings 1-5, prices positive and plausible
table(dataset$district)


# ============================================================
# 9. WRITE THE CLEANED DATA SET
# ============================================================
# row.names = FALSE: no extra column with row numbers
write.csv(dataset, file.path("analysis", "dataset.csv"), row.names = FALSE)
