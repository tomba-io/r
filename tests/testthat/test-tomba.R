# ---------------------------------------------------------------------------
# Unit tests for the tomba R package
# ---------------------------------------------------------------------------
# Tests are split into two groups:
#   1. Offline tests -- always run, verify object construction and path logic.
#   2. Integration tests -- skipped unless TOMBA_API_KEY and TOMBA_SECRET_KEY are set.
# ---------------------------------------------------------------------------

# ===========================================================================
# Helper: create a Tomba client from env-vars (or skip)
# ===========================================================================

has_credentials <- function() {
  nchar(Sys.getenv("TOMBA_API_KEY")) > 0 && nchar(Sys.getenv("TOMBA_SECRET_KEY")) > 0
}

make_client <- function() {
  Tomba(
    key    = Sys.getenv("TOMBA_API_KEY"),
    secret = Sys.getenv("TOMBA_SECRET_KEY")
  )
}

# ===========================================================================
# Offline: Tomba class construction
# ===========================================================================

test_that("Tomba class stores key and secret", {
  cl <- Tomba(key = "ta_test_key", secret = "ts_test_secret")
  expect_equal(cl@key, "ta_test_key")
  expect_equal(cl@secret, "ts_test_secret")
})

test_that("Tomba class accepts empty strings", {
  cl <- Tomba(key = "", secret = "")
  expect_equal(cl@key, "")
  expect_equal(cl@secret, "")
})

# ===========================================================================
# Offline: Constants are correct
# ===========================================================================

test_that("DEFAULT_BASE_URL ends with /v1/", {
  expect_match(tomba:::DEFAULT_BASE_URL, "/v1/$")
})

test_that("Path constants have no trailing slashes (except base URL)", {
  paths <- c(
    tomba:::ACCOUNT_PATH, tomba:::USAGE_PATH, tomba:::LOGS_PATH,
    tomba:::SEARCH_PATH, tomba:::FINDER_PATH,
    tomba:::AUTHOR_PATH, tomba:::LINKEDIN_PATH, tomba:::PHONE_PATH,
    tomba:::PHONE_VALIDATOR_PATH,
    tomba:::VERIFIER_PATH, tomba:::SOURCES_PATH, tomba:::COUNT_PATH,
    tomba:::STATUS_PATH,
    tomba:::AUTOCOMPLETE_PATH, tomba:::FORMAT_PATH, tomba:::LOCATION_PATH,
    tomba:::SIMILAR_PATH,
    tomba:::TECHNOLOGY_PATH, tomba:::PERSON_PATH, tomba:::COMPANY_PATH,
    tomba:::COMBINED_PATH,
    tomba:::REVEAL_PATH, tomba:::KEYS_PATH, tomba:::FLAG_PATH,
    tomba:::LEADS_PATH,
    tomba:::LEADS_LISTS_PATH, tomba:::LEADS_ATTRIBUTES_PATH,
    tomba:::BULK_PATH
  )
  for (p in paths) {
    expect_false(grepl("/$", p), info = paste("Path ends with slash:", p))
  }
})

test_that("AUTOCOMPLETE_PATH is domain-suggestions (not domains-suggestion)", {
  expect_equal(tomba:::AUTOCOMPLETE_PATH, "domain-suggestions")
})

test_that("COUNT_PATH has no trailing slash", {
  expect_equal(tomba:::COUNT_PATH, "email-count")
})

test_that("STATUS_PATH has no trailing slash", {
  expect_equal(tomba:::STATUS_PATH, "domain-status")
})

test_that("VALID_BULK_TYPES contains expected entries", {
  expect_true("search"           %in% tomba:::VALID_BULK_TYPES)
  expect_true("similar"          %in% tomba:::VALID_BULK_TYPES)
  expect_true("company"          %in% tomba:::VALID_BULK_TYPES)
  expect_true("finder"           %in% tomba:::VALID_BULK_TYPES)
  expect_true("enrich"           %in% tomba:::VALID_BULK_TYPES)
  expect_true("linkedin"         %in% tomba:::VALID_BULK_TYPES)
  expect_true("author"           %in% tomba:::VALID_BULK_TYPES)
  expect_true("verifier"         %in% tomba:::VALID_BULK_TYPES)
  expect_true("phone-finder"     %in% tomba:::VALID_BULK_TYPES)
  expect_true("phone-validator"  %in% tomba:::VALID_BULK_TYPES)
})

# ===========================================================================
# Offline: bulk type validation
# ===========================================================================

test_that(".validate_bulk_type rejects invalid types", {
  expect_error(tomba:::.validate_bulk_type("invalid-type"), "Invalid bulk_type")
  expect_error(tomba:::.validate_bulk_type(NULL), "Invalid bulk_type")
})

test_that(".validate_bulk_type accepts valid types", {
  for (bt in tomba:::VALID_BULK_TYPES) {
    expect_silent(tomba:::.validate_bulk_type(bt))
  }
})

# ===========================================================================
# Offline: internal helpers exist
# ===========================================================================

test_that(".tomba_headers returns correct header names", {
  cl <- Tomba(key = "ta_k", secret = "ts_s")
  h <- tomba:::.tomba_headers(cl)
  expect_true("X-Tomba-Key"    %in% names(h))
  expect_true("X-Tomba-Secret" %in% names(h))
  expect_true("User-Agent"     %in% names(h))
  expect_equal(unname(h["X-Tomba-Key"]),    "ta_k")
  expect_equal(unname(h["X-Tomba-Secret"]), "ts_s")
})

# ===========================================================================
# Integration: Account / Usage / Logs
# ===========================================================================

test_that("account works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- account(cl)
  expect_type(result, "list")
})

test_that("usage works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- usage(cl)
  expect_type(result, "list")
})

test_that("logs works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- logs(cl)
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Search / Finder
# ===========================================================================

test_that("domain_search works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- domain_search(cl, domain = "tomba.io")
  expect_type(result, "list")
})

test_that("email_finder works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- email_finder(cl,
    domain = "tomba.io",
    fname  = "Mohamed",
    lname  = "Ben rebia"
  )
  expect_type(result, "list")
})

test_that("author_finder works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- author_finder(cl,
    url = "https://clearbit.com/blog/company-name-to-domain-api"
  )
  expect_type(result, "list")
})

test_that("linkedin_finder works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- linkedin_finder(cl,
    url = "https://www.linkedin.com/in/alex-maccaw-ab592978"
  )
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Phone
# ===========================================================================

test_that("phone_finder works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- phone_finder(cl, email = "b.mohamed@tomba.io")
  expect_type(result, "list")
})

test_that("phone_validator works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- phone_validator(cl, phone = "+16502530000", country_code = "US")
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Verifier / Sources
# ===========================================================================

test_that("email_verifier works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- email_verifier(cl, email = "b.mohamed@tomba.io")
  expect_type(result, "list")
})

test_that("email_sources works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- email_sources(cl, email = "b.mohamed@tomba.io")
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Count / Status / Autocomplete
# ===========================================================================

test_that("count works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- count(cl, domain = "tomba.io")
  expect_type(result, "list")
})

test_that("status works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- status(cl, domain = "gmail.com")
  expect_type(result, "list")
})

test_that("autocomplete works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- autocomplete(cl, search = "google")
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Format / Location / Similar / Technology
# ===========================================================================

test_that("email_format works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- email_format(cl, domain = "tomba.io")
  expect_type(result, "list")
})

test_that("get_location works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- get_location(cl, domain = "tomba.io")
  expect_type(result, "list")
})

test_that("similar_domains works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- similar_domains(cl, domain = "tomba.io")
  expect_type(result, "list")
})

test_that("technology_check works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- technology_check(cl, domain = "tomba.io")
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Enrichment
# ===========================================================================

test_that("person_find works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- person_find(cl, email = "b.mohamed@tomba.io")
  expect_type(result, "list")
})

test_that("company_find works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- company_find(cl, domain = "tomba.io")
  expect_type(result, "list")
})

test_that("combined_find works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- combined_find(cl, email = "b.mohamed@tomba.io")
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Keys
# ===========================================================================

test_that("list_keys works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- list_keys(cl)
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Flags
# ===========================================================================

test_that("list_flags works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- list_flags(cl)
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Leads
# ===========================================================================

test_that("list_leads works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- list_leads(cl)
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Leads Lists
# ===========================================================================

test_that("list_leads_lists works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- list_leads_lists(cl)
  expect_type(result, "list")
})

# ===========================================================================
# Integration: Lead Attributes
# ===========================================================================

test_that("list_lead_attributes works", {
  skip_if_not(has_credentials(), "No API credentials")
  cl <- make_client()
  result <- list_lead_attributes(cl)
  expect_type(result, "list")
})
