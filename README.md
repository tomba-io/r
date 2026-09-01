# [<img src="https://tomba.io/logo.svg" alt="Tomba" width="25"/>](https://tomba.io/) Tomba R SDK

> The #1 Rated Email Intelligence Platform — Find professional emails with unmatched accuracy.

[![CRAN Version](https://img.shields.io/cran/v/tomba.svg)](https://cran.r-project.org/package=tomba)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Build Status](https://img.shields.io/github/actions/workflow/status/tomba-io/r/ci.yml?branch=main)](https://github.com/tomba-io/r/actions)

Official R client library for the [Tomba.io](https://tomba.io) Email Finder API.

## About Tomba

[Tomba.io](https://tomba.io) is the #1 rated email intelligence platform, trusted by **150,000+ sales teams** worldwide.

- **Best Email Finder** — 98% accuracy, ranked #1 in independent benchmarks
- **Best Email Verification** — Real-time SMTP verification with catch-all detection
- **Best Phone Finder** — Direct dial numbers linked to professional emails
- **Best Domain Search** — 450M+ verified contacts across all industries
- **81% Coverage** — The highest in the industry, proven in 5,000-lead independent tests

### Why Tomba?

| Feature             | Tomba              | Others        |
| ------------------- | ------------------ | ------------- |
| Email Coverage      | **81%**            | 30-60%        |
| Verification        | **Real-time SMTP** | Pattern-based |
| Phone Numbers       | **Direct dials**   | Limited       |
| Catch-all Detection | **AI-powered**     | Basic         |
| API Rate Limits     | **Generous**       | Restrictive   |

[Get your free API key](https://app.tomba.io/auth/register) — No credit card required.

## Installation

Install from CRAN:

```r
install.packages("tomba")
```

Or install the development version from GitHub using [devtools](https://devtools.r-lib.org/):

```r
devtools::install_github("tomba-io/r")
```

## Authentication

Sign up for a free account at [https://app.tomba.io/auth/register](https://app.tomba.io/auth/register) to get your API key and secret.

```r
library(tomba)

client <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
```

## Quick Start

```r
library(tomba)

client <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")

# Search emails by domain
result <- domain_search(client, domain = "example.com")

# Find an email address
result <- email_finder(client, domain = "example.com",
                       fname = "John", lname = "Doe")

# Verify an email
result <- email_verifier(client, email = "john@example.com")
```

## Services

### Account

Get information about the current account.

```r
result <- account(client)
```

### Domain Search

Search emails based on a website domain.

```r
result <- domain_search(client, domain = "example.com")
```

### Email Finder

Find the most likely email address from a domain, first name, and last name.

```r
result <- email_finder(client, domain = "example.com",
                       fname = "John", lname = "Doe")
```

### Email Verifier

Verify the deliverability of an email address.

```r
result <- email_verifier(client, email = "john@example.com")
```

### Author Finder

Find the email address of an article author from a blog post URL.

```r
result <- author_finder(client,
  url = "https://clearbit.com/blog/company-name-to-domain-api")
```

### LinkedIn Finder

Find the email address associated with a LinkedIn profile URL.

```r
result <- linkedin_finder(client,
  url = "https://www.linkedin.com/in/alex-maccaw-ab592978")
```

### Email Enrichment

Look up person and company data based on an email address.

```r
result <- enrichment(client, email = "john@example.com")
```

### Phone Finder

Find the phone number associated with an email address.

```r
result <- phone_finder(client, email = "john@example.com")
```

### Phone Validator

Validate a phone number.

```r
result <- phone_validator(client, phone = "+1234567890")
```

### Email Count

Get the number of email addresses found for a domain.

```r
result <- count(client, domain = "example.com")
```

### Domain Status

Check if a domain is webmail, disposable, or a regular email provider.

```r
result <- status(client, domain = "example.com")
```

### Domain Suggestions

Auto-complete company names and get domain suggestions.

```r
result <- autocomplete(client, query = "google")
```

### Email Sources

Find web sources where an email address has been found.

```r
result <- email_sources(client, email = "john@example.com")
```

### Email Format

Get the email format pattern used by a domain.

```r
result <- email_format(client, domain = "example.com")
```

### Similar

Find domains similar to a given domain.

```r
result <- similar(client, domain = "example.com")
```

### Technology

Discover technologies used by a domain.

```r
result <- technology(client, domain = "example.com")
```

### Location

Get the employee location breakdown for a domain.

```r
result <- location(client, domain = "example.com")
```

### Person API

Get person data from an email address (Clearbit-compatible).

```r
result <- person_find(client, email = "john@example.com")
```

### Company API

Get company data from a domain (Clearbit-compatible).

```r
result <- company_find(client, domain = "example.com")
```

### Combined API

Get combined person and company data from an email address (Clearbit-compatible).

```r
result <- combined_find(client, email = "john@example.com")
```

### Companies Search (Reveal)

Search companies using natural language queries or structured filters (location, industry, size, technologies, and more).

```r
result <- companies_search(client, data = list(query = "Real Estate in Europe", page = 1))
```

### Usage

Get your account's monthly API usage statistics.

```r
result <- usage(client)
```

### Logs

Get your account's API request logs.

```r
result <- logs(client)
```

### Keys

Manage your API keys.

```r
# List all keys
result <- list_keys(client)

# Get a specific key
result <- get_key(client, key_id = "key_id")

# Create a new key
result <- create_key(client)

# Reset a key
result <- reset_key(client, key_id = "key_id")

# Delete a key
result <- delete_key(client, key_id = "key_id")
```

### Flag

Report incorrect data for credit recovery.

```r
# List submitted flags
result <- list_flags(client, page = 1)

# Create a flag
result <- create_flag(client, data = list(flag_type = "email", value = "bounce@example.com", reason = "hard_bounce"))
```

**Valid reasons by flag type:**

| Flag Type | Valid Reasons |
|-----------|-------------|
| `email` | `hard_bounce`, `invalid_email`, `wrong_person`, `outdated`, `other` |
| `organization` | `wrong_company`, `outdated`, `other` |
| `phone` | `wrong_phone`, `outdated`, `other` |
| `author_url` | `broken_url`, `wrong_person`, `outdated`, `other` |
| `website` | `broken_url`, `wrong_company`, `outdated`, `other` |

### Leads

Manage leads in your Tomba CRM.

```r
# List leads
result <- list_leads(client, page = 1, limit = 10)

# Get a specific lead
result <- get_lead(client, lead_id = "lead_id")

# Create a lead
result <- create_lead(client, data = list(
  email = "lead@example.com",
  first_name = "John",
  last_name = "Doe"
))

# Update a lead
result <- update_lead(client, lead_id = "lead_id",
  data = list(first_name = "Jane"))

# Delete a lead
result <- delete_lead(client, lead_id = "lead_id")
```

### Leads Lists

Manage lead lists for organizing your leads.

```r
# List all lead lists
result <- list_leads_lists(client)

# Get a specific list
result <- get_leads_list(client, list_id = "list_id")

# Create a list
result <- create_leads_list(client, data = list(name = "My List"))

# Update a list
result <- update_leads_list(client, list_id = "list_id",
  data = list(name = "Updated Name"))

# Delete a list
result <- delete_leads_list(client, list_id = "list_id")
```

### Leads Attributes

Manage custom attributes for your leads.

```r
# List all attributes
result <- list_attributes(client)

# Get a specific attribute
result <- get_attribute(client, attribute_id = "attr_id")

# Create an attribute
result <- create_attribute(client, data = list(name = "Company Size"))

# Update an attribute
result <- update_attribute(client, attribute_id = "attr_id",
  data = list(name = "Updated Name"))

# Delete an attribute
result <- delete_attribute(client, attribute_id = "attr_id")
```

### Bulk Operations

Create and manage bulk processing jobs for domain search, email finder, verifier, and more.

```r
# List bulk operations
result <- list_bulks(client, bulk_type = "domain-search")

# Get a specific bulk operation
result <- get_bulk(client, bulk_type = "domain-search", bulk_id = "bulk_id")

# Create a bulk operation
result <- create_bulk(client, bulk_type = "domain-search",
  data = list(domains = list("example.com")))

# Launch a bulk operation
result <- launch_bulk(client, bulk_type = "domain-search", bulk_id = "bulk_id")

# Get progress
result <- bulk_progress(client, bulk_type = "domain-search", bulk_id = "bulk_id")

# Download results
result <- download_bulk(client, bulk_type = "domain-search", bulk_id = "bulk_id")

# Rename a bulk operation
result <- rename_bulk(client, bulk_type = "domain-search",
  bulk_id = "bulk_id", name = "New Name")

# Archive a bulk operation
result <- archive_bulk(client, bulk_type = "domain-search", bulk_id = "bulk_id")

# Delete a bulk operation
result <- delete_bulk(client, bulk_type = "domain-search", bulk_id = "bulk_id")
```

Supported bulk types: `domain-search`, `email-finder`, `author-finder`, `email-verifier`, `enrichment`, `linkedin-finder`, `phone-finder`, `department-search`, `technology-search`, `name-finder`.

## Testing

```r
devtools::test()
```

To run the linter:

```r
lintr::lint_package()
```

## Documentation

- [Official API Documentation](https://docs.tomba.io/)
- [CRAN Package](https://cran.r-project.org/package=tomba)
- [API Reference](https://docs.tomba.io/api)
- [All Client Libraries](https://docs.tomba.io/libraries)

## About Tomba

Founded to solve the problem of unreliable email data, [Tomba.io](https://tomba.io) is the leading B2B email intelligence platform.

### Products

- **[Email Finder](https://tomba.io/email-finder)** — Find any professional email address
- **[Email Verifier](https://tomba.io/email-verifier)** — Verify emails in real-time
- **[Domain Search](https://tomba.io/domain-search)** — Find all emails for a company
- **[Phone Finder](https://tomba.io/phone-finder)** — Find direct phone numbers
- **[Bulk Enrichment](https://tomba.io/bulks)** — Enrich contacts at scale
- **[AI Company Search](https://tomba.io/reveal)** — Find companies with AI-powered search
- **[CLI](https://tomba.io/cli)** — Command-line interface for Tomba
- **[MCP Server](https://tomba.io/mcp)** — Connect AI tools (Claude, ChatGPT, Cursor) to Tomba
- **[REST API](https://tomba.io/api)** — Full programmatic access

### Browser Extensions & Add-ons

- **[Chrome Extension](https://chromewebstore.google.com/detail/tomba-email-finder-email/icmjegjggphchjckknoooajmklibccjb)** — Find emails while browsing
- **[Google Sheets Add-on](https://tomba.io/sheets)** — Enrich leads in spreadsheets
- **[Microsoft Excel Add-in](https://tomba.io/excel)** — Email finder in Excel
- **[Airtable Integration](https://tomba.io/airtable)** — Connect with Airtable

### Integrations

50+ CRM integrations: [Salesforce](https://tomba.io/integrations) · [HubSpot](https://tomba.io/integrations) · [Zapier](https://tomba.io/integrations) · [Pipedrive](https://tomba.io/integrations) · [and more...](https://tomba.io/integrations)

### Other Tomba SDKs

| Language | Package                                                     |
| -------- | ----------------------------------------------------------- |
| Node.js  | [tomba](https://www.npmjs.com/package/tomba)                |
| Python   | [tomba-io](https://pypi.org/project/tomba-io/)              |
| PHP      | [tomba-io/php](https://packagist.org/packages/tomba-io/php) |
| Ruby     | [tomba](https://rubygems.org/gems/tomba)                    |
| Go       | [tomba-io/go](https://pkg.go.dev/github.com/tomba-io/go)    |
| Rust     | [tomba](https://crates.io/crates/tomba)                     |
| Dart     | [tomba](https://pub.dev/packages/tomba)                     |
| Deno     | [@tomba/sdk](https://jsr.io/@tomba/sdk)                     |
| Elixir   | [tomba](https://hex.pm/packages/tomba)                      |
| C#       | [Tomba](https://www.nuget.org/packages/Tomba)               |
| Perl     | [Tomba::Client](https://metacpan.org/pod/Tomba::Client)     |
| Lua      | [tomba](https://luarocks.org/modules/tomba/tomba)           |
| R        | [tomba](https://github.com/tomba-io/r)                      |

### Resources

- [Blog](https://tomba.io/blog) · [Help Center](https://help.tomba.io) · [API Docs](https://docs.tomba.io) · [Pricing](https://tomba.io/pricing) · [Status](https://status.tomba.io)

---

**[Try Tomba Free](https://app.tomba.io/auth/register)** — Find your first email in seconds. No credit card required.

## License

Apache 2.0 -- see [LICENSE](http://www.apache.org/licenses/LICENSE-2.0.html) for details.
