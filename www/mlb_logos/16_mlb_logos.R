# Install once if needed:
# install.packages("curl")

library(curl)

dir.create(
  "www/mlb_logos",
  recursive = TRUE,
  showWarnings = FALSE
)

logo_urls <- c(
  "arizona_diamondbacks.png" =
    "https://logos-world.net/wp-content/uploads/2020/05/Arizona-Diamondbacks-Symbol.png",

  "chicago_cubs.png" =
    "https://cdn.freebiesupply.com/images/large/2x/chicago-cubs-bear-logo.png",

  "chicago_white_sox.png" =
    "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c1/Chicago_White_Sox.svg/500px-Chicago_White_Sox.svg.png",

  "cincinnati_reds.png" =
    "https://upload.wikimedia.org/wikipedia/commons/thumb/0/01/Cincinnati_Reds_Logo.svg/1280px-Cincinnati_Reds_Logo.svg.png",

  "los_angeles_angels.png" =
    "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8b/Los_Angeles_Angels_of_Anaheim.svg/250px-Los_Angeles_Angels_of_Anaheim.svg.png?_=20100410173154",

  "milwaukee_brewers.png" =
    "https://logodownload.org/wp-content/uploads/2020/10/milwaukee-brewers-logo-1.png",

  "minnesota_twins.png" =
    "https://cdn.freebiesupply.com/images/thumbs/2x/minnesota-twins-logo.png",

  "pittsburgh_pirates.png" =
    "https://cdn.freebiesupply.com/images/large/2x/pittsburgh-pirates-logo-transparent.png"
)

# Delete any old/corrupt copies first
old_files <- file.path("www/mlb_logos", names(logo_urls))
unlink(old_files[file.exists(old_files)])

download_logo <- function(filename, url) {

  destination <- file.path("www/mlb_logos", filename)

  message("Downloading: ", filename)

  handle <- new_handle(
    useragent = paste(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      "AppleWebKit/537.36 Chrome/126.0 Safari/537.36"
    ),
    followlocation = TRUE
  )

  tryCatch({

    curl_download(
      url = url,
      destfile = destination,
      quiet = FALSE,
      mode = "wb",
      handle = handle
    )

    size <- file.info(destination)$size

    # PNG files begin with this 8-byte signature
    connection <- file(destination, "rb")
    signature <- readBin(connection, what = "raw", n = 8)
    close(connection)

    expected_signature <- as.raw(
      c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)
    )

    valid_png <- identical(signature, expected_signature)

    if (!valid_png || is.na(size) || size < 1000) {
      unlink(destination)

      message(
        "FAILED validation: ",
        filename,
        " was not downloaded as a valid PNG."
      )

      return(FALSE)
    }

    message(
      "Success: ",
      filename,
      " (",
      round(size / 1024, 1),
      " KB)"
    )

    TRUE

  }, error = function(e) {

    if (file.exists(destination)) {
      unlink(destination)
    }

    message("FAILED: ", filename, " — ", e$message)
    FALSE
  })
}

results <- mapply(
  FUN = download_logo,
  filename = names(logo_urls),
  url = unname(logo_urls),
  USE.NAMES = TRUE
)

cat("\nDownload results:\n")
print(results)

cat("\nValid files now in www/mlb_logos:\n")
print(list.files("www/mlb_logos"))