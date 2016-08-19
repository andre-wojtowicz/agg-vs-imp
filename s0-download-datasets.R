# ---- step-0-download-datasets ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S0.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 0: download datasets")

if (!dir.exists(DATASETS.DIR))
{
    dir.create(DATASETS.DIR)
}

flog.info("Downloading datasets:")
flog.info(DATASETS.URL)

temp.dir  = tempdir()
temp.file = tempfile()
content   = getBinaryURL(DATASETS.URL,
                         .opts = curlOptions(followlocation = TRUE),
                         httpheader = c("User-Agent" = "R"))

writeBin(content, temp.file)

datasets.files = paste0(DATASETS.NAMES, ".rds")

unzip(zipfile = temp.file,
      files   = datasets.files,
      exdir   = temp.dir)

unlink(temp.file)

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.zip.file.path =
        file.path(temp.dir, datasets.files[which(DATASETS.NAMES == dataset.name)])

    dataset.destination.file.path =
        replace.strings(DATASETS.NAME.PATTERN, dataset.name, DATASETS.ORIGIN)


    if (!file.exists(dataset.destination.file.path) | OVERWRITE.OUTPUT.FILES)
    {
        flog.info("Saving dataset")

        file.rename(dataset.zip.file.path,
                    dataset.destination.file.path)

    } else {
        flog.warn("Dataset exists, skipping")
    }
}

flog.debug("Temp files and directories cleanup")

unlink(temp.dir)
