# ---- step-0-download-datasets ----

source("init.R")

setup.logger(file.path(LOGGER.OUTPUT.DIR, LOGGER.OUTPUT.S0.FILE),
             LOGGER.OVERWRITE.EXISTING.FILES)

flog.info("Step 0: download datasets")

flog.info("Downloading datasets from:")
flog.info(DATASETS.URL)

temp.dir  = tempdir()
temp.file = tempfile()
content   = getBinaryURL(DATASETS.URL,
                         .opts = curlOptions(followlocation = TRUE),
                         httpheader = c("User-Agent" = "R"))

writeBin(content, temp.file)

datasets.zip.paths = as.character(
                        sapply(DATASETS.NAMES,
                            function(dataset.name){
                                file.path("data-collection", dataset.name,
                                          "preprocessed", "dataset.rds")}))

unzip(zipfile = temp.file,
      files   = datasets.zip.paths,
      exdir   = temp.dir)

unlink(temp.file)

for (dataset.name in DATASETS.NAMES)
{
    flog.info(paste("Dataset:", dataset.name))

    dataset.zip.file.path =
        file.path(temp.dir, datasets.zip.paths[which(DATASETS.NAMES ==
                                                     dataset.name)])

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

removeDirectory(file.path(temp.dir, "data-collection"), recursive = TRUE)

unlink(temp.dir)
