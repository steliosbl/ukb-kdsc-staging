#' Upload a local file to the DNAnexus project
upload_to_dx <- function(in_path, dest_path, overwrite = TRUE) {
  Sys.unsetenv("DX_WORKSPACE_ID")
  system("dx cd $DX_PROJECT_CONTEXT_ID")
  if (overwrite) {
    system(sprintf("dx rm %s", dest_path))
  }
  system(sprintf("dx mkdir -p %s", dirname(dest_path)))
  system(
    sprintf("dx upload -r %s --destination %s", in_path, dest_path)
  )
}

#' Download a file from DNAnexus to local disk
download_from_dx <- function(in_path, dest_path) {
  Sys.unsetenv("DX_WORKSPACE_ID")
  system("dx cd $DX_PROJECT_CONTEXT_ID")
  system(
    sprintf("dx download %s -o %s -r -f", in_path, dest_path)
  )
}

#' Extract the UKB data dictionary (for field extraction)
#' Returns the data dictionary as a data.table
get_data_dictionary <- function() {
  if (length(list.files("data/", pattern = "data_dictionary")) == 0) {
    if (system("python3 -c 'import pandas'", ignore.stderr = TRUE)) {
      system("pip install pandas", wait = TRUE)
    }
    Sys.unsetenv("DX_WORKSPACE_ID")
    system("dx cd $DX_PROJECT_CONTEXT_ID")
    project_files <- system("dx ls", intern = TRUE)
    dataset_file <- project_files[grepl("^app", project_files) & grepl("\\.dataset$", project_files)]
    system(sprintf("dx extract_dataset %s -ddd -o data/", dataset_file), wait = TRUE)
  }
  data_dict_file <- list.files("data/", pattern = "data_dictionary")
  fread(paste0("data/", data_dict_file))
}

#' Extract the UKB codings dictionary
get_codings_dictionary <- function() {
  if (length(list.files("data/", pattern = "codings")) == 0) {
    if (system("python3 -c 'import pandas'", ignore.stderr = TRUE)) {
      system("pip install pandas", wait = TRUE)
    }
    Sys.unsetenv("DX_WORKSPACE_ID")
    system("dx cd $DX_PROJECT_CONTEXT_ID")
    project_files <- system("dx ls", intern = TRUE)
    dataset_file <- project_files[grepl("^app", project_files) & grepl("\\.dataset$", project_files)]
    system(sprintf("dx extract_dataset %s -ddd -o data/", dataset_file), wait = TRUE)
  }
  code_dict_file <- list.files("data/", pattern = "codings")
  fread(paste0("data/", code_dict_file))
}

#' Run dx table-exporter for a set of fields and wait for completion
#'
#' @param field_file Path to the field list file
#' @param entity Entity type to extract
#' @param output_name Output folder name on DNAnexus
#' @param dest_folder Local destination folder for the download
#' @param instance_type DNAnexus instance type
#' @param timeout Maximum wait time in seconds
run_table_exporter <- function(field_file, entity, output_name, dest_folder,
                               instance_type = "mem1_ssd1_v2_x8",
                               timeout = 3600) {
  Sys.unsetenv("DX_WORKSPACE_ID")
  system("dx cd $DX_PROJECT_CONTEXT_ID")
  # Detect dataset file
  dataset_file <- system("dx ls *.dataset", intern = TRUE)

  # Upload field list and capture file ID
  file_id <- system(sprintf("dx upload %s --destination ukb-kdsc-staging/extracted/ --brief", field_file), intern = TRUE)

  # Run table exporter (pass uploaded file via -field_names_file_txt)
  cmd <- sprintf(
    "dx run table-exporter -idataset_or_cohort_or_dashboard=%s -ientity=%s -ifield_names_file_txt=%s --destination=%s --instance-type %s --brief -y",
    dataset_file, entity, file_id, output_name, instance_type
  )
  job_id <- system(cmd, intern = TRUE)
  cat(sprintf("Launched table-exporter job: %s for entity: %s\n", job_id, entity))

  # Poll for completion
  elapsed <- 0
  while (elapsed < timeout) {
    state <- system(sprintf("dx find jobs --id %s --json | python3 -c \"import sys,json; print(json.load(sys.stdin)[0]['state'])\"", job_id), intern = TRUE)
    if (state == "done") {
      cat(sprintf("Job %s completed successfully.\n", job_id))
      break
    } else if (state == "failed") {
      stop(sprintf("Job %s failed!", job_id))
    }
    Sys.sleep(60)
    elapsed <- elapsed + 60
    cat(sprintf("  Waiting... %d seconds elapsed (state: %s)\n", elapsed, state))
  }

  # Download results
  system(sprintf("mkdir -p %s", dest_folder), wait = TRUE)
  system(sprintf("dx download '%s/*' -o %s/", output_name, dest_folder), wait = TRUE)
}
