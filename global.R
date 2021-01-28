
################################QC metrics object for advanced settings###################################

lst_qcMetrics <- PTXQC:::getMetricsObjects(FALSE)

##ordering the metrics
df.meta <- PTXQC:::getMetaData(lst_qcMetrics = lst_qcMetrics)
lst_qcMetrics_ord <- gsub("qcMetric_", "", names(lst_qcMetrics[df.meta$.id]))


##########################################url, file paths####################################################

yamlpath <- tempfile("yamldefault.yaml", tempdir())


