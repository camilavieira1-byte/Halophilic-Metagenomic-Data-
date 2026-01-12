conda create -n ncbi_tools -c bioconda entrez-direct
conda activate ncbi_tools

efetch -db biosample -input unique_biosamples.txt -format native > biosamples_extracted.txt

