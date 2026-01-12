import re
import time
from Bio import Entrez
import pandas as pd


# CONFIG

Entrez.email = "xxxxx"  # Replace with your email
Entrez.tool = "halophilic_metagenome_script"

# Combined PubMed query
search_query = """
("halophilic metagenome" OR "halophiles AND metagenome" OR "halotolerant AND metagenome"
 OR "hypersaline environment metagenome" OR "hypersaline AND metagenome"
 OR "salt environment AND metagenome" OR "salt soil AND metagenome"
 OR "salt tolerant AND metagenome" OR "salt lake AND metagenome"
 OR "salt AND metagenome" OR "saline AND metagenome"
 OR "saline environment AND metagenome" OR "saline soil AND metagenome"
 OR "saline lake AND metagenome")
"""


# SAFE ENTrez ELINK FUNCTION

def safe_elink(dbfrom, id, db=None, linkname=None, retries=3, wait=2):
    """Fetch elink with retries and safe handling if no links exist"""
    for attempt in range(retries):
        try:
            handle = Entrez.elink(dbfrom=dbfrom, id=id, db=db, linkname=linkname)
            result = Entrez.read(handle)
            handle.close()
            return result
        except Exception as e:
            if attempt < retries - 1:
                time.sleep(wait)
            else:
                print(f"Warning: elink failed for PMID {id} ({db}/{linkname}): {e}")
                return None


# SEARCH PUBMED

handle = Entrez.esearch(db="pubmed", term=search_query, retmax=5000)
record = Entrez.read(handle)
id_list = record["IdList"]
handle.close()
print(f"Found {len(id_list)} articles.")

results = []


# PROCESS EACH ARTICLE

for pmid in id_list:
    print(f"Processing PMID {pmid}...")
    try:
        fetch_handle = Entrez.efetch(db="pubmed", id=pmid, rettype="xml", retmode="xml")
        paper = Entrez.read(fetch_handle)
        fetch_handle.close()

        article_data = paper['PubmedArticle'][0]['MedlineCitation']['Article']
        title = article_data.get('ArticleTitle', "N/A")
        abstract = article_data.get('Abstract', {}).get('AbstractText', ["No abstract"])[0]
        pubmed_link = f"https://pubmed.ncbi.nlm.nih.gov/{pmid}/"

    except Exception as e:
        print(f"Error fetching PubMed data for PMID {pmid}: {e}")
        title, abstract, pubmed_link = "N/A", "N/A", "N/A"

    
    # FETCH LINKED BIOPROJECT / SRA
    
    bioprojects, sras = set(), set()

    # BioProject links
    elinks_bp = safe_elink(dbfrom="pubmed", id=pmid, db="bioproject")
    if elinks_bp and 'LinkSetDb' in elinks_bp[0]:
        for linkset in elinks_bp[0]['LinkSetDb']:
            for link in linkset['Link']:
                bioprojects.add(link['Id'])

    # SRA links
    elinks_sra = safe_elink(dbfrom="pubmed", id=pmid, db="sra")
    if elinks_sra and 'LinkSetDb' in elinks_sra[0]:
        for linkset in elinks_sra[0]['LinkSetDb']:
            for link in linkset['Link']:
                sras.add(link['Id'])

    
    # FETCH PMC FULL-TEXT (if available)
   
    bioproject_text, sra_text, biosample_text, genome_text = set(), set(), set(), set()
    try:
        pmc_links = safe_elink(dbfrom="pubmed", id=pmid, db="pmc")
        if pmc_links and pmc_links[0].get("LinkSetDb"):
            pmc_id = pmc_links[0]["LinkSetDb"][0]["Link"][0]["Id"]
            fulltext_handle = Entrez.efetch(db="pmc", id=pmc_id, rettype="full", retmode="xml")
            fulltext = fulltext_handle.read()
            fulltext_handle.close()

            # Regex search for IDs in full text
            bioproject_text.update(re.findall(r"PRJNA\d+", fulltext))
            sra_text.update(re.findall(r"SRR\d+", fulltext))
            biosample_text.update(re.findall(r"SAMN\d+", fulltext))
            genome_text.update(re.findall(r"(GCA_\d+|GCF_\d+)", fulltext))
    except Exception:
        pass

   
    # SAVE RESULTS
    
    results.append({
        "PMID": pmid,
        "Title": title,
        "Abstract": abstract,
        "PubMed Link": pubmed_link,
        "BioProject Links (Entrez)": ", ".join(bioprojects) if bioprojects else "N/A",
        "SRA Links (Entrez)": ", ".join(sras) if sras else "N/A",
        "BioProject IDs (Text)": ", ".join(bioproject_text) if bioproject_text else "N/A",
        "SRA IDs (Text)": ", ".join(sra_text) if sra_text else "N/A",
        "BioSample IDs (Text)": ", ".join(biosample_text) if biosample_text else "N/A",
        "Genome/MAG IDs (Text)": ", ".join(genome_text) if genome_text else "N/A"
    })

    time.sleep(0.5)  # polite delay


# SAVE TO EXCEL

df = pd.DataFrame(results)
df.to_excel("results.xlsx", index=False)
print("✅ Finished! Results saved to results.xlsx")

