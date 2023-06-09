# cleaning-keyword-data
As part of my work with the Data-Driven EnviroLab, I looked into the publication landscape of climate accounting technologies. For this project, I used Publish or Perish to extract abstracts, author keywords, and index keywords for several thousand publications on climate-relevant systems like blockchain accounting from SCOPUS, Web of Science, and Google Scholar. I applied Natural Language Processing to these keywords to highlight patterns in discourse surrounding and types of papers published on climate accounting. 

Unfortunately, these databases needed a gargantuan amount of cleaning. Many keywords were featured as variations of each other; for example, the word "blockchain" was also featured as "blockchain technology," "blockchains," "the blockchain," and even "block chain," among many, many other derivations. Similarly, some keywords were coded alpha-numerically (e.g. "1_climate" and "COP-26") and needed to be addressed. To tackle these issues, I wrote several lines of code to group keywords that referred to the same concept, using `grep` and `regex` functions. These codes are uploaded to this repository as R scripts. 
