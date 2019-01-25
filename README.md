# Replication Package: Mining Software Defects: Should We Consider Affected Releases?

**Authors:** Suraj Yatish (UofAdelaide), Jirayus Jiarpakdee (Monash), [Patanamon Thongtanunam (UniMelb)](http://patanamon.com), [Chakkrit Tantithamthavorn (Monash)](http://www.chakkrit.com)

**Contact:** chakkrit.tantithamthavorn@monash.edu

### Abstract

With the rise of the Mining Software Repositories (MSR) field, defect datasets extracted from software repositories play a foundational role in many empirical studies related to software quality. At the core of defect data preparation is the identification of post-release defects. Prior studies leverage many heuristics (e.g., keywords and issue IDs) to identify post-release defects. However, such heuristic approach is based on several assumptions, which pose common threats to the validity of many studies. In this paper, we set out to investigate the nature of the difference of defect datasets generated by the heuristic approach and the realistic approach that leverages the earliest affected release that is realistically estimated by a software development team for a given defect. In addition, we investigate the impact of defect identification approaches on the predictive accuracy and the ranking of defective modules that are produced by defect models. Through a case study of defect datasets of 32 releases, we conclude that the heuristic approach has a large impact on both defect count datasets and binary defect datasets. On the other hand, the heuristic approach has a minimal impact on the predictive accuracy and the ranking of defective modules that are produced by defect count models and defect classification models. Our findings suggest that practitioners and researchers should not be too concerned about the predictive accuracy and the ranking of defective modules produced by defect models that are constructed using heuristic defect datasets.

## Replication guide


### Prerequisites

- Unix-compatible OS (Linux or OS X)
- R 3.4 or higher (3.5.1 was used)
- At least 2Gb of RAM
- At least 1 CPU core (12 cores are preferable)
- Few hours of computational time for 12 cores
- Run the following command to install R (for Ubuntu).

### Step 1 - install R libraries.

```R
sudo apt-get install r-base r-base-dev libcurl4-gnutls-dev libcurl4-openssl-dev libssl-dev
Rscript install_libraries.R
```

### Step 2 - Produce results for RQ1.

```
Rscript RQ1-analysis.R
```

***Output:***

- `figures/figure4-a.pdf`
- `figures/figure4-b.pdf`
- `figures/figure5.pdf`
- `figures/figure8.pdf`


### Step 3 - Build defect models for RQ2 and RQ3. (This step takes few hours for 12 CPU cores. 
The results are produced and available at ...)

```
Rscript RQ2-3-model-building.R
```

***Output:***

- `models/` all model results of defect models

### Step 4 - Run RQ2 and RQ3 analysis for defect count models

```
Rscript RQ2-3-analyze-defect-count-models.R
```

***Output:***

- `figures/figure6-a.pdf`
- `figures/figure7-a.pdf`
- `figures/figure9-a.pdf`

### Step 5 - Run RQ2 and RQ3 analysis for defect classification models

```
Rscript RQ2-3-analyze-defect-classification-models.R
```

***Output:***

- `figures/figure6-b.pdf`
- `figures/figure7-b.pdf`
- `figures/figure9-b.pdf`
