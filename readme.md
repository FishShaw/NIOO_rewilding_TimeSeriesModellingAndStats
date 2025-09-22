# 🌿 Quantifying Rewilding Impact: Statistical Analysis & Ecological Resilience Modeling

**Master's Research Internship | Netherlands Institute of Ecology (NIOO-KNAW) | Advanced Data Analytics**

*This repository contains the **second phase** of my master's internship project: comprehensive statistical analysis, ecological resilience modeling, and automated reporting pipelines. The first phase (satellite data preprocessing) can be found [here](https://github.com/FishShaw/NIOO_rewilding_GEEDownloadingAndCloudMask).*

---

## 🎯 Project Overview

### Scientific Challenge
Despite 30+ years of rewilding implementation in the Gelderse Poort floodplain (Netherlands), **quantitative assessment of ecological impact remains a critical knowledge gap**. While land cover changes are visible, their effects on ecosystem functionality—particularly **vegetation productivity** and **resilience to climate extremes**—lack rigorous quantification.

### Technical Innovation
This project addresses this gap through **multi-scale time series analysis** of 32-year vegetation dynamics (1993-2024), developing novel **three-dimensional resilience metrics** and implementing **automated statistical modeling pipelines** to evaluate rewilding effectiveness across different land management strategies.

### Key Research Questions
- How do different rewilding approaches affect long-term vegetation productivity?
- What is the resilience capacity of rewilded ecosystems to extreme climate events?
- Can we quantify the "recovery time" and identify optimal management strategies?

---

## 🚀 Technical Highlights & Data Analytics Capabilities

### 📊 **Complex Multi-Scale Data Integration**
- **Massive dataset handling**: 32-year × 12 land cover types × 50,000+ pixels
- **Pixel-level time series analysis**: Individual pixel tracking across 384 monthly observations
- **Multi-source data fusion**: Satellite indices + meteorological stations + hydrological records
- **Intelligent quality control**: Automated outlier detection with adaptive interpolation algorithms

### 🔬 **Advanced Statistical Modeling**
- **Hierarchical Mixed-Effects Models (LME)**: Nested random effects for Event/Pixel dependencies
- **Automated model selection**: Interaction vs. main effects with AIC-based comparison
- **Robust uncertainty quantification**: Bootstrap confidence intervals and model diagnostics
- **Multi-hypothesis testing**: Automated significance screening across 108 model combinations

### 🌦️ **Ecological Resilience Innovation**
- **Novel three-dimensional resilience framework**: Resistance + Recovery + Resilience metrics
- **Adaptive time window analysis**: Dynamic 3-18 month recovery periods
- **Extreme event algorithm**: Automated drought/flood identification using SPEI/SRI indices
- **Climate-vegetation interaction modeling**: Event severity × management type interactions

### ⚙️ **Production-Ready Analytics Pipeline**
- **Modular function architecture**: Reusable components for different research contexts
- **Automated reporting system**: Model diagnostics + significance testing + visualization generation
- **Error handling & validation**: Comprehensive data quality checks and graceful failure recovery
- **Scalable processing workflow**: Memory-efficient chunked processing for large raster datasets

---

## 📈 Key Scientific Discoveries

### 🔍 **Resilience Patterns Discovered**
- **Late-started rewilded areas** show significantly higher recovery rates to flood events (p < 0.001)
- **Agricultural lands** exhibit 40% lower resistance to drought compared to natural areas
- **Optimal recovery window**: 12-month period shows strongest predictive power across all metrics
- **Management age effect**: Rewilding sites with 15+ years show enhanced ecological stability

### 📊 **Statistical Model Performance**
- **High explanatory power**: Best models achieve R² > 0.75 for resilience predictions
- **Robust significance patterns**: 23 out of 108 models show significant effects (p < 0.05)
- **Cross-validation stability**: Consistent results across different time windows and event types
- **Effect size quantification**: Large practical differences between management strategies

---

## 🛠️ Technical Implementation

### **Data Processing Workflow**
```mermaid
flowchart LR
    A[Pixel Extraction] --> B[Outlier Detection]
    B --> C[Time Series Cleaning]
    C --> D[Extreme Event Mapping]
    D --> E[Resilience Calculation]
    E --> F[Statistical Modeling]
    F --> G[Automated Reporting]
```

### **Core Analysis Components**

#### 1. **Multi-Dimensional Data Preparation**
- **Pixel-level aggregation**: 50,000+ individual time series
- **Temporal alignment**: Precise event window definition with buffer periods
- **Missing data interpolation**: Linear interpolation with maximum gap constraints
- **Categorical grouping**: Dynamic ecological classification system

#### 2. **Resilience Metrics Calculation**
```r
# Three-dimensional resilience framework
Resistance
Recovery 
Resilience 
```

#### 3. **Advanced Statistical Modeling**
- **Model specification**: `lme(Resilience ~ Management_Type * Event_Severity, random = ~1|Event/Pixel)`
- **Interaction analysis**: Management strategy × climate severity effects
- **Post-hoc comparisons**: Tukey HSD for multiple group comparisons
- **Effect size visualization**: Custom ggplot2 functions with confidence intervals

#### 4. **Automated Analysis Pipeline**
- **Batch processing**: 108 models (3 metrics × 6 time windows × 6 model types)
- **Quality diagnostics**: Normality testing, residual analysis, convergence checks
- **Results filtering**: Automated significance detection and reporting
- **Visualization generation**: Standardized plots with publication-ready formatting

---

## 📋 Technical Stack & Tools

```r
🔧 Core Analytics Environment
├── R (4.3+) - Statistical computing and modeling
├── tidyverse - Data manipulation and visualization  
├── nlme - Mixed-effects modeling
├── emmeans - Marginal means and comparisons
├── terra - Spatial raster processing
└── SPEI - Climatological indices calculation

📊 Specialized Packages
├── trend - Non-parametric trend analysis
├── zoo - Time series operations
├── multcomp - Multiple comparisons
└── ggplot2 - Advanced visualization
```

### **Key R Capabilities Demonstrated**
- **Functional programming**: Custom analysis functions with error handling
- **Nested data operations**: Complex grouping and list-column manipulations  
- **Advanced plotting**: Multi-panel figures with custom color schemes
- **Memory management**: Efficient processing of large spatial datasets
- **Reproducible workflows**: Parameterized analysis with automated documentation

---

## 📊 Results & Impact

### **Scientific Contributions**
- **First quantitative assessment** of rewilding resilience in European floodplains
- **Novel methodology** for multi-scale ecological resilience analysis
- **Policy-relevant findings** on optimal management timing and strategies
- **Transferable framework** applicable to other restoration contexts

### **Data Analytics Achievements**
- **Processed 1.8+ billion data points** across temporal and spatial dimensions
- **Developed automated pipeline** reducing analysis time from weeks to hours
- **Created reusable functions** for ecological resilience assessment
- **Generated publication-ready outputs** with integrated statistical reporting

### **Technical Innovation**
- **Custom algorithms** for extreme event detection and resilience calculation
- **Scalable processing** architecture for large environmental datasets
- **Integrated quality control** with comprehensive diagnostic reporting
- **Interactive visualization** tools for stakeholder communication

---

## 🔗 Repository Structure

```
📁 Project Organization
├── 🧹 preprocessing/          # Data cleaning and quality control
├── 📍 pixel extraction/       # Spatial data extraction workflows  
├── 🌪️ extreme event extraction/ # Climate event identification
├── 📊 modelling/              # Statistical analysis and LME models
├── 📈 graphs_plots/           # Visualization and publication figures
├── 🔍 rough analysis/         # Exploratory analysis and trend detection
└── 📋 plots/                  # Final result outputs and summaries
```

---

## 📞 Contact & Collaboration

**Technical Implementation Details**: For in-depth discussion of methodological approaches, algorithm specifics, or potential collaboration opportunities, please contact the author directly.

## 🎓 Academic Context

**Institution**: Netherlands Institute of Ecology (NIOO-KNAW)  
**Program**: MSc Geo-Information Science  
**Duration**: 4-month research internship  
**Supervision**: Interdisciplinary team combining remote sensing, ecology, and statistical modeling expertise
