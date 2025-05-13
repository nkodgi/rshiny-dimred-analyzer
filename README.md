# 💻 R Shiny Dimensionality Reduction & Statistical Analysis App

### 👩‍🔬👨‍🔬👩‍🔬👩‍🔬 Created by:  
**Radha Patel**, **Akber Shaikh**, **Jamie English**, and **Natasha Kodgi**  
*Biomedical Data Visualization – Spring 2025*

---

## 🎯 Purpose

We developed a user-friendly **R Shiny application** that allows anyone, regardless of coding experience, to perform **dimensionality reduction** and **statistical analysis** on any dataset. 

To demonstrate the app, we used the **Wisconsin Breast Cancer dataset** (569 samples, 30 features), enabling users to apply a range of techniques including **PCA**, **Sparse PCA**, **Kernel PCA**, **MDS**, and **t-SNE**. Among these, *t-SNE* provided the clearest separation between **benign** and **malignant** tumors.

The app also features statistical tools such as:
- ✅ **Welch’s t-tests**
- 📏 **Confidence interval calculations**
- 📊 **Visualizations (e.g., box plots)**

These tools revealed that **tumor size-related features** showed the strongest group differences, highlighting their importance in diagnosis. This app is ideal for clinicians, biologists, and researchers handling **high-dimensional biomedical data**.

---

## 🚀 How to Run the App in RStudio
1. Clone this repository to your local machine or download the .csv and .r files. If you're doing this with command line:
   ```bash
   git clone https://github.com/nkodgi/rshiny-dimred-analyzer.git
   ```
3. Open the .R file (e.g., app.R) in RStudio.
4. Click "Run App" (top right corner of the editor).
5. Once launched, add the data you would like to use (or the test data).
---

## 🧠 Team Contributions

### 🔧 **Akber Shaikh (A.S.)**
- Designed the full app structure (tabs, buttons, data upload/download).
- Wrote the *“System Design and Features”* section of the paper.

### 🎨 **Radha Patel (R.P.)**
- Implemented dimensionality reduction methods and visualizations.
- Wrote the *“Introduction and Dataset”* section.

### 📈 **Jamie English (J.E.)**
- Conducted statistical analyses and integrated them into the app.
- Wrote the *Abstract*.

### 📊 **Natasha Kodgi (N.K.)**
- Performed statistical analysis and created visualizations.
- Authored the *“Conclusion and Future Work”* section.

## 🤝 Joint Efforts

All authors collaborated on the:
- *“Methods”* section  
- *“Results and Findings”* section

---

## 📢 Declaration

The authors declare **no conflict of interest**.

---

## Dataset Citation

Street, W. N., Wolberg, W. H. & Mangasarian, O. L. Nuclear feature extraction for breast tumor diagnosis. in Biomedical Image Processing and Biomedical Visualization vol. 1905 861–870 (SPIE, 1993).

