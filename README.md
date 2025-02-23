# Diabetes Risk - A Multilevel Analysis of Health, Lifestyle, Demographics, and Socioeconomic Factors

## Overview
This project analyzes the risk factors associated with type 2 diabetes mellitus (T2DM) using data from the 2015 Behavioral Risk Factor Surveillance System (BRFSS). The study employs exploratory data analysis (EDA), logistic regression, and probit modeling to identify significant predictors of diabetes risk, including demographic, lifestyle, clinical, and socioeconomic factors.

## Key Findings
- **Significant Predictors**: Obesity, high cholesterol, age, gender, diet, physical activity, mental health, and socioeconomic status.
- **Public Health Implications**:
  - The need for targeted health interventions to promote healthier lifestyles.
  - Improvement of healthcare accessibility.
  - Addressing socioeconomic disparities.
  - Customizing prevention strategies based on demographic traits.

## Dataset
- **Source**: 2015 Behavioral Risk Factor Surveillance System (BRFSS)
- **Features**:
  - Demographic attributes (age, gender, race, income level, education, etc.)
  - Lifestyle factors (diet, exercise, smoking, alcohol consumption)
  - Clinical variables (obesity, cholesterol levels, blood pressure, etc.)
  - Socioeconomic factors (income, healthcare access, employment status)

## Methodology
1. **Exploratory Data Analysis (EDA)**:
   - Data cleaning and preprocessing.
   - Visualization of key trends and correlations.
2. **Statistical Modeling**:
   - Logistic regression and probit modeling to identify significant risk factors.
   - Model evaluation using AIC and BIC criteria.
3. **Interpretation & Policy Recommendations**:
   - Identifying high-risk groups.
   - Suggesting policy measures to reduce diabetes prevalence.

## Installation & Usage
### Prerequisites
- Python 3.x
- Required libraries: `pandas`, `numpy`, `matplotlib`, `seaborn`, `statsmodels`, `scikit-learn`

### Installation
```bash
pip install -r requirements.txt
```

### Running the Analysis
```bash
python main.py
```

## Results & Visualization
- Summary statistics and correlation heatmaps.
- Logistic regression and probit model results.
- Policy recommendations based on model insights.

## Future Work
- Expanding the dataset to include more recent BRFSS surveys.
- Incorporating machine learning models for predictive analysis.
- Analyzing regional variations in diabetes risk factors.

## Contributors
- Porimol Chandro (p.chandro@student.uw.edu.pl)
- Thi Hoang Nhu Ho (t.ho2@student.uw.edu.pl)

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
