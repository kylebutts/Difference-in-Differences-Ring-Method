# Difference-in-Differences with Geocoded Microdata

[Kyle Butts](https://www.kylebutts.com/)<sup>1</sup>
<br>
<sup>1</sup>University of Colorado: Boulder

#### [Published](https://www.sciencedirect.com/science/article/abs/pii/S0094119022000705) | [Paper](https://arxiv.org/abs/2110.10192) | [Five-minute Summary](https://www.kylebutts.com/papers/did-rings/)


## Abstract

I formalize a commonly-used estimator for the effects of spatially-targeted treatment with geocoded microdata. 
This estimator compares units immediately next to treatment to units slightly further away. I introduce intuitive 
identifying assumptions for the average treatment effect among affected units and illustrate problems when these 
assumptions fail. I propose a new method that allows for nonparametric estimation following methods introduced in 
Cattaneo et al. (2019) that allows estimation without requiring knowledge of exactly how far treatment effects are 
experienced. Since treatment effects can change with distance, the proposed estimator improves estimation by 
estimating a treatment effect curve.

## Replication

**Figure 1:** Rings Method
- `figure-example_problems.R`

**Figure 2:** Example of Problems with Ad-Hoc Ring Selection
- `figure-example_problems.R`

**Figure 3:** Price Gradient of Distance from Offender
- `analysis-linden_rockoff.R`

**Figure 4:** Effects of Offender Arrival on Home Prices (Linden and Rockoff 2008)
- `analysis-linden_rockoff.R`
- `helper-nonparametric_rings_estimator.R`
- `helper-parametric_rings_estimator.R`
- `helper-plot_rings.R`

**Table 1:** Monte Carlo Simulations
- `analysis-simulations.R`


## Citation

```
@article{butts2023jue,
  title={JUE Insight: Difference-in-differences with geocoded microdata},
  author={Butts, Kyle},
  journal={Journal of Urban Economics},
  volume={133},
  pages={103493},
  year={2023},
  publisher={Elsevier}
}
```


