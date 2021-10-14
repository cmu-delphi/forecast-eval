The **weighted interval score** (WIS) is a proper score that combines a set of prediction interval scores. As described in [this article](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618)  it "can be interpreted as a generalization of the absolute error to probabilistic forecasts and allows for a decomposition into a measure of sharpness [spread] and penalties for over- and underprediction." With certain weight settings, the WIS is an approximation of the continuous ranked probability score, and can also be calculated in the form of an average pinball loss. A smaller WIS indicates better performance.