#### Machine Learning modeling tips

- `Keep` only those `variables in a model that are highly correlated with the target or outcome`. 
- `Feature Engineering (FE)` is the `key` for `obtaining great results when dealing` with `structured data`. FE involves creating features from existing variables such that these new FEngineered variables are highly correlated with the target.
- FE is the best approach if you understand the data. The first step is taking the provided data and using it to accurately plot histograms to help you explore more. You will then typically spend a large amount of time generating features and then testing which ones correlate with the given target variables.
- When dealing with `unstructured data (like images, sound, text)`, `Neural Networks` steal the show.
- For structured data, use algorithms like `xgboost`, `random forest`, `gradient boosting trees`.
-  If you are dealing with a problem that consists of a lot of structured data, your best bet at success is using the features engineering approach. On the other hand, if you are dealing with unstructured data or has a lot of images, then the recommended approach is building and training neural networks. Overall, it’s always the mix of the two that takes the prize.   
