#### Machine Learning modeling tips

- `Keep` only those `variables in a model that are highly correlated with the target or outcome`. 
- `Feature Engineering (FE)` is the `key` for `obtaining great results when dealing` with `structured data`. FE involves creating features from existing variables such that these new FEngineered variables are highly correlated with the target.
- FE is the best approach if you understand the data. The first step is taking the provided data and using it to accurately plot histograms to help you explore more. You will then typically spend a large amount of time generating features and then testing which ones correlate with the given target variables.
- When dealing with `unstructured data (like images, sound, text)`, `Neural Networks` steal the show.
- For structured data, use algorithms like `xgboost`, `random forest`, `gradient boosting trees`.
-  If you are dealing with a problem that consists of a lot of structured data, your best bet at success is using the features engineering approach. On the other hand, if you are dealing with unstructured data or has a lot of images, then the recommended approach is building and training neural networks. Overall, it’s always the mix of the two that takes the prize.
-  A `gradient boosting machine (gbm)`, much like a random forest, is a machine-learning technique based on ensembling weak prediction models, generally decision trees. It uses gradient boosting, a way to improve any machine-learning model by iteratively training new models that specialize in addressing the weak points of the previous models. Applied to decision trees, the use of the gradient boosting technique results in models that strictly outperform random forests most of the time, while having similar properties.
- `Deep learning` `completely automates` what used to be the most crucial step in a machine-learning workflow: `feature engineering`. With deep learning, you learn all features in one pass rather than having to engineer them yourself.
- In 2016, Kaggle was dominated by two approaches: gradient boosting machines and deep learning. Specifically, gradient boosting is used for problems where structured data is available, whereas deep learning is used for perceptual problems such as image classification. Practitioners of the former almost always use the excellent XGBoost library. Meanwhile, most of the Kaggle entrants leveraging deep learning use the Keras library, due to its ease of use and flexibility. `XGBoost` and `Keras` both support the two most popular data science languages: R and Python. gradient boosting machines, for shallow-learning problems; and deep learning, for perceptual problems. In technical terms, this means you’ll need to be familiar with `XGB` and `Keras`—the two libraries that currently dominate Kaggle competitions.
- In machine learning, a `category` in a `classification problem` is called a `class`. `Data points` are called `samples`. The class associated with a specific sample is called a `label`.
     