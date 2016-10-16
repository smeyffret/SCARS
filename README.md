# SCARS

Evaluation platform used in SCARS [publications](https://liris.cnrs.fr/publis?author=3502&mode=liste).

SCARS is a P2P recommender system, as describe in "Local and social recommendation in decentralized architectures" [thesis](https://liris.cnrs.fr/publis/?id=5900).

## Project

### Core

The API is in the project **Core**. Extends `fr.cnrs.liris.scars.api.Scorer` to define your own algorithm.

### Scorer

The recommendation algorithms are located in the project **Scorer**.

### Evaluation

This project defines the algorithm to evaluate and the datasets to use for evaluation.

### DBEpinions, FileEpinions

Those projects contain the logic to transform a flat dataset file (or database) into a logical dataset in memory.

## Datasets

The following datasets contain data from www.epinions.com

0. http://www.trustlet.org/epinions.html
0. https://alchemy.cs.washington.edu/data/epinions/
0. http://liris.cnrs.fr/red/

SCARS is compatible with other datasets, such as Flixster, Synpinion, etc.

Datasets much follow the structure below:
### Ratings

```
user_id item_id rating_float
```

### Trust

```
user_id friend_user_id trust_float
```

## Getting started

Download the dataset locally and execute `Evaluation/runScars` with one of the following option:

<pre>
    Usage:
      runScars --epinions dir
      runScars --flixster dir
      runScars --synpinions dir
      runScars --database
      
      OPTIONS:
        --load dir
        --reload
        --save dir
        --view
        --process {stats|actors|friends|extended|items}
        --metrics [atotal,acov,amae,rae,rrse,wae,rwse,srcc,asrcc,pcc,apcc,actors,scores,confidence,sconfidence] (',' separated multivalues)
</pre>

