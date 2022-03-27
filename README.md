# FINDtestdirData


### Available Datasets

- `testdir`: test directory
- `selftests`: directory of selftsts


### Preview and Main Data

There are two versions of each data set: A regularly refreshed 'preview' version and an approved 'main' version. Each refresh of 'preview' automatically triggers an update of a pull request.


#### Main (Production)

[https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/**main**/data/**testdir**.csv](https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/main/data/testdir.csv)


#### Preview

[https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/**preview-testdir**/data/**testdir**.csv](https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/preview-testdir/data/testdir.csv)



### Use in R

With latest shinyfind, the 'main' set can read into R, using:

```r
shinyfind::get_data("testdir")
```

The 'preview' dataset:


```r
shinyfind::get_data("testdir", version = "preview")
```

Alternatively, use `options()` to set the version. This is useful in shiny apps, if you want to maintain a main and a preview version of an app.

```r
options(find.data.version = "preview")
shinyfind::get_data("testdir")
```

These functions are memoised. By default, they download the data once a day, at 7am UTC.

Same use for other dataset:

```r
shinyfind::get_data("selftests")
shinyfind::get_data("selftests", "preview")
```


### Updates

Data is updated locally by running:

```r
source("script/update-all.R")
```

This processes all downloads and pushes the data to the `preview` branch. GHA will automatically open an PR afterwards.


### Review

If the updated data (on the `preview-` branch) differs from main, an automated pull request is generated. This PR can be assigned to any colaborator of this repo.


### How to add a new data set `myname`

1. Add an R script, `updata-myname.R`, to prepare the data. The script writes `data/myname.csv`.

2. Copy `.github/workflows/testdir-pull-request.yml` and rename to `.github/workflows/myname-pull-request.yml`. Substitute all `testdir` by `myname`.

3. Update `update-all.R`




### TODO

`data/testdir_data.csv` is there because a production app is using it. Remove once the app switches to using `shinyfind::get_data("testdir")`.

