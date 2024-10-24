# FINDtestdirData

Data in the `data/` directory is organized by the [apps](https://github.com/finddx/shinytestdir) that use them.


### Available Datasets

**covid19**
- `testdir.csv`: test directory
- `selftests.csv`: directory of selftests
- `eqa_covid_testdir`: EQA test directory

**moneypox**
- `mpx_testdir.csv`: monkeypox test directory

**ntd**
- `chagas_testdir.csv`: Chagas test directory


### Preview and Main Data

There are two versions of each data set: A regularly refreshed 'preview' version and an approved 'main' version. Each refresh of 'preview' automatically triggers an update of a pull request.


#### Main (Production)

[https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/**main**/data/covid19/**testdir**.csv](https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/main/data/covid19/testdir.csv)


#### Preview

[https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/**preview-testdir**/data/covid19/**testdir**.csv](https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/preview-testdir/data/covid19/testdir.csv)



### Use in R

With latest shinyfind, the 'main' set can read into R, using:

```r
shinyfind::get_data("testdir", app = "covid19")
shinyfind::get_data("mpx_testdir", app = "monkeypox")
```

The 'preview' dataset:


```r
shinyfind::get_data("testdir", app = "covid19", version = "preview")
```

where `covid19` is the name of the folder in this repo and also the [name of the app's folder in shinytestdir](https://github.com/finddx/shinytestdir)

Alternatively, use `options()` to set the version. This is useful in shiny apps, if you want to maintain a main and a preview version of an app.

```r
options(find.data.version = "preview")
shinyfind::get_data("testdir", "covid19")
```

These functions are memoised. By default, they download the data once a day, at 7am UTC.

Same use for other dataset:

```r
shinyfind::get_data("selftests", "covid19")
shinyfind::get_data("selftests", "covid19", "preview")
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

