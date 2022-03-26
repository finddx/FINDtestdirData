# FINDtestdirData


### Available Datasets

- `testdir`



### Preview and Main Data

There are two versions of each data set: A regularly refreshed 'preview' version and an approved 'main' version. Each refresh of 'preview' automatically triggers an update of a pull request.


#### Main (Production)

[https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/**main**/data/**testdir**.csv](https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/main/data/testdir.csv)


#### Preview

[https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/**preview**/data/**testdir**.csv](https://raw.githubusercontent.com/dsbbfinddx/FINDtestdirData/main/data/testdir.csv)




### Regular Updates

The plan is to do regular updates via GHA. Authentication problems keep us from using them.

Instead, all data can be update locally by running `script/update-all.R`.
This processes all downloads and pushes the data to the `preview` branch. GHA will automatically open an PR afterwards.


### How to add a new data set `myname`

1. Add an R script, `updata-myname.R`, to prepare the data. The script writes `data/myname.csv`.

2. Copy `.github/workflows/testdir-pull-request.yml` and rename to `.github/workflows/myname-pull-request.yml`. Substitute all `testdir` by `myname`.

3. Update `update-all.R`

