import os
import glob
import csv
import time
import numpy as np
import pandas as pd
import matplotlib
import scipy.stats as stats
import plotly.figure_factory as ff
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import timedelta
from matplotlib.colors import ListedColormap
from matplotlib.patches import Patch
from matplotlib.lines import Line2D
from pandarallel import pandarallel

pandarallel.initialize()

plt.rcParams.update({'font.size': 42})

output_directory = os.path.join(os.getcwd(), "Output")
if not os.path.exists(output_directory):
    os.makedirs(output_directory)

plot_directory = os.path.join(os.getcwd(), "Plots")
if not os.path.exists(plot_directory):
    os.makedirs(plot_directory)

scatter_directory = os.path.join(plot_directory, "ScatterPlots")
if not os.path.exists(scatter_directory):
    os.makedirs(scatter_directory)

data_directory = os.path.join(os.getcwd(), "Data")
if not os.path.exists(data_directory):
    os.makedirs(data_directory)


def checkRain(row, df):
    index = row.name
    previousSixStart = index - timedelta(hours=6)
    nextSixEnd = index + timedelta(hours=6)

    if row["isRain"] == False and True in df.loc[previousSixStart:index]["isRain"].values and True in df.loc[index:nextSixEnd]["isRain"].values:
        last_rain = df.loc[previousSixStart:index].where(
            df.loc[previousSixStart:index]["isRain"] == True).last_valid_index()
        next_rain = df.loc[index:nextSixEnd].where(
            df.loc[index:nextSixEnd]["isRain"] == True).first_valid_index()
        time_diff = ((next_rain - last_rain).total_seconds())/60
        if time_diff < 360:
            row["isRain"] = True
            row["Flow"] == "StormFlow"
            return "StormFlow"
        elif time_diff >= 360:
            return "BaseFlow"
    elif row["isRain"] == False and True in df.loc[previousSixStart:index]["isRain"].values and True not in df.loc[index:nextSixEnd]["isRain"].values:
        return "BaseFlow"
    elif row["isRain"] == True:
        row["Flow"] == "StormFlow"
        return "StormFlow"
    elif row["isRain"] == False and True in df.loc[previousSixStart:index]["isRain"].values and True not in df["isRain"].loc[index:nextSixEnd].values \
            and row["Flow"] == "StormFlow" and "BaseFlow" not in df.loc[previousSixStart:index]["Flow"].values:
        row["isRain"] = True
        return "StormFlow"
    elif row["isStorm"] == "NA":
        return "NA"
    else:
        return "BaseFlow"


def correctData(filename, columns, target, units):
    dfs = []
    files = glob.glob(os.path.join(os.getcwd(), data_directory, filename))

    for file in files:
        df = pd.read_excel(file, index_col=0, parse_dates=True, names= columns)
        df.index = pd.to_datetime(df.index)
        name = file.split("/")[-1].replace(".xlsx", "")
        df.columns.name = file.split("/")[-1].replace(".xlsx", "")
        df = df.groupby(df.index).mean()
        dfs.append(df)

    for df in dfs:
        before_df = df.copy(deep=True)
        before_df.columns.name = "Before"

        df["Flow"] = np.where(df["Flow"] > 0.1, "StormFlow", "BaseFlow")
        df["isRain"] = np.where(df["Rain"] == 0, False, True)
        df["isStorm"] = "BaseFlow"

        start = time.perf_counter()

        df["isStorm"] = df.parallel_apply(checkRain, axis=1, args=(df,))

        end2 = time.perf_counter()
        print(end2 - start)

        df["stormNum"] = (df["isStorm"] != df["isStorm"].shift()).cumsum()
        df["isStormCopy"] = df["isStorm"]
        df["isStormCopy"].where(df["isStorm"] == "StormFlow", 1, inplace=True)
        df["isStormCopy"].where(df["isStorm"] == "BaseFlow", 0, inplace=True)
        df["isStormCopy"] = df["isStormCopy"].astype(float)
        df["isStorm"] = df["isStorm"].astype(str) + df["stormNum"].astype(str)

        df["ZScore"] = df.groupby(df["isStorm"])[
                                  target].transform(stats.zscore)
        df["ZScore_bool"] = df["ZScore"].apply(
            lambda x: True if abs(x) >= 3 else False)


        other = df[df["isStorm"].str.contains("NA")]
        other[target] = other[target].fillna(other.groupby(other["isStorm"])[
                                                     target].transform(np.mean))
        other[target].where((other["ZScore_bool"] == False) & (other[target] > 0), other.groupby(
            other["isStorm"])[target].transform(np.mean), inplace=True)

        base = df[df["isStorm"].str.contains("BaseFlow")]
        base[target] = base[target].fillna(base.groupby(base["isStorm"])[
                                                   target].transform(np.mean))
        base[target].where((base["ZScore_bool"] == False) & (base[target] > 0), base.groupby(
            base["isStorm"])[target].transform(np.mean), inplace=True)

        storm = df[df["isStorm"].str.contains("StormFlow")]
        storm[target] = storm[target].fillna(storm.groupby(storm["isStorm"])[
                                                     target].transform(np.median))
        storm[target].where((storm["ZScore_bool"] == False) & (storm[target] > 0), storm.groupby(
            storm["isStorm"])[target].transform(np.median), inplace=True)


        combined_df = pd.concat([other, base, storm]).sort_index()
        combined_df.columns.name = "After"
        visualizeDataChange([before_df, combined_df], target, name, units)
        combined_df["Flow"] = before_df["Flow"]
        combined_df = combined_df.drop(
            ["stormNum", "isStorm", "ZScore", "ZScore_bool", "isRain", "isStormCopy"], axis=1)

        combined_df.to_excel(os.path.join(
            output_directory, df.columns.name + "_QAQC.xlsx"))




def visualizeDataChange(dfs, target_variable, name, units):
    cmap = ListedColormap(['gray', 'white'])

    patches = [Patch(facecolor='gray', edgecolor='b', label='StormFlow'), Patch(
        facecolor='white', edgecolor='b', label='BaseFlow')]
    lines = [Line2D([0], [0], color='b', lw=2, label='Before'),
             Line2D([0], [0], color='orange', lw=2, label='After')]

    fig = plt.figure(figsize=[35, 20])
    ax = fig.add_subplot(111)
    for df in dfs:
        ax.plot(df[target_variable].rolling(window="1D").mean(),
                alpha=0.7, label=target_variable + " " + df.columns.name)
    ax2 = ax.twinx()
    ax2.scatter(dfs[0].index.values, dfs[0]
                [target_variable].subtract(dfs[1][target_variable]))
    ax.set_xlabel("DateTime (Year-Month)")
    ax.set_ylabel(target_variable + " " + units, labelpad=25)
    ax2.set_ylabel(target_variable + " Change (Before - After)",
                   rotation=270, labelpad=65)
    ax.set_yscale("symlog")
    ax2.set_yscale("symlog")
    ax.xaxis.set_major_locator(mdates.MonthLocator(interval=1))
    plt.xticks(rotation=45)
    ax.legend(handles=patches + lines,
              bbox_to_anchor=(1.10, 1), loc='upper left')
    plt.tight_layout()
    plt.savefig(os.path.join(scatter_directory, name + "_"
                             + target_variable + "_BeforeAfterVisualization.jpg"))
    plt.close()


correctData("CSW_2020.xlsx", ["Datetime", "Velocity", "Flow", "Rain"], "Velocity", "ft/s")
correctData("Commons_2020.xlsx", ["Datetime", "Depth", "Velocity", "Flow", "Rain"], "Depth", "in")
