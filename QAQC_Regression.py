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
from sklearn.metrics import r2_score
from scipy.optimize import curve_fit
from pandarallel import pandarallel
from mpl_axes_aligner import align

pandarallel.initialize()

plt.rcParams.update({
    "font.size": 42,
    "font.family": "sans-serif",
    "font.sans-serif": ["Palatino"]
})


data_directory = os.path.join(os.getcwd(), "Data")
if not os.path.exists(data_directory):
    os.makedirs(data_directory)

output_directory = os.path.join(os.getcwd(), "Output")
if not os.path.exists(output_directory):
    os.makedirs(output_directory)

plot_directory = os.path.join(os.getcwd(), "Plots")
if not os.path.exists(plot_directory):
    os.makedirs(plot_directory)

scatter_directory = os.path.join(plot_directory, "ScatterPlots")
if not os.path.exists(scatter_directory):
    os.makedirs(scatter_directory)

regression_directory = os.path.join(plot_directory, "RegressionPlots")
if not os.path.exists(regression_directory):
    os.makedirs(regression_directory)

commons_regression = os.path.join(regression_directory, "Commons")
if not os.path.exists(commons_regression):
    os.makedirs(commons_regression)

csw_regression = os.path.join(regression_directory, "CSW")
if not os.path.exists(csw_regression):
    os.makedirs(csw_regression)

differences = os.path.join(output_directory, "Differences")
if not os.path.exists(differences):
    os.makedirs(differences)

def test_event_difference(df, target_variable):
    storm = df[df["isStorm"].str.contains("StormFlow")]
    base = df[df["isStorm"].str.contains("BaseFlow")]

    storms = storm["isStorm"].unique()
    bases = base["isStorm"].unique()

    num_both = 0
    num_storm = 0
    num_base = 0
    num_neither = 0

    for unique_storm in storms:
        if len(df[df["isStorm"] == unique_storm][target_variable].dropna()) == 0 or len(storm[target_variable].dropna()) == 0:
            continue
        if len(df[df["isStorm"] == unique_storm][target_variable].dropna()) == 0 or len(base[target_variable].dropna()) == 0:
            continue

        storm_prob = stats.mannwhitneyu(df[df["isStorm"] == unique_storm][target_variable].dropna(), storm[target_variable].dropna()).pvalue
        base_prob = stats.mannwhitneyu(df[df["isStorm"] == unique_storm][target_variable].dropna(), base[target_variable].dropna()).pvalue

        if storm_prob > 0.05:
            num_storm += 1
        elif base_prob > 0.05:
            num_base += 1
        elif storm_prob > 0.05 and base_prob > 0.05:
            num_both += 1
        else:
            num_neither += 1

    diff_df = pd.DataFrame({"num_storm" : num_storm,
                                "num_base" : num_base,
                                "num_both" : num_both,
                                "num_neither" : num_neither
                                },
                               index=[0])

    diff_df.to_csv(os.path.join(differences, target_variable  + "_" + "storm" + ".csv"))

    num_both = 0
    num_storm = 0
    num_base = 0
    num_neither = 0

    for unique_base in bases:
        if len(df[df["isStorm"] == unique_base][target_variable].dropna()) == 0 or len(storm[target_variable].dropna()) == 0:
            continue
        if len(df[df["isStorm"] == unique_base][target_variable].dropna()) == 0 or len(base[target_variable].dropna()) == 0:
            continue

        storm_prob = stats.mannwhitneyu(df[df["isStorm"] == unique_base][target_variable].dropna(), storm[target_variable].dropna()).pvalue
        base_prob = stats.mannwhitneyu(df[df["isStorm"] == unique_base][target_variable].dropna(), base[target_variable].dropna()).pvalue

        if storm_prob > 0.05:
            num_storm += 1
        elif base_prob > 0.05:
            num_base += 1
        elif storm_prob > 0.05 and base_prob > 0.05:
            num_both += 1
        else:
            num_neither += 1

    diff_df = pd.DataFrame({"num_storm" : num_storm,
                                "num_base" : num_base,
                                "num_both" : num_both,
                                "num_neither" : num_neither
                                },
                               index=[0])

    diff_df.to_csv(os.path.join(differences, target_variable  + "_" + "base" + ".csv"))

def test_difference(df, target_variable):
    storm = df[df["isStorm"] == "StormFlow"]
    base = df[df["isStorm"] == "BaseFlow"]

    resamples = ["1D", "12H", "6H", "3H", "1H"]

    for period in resamples:
        num_both = 0
        num_storm = 0
        num_base = 0
        num_neither = 0
        for window in df.resample(period):
            if len(window[1][target_variable].dropna()) == 0 or len(storm[target_variable].dropna()) == 0:
                continue
            if len(window[1][target_variable].dropna()) == 0 or len(base[target_variable].dropna()) == 0:
                continue

            storm_prob = stats.mannwhitneyu(window[1][target_variable].dropna(), storm[target_variable].dropna()).pvalue
            base_prob = stats.mannwhitneyu(window[1][target_variable].dropna(), base[target_variable].dropna()).pvalue

            if storm_prob > 0.05:
                num_storm += 1
            elif base_prob > 0.05:
                num_base += 1
            elif storm_prob > 0.05 and base_prob > 0.05:
                num_both += 1
            else:
                num_neither += 1

        diff_df = pd.DataFrame({"num_storm" : num_storm,
                                    "num_base" : num_base,
                                    "num_both" : num_both,
                                    "num_neither" : num_neither
                                    },
                                   index=[0])

        diff_df.to_csv(os.path.join(differences, target_variable  + "_" + period + ".csv"))




def get_equation(order, coefficients):
    if order == "first":
        return 'y = %.5f * x + %.5f' % tuple(coefficients)
    elif order == "second":
        return 'y = %.5f * x + %.5f * x^2 + %.5f' % tuple(coefficients)
    elif order == "third":
        return 'y = %.5f * x + %.5f * x^2 + %.5f * x^3 + %.5f' % tuple(coefficients)
    elif order == "fourth":
        return 'y = %.5f * x + %.5f * x^2 + %.5f * x^3 +\n %.5f * x^4 + %.5f' % tuple(coefficients)
    elif order == "fifth":
        return 'y = %.5f * x + %.5f * x^2 + %.5f * x^3 +\n %.5f * x^4 + %.5f * x^5 + %.5f' % tuple(coefficients)
    elif order == "sixth":
        return 'y = %.5f * x + %.5f * x^2 + %.5f * x^3 +\n %.5f * x^4 + %.5f * x^5 + %.5f * x^6 + %.5f' % tuple(coefficients)
    elif order == "seventh":
        return 'y = %.5f * x + %.5f * x^2 + %.5f * x^3 +\n %.5f * x^4 + %.5f * x^5 + %.5f * x^6 + %.5f x^7 + %.5f' % tuple(coefficients)


def test_seventhPower(x, a, b, c, d, e, f, g, h):
	return (a * x) + (b * x**2) + (c * x**3) + (d * x**4) + (e * x**5) + (f * x**6) + (g * x**7) + h


def test_sixthPower(x, a, b, c, d, e, f, g):
	return (a * x) + (b * x**2) + (c * x**3) + (d * x**4) + (e * x**5) + (f * x**6) + g


def test_fifthPower(x, a, b, c, d, e, f):
	return (a * x) + (b * x**2) + (c * x**3) + (d * x**4) + (e * x**5) + f


def test_fourthPower(x, a, b, c, d, e):
	return (a * x) + (b * x**2) + (c * x**3) + (d * x**4) + e


def test_thirdPower(x, a, b, c, d):
	return (a * x) + (b * x**2) + (c * x**3) + d


def test_secondPower(x, a, b, c):
	return (a * x) + (b * x**2) + c


def test_firstPower(x, a, b):
	return (a * x) + b


def test_regression(df, dir, target, units):
    name = df.name
    rsq = {"p1": np.nan, "p2": np.nan, "p3": np.nan,
           "p4": np.nan, "p5": np.nan, "p6": np.nan, "p7": np.nan}

    if len(df) > 1:
        try:
            popt1, pcov1 = curve_fit(
                test_firstPower, df["cumulativeRain"], df[target])
        except:
            return
        popt1, pcov1 = curve_fit(
            test_firstPower, df["cumulativeRain"], df[target])
        a1, b1 = popt1
        rsq["p1"] = r2_score(df[target], test_firstPower(
            df["cumulativeRain"], *popt1))

    if len(df) > 2:
        popt2, pcov2 = curve_fit(
            test_secondPower, df["cumulativeRain"], df[target])
        a2, b2, c2 = popt2
        rsq["p2"] = r2_score(df[target], test_secondPower(
            df["cumulativeRain"], *popt2))

    if len(df) > 3:
        popt3, pcov3 = curve_fit(
            test_thirdPower, df["cumulativeRain"], df[target])
        a3, b3, c3, d3 = popt3
        rsq["p3"] = r2_score(df[target], test_thirdPower(
            df["cumulativeRain"], *popt3))

    if len(df) > 4:
        popt4, pcov4 = curve_fit(
            test_fourthPower, df["cumulativeRain"], df[target])
        a4, b4, c4, d4, e4 = popt4
        rsq["p4"] = r2_score(df[target], test_fourthPower(
            df["cumulativeRain"], *popt4))

    if len(df) > 5:
        popt5, pcov5 = curve_fit(
            test_fifthPower, df["cumulativeRain"], df[target])
        a5, b5, c5, d5, e5, f5 = popt5
        rsq["p5"] = r2_score(df[target], test_fifthPower(
            df["cumulativeRain"], *popt5))

    if len(df) > 6:
        popt6, pcov6 = curve_fit(
            test_sixthPower, df["cumulativeRain"], df[target])
        a6, b6, c6, d6, e6, f6, g6 = popt6
        rsq["p6"] = r2_score(df[target], test_sixthPower(
            df["cumulativeRain"], *popt6))

    if len(df) > 7:
        popt7, pcov7 = curve_fit(
            test_seventhPower, df["cumulativeRain"], df[target])
        a7, b7, c7, d7, e7, d7, g7, f7 = popt7
        rsq["p7"] = r2_score(df[target], test_seventhPower(
            df["cumulativeRain"], *popt7))

    best_fit = max(rsq, key=rsq.get)

    if rsq[best_fit] >= 0.95:
        if best_fit == "p1":
            equation = get_equation("first", popt1)
            plot_regression(test_firstPower(
                df["cumulativeRain"], *popt1), df[target], df["cumulativeRain"], rsq[best_fit], equation, name, dir, target, units)
            return pd.DataFrame(test_firstPower(df["cumulativeRain"], *popt1))
        elif best_fit == "p2":
            equation = get_equation("second", popt2)
            plot_regression(test_secondPower(
                df["cumulativeRain"], *popt2), df[target], df["cumulativeRain"], rsq[best_fit], equation, name, dir, target, units)
            return pd.DataFrame(test_secondPower(df["cumulativeRain"], *popt2))
        elif best_fit == "p3":
            equation = get_equation("third", popt3)
            plot_regression(test_thirdPower(
                df["cumulativeRain"], *popt3), df[target], df["cumulativeRain"], rsq[best_fit], equation, name, dir, target, units)
            return pd.DataFrame(test_thirdPower(df["cumulativeRain"], *popt3))
        elif best_fit == "p4":
            equation = get_equation("fourth", popt4)
            plot_regression(test_fourthPower(
                df["cumulativeRain"], *popt4), df[target], df["cumulativeRain"], rsq[best_fit], equation, name, dir, target, units)
            return pd.DataFrame(test_fourthPower(df["cumulativeRain"], *popt4))
        elif best_fit == "p5":
            equation = get_equation("fifth", popt5)
            plot_regression(test_fifthPower(
                df["cumulativeRain"], *popt5), df[target], df["cumulativeRain"], rsq[best_fit], equation, name, dir, target, units)
            return pd.DataFrame(test_fifthPower(df["cumulativeRain"], *popt5))
        elif best_fit == "p6":
            equation = get_equation("sixth", popt6)
            plot_regression(test_sixthPower(
                df["cumulativeRain"], *popt6), df[target], df["cumulativeRain"], rsq[best_fit], equation, name, dir, target, units)
            return pd.DataFrame(test_sixthPower(df["cumulativeRain"], *popt6))
        elif best_fit == "p7":
            equation = get_equation("seventh", popt7)
            plot_regression(test_seventhPower(
                df["cumulativeRain"], *popt7), df[target], df["cumulativeRain"], rsq[best_fit], equation, name, dir, target, units)
            return pd.DataFrame(test_seventhPower(df["cumulativeRain"], *popt7))



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
            return "StormFlow"
        elif time_diff >= 360:
            return "BaseFlow"
    elif row["isRain"] == False and True in df.loc[previousSixStart:index]["isRain"].values and True not in df.loc[index:nextSixEnd]["isRain"].values:
        return "BaseFlow"
    elif row["isRain"] == True:
        return "StormFlow"
    else:
        return "BaseFlow"


def correctData(filename, columns, target, units, dir):
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

        df["Rain"] = df["Rain"].fillna(0)

        df["isFlow"] = np.where(df["Flow"] < 0.1, 1, 0)

        df["isRain"] = np.where(df["Rain"] == 0, False, True)
        df["isStorm"] = "BaseFlow"

        df["isStorm"] = df.parallel_apply(checkRain, axis=1, args=(df,))

        target_variable = target

        df["stormNum"] = (df["isStorm"] != df["isStorm"].shift()).cumsum()

        df["isStorm_Rain"] = df["isStorm"].astype(str) + df["stormNum"].astype(str)

        df["flowSum"] = df.groupby(["stormNum"])["isFlow"].cumsum()
        df["stormNum"] = np.where((df["isStorm"] == "BaseFlow") & (df["flowSum"] == 0), df["stormNum"] - 1, df["stormNum"])
        df["wasChanged"] = np.where((df["isStorm"] == "BaseFlow") & (df["flowSum"] == 0), "Yes", "")
        df["isStorm"] = np.where((df["isStorm"] == "BaseFlow") & (df["flowSum"] == 0), "StormFlow", df["isStorm"])

        test_difference(df, target_variable)

        df["stormNum"] = (df["isStorm"] != df["isStorm"].shift()).cumsum()

        df["isStormCopy"] = df["isStorm"]
        df["isStormCopy"].where(df["isStorm"] == "StormFlow", 1, inplace=True)
        df["isStormCopy"].where(df["isStorm"] == "BaseFlow", 0, inplace=True)
        df["isStormCopy"] = df["isStormCopy"].astype(float)
        df["isStorm"] = df["isStorm"].astype(str) + df["stormNum"].astype(str)

        test_event_difference(df, target_variable)

        df["ZScore"] = df.groupby(df["isStorm"]).apply(moving_zscore, (target))
        df["emwa"] = df.groupby(df["isStorm"]).apply(moving_ewma, (target))
        df["ewmstd"] = df.groupby(df["isStorm"]).apply(moving_ewmstd, (target))
        df["ZScore_bool"] = df["ZScore"].apply(
            lambda x: True if abs(x) >= 3 else False)
        df["cumulativeRain"] = df.groupby("isStorm")["Rain"].cumsum()
        df["Calculated" + target] = np.nan


        other = df[df["isStorm"].str.contains("NA")]
        other[target] = other[target].fillna(other.groupby(other["isStorm"])[
                                                     target].transform(np.mean))
        other[target].where((other["ZScore_bool"] == False) & (other[target] > 0),
            np.nan, inplace=True)

        other[target] = other[target].interpolate(method = "linear")

        base = df[df["isStorm"].str.contains("BaseFlow")]
        base[target] = base[target].fillna(base.groupby(base["isStorm"])[
                                                   target].transform(np.mean))
        base[target].where((base["ZScore_bool"] == False) & (base[target] > 0),
            np.nan, inplace=True)

        base[target] = base[target].interpolate(method = "linear")

        storm = df[df["isStorm"].str.contains("StormFlow")]
        storm[target] = storm[target].fillna(storm.groupby(storm["isStorm"])[
                                                     target].transform(np.median))
        storm["Calculated" + target] = storm.groupby(
            "isStorm").apply(test_regression, dir = dir, target = target, units = units)

        storm["Calculated" + target + "_bool"] = storm["Calculated" + target].isnull()

        storm[target] = np.where(((storm["ZScore_bool"] == True) & (storm["Calculated" + target + "_bool"] == True)),
            np.nan, storm[target])

        storm[target] = np.where(storm[target] < 0,
                                  storm.groupby(storm["isStorm"])[target].transform(np.median), storm[target])

        storm[target] = storm[target].interpolate(method = "linear")

        combined_df = pd.concat([other, base, storm]).sort_index()
        combined_df.columns.name = "After"
        visualizeDataChange([before_df, combined_df], target, name, units)

        combined_df.to_excel(os.path.join(
            output_directory, df.columns.name + "_withRegression_QAQC.xlsx"))


def moving_zscore(df, target):
    return pd.DataFrame((df[target] - df[target].ewm(alpha = 0.05, ignore_na = True).mean())/df[target].ewm(alpha = 0.05, ignore_na = True).std())

def moving_ewma(df, target):
    return pd.DataFrame(df[target].ewm(alpha = 0.05, ignore_na = True).mean())

def moving_ewmstd(df, target):
    return pd.DataFrame(df[target].ewm(alpha = 0.05, ignore_na = True).std())


def visualizeDataChange(dfs, target_variable, name, units):
    cmap = ListedColormap(['gray', 'white'])

    patches = [Patch(facecolor="gray", edgecolor="b", label="StormFlow"), Patch(
        facecolor="white", edgecolor="b", label="BaseFlow")]
    lines = [Line2D([0], [0], color="orange", lw=6, label="Before", linestyle = "dotted"),
             Line2D([0], [0], color="#66CAD1", lw=6, label="After", linestyle = "dashed"),
             Line2D([0], [0], color="#EE6055", lw=6, label="Moving 1-day\nAverage, Before"),
             Line2D([0], [0], color="#070600", lw=6, label="Moving 1-day\nAverage, After")]
    markers = [Line2D([], [], color="#465775", marker="*",
                      linestyle="None", markersize=50, label="Change in Value")]

    fig = plt.figure(figsize=[35, 20])
    ax = fig.add_subplot(111)
    ax2 = ax.twinx()


    if name == "CSW_2020":
        ax.set_ylim(0, 300)
        ax2.set_ylim(0, 300)
        ax.set_xlim([dfs[1].index.min(), dfs[1].index.max()])

    if name == "Commons_2020":
        ax.set_ylim(-10, 60)
        ax2.set_ylim(-10, 60)
        ax.set_xlim([dfs[1].index.min(), dfs[1].index.max()])

    for df in dfs:
        if df.columns.name == "Before":
            ax.plot(df[target_variable], alpha=0.6, lw = 2, label=target_variable + " " + df.columns.name, color="orange", linestyle = "dotted")
            ax.plot(df[target_variable].rolling("1D").mean(), lw = 6, alpha=1.0, label=target_variable + " " + df.columns.name, color="#EE6055")


        if df.columns.name == "After":
            ax.plot(df[target_variable], alpha=0.6, lw = 2, label=target_variable + " " + df.columns.name, color="#66CAD1", linestyle = "dashed")
            ax.plot(df[target_variable].rolling("1D").mean(), lw = 2, alpha=1.0, label=target_variable + " " + df.columns.name, color="#070600")
            ax.pcolorfast(ax.get_xlim(), ax.get_ylim(),
                            df["isStormCopy"].values[np.newaxis],
                            cmap=cmap, alpha=0.4)

    ax2.scatter(dfs[0].index.values, dfs[0][target_variable].subtract(
        dfs[1][target_variable]), color="#465775", marker="*", s=50)

    ax.set_xlabel("DateTime (Year-Month)", labelpad=20)
    ax.set_ylabel(target_variable + " (" + units + ")", labelpad=25)
    ax2.set_ylabel(target_variable + " Change (Before - After)",
                   rotation=270, labelpad=65)
    ax.set_yscale("symlog")
    ax2.set_yscale("symlog")
    ax.xaxis.set_major_locator(mdates.MonthLocator(interval=1))
    ax.tick_params(axis = "both", labelrotation=45, direction = "out", length = 15, width = 2)
    ax2.tick_params(axis = "both", labelrotation=45, direction = "out", length = 15, width = 2)

    ax.legend(handles=patches + lines + markers,
              bbox_to_anchor=(1.10, 1), loc='upper left')
    plt.tight_layout()
    plt.savefig(os.path.join(scatter_directory, name + "_" + target_variable
                             + "_withRegression_BeforeAfterVisualization.jpg"), transparent=True)
    plt.close()


def plot_regression(calculated, original, cumulativeRain, rsq, equation, name, dir, target, units):
    fig = plt.figure(figsize=[35, 25])
    fig.text(0.05, 0.02, "r-squared = " + str(rsq) + "\n"
             + "Fit Equation = " + equation, fontsize=50)
    ax = fig.add_subplot(111)
    ax.set_xlim([calculated.index.min(), calculated.index.max()])
    ax.scatter(original.index, original, color="k", alpha=0.5)
    ax.plot(calculated.index, calculated)
    ax.tick_params(axis = "both", labelrotation=45, direction = "out", length = 15, width = 2)
    ax.set_xlabel("Datetime")
    ax.xaxis.set_major_locator(mdates.MinuteLocator(0))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d %H:%M"))
    ax.fmt_xdata = mdates.DateFormatter("%Y-%m-%d %H:%M")
    fig.autofmt_xdate(rotation = 20)
    ax.set_ylabel(target + " (" + units + ")")
    plt.savefig(os.path.join(dir,
                             name + "_regression.jpg"), transparent=True)
    plt.close()


correctData("CSW_2020.xlsx", ["Datetime", "Velocity", "Flow", "Rain"], "Velocity", "ft/s", csw_regression)
correctData("Commons_2020.xlsx", ["Datetime", "Depth", "Velocity", "Flow", "Rain"], "Depth", "in", commons_regression)
