import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def gen_plot(df, means, _x, _y):
    ax = df.plot.scatter(x=_x, y=_y, s=4.0)
    return means.plot(x=_x, y=_y, ax=ax, color='red')


class Plot:
    def __init__(self,name: str, color: str, values: pd.Series):
        self.name = name
        self.values = values
        self.color = color
    name: str
    color: str
    values: pd.Series


class PlotBundle:
    title: str
    filename: str
    plots: list[Plot]
    def __init__(self, title: str, filename: str, plots: list[Plot]) -> None:
        self.title = title
        self.filename = filename
        self.plots = plots


df = pd.read_csv(r'data.csv')
means = pd.read_csv(r'means.csv')
# for i in ['B', 'U', 'L', 'C', 'D', 'D-C']:
#     pl = gen_plot(df,means,'n',i)
#     pl.figure.savefig("output"+i+".png")
#
vars = ["B", "U", "L", "C", "D", "D-C"]

for bundle in vars:
    plt.clf()
    plt.plot(df["n"], df[bundle], "b.", markersize = 0.6, label = "Wyniki poszczegolnych prob")
    plt.plot(means["n"], means[bundle], "ro", markersize = 2, label = "Wartosc srednia")
    plt.title(bundle)
    plt.xlabel("n")
    plt.legend()
    plt.savefig("plots/plot"+bundle+".png")

N = means["n"]
Bn = means["B"]
Un = means["U"]
Ln = means["L"]
Cn = means["C"]
Dn = means["D"]


bundles = [
        PlotBundle("b(n)/n | b(n)/sqrt(n)",
                   "bnfunc",
                   [
                       Plot("b(n)/n", "red", Bn / N),
                       Plot("b(n)/sqrt(n)", "blue", Bn / N**1/2),
                   ]),
        PlotBundle("u(n)/n",
                   "unfunc",
                   [
                       Plot("u(n)/n", "red", Un / N),
                   ]),
        PlotBundle("l(n)/ln n",
                   "lnfunc1",
                   [
                       Plot("l(n)/ln n", "blue", Ln / np.log2(N)),
                   ]),
        PlotBundle("l(n) / (ln n) / ln ln n",
                   "lnfunc2",
                   [
                       Plot("l(n)/ln n/ln ln n", "red", Ln / (np.log2(N) / np.log2(np.log2(N)))),
                   ]),
        PlotBundle("l(n) / ln ln n",
                   "lnfunc3",
                   [
                       Plot("l(n)/ln ln n", "green", Ln / np.log2(np.log2(N))),
                   ]),
        PlotBundle("c(n)/n",
                   "cnfunc1",
                   [
                       Plot("c(n)/n", "red", Cn / N),
                   ]),
        PlotBundle("c(n)/nln n",
                   "cnfunc2",
                   [
                       Plot("c(n)/nln n", "blue", Cn / (N * np.log2(N))),
                   ]),
        PlotBundle("c(n)/n^2",
                   "cnfunc3",
                   [
                       Plot("c(n)/n^2", "green", Cn / (N * N)),
                   ]),

        PlotBundle("d(n)/n",
                   "dnfunc1",
                   [
                       Plot("d(n)/n", "red", Dn / N),
                   ]),

        PlotBundle("d(n)/nln n",
                   "dnfunc2",
                   [
                       Plot("d(n)/nln n", "blue", Dn / (N * np.log2(N))),
                   ]),
        PlotBundle("d(n)/n^2",
                   "dnfunc3",
                   [
                       Plot("d(n)/n^2", "green", Dn / (N * N)),
                   ]),

        PlotBundle("(d(n)-c(n))/n",
                   "dn-cnfunc1",
                   [
                       Plot("(d(n) - c(n))/n", "red", (Dn - Cn) / N),
                   ]),
        PlotBundle("(d(n)-c(n))/nln n",
                   "dn-cnfunc2",
                   [
                       Plot("(d(n) - c(n))/nln n", "blue", (Dn - Cn) / (N * np.log2(N))),
                   ]),
        PlotBundle("(d(n)-c(n))/n ln ln n",
                   "dn-cnfunc3",
                   [
                       Plot("(d(n) - c(n))/nln ln n", "green", (Dn - Cn) / (N * np.log2(np.log2(N)))),
                   ]),

        ]


for bundle in bundles:
    plt.clf()
    for plot in bundle.plots:
        plt.plot(means["n"], plot.values, color=plot.color, marker = '.', linestyle='none', markersize = 1.5, label = "Wartosci funkcji " + plot.name)
    plt.title(bundle.title)
    plt.xlabel("n")
    plt.legend()
    plt.savefig("plots/plot"+bundle.filename+".png")
