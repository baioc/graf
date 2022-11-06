# graf

![release](https://img.shields.io/github/v/release/baioc/graf?sort=semver)

Plot discretized line charts in your terminal.

![examples](https://user-images.githubusercontent.com/27034173/200157439-a43b3256-ea68-46b3-85f2-0902fdb3069e.gif)


## Usage

```
Usage: graf [OPTION]...

Options:
    -f, --file FILE      If FILE is - or not specified, read from stdin.
    -b, --batch          Only plot on end of input (as opposed to real-time).
    -t, --title TITLE    Display TITLE on top of the plotted chart.
    -s, --stats          Show statistics, which are hidden by default.
    -c, --color COLOR    Color to plot the line in. See options below.
    -n, --lines N        Plot N <= 8 parallel lines. Default is inferred.
    -p, --permissive     Ignore badly-formatted lines instead of halting.
    -r, --range MIN:MAX  Fix plot bounds instead of choosing them dynamically.
    -d, --digits DIGITS  Ensure at least DIGITS significant digits are printed.
    -W, --width WIDTH    Maximum TUI width. Defaults to terminal width.
    -H, --height HEIGHT  Maximum TUI height. Defaults to terminal height.
    -h, --help           Print this help message and exit the program.

Notes:
    - A single quantization range is used for the entire chart, so make sure
    timeseries are similarly scaled when there are more than one.
    - When the chart includes multiple lines, a default title is added in order
    to help disambiguate them; furthermore, each timeseries is colored differently.
    - Options '--title' and '--color' can be specified multiple times, in which
    case they will be applied to each timeseries in a corresponding position.
```


## Examples

Notice that, in order to plot data as it is streamed in real time, we either turn buffering off completely or flush on every newline.

```sh
$ ping example.com \
| stdbuf -oL tail -n +2 \
| sed -u 's/.*time=\(.*\) ms/\1/' \
| graf -t "ping (ms)" --stats -W 80 -H 24
```

```sh
$ vmstat -n 1 \
| stdbuf -oL tr -s ' ' \
| stdbuf -oL cut -d ' ' -f 14,15,16 \
| graf -n 3 --permissive --range 0:100 -t "user%" -c 'y' -t "system%" -c 'r' -t "idle%" -c 'g'
```

```sh
$ while curl -sS -L -w '\n' http://api.coincap.io/v2/rates/bitcoin; do sleep 1; done \
| sed -u 's/.*"rateUsd":"\([^"]*\)".*/\1/' \
| xargs -L 1 python3 -c 'import sys; print(float(sys.argv[1]) * 1e-8)' \
| graf -t "Satoshi price (U\$D)" --digits 15 --color cyan
```

```sh
$ python3 -c 'from math import *; [print(sin(4*pi * p/100), cos(4*pi * p/100), sep="\t") for p in range(0, 100)]' > tmp.tsv \
&& graf -f tmp.tsv --batch; echo \
&& rm tmp.tsv
```

![sincos](https://user-images.githubusercontent.com/27034173/200157085-7a2ccf83-5966-4f7d-8b50-3d735dd4e188.png)
