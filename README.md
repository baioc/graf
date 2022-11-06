# graf

Plot discretized line charts in your terminal.


## Usage

```
graf [OPTION]...

Options:
    -h, --help           Print this help message and exit the program.
    -f, --file FILE      If FILE is - or not specified, read from stdin.
    -n, --lines N        Plot N <= 8 parallel lines. Default is inferred from 1st input.
    -t, --title TITLE    Display TITLE on top of the plotted chart.
    -c, --color COLOR    Color to plot the line in. See options below.
    -s, --stats          Show statistics, which are hidden by default.
    -r, --range MIN:MAX  Fix plot bounds instead of choosing them dynamically.
    -d, --digits DIGITS  Ensure at least DIGITS significant digits are printed.
    -W, --width WIDTH    Maximum TUI width. Defaults to terminal width.
    -H, --height HEIGHT  Maximum TUI height. Defaults to terminal height.

Notes:
    - A single quantization range is used for the entire chart, so make sure
    timeseries are similarly scaled when there are more than one.
    - When the chart includes multiple lines, a default title is added in order
    to help disambiguate them; furthermore, each line is colored differently.
    - Statistics are shown for each different timeseries.
    - Options '--title' and '--color' can be specified multiple times, in which
    case they will be applied to each timeseries in a corresponding position.
```


## Examples

```sh
$ ping example.com \
| stdbuf -oL tail -n +2 \
| sed -u 's/.*time=\(.*\) ms/\1/' \
| graf -t "ping (ms)" --stats -W 80 -H 24
```

```sh
$ vmstat -n 1 \
| stdbuf -oL tail -n +3 \
| stdbuf -oL tr -s ' ' \
| stdbuf -oL cut -d ' ' -f 14,15,16 \
| graf -n 3 --range 0:100 -t "user%" -c 'y' -t "system%" -c 'r' -t "idle%" -c 'g' -s
```

```sh
$ while curl -sS -L -w '\n' http://api.coincap.io/v2/rates/bitcoin; do sleep 1; done \
| sed -u 's/.*"rateUsd":"\([^"]*\)".*/\1/' \
| xargs -L 1 python3 -c 'import sys; print(float(sys.argv[1]) * 1e-8)' \
| graf -t "Satoshi price (U\$D)" --digits 15 --color cyan -f -
```
