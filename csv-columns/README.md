# csv-columns

The columns of every record of a CSV or TSV file rearranged as awk's `print`
does. Keeping, deleting, showing and hiding records by a column is
Tuatara's own: `C-x / k`, `C-x / d`, `C-x / f` and `C-x / h`.

Columns are counted from 0, as [csvdt](https://github.com/yardquit/csvdt)
counts them: `$0` is the first column.

## Loading it

In `~/.config/tuatara/init.lisp`, with this repository named as a source:

```lisp
(load-package "csv-columns" :source "tuatara-packages")
```

## Commands

In a CSV or TSV buffer, under `C-c r`:

| Key | Command | What it does |
|---|---|---|
| `C-c r a` | `csv-rearrange-columns` | Rearranges the columns of every line as awk's `print` would. |

### Rearranging columns

`C-c r a` asks for the columns as awk's `print` writes them, counted from 0:

```
$0,$1,$3,$4"cent","hello"
```

on five columns leaves out the third (`$2`), joins `cent` to the value of the
fifth and adds a sixth column, now the fifth, holding `hello`:

```
a,b,c,d,e   ->   a,b,d,ecent,hello
```

- `$N` is column N from 0, `$NF` the last column of each record, `$*` the
  whole record: alone, its columns stay its own; joined to other parts, it is
  one column holding the record's text as written.
- `"text"` is text as it is (`\"` a quote, `\\` a backslash). Parts side by
  side are joined: `$4"cent"`, `$2" "$0`.
- A column of one field alone is written exactly as it stood, its quotes
  kept. A column joined from parts is quoted when it needs to be (it holds
  the separator, a quote or a line end, or begins or ends with a space), a
  quote inside doubled.
- A column a record lacks is empty, as in awk.
- Every line is rearranged, the header too, as awk would; with a selection,
  only the lines it touches. A line that comes out as it was is left alone,
  and not counted as rearranged.

## Large files

Each command is one step to undo (`u`). A large file is done in the
background on every core, `P` lit in the mode line (`C-x j` shows how far it
has got), the editor answering meanwhile. On an 11.5 GB file of 100 million records,
rearranging every record as above took 30 seconds.

A record is a line, as Tuatara's CSV mode reads one: a quoted field does not
run on over a line end.
