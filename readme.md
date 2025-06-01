# soon

A basic CLI tool for tracking upcoming events:

```bash
Usage: soon [--index <Numeric>] [--grouping <Rainbow>] [add | del | sort]

Simple CLI tracker for upcoming events

Options and flags:
    --help
        Display this help text.
    --index <Numeric>, -i <Numeric>
        alphabet to use when indexing
    --grouping <Rainbow>, -g <Rainbow>
        how to group events when printing

Subcommands:
    add
        add an event
    del
        delete an event
    sort
        sort the stored events
```

Run with no arguments to show all upcoming event timers, run the `add` subcommand to add one
