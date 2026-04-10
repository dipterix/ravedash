# Create a random temporary file path for current session

Create a random temporary file path for current session

## Usage

``` r
temp_file(
  pattern = "file",
  fileext = "",
  persist = c("process", "app-session", "package-cache")
)

temp_dir(check = FALSE, persist = c("process", "app-session", "package-cache"))
```

## Arguments

- pattern, fileext:

  see [`tempfile`](https://rdrr.io/r/base/tempfile.html)

- persist:

  persist level, choices are `'app-session'`, `'package-cache'`, and
  `'process'`; see 'Details'. 'RAVE' application session, default),
  `'package-cache'` (package-level cache directory)

- check:

  whether to create the temporary directory

## Value

A file or a directory path to persist temporary data cache

## Details

R default [`tempdir`](https://rdrr.io/r/base/tempfile.html) usually gets
removed once the R process ends. This behavior might not meet all the
needs for 'RAVE' modules. For example, some data are 'RAVE'
session-based, like current or last visited subject, project, or state
data (like bookmarks, configurations). This session-based information
will be useful when launching the same 'RAVE' instance next time, hence
should not be removed when users close R. Other data, such as
subject-related, or package-related should last even longer. These types
of data may be cache of subject power, package-generated color schemes,
often irrelevant from R or 'RAVE' sessions, and can be shared across
different 'RAVE' instances.

The default scheme is `persist='process'`. Under this mode, this
function behaves the same as
[`tempfile`](https://rdrr.io/r/base/tempfile.html). To store data in
'RAVE' session-based manner, please use `persist='app-session'`. The
actual path will be inside of 'RAVE' session folder, hence this option
is valid only if 'RAVE' instance is running. When 'RAVE' instance is not
running, the result falls back to `persist='process'`. When
`persist='process'`, To cache larger and session-irrelevant data, use
`'package-cache'`.

The 'RAVE' session and package cache are not cleared even when R process
ends. Users need to clean the data by themselves. See
[`remove_session`](https://dipterix.org/ravedash/reference/rave-session.md)
or
[`remove_all_sessions`](https://dipterix.org/ravedash/reference/rave-session.md)
about removing session-based folders, or
[`clear_cached_files`](http://rave.wiki/ravecore/reference/cache_path.md)
to remove package-based cache.

## Examples

``` r
temp_dir()
#> [1] "/var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpN4CGME"
temp_dir(persist = "package-cache")
#> [1] "/Users/runner/rave_data/cache_dir//package-cache"
```
