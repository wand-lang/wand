## 0.80.5 - 2026-09-19

One filesystem fix.

### `FS.glob` answered nothing for an absolute pattern

The walk always started at the working directory, so a pattern that names
its own directory could never match. It came back empty and said nothing.

```
FS.glob /var/log/*.log

-- before    -- now
[]           [/var/log/displaypolicyd.stdout.log, /var/log/fsck_apfs.log]
```

An absolute pattern now starts its walk where the pattern says.
`FS.glob_in` is unchanged: it always searches the directory given to it,
and an absolute pattern there simply matches whole paths.
