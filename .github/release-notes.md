## 0.91.1 - 2026-09-28

A long list of values no longer hides the value that was wrong.

### Fixed

- **A long list of values no longer hides the value that was wrong.** When
  a document has a value that an enum does not have, the error lists the
  values the enum has. For a large enum, the value that was received was
  lost at the end of a very long line. Now, when there are more than five
  values, the error shows the count and the first five:

  ```
  .stopSignal: expected one of 65 values (SIGABRT, SIGALRM, SIGBUS, SIGCHLD, SIGCLD, …), got "SIGNEW"
  ```

  An enum with five values or fewer still lists all of them.
