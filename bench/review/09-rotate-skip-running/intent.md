Skip the rotation while app-server is running (checked with pgrep), so it never writes into a file we just renamed. Also keep two old generations instead of one.
