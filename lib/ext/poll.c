/* poll(2), which OCaml's Unix does not expose. `Unix.select` stops at
   FD_SETSIZE (1024 on most systems), and a descriptor past it is undefined
   behaviour rather than an error.

   `fds` and `events` are arrays of equal length; an event is 1 for
   readable, 2 for writable, 3 for both. The answer is an array of the same
   length that is nonzero where the descriptor is ready. Hang-up and error
   count as ready, so the reader finds out what happened by reading.

   A signal ends the wait early and answers with nothing ready. */

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <poll.h>
#include <stdlib.h>

CAMLprim value wand_poll(value v_fds, value v_events, value v_timeout)
{
  CAMLparam3(v_fds, v_events, v_timeout);
  CAMLlocal1(result);
  mlsize_t n = Wosize_val(v_fds);
  int timeout = Int_val(v_timeout);
  struct pollfd *pfds = NULL;
  int rc, err;

  if (n > 0) {
    pfds = malloc(n * sizeof(struct pollfd));
    if (pfds == NULL) caml_raise_out_of_memory();
  }
  for (mlsize_t i = 0; i < n; i++) {
    int ev = Int_val(Field(v_events, i));
    pfds[i].fd = Int_val(Field(v_fds, i));
    pfds[i].events = ((ev & 1) ? POLLIN : 0) | ((ev & 2) ? POLLOUT : 0);
    pfds[i].revents = 0;
  }

  caml_enter_blocking_section();
  rc = poll(pfds, (nfds_t)n, timeout);
  err = errno;
  caml_leave_blocking_section();

  if (rc < 0 && err != EINTR) {
    free(pfds);
    caml_unix_error(err, "poll", Nothing);
  }

  result = caml_alloc(n, 0);
  for (mlsize_t i = 0; i < n; i++) {
    int ready = rc > 0 && pfds[i].revents != 0;
    Store_field(result, i, Val_bool(ready));
  }
  free(pfds);
  CAMLreturn(result);
}
