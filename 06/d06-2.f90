program d06_2
  implicit none

  integer, parameter :: markerlen = 14

  character :: cut(markerlen)
  integer :: fd
  integer :: ipos, ii

  open(newunit=fd, file="input.dat", form="formatted", access="stream")
  read(fd, "(*(a))", advance="no") cut(1 : markerlen - 1)
  ipos = markerlen
  do
    read(fd, "(a)", advance="no") cut(modulo(ipos - 1, markerlen) + 1)
    do ii = 1, markerlen
      if (count(cut == cut(ii)) > 1) exit
    end do
    if (ii > markerlen) exit
    ipos = ipos + 1
  end do
  close(fd)

  print "(a, i0)", "End of first marker: ", ipos

end program d06_2
