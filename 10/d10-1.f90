program d10_1
  implicit none

  integer, parameter :: max_line_len = 1024

  integer :: fd, iostat
  character(max_line_len) :: buffer
  integer :: xx, addxval, ncycles, totalcycles, strengthsum
  integer :: icycle

  totalcycles = 0
  strengthsum = 0
  xx = 1
  open(newunit=fd, file="input.dat", action="read")
  do
    read(fd, "(a)", iostat=iostat) buffer
    if (iostat /= 0) exit
    if (buffer(1:4) == "addx") then
      ncycles = 2
      read(buffer(5:), *) addxval
    else
      ncycles = 1
      addxval = 0
    end if
    do icycle = 1, ncycles
      totalcycles = totalcycles + 1
      if (modulo(totalcycles, 40) == 20) strengthsum = strengthsum + totalcycles * xx
    end do
    xx = xx + addxval
  end do
  close(fd)

print "(a, i0)",  "Sum of signal strengths: ", strengthsum

end program d10_1
