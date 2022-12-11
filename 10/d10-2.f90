program d10_2
  implicit none

  integer, parameter :: pixels_per_row = 40
  character(5), parameter :: formstr = "(40a)"

  integer, parameter :: max_line_len = 1024

  integer :: fd, iostat
  character(max_line_len) :: buffer
  integer :: xx, addxval, ncycles, totalcycles, strengthsum, row, col
  character :: rowpixels(0 : pixels_per_row - 1)
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
      row = (totalcycles - 1) / pixels_per_row
      col = modulo(totalcycles - 1, pixels_per_row)
      if (abs(xx - col) <= 1) then
        rowpixels(col) = "#"
      else
        rowpixels(col) = "."
      endif
      if (col == pixels_per_row - 1) print formstr, rowpixels
    end do
    xx = xx + addxval
  end do
  close(fd)

end program d10_2
