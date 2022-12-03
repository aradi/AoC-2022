program d01_1
  implicit none

  integer :: fd, iostat, imaxcal, maxcal, curcal, cal
  integer, allocatable :: cals(:)
  character(100) :: buffer

  curcal = 0
  allocate(cals(0))
  open(newunit=fd, file="input.dat", form="formatted", action="read")
  do
    read(fd, "(a)", iostat=iostat) buffer
    if (iostat /= 0) exit
    if (len_trim(buffer) > 0) then
      read(buffer, *) cal
      curcal = curcal + cal
    else
      cals = [cals, curcal]
      curcal = 0
    end if
  end do
  close(fd)
  imaxcal = maxloc(cals, dim=1)
  maxcal = cals(imaxcal)
  print "(a, i0, a, i0)", "Elf with most calories: ", imaxcal, " calories: ", maxcal

end program d01_1
