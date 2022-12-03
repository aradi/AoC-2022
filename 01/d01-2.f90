program d01_2
  implicit none

  integer :: fd, iostat, cal, curcal, ical, imaxcal, maxcal, itop
  integer, allocatable :: cals(:)
  integer :: tops(3)
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

  tops(:) = 0
  do ical = 1, size(cals)
    itop = findloc(cals(ical) > tops, .true., dim=1)
    if (itop > 0) then
      tops(itop + 1 : size(tops)) = tops(itop : size(tops) - 1)
      tops(itop) = cals(ical)
    end if
  end do
  print "(a, i0, a, i0)", "Sum of top ", size(tops), " calories: ", sum(tops)

end program d01_2
