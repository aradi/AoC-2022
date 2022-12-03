program d03_2
  implicit none

  integer :: fd, iostat
  character(100) :: buffer
  logical :: found(52), triple_candidate(52)
  integer, allocatable :: priorities(:)
  integer :: total_priority
  integer :: ii

  total_priority = 0
  open(newunit=fd, file="input.dat", action="read")
  file_reader: do
    triple_candidate(:) = .true.
    do ii = 1, 3
      read(fd, *, iostat=iostat) buffer
      if (iostat /= 0) exit file_reader
      priorities = iachar_to_priority(iachar(transfer(trim(buffer), ['a'])))
      found(:) = .false.
      found(priorities) = .true.
      triple_candidate = triple_candidate .and. found
    end do
    total_priority = total_priority + findloc(triple_candidate, .true., dim=1)
  end do file_reader
  close(fd)
  print "(a, i0)", "Sum of group triple priorities: ", total_priority

contains

  elemental function iachar_to_priority(charcode) result(priority)
    integer, intent(in) :: charcode
    integer :: priority

    if (charcode >= iachar("a")) then
      priority = charcode - iachar("a") + 1
    else
      priority = charcode - iachar("A") + 27
    end if

  end function iachar_to_priority

end program d03_2
