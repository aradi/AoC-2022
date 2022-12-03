program d03_1
  implicit none

  integer :: fd, iostat
  character(100) :: buffer
  logical :: found1(52), found2(52)
  integer, allocatable :: priorities(:)
  integer :: total_priority

  open(newunit=fd, file="input.dat", action="read")
  total_priority = 0
  do
    read(fd, *, iostat=iostat) buffer
    if (iostat /= 0) exit
    priorities = iachar(transfer(trim(buffer), ['a']))
    where (priorities >= iachar("a"))
      priorities = priorities - iachar("a") + 1
    elsewhere
      priorities = priorities - iachar("A") + 27
    end where
    found1(:) = .false.
    found1(priorities(: size(priorities) / 2)) = .true.
    found2(:) = .false.
    found2(priorities(size(priorities) / 2 + 1 :)) = .true.
    print *, "PRIO:", findloc(found1 .and. found2, .true., dim=1)
    total_priority = total_priority + findloc(found1 .and. found2, .true., dim=1)
  end do
  close(fd)
  print "(a, i0)", "Total priority of double items: ", total_priority

end program d03_1
