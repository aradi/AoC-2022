! Note: this solution assumes, that each directory is visited only once and the files in each
! directory are listed only once.
!
program d07_1
  implicit none

  integer, parameter :: size_limit = 100000

  integer, parameter :: max_line_len = 100

  integer, allocatable :: dirsizes(:), pathinds(:)
  integer :: fd, iostat, filesize, dirsizesum
  character(max_line_len) :: buffer, filename

  dirsizes = [0]
  pathinds =  [1]
  open(newunit=fd, file="input.dat", action="read")
  do
    read(fd, "(a)", iostat=iostat) buffer
    if (iostat /= 0 .or. len_trim(buffer) < 4) exit
    if (buffer(1:4) == "$ cd") then
      select case (trim(buffer(6:)))
      case ("..")
        pathinds = pathinds(1 : size(pathinds) - 1)
      case ("/")
        pathinds = [pathinds(1 : 1)]
      case default
        dirsizes = [dirsizes, 0]
        pathinds = [pathinds, size(dirsizes)]
      end select
    else if (buffer(1:4) /= "dir " .and. buffer(1:4) /= "$ ls") then
      read(buffer, *) filesize, filename
      dirsizes(pathinds) = dirsizes(pathinds) + filesize
    end if
  end do
  close(fd)

  dirsizesum = sum(pack(dirsizes, dirsizes <= size_limit))
  print "(a, i0)", "Minimal directory size to delete: ", dirsizesum

end program d07_1
