program d07_2
    implicit none

    integer, parameter :: disk_size = 70000000
    integer, parameter :: space_needed = 30000000

    integer, parameter :: max_line_len = 100

    integer, allocatable :: dirsizes(:), pathinds(:)
    integer :: fd, iostat, filesize, tobefreed, minsize
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

    tobefreed = space_needed - (disk_size - dirsizes(1))
    minsize = minval(pack(dirsizes, dirsizes >= tobefreed))
    print "(a, i0)", "Minimal directory size to delete: ", minsize

  end program d07_2
